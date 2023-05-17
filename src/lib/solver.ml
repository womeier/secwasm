open Z3
open Sec

type label_var = string * Expr.expr 
type label_id = int
type label_map = (label_id, label_var) Hashtbl.t

type constraint_solver = {
  ctx : context;
  solver : Solver.solver;
  label_sort : Sort.sort;
  next_label_id : int;
  labels : label_map;
}

let tester label_sort label =
  Enumeration.get_tester_decl label_sort @@ match label with 
  | Public -> 0
  | Secret -> 1

let label_const label_sort label = 
  Enumeration.get_tester_decl label_sort @@ match label with 
  | Public -> 0
  | Secret -> 1


let init_solver = 
  let ctx = mk_context  [("model", "true"); ("proof", "false")] in
  let label_sort = Enumeration.mk_sort_s ctx "label" ["Public"; "Secret"] in
  let solver = Solver.mk_solver ctx None in 
  let next_label_id = 0 in 
  let labels = Hashtbl.create 1234 in 
  {
    ctx;
    solver;
    label_sort;
    next_label_id;
    labels;
  }

let leq ctx label_sort l1 l2 = 
  let is_public = tester label_sort Public in
  let is_secret = tester label_sort Secret in
  let l1_is_public = Expr.mk_app ctx is_public [l1] in 
  let l2_is_secret = Expr.mk_app ctx is_secret [l2] in
  Boolean.mk_or ctx [l1_is_public; l2_is_secret]

let lub ctx label_sort l1 l2 = 
  let is_public = tester label_sort Public in
  let l1_is_public = Expr.mk_app ctx is_public [l1] in
  let secret = label_const label_sort Secret in 
  Boolean.mk_ite ctx l1_is_public l2 (Expr.mk_app ctx secret []) 

let mk_fresh_label_var (cs : constraint_solver) (lid : label_id) = 
  let id = cs.next_label_id in 
  let l_name = "l" ^ (string_of_int id) in 
  let l = Expr.mk_const_s cs.ctx l_name cs.label_sort in 
  Hashtbl.add cs.labels lid (l_name, l);
  l, {cs with next_label_id = cs.next_label_id + 1}

let mk_eq_const cs v_id lbl = 
  let (lvar, cs') = mk_fresh_label_var cs v_id in 
  let lbl_fun = label_const cs.label_sort lbl in 
  Solver.add cs.solver [Boolean.mk_eq cs.ctx lvar (FuncDecl.apply lbl_fun [])];
  cs'

let mk_leq (cs : constraint_solver) v_id1 v_id2 = 
  let _, l1 = Hashtbl.find cs.labels v_id1 in 
  let _, l2 = Hashtbl.find cs.labels v_id2 in 
  let l1_leq_l2 = leq cs.ctx cs.label_sort l1 l2 in
  Solver.add cs.solver [l1_leq_l2];
  cs

let mk_lub (cs : constraint_solver) (lids : label_id list) (res_lid) =
  let f ((_, l1), cs) (_, l2) =
    let id = cs.next_label_id in 
    let l_name = "l" ^ (string_of_int id) in 
    let l3 = Expr.mk_const_s cs.ctx l_name cs.label_sort in 
    let l3_eq_lub_l1_l2 = Boolean.mk_eq cs.ctx l3 (lub cs.ctx cs.label_sort l1 l2) in 
    let _ = Solver.add cs.solver [l3_eq_lub_l1_l2] in 
    let cs' = {cs with next_label_id = id + 1} in 
    ((l_name, l3), cs')
  in 
  let labels = List.map (fun v_id -> Hashtbl.find cs.labels v_id) lids in 
  match labels with 
  | l1 :: (_l2 :: _) as labels' -> 
    let (l3, cs') = List.fold_left f (l1, cs) labels' in 
    Hashtbl.add cs'.labels res_lid l3;
    cs'
  | _ -> failwith "TODO: Err msg"

let check_constraints (cs : constraint_solver) = 
  match Z3.Solver.check cs.solver [] with 
  | Solver.SATISFIABLE -> true
  | Solver.UNKNOWN | Solver.UNSATISFIABLE -> false
