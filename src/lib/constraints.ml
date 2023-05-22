module L = Sec

let config = [("model", "true"); ("proof", "false")]

type label_var = int

type solver_state_type = {
  z3_context : Z3.context; 
  z3_solver : Z3.Solver.solver;
  label_sort : Z3.Sort.sort; 
  public_literal : label_var;
  secret_literal : label_var;
  next_label_var : label_var;
  label_var_map : (label_var, Z3.Expr.expr) Hashtbl.t;
}
exception NoExpressionForLabelVariableError of label_var
exception ExpressionNotLabelValue of Z3.Expr.expr
exception NoModelError

let new_solver_state () =
  let z3_context = Z3.mk_context config in 
  let z3_solver = Z3.Solver.mk_solver z3_context None in 
  let label_sort = Z3.Enumeration.mk_sort_s z3_context "label" [L.str_l L.Public; L.str_l L.Secret] in
  let public_literal_expr = Z3.Expr.mk_const_f z3_context (Z3.Enumeration.get_const_decl label_sort 0) in 
  let secret_literal_expr = Z3.Expr.mk_const_f z3_context (Z3.Enumeration.get_const_decl label_sort 1) in
  let public_literal = 0 in 
  let secret_literal = 1 in
  let next_label_var = 2 in 
  let label_var_map = Hashtbl.create 100 in 
  let _ = Hashtbl.add label_var_map public_literal public_literal_expr in 
  let _ = Hashtbl.add label_var_map secret_literal secret_literal_expr in 
  {z3_context; z3_solver; label_sort; public_literal; secret_literal; next_label_var; label_var_map}
  
let get_label_tester_func_decl label_sort label =
  Z3.Enumeration.get_tester_decl label_sort @@ match label with 
  | L.Public -> 0
  | L.Secret -> 1

let get_label_value_from_expr model label_sort expr =
  let has_label e l = 
    match Z3.Model.eval model (Z3.FuncDecl.apply (get_label_tester_func_decl label_sort l) [e]) true with 
    | None -> false 
    | Some bexpr -> Z3.Boolean.is_true bexpr
  in 
  if has_label expr L.Public then 
    L.Public 
  else if has_label expr L.Secret then 
    L.Secret
  else
    raise (ExpressionNotLabelValue expr)  

let get_label_expr_for_label_var label_var_map l = 
  match Hashtbl.find_opt label_var_map l with 
  | None -> raise (NoExpressionForLabelVariableError l)
  | Some lexpr -> lexpr

let make_fresh_label_var ctx label_sort = 
  Z3.Expr.mk_fresh_const ctx "l" label_sort

let make_leq_constraint ctx label_sort l1 l2 = 
  let is_public = get_label_tester_func_decl label_sort L.Public in
  let is_secret = get_label_tester_func_decl label_sort L.Secret in
  let l1_is_public = Z3.Expr.mk_app ctx is_public [l1] in 
  let l2_is_secret = Z3.Expr.mk_app ctx is_secret [l2] in
  Z3.Boolean.mk_or ctx [l1_is_public; l2_is_secret]

let make_eq_constraint ctx l1 l2 = 
  Z3.Boolean.mk_eq ctx l1 l2

let make_lub_expr ctx label_sort secret_literal_expr l1 l2 = 
  let is_public = get_label_tester_func_decl label_sort L.Public in
  let l1_is_public = Z3.Expr.mk_app ctx is_public [l1] in
  Z3.Boolean.mk_ite ctx l1_is_public l2 secret_literal_expr

let is_feasible solver = 
  match Z3.Solver.check solver [] with 
  | Z3.Solver.SATISFIABLE -> true
  | Z3.Solver.UNKNOWN | Z3.Solver.UNSATISFIABLE -> false

let get_model solver = 
  let _ = is_feasible solver in 
  match Z3.Solver.get_model solver with 
  | None -> raise NoModelError
  | Some model -> model

  module type MONAD =
  sig
   type 'a t
   val return : 'a -> 'a t
   val bind  : 'a t -> ('a -> 'b t) -> 'b t
  end
  
  module type STATEM = sig
    type state
    include MONAD
    val get : state t
    val put : state -> unit t
    val runState : 'a t -> state -> state * 'a
  end
  
  module StateM (S : sig type t end)
    : STATEM with type state = S.t = struct
    type state = S.t
    type 'a t = state -> state * 'a
    let return v s = (s, v)
    let bind m k s = let s', a = m s in k a s'
    let get s = (s, s)
    let put s' _ = (s', ())
    let runState m init = m init
  end

module SolverState = StateM (struct type t = solver_state_type end)

type constraint_type = Eq | Leq

module Solver = struct
  open SolverState
  let (>>=) = SolverState.bind

  let fresh_label_var  = 
    get >>= fun ({z3_context; label_sort; next_label_var; label_var_map = m; _} as s) ->
    let l = next_label_var in 
    let lexpr = make_fresh_label_var z3_context label_sort in 
    let _ = Hashtbl.add m l lexpr in 
    put {s with next_label_var = next_label_var + 1} >>= fun () ->
    return l

  let assert_constraint ct l1 l2 = 
    get >>= fun {z3_context; z3_solver; label_sort; label_var_map = m; _} -> 
    let l1expr = get_label_expr_for_label_var m l1 in 
    let l2expr = get_label_expr_for_label_var m l2 in 
    let f = match ct with Eq -> make_eq_constraint z3_context | Leq -> make_leq_constraint z3_context label_sort in
    let _ = Z3.Solver.add z3_solver [f l1expr l2expr] in 
    return (is_feasible z3_solver)
    
  let lub l1 l2 =
    get >>= fun {z3_context; label_sort; secret_literal; label_var_map = m; _} -> 
    let l1expr = get_label_expr_for_label_var m l1 in 
    let l2expr = get_label_expr_for_label_var m l2 in 
    let sexpr = get_label_expr_for_label_var m secret_literal in
    fresh_label_var >>= fun l3 -> 
    let l3expr = make_lub_expr z3_context label_sort sexpr l1expr l2expr in 
    let _ = Hashtbl.add m l3 l3expr in 
    return l3 

  let get_value_of_variable l = 
    get >>= fun {z3_solver; label_sort; label_var_map; _} -> 
    let model = get_model z3_solver in
    let lexpr = get_label_expr_for_label_var label_var_map l in 
    return (get_label_value_from_expr model label_sort lexpr)

  let public =
    get >>= fun {public_literal; _} -> 
    return public_literal

  let secret =
    get >>= fun {secret_literal; _} ->
    return secret_literal 
end

let rec fold_left f acc xs = match xs with
| [] -> SolverState.return acc
| x :: xs' ->
  fold_left f (f acc x) xs' 

let rec fold_right f xs acc = match xs with
  | [] -> SolverState.return acc
  | x :: xs' ->
    SolverState.bind (fold_right f xs' acc) (f x) 

let map f xs = 
  let f' x acc = SolverState.bind (f x) (fun x' -> SolverState.return (x' :: acc)) in 
  fold_right f' xs []




