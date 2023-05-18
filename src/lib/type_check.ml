(*
   TODO:
   - Fix lattice to allow arbitrary lattice
   - Figure out how to pass around typing context and stack-of-stacks
*)

open Ast
open Sec

type context = {
  memory : wasm_memory option; (* number of memories *)
  func_types : fun_type list;
  globals : wasm_global list;
  locals : labeled_value_type list;
  labels : labeled_value_type list list;
  return : labeled_value_type list;
}

let empty_context =
  {
    memory = None;
    func_types = [];
    globals = [];
    locals = [];
    labels = [];
    return = [];
  }

type pc_type = SimpleLattice.t
type stack_type = labeled_value_type list
type stack_of_stacks_type = (stack_type * pc_type) list

(* ======= Helpers ============== *)

let split_at_index n l =
  let rec f n l acc =
    if n <= 0 then (acc, l)
    else
      match l with
      | [] -> failwith "split_at_index: list is empty!"
      | x :: xs -> f (n - 1) xs (x :: acc)
  in
  let fst, lst = f n l [] in
  (List.rev_append fst [], lst)

(* ======== Debugging ============*)

let rec print_g (g : stack_of_stacks_type) =
  match g with
  | [] -> "[]"
  | (st, pc) :: g' ->
      Printf.sprintf "(%s, %s) :: %s" (print_st st) (str_l pc) (print_g g')

and print_st (st : stack_type) =
  match st with
  | [] -> "[]"
  | { t; lbl } :: st' ->
      Printf.sprintf "{%s, %s} :: %s" (pp_type t) (str_l lbl) (print_st st')

let fail_g g = failwith (print_g g)
let fail_st st = failwith (print_st st)

(* ======= Notation ============= *)

let ( <> ) v1 v2 = SimpleLattice.lub v1 v2
let ( <<= ) v1 v2 = SimpleLattice.leq v1 v2

(* ======= Error handling ======= *)

exception NotImplemented of string
exception InternalError of string
exception TypingError of string
exception PrivacyViolation of string

let t_err0 msg = raise (TypingError msg)

let t_err2 msg (t1 : value_type) (t2 : value_type) =
  let error_msg = Printf.sprintf msg (pp_type t1) (pp_type t2) in
  raise (TypingError error_msg)

let t_err2i msg (i1 : int) (i2 : int) =
  let error_msg = Printf.sprintf msg i1 i2 in
  raise (TypingError error_msg)

let p_err3 msg l1 l2 l3 =
  let error_msg = Printf.sprintf msg (str_l l1) (str_l l2) (str_l l3) in
  raise (PrivacyViolation error_msg)

let p_err4 msg l1 l2 l3 l4 =
  let error_msg =
    Printf.sprintf msg (str_l l1) (str_l l2) (str_l l3) (str_l l4)
  in
  raise (PrivacyViolation error_msg)

(* error messages are checked in test-suite, don't inline *)

let err_drop = TypingError "drop expected 1 value on the stack"
let err_binop = TypingError "binop expected 2 values on the stack"

let err_globalset2 l1 l2 l3 =
  PrivacyViolation
    (Printf.sprintf "global.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
       (str_l l1) (str_l l2) (str_l l3))

let err_globalset3 = TypingError "global.set expected 1 value on the stack"

let err_globalset_imut =
  TypingError "global.set expected global var to be mutable"

let err_localset2 l1 l2 l3 =
  PrivacyViolation
    (Printf.sprintf "local.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
       (str_l l1) (str_l l2) (str_l l3))

let err_localset3 = TypingError "local.set expected 1 value on the stack"
let err_load_nomemory = TypingError "load expected memory in the context"
let err_load_addrexists = TypingError "load expected 1 value on the stack"
let err_store_nomemory = TypingError "store expected memory in the context"
let err_store_addrexists = TypingError "store expected 1 value on the stack"

let err_store2 l1 l2 l3 l4 =
  PrivacyViolation
    (Printf.sprintf
       "store expected pc ⊔ la ⊔ lv ⊑ lm but was pc=%s, la=%s, lv=%s, lm=%s"
       (str_l l1) (str_l l2) (str_l l3) (str_l l4))

let err_block1 i1 i2 =
  TypingError
    (Printf.sprintf "block needs %d values on the stack (found %d)" i1 i2)

let err_block2 i1 i2 =
  TypingError
    (Printf.sprintf "block must leave %d values on the stack (found %d)" i1 i2)

let err_block3 s1 s2 =
  TypingError
    (Printf.sprintf "block needs values with types ⊑ %s on the stack (found %s)"
       (print_st s1) (print_st s2))

let err_block4 s1 s2 =
  TypingError
    (Printf.sprintf
       "block must leave values with types ⊑ %s on the stack (found %s)"
       (print_st s1) (print_st s2))

let err_call1 l1 l2 =
  TypingError
    (Printf.sprintf "can't call function labeled as %s (pc: %s)" (str_l l1)
       (str_l l2))

let err_call2 i1 i2 =
  TypingError
    (Printf.sprintf "call needs %d values on the stack (found %d)" i1 i2)

let err_call3 s1 s2 =
  TypingError
    (Printf.sprintf "call needs values with types ⊑ %s on the stack (found %s)"
       (print_st s1) (print_st s2))

let err_function1 i1 i2 =
  TypingError
    (Printf.sprintf "function must leave %d value on the stack (found %d)" i1 i2)

let err_function2 s1 s2 =
  TypingError
    (Printf.sprintf
       "function must leave values with types ⊑ %s on the stack (found %s)"
       (print_st s1) (print_st s2))

let err_branch_prefix s1 s2 =
  TypingError
    (Printf.sprintf "branching expected %s to be a prefix of the stack (%s)"
       (print_st s1) (print_st s2))

let err_branch_stack_security_level l s =
  TypingError
    (Printf.sprintf
       "branching expected security level of all values on stack %s to be \
        greater than %s"
       (print_st s) (str_l l))

let err_gamma_subtype g1 g2 =
  TypingError
    (Printf.sprintf
       "expected gamma %s to have lower security level than gamma' %s"
       (print_g g1) (print_g g2))

(* ======= Type checking ======= *)

let lookup_global (c : context) (idx : int) =
  if 0 <= idx && idx < List.length c.globals then List.nth c.globals idx
  else t_err0 ("did not find global variable of index " ^ Int.to_string idx)

let lookup_local (c : context) (idx : int) =
  if 0 <= idx && idx < List.length c.locals then List.nth c.locals idx
  else t_err0 ("did not find local variable of index " ^ Int.to_string idx)

let lookup_func_type (c : context) (idx : int) =
  if 0 <= idx && idx < List.length c.func_types then List.nth c.func_types idx
  else t_err0 ("did not find function of index " ^ Int.to_string idx)

let lookup_label (c : context) (idx : int) =
  if idx < List.length c.labels then List.nth c.labels idx
  else failwith ("expected label of index " ^ Int.to_string idx)

let check_stack s1 s2 =
  assert (
    List.length s1 = List.length s2
    && List.for_all2
         (fun { t = t1; lbl = _lbl1 } { t = t2; lbl = _lbl2 } -> t1 == t2)
         s1 s2)

let same_lengths l1 l2 = List.compare_lengths l1 l2 == 0
let leq_ty { t = t1; lbl = l1 } { t = t2; lbl = l2 } = t1 == t2 && l1 <<= l2

let leq_stack (s1 : labeled_value_type list) (s2 : labeled_value_type list) =
  same_lengths s1 s2 && List.for_all2 leq_ty s1 s2

let leq_gamma (g1 : stack_of_stacks_type) (g2 : stack_of_stacks_type) =
  same_lengths g1 g2
  && List.for_all2 (fun (st1, _) (st2, _) -> leq_stack st1 st2) g1 g2

let prefix_split s1 s2 =
  let rec prefix_helper acc s1 s2 =
    match (s1, s2) with
    | [], _ -> Some (acc, s2)
    | _, [] -> None
    | v1 :: s1', v2 :: s2' ->
        if v1.t == v2.t then prefix_helper (v1 :: acc) s1' s2' else None
  in
  prefix_helper [] s1 s2

let lift_st lbl_lift_to st =
  let lift_st_helper ({ lbl; _ } as v) acc =
    { v with lbl = lbl_lift_to <> lbl } :: acc
  in
  List.fold_right lift_st_helper st []

let lift_g lbl_lift_to g =
  let lift_gamma_helper (st, pc) acc =
    (lift_st lbl_lift_to st, lbl_lift_to <> pc) :: acc
  in
  List.fold_right lift_gamma_helper g []

let lift (lbl_lift_to : SimpleLattice.t) (g : stack_of_stacks_type) =
  lift_g lbl_lift_to g

let rec check_instr ((g, c) : stack_of_stacks_type * context)
    (i : wasm_instruction) : stack_of_stacks_type * context =
  match g with
  | (st, pc) :: g' -> (
      match i with
      | WI_Unreachable ->
          (* Note: We put no restrictions on the program context,
             i.e. our approach is termination insensitive! *)
          (g, c)
      | WI_Nop -> (g, c)
      | WI_Drop -> (
          match st with _ :: st' -> ((st', pc) :: g', c) | _ -> raise err_drop)
      | WI_Const _ -> (({ t = I32; lbl = pc } :: st, pc) :: g', c)
      | WI_BinOp _ -> (
          match st with
          | v1 :: v2 :: st' ->
              assert (v1.t == v2.t);
              (* Label should be lbl1 ⊔ lbl2 ⊔ pc *)
              let lbl3 = v1.lbl <> v2.lbl <> pc in
              (({ t = v1.t; lbl = lbl3 } :: st', pc) :: g', c)
          | _ -> raise err_binop)
      | WI_Call idx -> (
          match lookup_func_type c idx with
          | FunType (ft_in, lbl, ft_out) ->
              (* Check if entering function is allowed *)
              if not (pc <<= lbl) then raise (err_call1 lbl pc);
              (* Check enough arguments *)
              if List.length ft_in > List.length st then
                raise (err_call2 (List.length ft_in) (List.length st));
              (* st' corresponds to st in the paper *)
              let tau1, st' = split_at_index (List.length ft_in) st in
              (* actual input argument types <= labeled input types *)
              if not (leq_stack tau1 ft_in) then raise (err_call3 ft_in tau1);
              ((ft_out @ st', pc) :: g', c))
      | WI_LocalGet idx ->
          let { t; lbl } = lookup_local c idx in
          (({ t; lbl = pc <> lbl } :: st, pc) :: g', c)
      | WI_LocalSet idx -> (
          match st with
          | { t = _; lbl = l } :: st' ->
              let { t = _; lbl = l' } = lookup_local c idx in
              (* Types are guaranteed to be I32, Check that pc ⊔ l ⊑ l' *)
              if not (l <> pc <<= l') then raise (err_localset2 pc l l');
              ((st', pc) :: g', c)
          | _ -> raise err_localset3)
      | WI_GlobalGet idx ->
          let { gtype = { t = ty; lbl = lbl' }; const = _; mut = _ } =
            lookup_global c idx
          in
          let lbl = pc <> lbl' in
          (({ t = ty; lbl } :: st, pc) :: g', c)
      | WI_GlobalSet idx -> (
          match st with
          | { t = _; lbl = l } :: st' ->
              let { gtype = { t = _; lbl = l' }; const = _; mut } =
                lookup_global c idx
              in
              (* Type are guaranteed to be I32 thus equal, Check that var is mutable and pc ⊔ l ⊑ l' *)
              if not mut then raise err_globalset_imut;
              if not (l <> pc <<= l') then raise (err_globalset2 pc l l');
              ((st', pc) :: g', c)
          | _ -> raise err_globalset3)
      | WI_Load lm -> (
          match c.memory with
          | None -> raise err_load_nomemory
          | Some _ -> (
              match st with
              | { t = _; lbl = la } :: st ->
                  (* l = l_a ⊔ l_m ⊔ pc *)
                  let lbl = la <> lm <> pc in
                  (({ t = I32; lbl } :: st, pc) :: g', c)
              | _ -> raise err_load_addrexists))
      | WI_Store lm -> (
          match c.memory with
          | None -> raise err_store_nomemory
          | Some _ -> (
              match st with
              | { t = _; lbl = lv } :: { t = _; lbl = la } :: st' ->
                  (* Addr and val are known to be I32, check that pc ⊔ l_a ⊔ l_v ⊑ l_m *)
                  if not (pc <> la <> lv <<= lm) then
                    raise (err_store2 pc la lv lm);
                  ((st', pc) :: g', c)
              | _ -> raise err_store_addrexists))
      | WI_Block (bt, exps) -> type_check_block (g, c) (bt, exps)
      | WI_Br i -> (
          (* Find the label (expected top of stack) that we're branching to *)
          let bt_out = lookup_label c i in
          (* Check that the top of the stack matches *)
          let (st, st') = split_at_index (List.length bt_out) st in 
          if not (leq_stack st bt_out) then raise (err_branch_prefix bt_out st);
          (* Check that pc ⊑ st *)
          if not (List.for_all (fun v -> pc <<= v.lbl) st) then
            raise (err_branch_stack_security_level pc st);
          (* g1 = g'[0 : i - 1], g2 = g'[i :] *)
          let g1, g2 = split_at_index (i - 1) g' in
          match lift pc ((st @ st', pc) :: g1) with
              | [] -> raise (InternalError "stack-of-stacks ill-formed")
              | (st'', pc') :: g1' ->
                ((st'', pc') :: g1' @ g2, c))
      | WI_BrIf _ -> raise (NotImplemented "br_if"))
  | _ -> raise (InternalError "stack-of-stacks ill-formed")

and check_seq ((g, c) : stack_of_stacks_type * context)
    (seq : wasm_instruction list) =
  List.fold_left check_instr (g, c) seq

and type_check_block ((g, c) : stack_of_stacks_type * context)
    ((BlockType (bt_in, bt_out), instrs) : block_type * wasm_instruction list) =
  match g with
  | [] -> raise (InternalError "blocks: stack-of-stacks ill-formed")
  | (stack, pc) :: g -> (
      let c' = { c with labels = bt_out :: c.labels } in
      if List.length bt_in > List.length stack then
        raise (err_block1 (List.length bt_in) (List.length stack));
      (* naming: bt_in and bt_out refer to tau1 and tau2 that are annotated
         tau1 refers to the type actually found, tau2 to the type that is computed based on the actual inputs
      *)
      let tau1, st = split_at_index (List.length bt_in) stack in
      (* actual input types <= labeled input types *)
      if not (leq_stack tau1 bt_in) then raise (err_block3 bt_in tau1);
      (* check expression with updated stack, context *)
      match check_seq ((tau1, pc) :: (st, pc) :: g, c') instrs with
      (* computed pc' is discarded *)
      | (tau2, _) :: (st', pc'') :: g', _ ->
          if List.length tau2 != List.length bt_out then
            raise (err_block2 (List.length bt_out) (List.length tau2));
          if not (leq_stack tau2 bt_out) then raise (err_block4 bt_out tau2);
          ((bt_out @ st', pc <> pc'') :: g', c)
      | _ -> raise (InternalError "blocks: stack-of-stacks ill-formed"))

let type_check_function (c : context) (f : wasm_func) =
  let { ftype = FunType (ft_in, l, ft_out); locals; body; _ } = f in
  let c' = { c with locals = ft_in @ locals } in
  let g_init = [ ([], l) ] in
  match check_seq (g_init, c') body with
  | [ (st, _pc) ], _ ->
      if List.length ft_out != List.length st then
        raise (err_function1 (List.length ft_out) (List.length st))
      else
        let st', _ = split_at_index (List.length ft_out) st in
        if not (leq_stack st' ft_out) then raise (err_function2 ft_out st)
  | _ -> raise (InternalError "function: stack-of-stacks ill-formed")

let type_check_module (m : wasm_module) =
  let c =
    {
      empty_context with
      globals = m.globals;
      memory = m.memory;
      func_types = List.map (fun f -> f.ftype) m.functions;
    }
  in
  let _ = List.map (type_check_function c) m.functions in
  ()
