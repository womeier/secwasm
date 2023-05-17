(*
   TODO:
   - Fix lattice to allow arbitrary lattice
   - Figure out how to pass around typing context and stack-of-stacks
*)

open Ast
open Sec

type context = {
  memories : int32; (* number of memories *)
  funcs : fun_type list;
  globals : wasm_global list;
  locals : labeled_value_type list;
  labels : labeled_value_type list list;
  return : labeled_value_type list;
}

let empty_context =
  {
    memories = 0l;
    funcs = [];
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
    if n = 0 then (acc, l)
    else
      match l with
      | [] -> failwith "split_at_index: list is empty!"
      | x :: xs -> f (n - 1) xs (x :: acc)
  in
  let fst, lst = f n l [] in
  (List.rev_append fst [], lst)

(* ======== Debugging ============*)

let str_l (l : pc_type) = match l with Public -> "L" | Secret -> "H"
let str_t (t : value_type) = match t with I32 -> "I32"

let rec print_g (g : stack_of_stacks_type) =
  match g with
  | [] -> "[]"
  | (st, pc) :: g' ->
      Printf.sprintf "(%s, %s) :: %s" (print_st st) (str_l pc) (print_g g')

and print_st (st : stack_type) =
  match st with
  | [] -> "[]"
  | { t; lbl } :: st' ->
      Printf.sprintf "{%s, %s} :: %s" (str_t t) (str_l lbl) (print_st st')

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

let str_t (t : value_type) = match t with I32 -> "I32"

let str_l (l : SimpleLattice.t) =
  match l with Public -> "Public" | Secret -> "Secret"

let t_err0 msg = raise (TypingError msg)

let t_err2 msg (t1 : value_type) (t2 : value_type) =
  let error_msg = Printf.sprintf msg (str_t t1) (str_t t2) in
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
let err_localget = TypingError "local.get lookup out of bounds"

let err_globalset1 t1 t2 =
  TypingError
    (Printf.sprintf "global.set src/ dst mismatch (src=%s, dst=%s)" (str_t t1)
       (str_t t2))

let err_globalset2 l1 l2 l3 =
  PrivacyViolation
    (Printf.sprintf "global.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
       (str_l l1) (str_l l2) (str_l l3))

let err_globalset3 = TypingError "global.set expected 1 value on the stack"

let err_globalset_imut =
  TypingError "global.set expected global var to be mutable"

let err_localset1 src dst =
  TypingError
    (Printf.sprintf "local.set src/ dst mismatch (src=%s, dst=%s)" (str_t src)
       (str_t dst))

let err_localset2 l1 l2 l3 =
  PrivacyViolation
    (Printf.sprintf "local.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
       (str_l l1) (str_l l2) (str_l l3))

let err_localset3 = TypingError "local.set expected 1 value on the stack"
let err_load_nomemory = TypingError "load expected memory in the context"
let err_load_addrexists = TypingError "load expected 1 value on the stack"
let err_load_addr_i32 = TypingError "load address is expected to be a i32"
let err_store_nomemory = TypingError "store expected memory in the context"
let err_store_val_i32 = TypingError "store expected value of type i32 to store"
let err_store_addrexists = TypingError "store expected 1 value on the stack"
let err_store_addr_i32 = TypingError "store address is expected to be a i32"

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

let err_function1 i1 i2 =
  TypingError
    (Printf.sprintf "function must leave %d value on the stack (found %d)" i1 i2)

let err_function2 s1 s2 =
  TypingError
    (Printf.sprintf
       "function must leave values with types ⊑ %s on the stack (found %s)"
       (print_st s1) (print_st s2))

(* ======= Type checking ======= *)

let lookup_global (c : context) (idx : int32) =
  if Int32.to_int idx < List.length c.globals then
    List.nth c.globals (Int32.to_int idx)
  else failwith ("expected global variable of index " ^ Int32.to_string idx)

let lookup_local (c : context) (idx : int32) =
  if Int32.to_int idx < List.length c.locals then
    List.nth c.locals (Int32.to_int idx)
  else failwith ("expected local variable of index " ^ Int32.to_string idx)

let check_stack s1 s2 =
  assert (
    List.length s1 = List.length s2
    && List.for_all2
         (fun { t = t1; lbl = _lbl1 } { t = t2; lbl = _lbl2 } -> t1 == t2)
         s1 s2)

let leq_ty { t = t1; lbl = l1 } { t = t2; lbl = l2 } = t1 == t2 && l1 <<= l2

let leq_stack (s1 : labeled_value_type list) (s2 : labeled_value_type list) =
  List.for_all2 leq_ty s1 s2

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
      | WI_Call _ -> raise (NotImplemented "call")
      | WI_LocalGet idx -> (
          try
            let { t; lbl } = lookup_local c idx in
            (({ t; lbl = pc <> lbl } :: st, pc) :: g', c)
          with _ -> raise err_localget)
      | WI_LocalSet idx -> (
          match st with
          | { t = src; lbl = l } :: st' ->
              let { t = dst; lbl = l' } = lookup_local c idx in
              (* Check that types are equal and pc ⊔ l ⊑ l' *)
              if not (src == dst) then raise (err_localset1 src dst)
              else if not (l <> pc <<= l') then raise (err_localset2 pc l l');
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
          | { t = src; lbl = l } :: st' ->
              let { gtype = { t = dst; lbl = l' }; const = _; mut } =
                lookup_global c idx
              in
              (* Check that types are equal, var is mutable and pc ⊔ l ⊑ l' *)
              if not (src == dst) then raise (err_globalset1 src dst);
              if not mut then raise err_globalset_imut;
              if not (l <> pc <<= l') then raise (err_globalset2 pc l l');
              ((st', pc) :: g', c)
          | _ -> raise err_globalset3)
      | WI_Load lm -> (
          if c.memories == 0l then raise err_load_nomemory
          else
            match st with
            | { t = addr; lbl = la } :: st ->
                if addr != I32 then raise err_load_addr_i32;
                (* l = l_a ⊔ l_m ⊔ pc *)
                let lbl = la <> lm <> pc in
                (({ t = I32; lbl } :: st, pc) :: g', c)
            | _ -> raise err_load_addrexists)
      | WI_Store lm -> (
          if c.memories == 0l then raise err_store_nomemory
          else
            match st with
            | { t = tval; lbl = lv } :: { t = taddr; lbl = la } :: st' ->
                (* Check that addr and val are I32 and pc ⊔ l_a ⊔ l_v ⊑ l_m *)
                if taddr != I32 then raise err_store_addr_i32;
                if tval != I32 then raise err_store_val_i32
                else if not (pc <> la <> lv <<= lm) then
                  raise (err_store2 pc la lv lm);
                ((st', pc) :: g', c)
            | _ -> raise err_store_addrexists)
      | WI_If _ -> raise (NotImplemented "if-then-else")
      | WI_Block (bt, exps) -> type_check_block (g, c) (bt, exps)
      | WI_Br _ -> raise (NotImplemented "br")
      | WI_BrIf _ -> raise (NotImplemented "br_if"))
  | _ -> raise (InternalError "stack-of-stacks ill-formed")

and check_seq ((g, c) : stack_of_stacks_type * context)
    (seq : wasm_instruction list) =
  List.fold_left check_instr (g, c) seq

and type_check_block ((g, c) : stack_of_stacks_type * context)
    ((BlockType (bt_in, bt_out), instrs) : block_type * wasm_instruction list) =
  match g with
  | [] -> raise (InternalError "blocks: stack-of-stacks ill-formed")
  | (st, pc) :: g -> (
      let c' = { c with labels = bt_out :: c.labels } in
      if List.length bt_in > List.length st then
        raise (err_block1 (List.length bt_in) (List.length st));
      let st', st'' = split_at_index (List.length bt_in) st in
      if not (leq_stack st' bt_in) then raise (err_block3 bt_in st');
      let g_ = (st', pc) :: (st'', pc) :: g in
      match check_seq (g_, c') instrs with
      | (st_, _pc) :: (st_', pc_') :: g_', _ ->
          let lst_ = List.length st_ in
          if lst_ < List.length bt_out then
            raise (err_block2 (List.length bt_out) lst_);
          if not (leq_stack st_ bt_out) then raise (err_block4 bt_out st_);
          ((st_ @ st_', pc <> pc_') :: g_', c)
      | _ -> raise (InternalError "blocks: stack-of-stacks ill-formed"))

let type_check_function (c : context) (f : wasm_func) =
  let { ftype = FunType (_ft_in, _lbl, ft_out); locals; body; _ } = f in
  let c' = { c with locals } in
  let g_init = [ ([], Public) ] in
  match check_seq (g_init, c') body with
  | [ (st, _pc) ], _ ->
      if not (List.length ft_out <= List.length st) then
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
      memories = Int32.of_int (List.length m.memories);
    }
  in
  let _ = List.map (type_check_function c) m.functions in
  ()
