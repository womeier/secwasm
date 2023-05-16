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
  labels : labeled_value_type list;
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

(* ======= Notation ============= *)

let ( <> ) v1 v2 = SimpleLattice.lub v1 v2
let ( <= ) v1 v2 = SimpleLattice.leq v1 v2

(* ======= Error handling ======= *)

exception NotImplemented of string
exception TypingError of string
exception PrivacyViolation of string

let t_err0 msg = raise (TypingError msg)

let t_err2 msg (t1 : value_type) (t2 : value_type) =
  let error_msg = Printf.sprintf msg (str t1) (str t2) in
  raise (TypingError error_msg)

let p_err3 msg l1 l2 l3 =
  let error_msg =
    Printf.sprintf msg (SimpleLattice.str l1) (SimpleLattice.str l2)
      (SimpleLattice.str l3)
  in
  raise (PrivacyViolation error_msg)

let p_err4 msg l1 l2 l3 l4 =
  let error_msg =
    Printf.sprintf msg (SimpleLattice.str l1) (SimpleLattice.str l2)
      (SimpleLattice.str l3) (SimpleLattice.str l4)
  in
  raise (PrivacyViolation error_msg)

(* error messages are checked in test-suite, don't inline *)

let err_msg_drop = "drop expected 1 value on the stack"
let err_msg_binop = "binop expected 2 values on the stack"
let err_msg_localget = "local.get lookup out of bounds"

let err_msg_globalset1 : (string -> string -> string, unit, string) format =
  "global.set src/ dst mismatch (src=%s, dst=%s)"

let err_msg_globalset2 :
    (string -> string -> string -> string, unit, string) format =
  "global.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"

let err_msg_globalset3 = "global.set expected 1 value on the stack"
let err_msg_globalset_imut = "global.set expected global var to be mutable"

let err_msg_localset1 : (string -> string -> string, unit, string) format =
  "local.set src/ dst mismatch (src=%s, dst=%s)"

let err_msg_localset2 :
    (string -> string -> string -> string, unit, string) format =
  "local.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"

let err_msg_localset3 = "local.set expected 1 value on the stack"
let err_msg_load_nomemory = "load expected memory in the context"
let err_msg_load_addrexists = "load expected 1 value on the stack"
let err_msg_load_addr_i32 = "load address is expected to be a i32"
let err_msg_store_nomemory = "store expected memory in the context"
let err_msg_store_val_i32 = "store expected value of type i32 to store"
let err_msg_store_addrexists = "store expected 1 value on the stack"
let err_msg_store_addr_i32 = "store address is expected to be a i32"

let err_msg_store2 :
    (string -> string -> string -> string -> string, unit, string) format =
  "store expected pc ⊔ la ⊔ lv ⊑ lm but was pc=%s, la=%s, lv=%s, lm=%s"

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

let pop (s1 : labeled_value_type list) (s2 : labeled_value_type list) =
  let l1 = List.length s1 in
  let l2 = List.length s2 in
  let l = min l1 l2 in
  check_stack s1 (Util.List.drop (l2 - l) s2);
  Util.List.take (l2 - l) s2

let check_instr (c : context) (pc : pc_type) (i : wasm_instruction)
    (stack : labeled_value_type list) :
    labeled_value_type list * labeled_value_type list =
  match i with
  | WI_Unreachable ->
      (* Note: We put no restrictions on the program context,
         i.e. our approach is termination insensitive! *)
      ([], [])
  | WI_Nop -> ([], [])
  | WI_Drop -> (
      match stack with v :: _ -> ([ v ], []) | _ -> t_err0 err_msg_drop)
  | WI_Const _ -> ([], [ { t = I32; lbl = pc } ])
  | WI_BinOp _ -> (
      match stack with
      | ({ t = t1; lbl = lbl1 } as v1) :: ({ t = t2; lbl = lbl2 } as v2) :: _ ->
          assert (t1 == t2);
          (* Label should be lbl1 ⊔ lbl2 ⊔ pc *)
          let lbl3 = lbl1 <> lbl2 <> pc in
          ([ v1; v2 ], [ { t = t1; lbl = lbl3 } ])
      | _ -> t_err0 err_msg_binop)
  | WI_Call _ -> raise (NotImplemented "call")
  | WI_LocalGet idx -> (
      try
        let { t; lbl } = lookup_local c idx in
        ([], [ { t; lbl = pc <> lbl } ])
      with _ -> t_err0 err_msg_localget)
  | WI_LocalSet idx -> (
      match stack with
      | ({ t = src; lbl = l } as h) :: _ ->
          let { t = dst; lbl = l' } = lookup_local c idx in
          (* Check that types are equal and pc ⊔ l ⊑ l' *)
          if not (src == dst) then t_err2 err_msg_localset1 src dst
          else if not (l <> pc <= l') then p_err3 err_msg_localset2 pc l l';
          ([ h ], [])
      | _ -> t_err0 err_msg_localset3)
  | WI_GlobalGet idx ->
      let { gtype = { t = ty; lbl = lbl' }; const = _; mut = _ } =
        lookup_global c idx
      in
      let lbl = pc <> lbl' in
      ([], [ { t = ty; lbl } ])
  | WI_GlobalSet idx -> (
      match stack with
      | ({ t = src; lbl = l } as h) :: _ ->
          let { gtype = { t = dst; lbl = l' }; const = _; mut } =
            lookup_global c idx
          in
          (* Check that types are equal, var is mutable and pc ⊔ l ⊑ l' *)
          if not (src == dst) then t_err2 err_msg_globalset1 src dst;
          if not mut then t_err0 err_msg_globalset_imut;
          if not (l <> pc <= l') then p_err3 err_msg_globalset2 pc l l';
          ([ h ], [])
      | _ -> t_err0 err_msg_globalset3)
  | WI_Load lm -> (
      if c.memories == 0l then t_err0 err_msg_load_nomemory
      else
        match stack with
        | { t = addr; lbl = la } :: _ ->
            if addr != I32 then t_err0 err_msg_load_addr_i32;
            (* l = l_a ⊔ l_m ⊔ pc *)
            let lbl = la <> lm <> pc in
            ([], [ { t = I32; lbl } ])
        | _ -> t_err0 err_msg_load_addrexists)
  | WI_Store lm -> (
      if c.memories == 0l then t_err0 err_msg_store_nomemory
      else
        match stack with
        | ({ t = tval; lbl = lv } as v) :: ({ t = taddr; lbl = la } as a) :: _
          ->
            (* Check that addr and val are I32 and pc ⊔ l_a ⊔ l_v ⊑ l_m *)
            if taddr != I32 then t_err0 err_msg_store_addr_i32;
            if tval != I32 then t_err0 err_msg_store_val_i32
            else if not (pc <> la <> lv <= lm) then
              p_err4 err_msg_store2 pc la lv lm;
            ([ v; a ], [])
        | _ -> t_err0 err_msg_store_addrexists)
  | WI_If _ -> raise (NotImplemented "if-then-else")
  | WI_Block _ -> raise (NotImplemented "block")
  | WI_Br _ -> raise (NotImplemented "br")
  | WI_BrIf _ -> raise (NotImplemented "br_if")

let rec check_seq (c : context) (pc : pc_type) (seq : wasm_instruction list) :
    labeled_value_type list =
  match seq with
  | [] -> []
  | _ ->
      let seq', i = Util.List.split_last seq in
      let stack = check_seq c pc seq' in
      let ins, outs = check_instr c pc i stack in
      pop ins stack @ outs

let type_check_function (c : context) (f : wasm_func) =
  let c' = { c with locals = f.locals } in
  let _ = check_seq c' Public f.body in
  ()

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
