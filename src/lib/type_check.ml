(*
   TODO:
   - Fix lattice to allow arbitrary lattice
   - Figure out how to pass around typing context and stack-of-stacks
*)

open Ast
open Sec

exception NotImplemented of string
exception InternalError of string
exception TypingError of string
exception PrivacyViolation of string

let err_drop = TypingError "drop expected 1 value on the stack"
let err_binop = TypingError "binop expected 2 values on the stack"
let err_localget = TypingError "local.get lookup out of bounds"

module type TYPECHECKER = sig
  type lt
  val type_check_module : lt wasm_module -> unit
end

let type_checker (type label_type) (lat : label_type lattice) = (module struct

type lt = label_type

type lv_ty = lt labeled_value_type

type pc_type = lt
type stack_type = lv_ty list 
type stack_of_stacks_type = (stack_type * pc_type) list

type context = {
  memories : int32; (* number of memories *)
  funcs : lt fun_type list;
  globals : lt wasm_global list;
  locals : lt labeled_value_type list;
  labels : lt labeled_value_type list;
  return : lt labeled_value_type list;
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

(* ======= Lattice functions ============= *)
let {leq; lub; str = lbl_to_str; bottom; top = _top} = lattice_functions lat

(* ======= Error handling ======= *)

let str_t (t : value_type) = match t with I32 -> "I32"

let t_err0 msg = raise (TypingError msg)

let t_err2 msg (t1 : value_type) (t2 : value_type) =
  let error_msg = Printf.sprintf msg (str t1) (str t2) in
  raise (TypingError error_msg)

let p_err3 msg l1 l2 l3 =
  let error_msg =
    Printf.sprintf msg (lbl_to_str l1) (lbl_to_str l2)
      (lbl_to_str l3)
  in
  raise (PrivacyViolation error_msg)

let p_err4 msg l1 l2 l3 l4 =
  let error_msg =
    Printf.sprintf msg (lbl_to_str l1) (lbl_to_str l2)
      (lbl_to_str l3) (lbl_to_str l4)
  in
  raise (PrivacyViolation error_msg)

(* error messages are checked in test-suite, don't inline *)

let err_globalset1 t1 t2 =
  TypingError
    (Printf.sprintf "global.set src/ dst mismatch (src=%s, dst=%s)" (str_t t1)
       (str_t t2))

let err_globalset2 l1 l2 l3 =
  PrivacyViolation
    (Printf.sprintf "global.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
       (lbl_to_str l1) (lbl_to_str l2) (lbl_to_str l3))

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
       (lbl_to_str l1) (lbl_to_str l2) (lbl_to_str l3))

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
       (lbl_to_str  l1) (lbl_to_str  l2) (lbl_to_str  l3) (lbl_to_str  l4))

let err_block1 i1 i2 =
  TypingError
    (Printf.sprintf "block needs %d values on the stack (found %d)" i1 i2)

let err_block2 i1 i2 =
  TypingError
    (Printf.sprintf "block must leave %d values on the stack (found %d)" i1 i2)
(* ======= Type checking ======= *)

let lookup_global (c : context) (idx : int32) =
  if Int32.to_int idx < List.length c.globals then
    List.nth c.globals (Int32.to_int idx)
  else failwith ("expected global variable of index " ^ Int32.to_string idx)

let lookup_local (c : context) (idx : int32) =
  if Int32.to_int idx < List.length c.locals then
    List.nth c.locals (Int32.to_int idx)
  else failwith ("expected local variable of index " ^ Int32.to_string idx)

let leq_ty (lty : lv_ty) (rty : lv_ty) = 
  match (lty, rty) with
  | {t = I32; lbl = lbl1}, {t = I32; lbl = lbl2} -> leq lbl1 lbl2

let rec match_prefix prefix stack = 
  match (prefix, stack) with 
  | [], _ -> true
  | _, [] -> false
  | t1 :: prefix', t2 :: stack' -> 
    leq_ty t1 t2 && match_prefix prefix' stack'

let leq_stack (lstack : stack_type) (rstack : stack_type) = 
  List.length lstack == List.length rstack 
  && match_prefix lstack rstack

let pop expected stack =
  let n = List.length expected in
  let got = Util.List.take n stack in 
  if leq_stack got expected then 
    (got, Util.List.drop n stack)
  else
    failwith "not implemented"

let rec check_instr (gamma, c : stack_of_stacks_type * context) i =
  match gamma with
  | [] -> 
    raise (InternalError "stack-of-stacks ill-formed")

  | (st, pc) :: gamma' ->
    match i with
    | WI_Unreachable ->
        (* Note: We put no restrictions on the program context,
          i.e. our approach is termination insensitive! *)
        gamma, c

    | WI_Nop -> 
      gamma,c

    | WI_Drop -> (
      match st with _ :: st' -> ((st', pc) :: gamma', c) | _ -> raise err_drop)

    | WI_Const _ -> 
      (({ t = I32; lbl = pc } :: st, pc) :: gamma', c)

    | WI_BinOp _ -> 
      (
          match st with
          | v1 :: v2 :: st' ->
              assert (v1.t == v2.t);
              (* Label should be lbl1 ⊔ lbl2 ⊔ pc *)
              let lbl3 = lub v1.lbl @@ lub v2.lbl pc in
              (({ t = v1.t; lbl = lbl3 } :: st', pc) :: gamma', c)
          | _ -> raise err_binop)

    | WI_Call _ ->
      raise (NotImplemented "call")

    | WI_LocalGet idx -> (
      try
        let { t; lbl } = lookup_local c idx in
        (({ t; lbl = lub pc lbl } :: st, pc) :: gamma', c)
      with _ -> raise err_localget)

    | WI_LocalSet idx -> (
      match st with
      | { t = src; lbl = l } :: st' ->
          let { t = dst; lbl = l' } = lookup_local c idx in
          (* Check that types are equal and pc ⊔ l ⊑ l' *)
          if not (src == dst) then raise (err_localset1 src dst)
          else if not (leq (lub l pc) l') then raise (err_localset2 pc l l');
          ((st', pc) :: gamma', c)
      | _ -> raise err_localset3)

    | WI_GlobalGet idx ->
      let { gtype = { t = ty; lbl = lbl' }; const = _; mut = _ } =
        lookup_global c idx
      in
      let lbl = lub pc lbl' in
      (({ t = ty; lbl } :: st, pc) :: gamma', c)

    | WI_GlobalSet idx -> (
      match st with
      | { t = src; lbl = l } :: st' ->
          let { gtype = { t = dst; lbl = l' }; const = _; mut } =
            lookup_global c idx
          in
          (* Check that types are equal, var is mutable and pc ⊔ l ⊑ l' *)
          if not (src == dst) then raise (err_globalset1 src dst);
          if not mut then raise err_globalset_imut;
          if not (leq (lub l pc) l') then raise (err_globalset2 pc l l');
          ((st', pc) :: gamma', c)
      | _ -> raise err_globalset3)


    | WI_Load lm -> (
        if c.memories == 0l then raise err_load_nomemory
        else
          match st with
          | { t = addr; lbl = la } :: st' ->
              if addr != I32 then raise err_load_addr_i32;
              (* l = l_a ⊔ l_m ⊔ pc *)
              let lbl = lub la (lub lm pc) in
              (({ t = I32; lbl } :: st', pc) :: gamma', c)
          | _ -> raise err_load_addrexists)


    | WI_Store lm -> (
      if c.memories == 0l then raise err_store_nomemory
      else
        match st with
        | { t = tval; lbl = lv } :: { t = taddr; lbl = la } :: st' ->
            (* Check that addr and val are I32 and pc ⊔ l_a ⊔ l_v ⊑ l_m *)
            if taddr != I32 then raise err_store_addr_i32;
            if tval != I32 then raise err_store_val_i32
            else if not (leq (lub pc (lub la lv)) lm) then
              raise (err_store2 pc la lv lm);
            ((st', pc) :: gamma', c)
        | _ -> raise err_store_addrexists)

    | WI_If _ -> raise (NotImplemented "if-then-else")
    | WI_Block block -> 
      type_check_block c gamma block

    | WI_Br _ -> raise (NotImplemented "br")
    | WI_BrIf _ -> raise (NotImplemented "br_if")

and type_check_block (ctx : context) (gamma : stack_of_stacks_type) (block : lt wasm_block) =
match gamma with 
| [] -> 
  failwith "TODO: Err msg"

| (stack, pc) :: gamma -> 
  let {btype = BlockType {params; result};  instrs} = block in
  let ctx' = { ctx with labels = (List.rev result) @ ctx.labels } in 
  let (t1, st) = pop params stack in 
  let gamma1 = (t1, pc) :: (st, pc) :: gamma in
  match List.fold_left check_instr (gamma1, ctx') instrs with
  | (t2, _pc') :: (st', pc'') :: gamma', _ when leq_stack t2 (List.rev result) ->
    (t2 @ st', lub pc pc'') :: gamma', ctx

  | _ -> 
    raise (InternalError "blocks: stack-of-stacks ill-formed")

let type_check_function (ctx : context) f =
  let {ftype = FunType {params; result; _}; locals; body; _} = f in
  let ctx' = {ctx with locals = params @ locals} in
  let block = {btype = BlockType {params = []; result}; instrs = body} in
  match type_check_block ctx' [([], bottom)] block with 
  | [(st, _pc)], _ when leq_stack st result ->
    ()

  | _ -> 
    raise (InternalError "function: stack-of-stacks ill-formed")

let type_check_module (m : lt wasm_module) =
  let ctx =
    {
      empty_context with
      globals = m.globals;
      memories = Int32.of_int (List.length m.memories);
    }
  in
  List.iter (type_check_function ctx) m.functions

end : TYPECHECKER with type lt = label_type)