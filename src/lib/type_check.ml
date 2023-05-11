(*
   TODO:
   - Fix lattice to allow arbitrary lattice
   - Figure out how to pass around typing context and stack-of-stacks
*)

open Ast
open Sec

exception NotImplemented of string
exception TypingError of string
exception PrivacyViolation of string

type context = {
  funcs : fun_type list;
  globals : wasm_global list;
  locals : labeled_value_type list;
  labels : labeled_value_type list;
  return : labeled_value_type list;
}

type pc_type = SimpleLattice.t
type stack_type = labeled_value_type list
type stack_of_stacks_type = stack_type * pc_type

let lookup_global (c : context) (idx : int32) =
  List.nth c.globals (Int32.to_int idx)

let empty_context =
  { funcs = []; globals = []; locals = []; labels = []; return = [] }

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
  | WI_Unreachable -> raise (NotImplemented "unreachable")
  | WI_Nop -> raise (NotImplemented "nop")
  | WI_Drop -> raise (NotImplemented "drop")
  | WI_Const _ -> ([], [ { t = I32; lbl = pc } ])
  | WI_BinOp _ -> (
      match stack with
      | ({ t = t1; lbl = lbl1 } as v1) :: ({ t = t2; lbl = lbl2 } as v2) :: _ ->
          assert (t1 == t2);
          (* Label should be lbl1 ⊔ lbl2 ⊔ pc *)
          let lbl3 = SimpleLattice.lub (SimpleLattice.lub lbl1 lbl2) pc in
          ([ v1; v2 ], [ { t = t1; lbl = lbl3 } ])
      | _ -> raise (TypingError "BinOp expected 2 values on the stack"))
  | WI_Call _ -> raise (NotImplemented "call")
  | WI_LocalGet _ -> raise (NotImplemented "local.get")
  | WI_LocalSet _ -> raise (NotImplemented "local.set")
  | WI_GlobalGet idx ->
      let { gtype = { t = ty; lbl = lbl' }; const = _ } = lookup_global c idx in
      let lbl = SimpleLattice.lub pc lbl' in
      ([], [ { t = ty; lbl } ])
  | WI_GlobalSet idx -> (
      match stack with
      | ({ t = src; lbl = l } as h) :: _ ->
          let { gtype = { t = dst; lbl = l' }; const = _ } =
            lookup_global c idx
          in
          (* Check that types are equal and pc ⊔ l ⊑ l' *)
          if not (src == dst) then
            raise
              (TypingError
                 (Printf.sprintf
                    "global.set: src/ dst mismatch (src=%s, dst=%s)" (str src)
                    (str dst)))
          else if not (SimpleLattice.leq (SimpleLattice.lub l pc) l') then
            raise
              (PrivacyViolation
                 (Printf.sprintf
                    "expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
                    (SimpleLattice.str l) (SimpleLattice.str pc)
                    (SimpleLattice.str l')));
          ([ h ], [])
      | _ -> raise (TypingError "SetGlobal expected 1 value on the stack"))
  | WI_Load -> raise (NotImplemented "load")
  | WI_Store -> raise (NotImplemented "load")
  | WI_If _ -> raise (NotImplemented "if-then-else")

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
  let _ = check_seq c Public f.body in
  true

let type_check_module (m : wasm_module) =
  let c = { empty_context with globals = m.globals } in
  List.for_all (type_check_function c) m.functions
