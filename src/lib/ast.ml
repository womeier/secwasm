open Sec

[@@@coverage exclude_file]

type value_type = I32

(* This is equivalent to tau in the paper (typing judgements) *)
type labeled_value_type = { t : value_type; lbl : SimpleLattice.t }
type stack_type = labeled_value_type list
type fun_type = FunType of stack_type * SimpleLattice.t * stack_type
type block_type = BlockType of stack_type * stack_type

type binop =
  | Add
  | Eq
  | Ge_s
  | Lt_s
  | Le_s
  | Mul
  | Sub
  | Shr_u
  | Shl
  | And
  | Or

[@@@ocamlformat "disable"]

type wasm_instruction =
  | WI_Unreachable                                                    (* trap unconditionally *)
  | WI_Drop                                                           (* drop value *)
  | WI_Const of int                                                   (* constant *)
  | WI_BinOp of binop                                                 (* binary numeric operator, deviates a bit from spec *)
  | WI_Call of int                                                    (* call function *)
  | WI_LocalGet of int                                                (* read local variable *)
  | WI_LocalSet of int                                                (* write local variable *)
  | WI_GlobalGet of int                                               (* read global variable *)
  | WI_GlobalSet of int                                               (* write global variable *)
  | WI_Load of SimpleLattice.t                                        (* read memory at address *)
  | WI_Store of SimpleLattice.t                                       (* write memory at address *)
  | WI_Block of block_type * wasm_instruction list                    (* block *)
  | WI_Br of int                                                      (* unconditional branch *)
  | WI_BrIf of int                                                    (* conditional branch *)
  | WI_Nop

[@@@ocamlformat "enable"]

type wasm_global = {
  gtype : labeled_value_type;
  const : wasm_instruction list;
  mut : bool;
}

type wasm_func = {
  ftype : fun_type;
  locals : labeled_value_type list;
  body : wasm_instruction list;
  export_name : string option; (* export name, should start with '$' *)
}

type wasm_memory = { size : int } (* in #pages *)

type wasm_module = {
  globals : wasm_global list;
  functions : wasm_func list;
  function_imports : (string * string * fun_type) list;
  memory : wasm_memory option;
}

(************************* PRETTY PRINTING ************************************)

let pp_type (t : value_type) = match t with I32 -> "i32"

let pp_labeled_type (t : labeled_value_type) =
  pp_type t.t (* TODO: have separate pp *)

let nl = "\n"

let pp_block_type (BlockType (bt_in, bt_out)) =
  let params =
    if List.length bt_in > 0 then
      " (param"
      ^ List.fold_left (fun _s l -> " " ^ pp_labeled_type l ^ _s) "" bt_in
      ^ ")"
    else ""
  in
  let result =
    if List.length bt_out > 0 then
      " (result"
      ^ List.fold_left (fun _s l -> " " ^ pp_labeled_type l ^ _s) "" bt_out
      ^ ")"
    else ""
  in
  params ^ result

let rec pp_instruction (indent : int) (instr : wasm_instruction) =
  let pp_instructions (indent' : int) (instructions : wasm_instruction list) =
    List.fold_left
      (fun _s i -> _s ^ pp_instruction indent' i ^ nl)
      "" instructions
  in

  let rec spaces (n : int) =
    match n with 0 -> "" | _ -> " " ^ spaces (n - 1)
  in

  spaces indent
  ^
  match instr with
  | WI_Unreachable -> "unreachable"
  | WI_Nop -> "nop"
  | WI_Const v -> "i32.const " ^ Int.to_string v
  | WI_BinOp Add -> "i32.add"
  | WI_BinOp Mul -> "i32.mul"
  | WI_BinOp Eq -> "i32.eq"
  | WI_BinOp Ge_s -> "i32.ge_s"
  | WI_BinOp Le_s -> "i32.le_s"
  | WI_BinOp Lt_s -> "i32.lt_s"
  | WI_BinOp Sub -> "i32.sub"
  | WI_BinOp Shr_u -> "i32.shr_u"
  | WI_BinOp Shl -> "i32.shl"
  | WI_BinOp And -> "i32.and"
  | WI_BinOp Or -> "i32.or"
  | WI_Call idx -> "call " ^ Int.to_string idx
  | WI_Drop -> "drop"
  | WI_LocalGet idx -> "local.get " ^ Int.to_string idx
  | WI_LocalSet idx -> "local.set " ^ Int.to_string idx
  | WI_GlobalGet idx -> "global.get " ^ Int.to_string idx
  | WI_GlobalSet idx -> "global.set " ^ Int.to_string idx
  | WI_Load _ -> "i32.load"
  | WI_Store _ -> "i32.store"
  | WI_Block (t, b) ->
      "block " ^ pp_block_type t ^ nl
      ^ pp_instructions (indent + 2) b
      ^ spaces indent ^ "end"
  | WI_Br idx -> "br " ^ Int.to_string idx
  | WI_BrIf idx -> "br_if " ^ Int.to_string idx

let pp_fn_params ftype =
  let ps = match ftype with FunType (plist, _, _) -> plist in
  if List.length ps > 0 then
    " (param"
    ^ List.fold_left (fun _s l -> " " ^ pp_labeled_type l ^ _s) "" ps
    ^ ")"
  else ""

let pp_fn_result ftype =
  match ftype with
  | FunType (_, _, []) -> ""
  | FunType (_, _, [ res ]) -> " (result " ^ pp_labeled_type res ^ ")"
  | _ ->
      failwith "PP error, instruction return type can be at most a single value"

let pp_function_import (imp : string * string * fun_type) =
  match imp with
  | m, name, ftype ->
      let params = pp_fn_params ftype and result = pp_fn_result ftype in
      "(import \"" ^ m ^ "\" \"" ^ name ^ "\" (func " ^ params ^ result ^ "))"

let pp_function (f : wasm_func) =
  let locals =
    if List.length f.locals > 0 then
      " (local"
      ^ List.fold_left (fun _s l -> " " ^ pp_labeled_type l ^ _s) "" f.locals
      ^ ")"
    else ""
  and params = pp_fn_params f.ftype
  and result = pp_fn_result f.ftype
  and export =
    match f.export_name with
    | Some name -> " (export \"" ^ name ^ "\")"
    | None -> ""
  and body =
    List.fold_left (fun _s i -> _s ^ nl ^ pp_instruction 2 i) "" f.body
  in
  "(func" ^ export ^ params ^ result ^ nl ^ locals ^ nl ^ body ^ nl ^ ")"

let pp_global g =
  let t = if g.mut then "(mut i32)" else "i32"
  and init =
    match g.const with
    | [ instr ] -> pp_instruction 0 instr
    | _ -> failwith "PP, global can only be initialized by single instruction"
  in
  "(global " ^ t ^ " " ^ init ^ ")"

let pp_memory (m : wasm_memory option) =
  match m with
  | None -> ""
  | Some mem -> "(memory " ^ Int.to_string mem.size ^ ")"

let pp_module (m : wasm_module) =
  "(module" ^ nl
  ^ List.fold_left
      (fun _s fi -> _s ^ nl ^ pp_function_import fi)
      "" m.function_imports
  ^ List.fold_left (fun _s f -> _s ^ nl ^ pp_function f) "" m.functions
  ^ nl
  ^ List.fold_left (fun _s g -> _s ^ nl ^ pp_global g) "" m.globals
  ^ nl ^ pp_memory m.memory ^ nl ^ nl ^ ")"
