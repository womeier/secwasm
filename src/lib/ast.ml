open Sec
open Types

[@@@coverage exclude_file]

type binop =
  | Add
  | Eq
  | Ge_s
  | Lt_s
  | Lt_u
  | Le_s
  | Mul
  | Sub
  | Shr_u
  | Shl
  | And
  | Or

type type_ = func_type Source.phrase
type var = int Source.phrase

[@@@ocamlformat "disable"]

type wasm_instruction = instr' Source.phrase
and instr' =
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
  | WI_Loop of block_type * wasm_instruction list                     (* loop *)
  | WI_Br of int                                                      (* unconditional branch *)
  | WI_BrIf of int                                                    (* conditional branch *)
  | WI_Nop

[@@@ocamlformat "enable"]

type wasm_global = {
  gtype : labeled_value_type * bool;
  const : wasm_instruction list;
}

type wasm_func = {
  ftype : func_type;
  locals : labeled_value_type list;
  body : wasm_instruction list;
  export_name : string option; (* export name, should start with '$' *)
}

type wasm_memory = { size : int } (* in #pages *)

type wasm_module = {
  globals : wasm_global list;
  functions : wasm_func list;
  function_imports : (string * string * func_type) list;
  memory : wasm_memory option;
}

let empty_module =
  { globals = []; functions = []; function_imports = []; memory = None }

let empty_instr_list =
  {
    ftype = FunType ([], Public, []);
    locals = [];
    body = [];
    export_name = None;
  }

(************************* PRETTY PRINTING ************************************)

let pp_type (with_labels : bool) ((_, l) : labeled_value_type) =
  if with_labels then
    match l with Secret -> "i32<Secret>" | Public -> "i32<Public>"
  else "i32"

let nl = "\n"

let pp_block_type f (BlockType (bt_in, bt_out)) =
  let params =
    if List.length bt_in > 0 then
      " (param" ^ List.fold_left (fun _s l -> " " ^ f l ^ _s) "" bt_in ^ ")"
    else ""
  in
  let result =
    if List.length bt_out > 0 then
      " (result" ^ List.fold_left (fun _s l -> " " ^ f l ^ _s) "" bt_out ^ ")"
    else ""
  in
  params ^ result

let rec pp_instruction f (indent : int) (instr : wasm_instruction) =
  let pp_instructions f (indent' : int) (instructions : wasm_instruction list) =
    List.fold_left
      (fun _s i -> _s ^ pp_instruction f indent' i ^ nl)
      "" instructions
  in

  let rec spaces (n : int) =
    match n with 0 -> "" | _ -> " " ^ spaces (n - 1)
  in

  spaces indent
  ^
  match instr.it with
  | WI_Unreachable -> "unreachable"
  | WI_Nop -> "nop"
  | WI_Const v -> "i32.const " ^ Int.to_string v
  | WI_BinOp Add -> "i32.add"
  | WI_BinOp Mul -> "i32.mul"
  | WI_BinOp Eq -> "i32.eq"
  | WI_BinOp Ge_s -> "i32.ge_s"
  | WI_BinOp Le_s -> "i32.le_s"
  | WI_BinOp Lt_s -> "i32.lt_s"
  | WI_BinOp Lt_u -> "i32.lt_u"
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
      "block " ^ pp_block_type f t ^ nl
      ^ pp_instructions f (indent + 2) b
      ^ spaces indent ^ "end"
  | WI_Loop (t, b) ->
      "loop " ^ pp_block_type f t ^ nl
      ^ pp_instructions f (indent + 2) b
      ^ spaces indent ^ "end"
  | WI_Br idx -> "br " ^ Int.to_string idx
  | WI_BrIf idx -> "br_if " ^ Int.to_string idx

let pp_fn_params f ftype =
  let ps = match ftype with FunType (plist, _, _) -> plist in
  if List.length ps > 0 then
    " (param" ^ List.fold_left (fun s l -> s ^ " " ^ f l) "" ps ^ ")"
  else ""

let pp_fn_result f ftype =
  match ftype with
  | FunType (_, _, []) -> ""
  | FunType (_, _, [ res ]) -> " (result " ^ f res ^ ")"
  | _ ->
      failwith "PP error, instruction return type can be at most a single value"

let pp_function_import f (imp : string * string * func_type) =
  match imp with
  | m, name, ftype ->
      let params = pp_fn_params f ftype and result = pp_fn_result f ftype in
      "  (import \"" ^ m ^ "\" \"" ^ name ^ "\" (func " ^ params ^ result ^ "))"

let pp_function (f' : labeled_value_type -> string) (f : wasm_func) =
  let locals =
    if List.length f.locals > 0 then
      "   (local"
      ^ List.fold_left (fun _s l -> " " ^ f' l ^ _s) "" f.locals
      ^ ")" ^ nl
    else ""
  and params = pp_fn_params f' f.ftype
  and result = pp_fn_result f' f.ftype
  and export =
    match f.export_name with
    | Some name -> " (export \"" ^ name ^ "\")"
    | None -> ""
  and body =
    List.fold_left (fun _s i -> _s ^ nl ^ (pp_instruction f') 4 i) "" f.body
  in
  let body' = if body <> "" then body ^ nl else body in
  "  (func" ^ export ^ params ^ result ^ locals ^ body' ^ "  )"

let pp_global f g =
  let t =
    if snd g.gtype then "(mut " ^ f (fst g.gtype) ^ ")" else f (fst g.gtype)
  and init = List.fold_left (fun s i -> s ^ pp_instruction f 2 i) "" g.const in
  "  (global " ^ t ^ " " ^ init ^ ")"

let pp_memory (m : wasm_memory option) =
  match m with
  | None -> ""
  | Some mem -> nl ^ "  (memory " ^ Int.to_string mem.size ^ ")"

let pp_module (m : wasm_module) (labled : bool) =
  "(module" ^ pp_memory m.memory
  ^ List.fold_left
      (fun _s g -> _s ^ nl ^ (pp_global (pp_type labled)) g)
      "" m.globals
  ^ List.fold_left
      (fun _s fi -> _s ^ nl ^ (pp_function_import (pp_type labled)) fi)
      "" m.function_imports
  ^ List.fold_left
      (fun _s f -> _s ^ nl ^ (pp_function (pp_type labled)) f)
      "" m.functions
  ^ nl ^ ")"
