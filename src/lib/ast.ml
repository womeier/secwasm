open Sec

type value_type = I32

let str (t : value_type) = match t with I32 -> "I32"

(* This is equivalent to tau in the paper (typing judgements) *)
type 'lt labeled_value_type = { t : value_type; lbl : 'lt }
type 'lt stack_type = 'lt labeled_value_type list

type 'lt fun_type =
  | FunType of { params : 'lt stack_type; label : 'lt; result : 'lt stack_type }

type 'lt block_type =
  | BlockType of { params : 'lt stack_type; result : 'lt stack_type }

type binop = Add | Eq

[@@@ocamlformat "disable"]

type 'lt wasm_instruction =
  | WI_Unreachable                                                    (* trap unconditionally *)
  | WI_Drop                                                           (* drop value *)
  | WI_Const of int32                                                 (* constant *)
  | WI_BinOp of binop                                                 (* binary numeric operator *)
  | WI_Call of int32                                                  (* call function *)
  | WI_LocalGet of int32                                              (* read local variable *)
  | WI_LocalSet of int32                                              (* write local variable *)
  | WI_GlobalGet of int32                                             (* read global variable *)
  | WI_GlobalSet of int32                                             (* write global variable *)
  | WI_Load of 'lt                                       (* read memory at address *)
  | WI_Store of 'lt                                       (* write memory at address *)
  | WI_If of 'lt fun_type * ('lt wasm_instruction list) * ('lt wasm_instruction list) (* if then else *)
  | WI_Block of 'lt wasm_block                     (* block *)
  | WI_Br of int32                                                    (* unconditional branch *)
  | WI_BrIf of int32                                                  (* conditional branch *)
  | WI_Nop
and 'lt wasm_block = { btype : 'lt block_type; instrs : 'lt wasm_instruction list }

[@@@ocamlformat "enable"]

type 'lt wasm_global = {
  gtype : 'lt labeled_value_type;
  const : 'lt wasm_instruction list;
  mut : bool;
}

type 'lt wasm_func = {
  ftype : 'lt fun_type;
  locals : 'lt labeled_value_type list;
  body : 'lt wasm_instruction list;
  export_name : string option; (* export name, should start with '$' *)
}

type 'lt wasm_memory = { min_size : int32; max_size : int32 option }
(* in #pages *)

type 'lt wasm_module = {
  globals : 'lt wasm_global list;
  functions : 'lt wasm_func list;
  memories : 'lt wasm_memory list;
}

let empty_module = {globals = []; functions = []; memories = []}

(************************* PRETTY PRINTING ************************************)

let pp_type (t : value_type) = match t with I32 -> "i32"

let pp_labeled_type (t : 'lt labeled_value_type) =
  pp_type t.t (* TODO: have separate pp *)

let nl = "\n"

let rec pp_instruction (indent : int) (instr : 'lt wasm_instruction) =
  let pp_instructions (indent' : int) (instructions : 'lt wasm_instruction list)
      =
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
  | WI_Const v -> "i32.const " ^ Int32.to_string v
  | WI_BinOp Add -> "i32.add"
  | WI_BinOp Eq -> "i32.eq"
  | WI_Call idx -> "call " ^ Int32.to_string idx
  | WI_Drop -> "drop"
  | WI_LocalGet idx -> "local.get " ^ Int32.to_string idx
  | WI_LocalSet idx -> "local.set " ^ Int32.to_string idx
  | WI_GlobalGet idx -> "global.get " ^ Int32.to_string idx
  | WI_GlobalSet idx -> "global.set " ^ Int32.to_string idx
  | WI_Load _ -> "i32.load"
  | WI_Store _ -> "i32.store"
  | WI_If (_, b1, b2) ->
      "if" ^ nl
      ^ pp_instructions (indent + 2) b1
      ^ nl ^ spaces indent ^ "else" ^ nl
      ^ pp_instructions (indent + 2) b2
      ^ nl ^ spaces indent ^ "end" ^ nl
  | WI_Block { instrs; _ } ->
      "(block " ^ nl ^ pp_instructions (indent + 2) instrs
  | WI_Br idx -> "br " ^ Int32.to_string idx
  | WI_BrIf idx -> "br_if " ^ Int32.to_string idx

let pp_function (f : 'lt wasm_func) =
  let ps = match f.ftype with FunType { params = plist; _ } -> plist in

  let locals =
    if List.length f.locals > 0 then
      " (local"
      ^ List.fold_left (fun _s l -> " " ^ pp_labeled_type l ^ _s) "" f.locals
      ^ ")"
    else ""
  and params =
    if List.length ps > 0 then
      " (param"
      ^ List.fold_left (fun _s l -> " " ^ pp_labeled_type l ^ _s) "" ps
      ^ ")"
    else ""
  and result =
    match f.ftype with
    | FunType { result = []; _ } -> ""
    | FunType { result = [ res ]; _ } -> " (result " ^ pp_labeled_type res ^ ")"
    | _ ->
        failwith
          "PP error, instruction return type can be at most a single value"
  and export =
    match f.export_name with
    | Some name -> " (export \"" ^ name ^ "\")"
    | None -> ""
  and body =
    List.fold_left (fun _s i -> _s ^ nl ^ pp_instruction 2 i) "" f.body
  in
  "(func" ^ export ^ params ^ result ^ nl ^ locals ^ nl ^ body ^ nl ^ ")"

let pp_module (m : 'lt wasm_module) =
  (* TODO: memory, globals, anything else? *)
  "(module" ^ nl
  ^ List.fold_left (fun _s f -> _s ^ nl ^ pp_function f) "" m.functions
  ^ nl ^ ")"
