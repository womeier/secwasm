open Sec

[@@@coverage exclude_file]

type value_type = I32

(* This is equivalent to tau in the paper (typing judgements) *)
type labeled_value_type = { t : value_type; lbl : SimpleLattice.t }
type stack_type = labeled_value_type list
type fun_type = FunType of stack_type * SimpleLattice.t * stack_type
type block_type = BlockType of stack_type * stack_type
type binop = Add | Eq

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

type wasm_memory = { min_size : int; max_size : int option } (* in #pages *)

type wasm_module = {
  globals : wasm_global list;
  functions : wasm_func list;
  memories : wasm_memory list;
}

(************************* PRETTY PRINTING ************************************)

let pp_type (t : value_type) = match t with I32 -> "i32"

let pp_labeled_type (t : labeled_value_type) =
  pp_type t.t (* TODO: have separate pp *)

let nl = "\n"

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
  | WI_BinOp Eq -> "i32.eq"
  | WI_Call idx -> "call " ^ Int.to_string idx
  | WI_Drop -> "drop"
  | WI_LocalGet idx -> "local.get " ^ Int.to_string idx
  | WI_LocalSet idx -> "local.set " ^ Int.to_string idx
  | WI_GlobalGet idx -> "global.get " ^ Int.to_string idx
  | WI_GlobalSet idx -> "global.set " ^ Int.to_string idx
  | WI_Load _ -> "i32.load"
  | WI_Store _ -> "i32.store"
  | WI_Block (_, b) -> "(block " ^ nl ^ pp_instructions (indent + 2) b
  | WI_Br idx -> "br " ^ Int.to_string idx
  | WI_BrIf idx -> "br_if " ^ Int.to_string idx

let pp_function (f : wasm_func) =
  let ps = match f.ftype with FunType (plist, _, _) -> plist in

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
    | FunType (_, _, []) -> ""
    | FunType (_, _, [ res ]) -> " (result " ^ pp_labeled_type res ^ ")"
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

let pp_module (m : wasm_module) =
  (* TODO: memory, globals, anything else? *)
  "(module" ^ nl
  ^ List.fold_left (fun _s f -> _s ^ nl ^ pp_function f) "" m.functions
  ^ nl ^ ")"
