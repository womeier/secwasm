
type value_type = I32Type

type 'a labeled_value_type = 
  {t: value_type; lbl: 'a}

type stack_type = value_type list
type fun_type = FunType of stack_type * stack_type
type global_type = GlobalType of value_type

type value = I32Value of int32

let type_of_value = function
  | I32Value _ -> I32Type

module I32Op = struct
  type binop = Add
  type cmpop = Eq
end

type binop = I32Op.binop
type cmpop = I32Op.cmpop

type loadop = Load of value_type
type storeop = Store of value_type

let type_of_binop (bop : binop) = match bop with
  | I32Op.Add -> I32Type

let type_of_cmpop (cop : cmpop) = match cop with
  | I32Op.Eq -> I32Type

type wasm_instruction = 
  | WI_Unreachable                              (* trap unconditionally *)
  | WI_Nop                                      (* do nothing *)
  | WI_Drop                                     (* drop value *)

  | WI_Const of value                           (* constant *)
  | WI_BinOp of binop                           (* binary numeric operator *)
  | WI_CmpOp of cmpop                           (* numeric comparison operator *)

  | WI_Call of int                              (* call function *)

  | WI_LocalGet of int                          (* read local variable *)
  | WI_LocalSet of int                          (* write local variable *)
  | WI_GlobalGet of int                         (* read global variable *)
  | WI_GlobalSet of int                         (* write global variable *)

  | WI_Load of value_type                       (* read memory at address *)
  | WI_Store of value_type                      (* write memory at address *)

  | WI_If of wasm_instruction list * wasm_instruction list
  

type wasm_global = {
  gtype : value_type;
  const : wasm_instruction list;
}

type wasm_func = {
  ftype : fun_type;
  locals : value_type list;
  body : wasm_instruction list;
  export_name : string option;                  (* export name, should start with '$' *)
}

type wasm_module = {
  globals : wasm_global list;
  functions : wasm_func list;
}

(************************* EXAMPLE ************************************)

let example_module: wasm_module = {
  globals = [];
  functions = [
    {
      ftype = FunType ([I32Type; I32Type], []);
      locals = [I32Type];
      body = [
        WI_Nop; 
        WI_LocalGet 0; 
        WI_LocalGet 1; 
        WI_BinOp I32Op.Add; 
        WI_Const (I32Value 0l); 
        WI_CmpOp I32Op.Eq; 
        WI_If (
          [WI_Nop; WI_Const (I32Value 2l); WI_LocalSet 0],
          [WI_Const (I32Value 42l); WI_LocalSet 0])
      ];
      export_name = Some "$hello"
    }
  ]
}


(************************* PRETTY PRINTING ************************************)

let pp_type (t : value_type) =
  match t with
  | I32Type -> "i32"

let nl = "\n"

let pp_binop (bop : binop) =
  match bop with 
  | I32Op.Add -> "i32.add"

let pp_cmpop (cop : cmpop) = 
  match cop with
  | I32Op.Eq -> "i32.eq"

let pp_const c = 
  match c with
  | I32Value v -> "i32.const " ^ (Int32.to_string v)

let rec pp_instruction (indent : int) (instr : wasm_instruction) =
  let pp_instructions (indent' : int) (instructions : wasm_instruction list) =
    List.fold_left (fun _s i -> _s ^ (pp_instruction indent' i) ^ nl) "" instructions in

  let rec spaces (n : int) =
    match n with
    | 0 -> ""
    | _ -> " " ^ (spaces (n-1)) in

  spaces indent ^
  (match instr with 
  | WI_Unreachable        -> "unreachable"
  | WI_Nop                -> "nop"
  | WI_Const v            -> pp_const v
  | WI_BinOp op           -> pp_binop op
  | WI_CmpOp op           -> pp_cmpop op
  | WI_Call idx           -> "call " ^ (string_of_int idx)
  | WI_Drop               -> "drop"

  | WI_LocalGet idx       -> "local.get " ^ (string_of_int idx)
  | WI_LocalSet idx       -> "local.set " ^ (string_of_int idx)
  | WI_GlobalGet idx      -> "global.get " ^ (string_of_int idx)
  | WI_GlobalSet idx      -> "global.set " ^ (string_of_int idx)

  | WI_Load t             -> (pp_type t) ^ ".load"
  | WI_Store t            -> (pp_type t) ^ ".store"

  | WI_If (b1, b2)    -> "if" ^ nl ^
                            pp_instructions (indent + 2) b1 ^ nl ^ (spaces indent) ^
                         "else" ^ nl ^
                            pp_instructions (indent + 2) b2 ^ nl ^ (spaces indent) ^
                         "end" ^ nl
  )

let pp_function (f : wasm_func) =
  let ps = match f.ftype with
           | FunType (plist, _) -> plist in

  let locals = if List.length f.locals > 0
               then " (local" ^ (List.fold_left (fun _s l -> " " ^ pp_type l ^ _s) "" f.locals) ^ ")"
               else ""

  and params = if List.length ps > 0
               then (" (param" ^ (List.fold_left (fun _s l -> " " ^ pp_type l ^ _s) "" ps) ^ ")")
               else ""

  and result = match f.ftype with
               | FunType (_, [res]) -> " (result " ^ (pp_type res) ^ ")"
               | _ -> ""

  and export = match f.export_name with
               | Some name -> " (export \"" ^ name ^ "\")"
               | None -> ""

  and body = List.fold_left (fun _s i -> _s ^ nl ^ (pp_instruction 2 i)) "" f.body
  in "(func" ^ export ^ params ^ result ^ nl ^ locals ^ nl
  ^ body ^ nl
  ^ ")"

let pp_module (m : wasm_module) = 
  "(module" ^ nl
  ^ (List.fold_left (fun _s f -> _s ^ nl ^ (pp_function f)) "" m.functions)
  ^ nl ^ ")"