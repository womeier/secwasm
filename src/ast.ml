type i_nn_type = 
  | I32
  (* | I64 *)

type fun_type = 
  | FunType of ((i_nn_type list) * (i_nn_type list))

type wasm_instruction =
  | WI_unreachable                              (* trap unconditionally *)
  | WI_nop                                      (* do nothing *)
  | WI_const of i_nn_type * int                 (* constant *)
  | WI_add of i_nn_type                         (* add, slight deviation from spec *)
  | WI_call of int                              (* call function *)
  | WI_drop                                     (* drop value *)

  | WI_local_get of int                         (* read local variable *)
  | WI_local_set of int                         (* write local variable *)
  | WI_global_get of int                        (* read global variable *)
  | WI_global_set of int                        (* write global variable *)

  | WI_load of i_nn_type                        (* read memory at address *)
  | WI_store of i_nn_type                       (* write memory at address *)

  | WI_if of wasm_instruction list * wasm_instruction list
  | WI_eq of i_nn_type                          (* eq, slightlty different then spec *)
  

type wasm_global = {
  gtype : i_nn_type;
  const : wasm_instruction list;
}

type wasm_func = {
  ftype : fun_type;
  locals : i_nn_type list;
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
      ftype = FunType ([I32; I32], []);
      locals = [I32];
      body = [WI_nop; WI_local_get 0; WI_local_get 1; WI_add I32; WI_const (I32, 0); WI_eq I32; WI_if ([WI_nop; WI_const (I32, 2); WI_local_set 0],
      [WI_const (I32, 42); WI_local_set 0])];
      export_name = Some "$hello"
    }
  ]
}


(************************* PRETTY PRINTING ************************************)

let pp_type (t : i_nn_type) =
  match t with
  | I32 -> "i32"

let nl = "\n"


let rec pp_instruction (indent : int) (instr : wasm_instruction) =
  let pp_instructions (indent' : int) (instructions : wasm_instruction list) =
    List.fold_left (fun _s i -> _s ^ (pp_instruction indent' i) ^ nl) "" instructions in

  let rec spaces (n : int) =
    match n with
    | 0 -> ""
    | _ -> " " ^ (spaces (n-1)) in

  spaces indent ^
  (match instr with 
  | WI_unreachable     -> "unreachable"
  | WI_nop             -> "nop"
  | WI_const (t, c)    -> (pp_type t) ^ ".const " ^ (string_of_int c)
  | WI_add t           -> (pp_type t) ^ ".add"
  | WI_call idx        -> "call " ^ (string_of_int idx)
  | WI_drop            -> "drop"

  | WI_local_get idx  -> "local.get " ^ (string_of_int idx)
  | WI_local_set idx  -> "local.set " ^ (string_of_int idx)
  | WI_global_get idx -> "global.get " ^ (string_of_int idx)
  | WI_global_set idx -> "global.set " ^ (string_of_int idx)

  | WI_load t         -> (pp_type t) ^ ".load"
  | WI_store t        -> (pp_type t) ^ ".store"

  | WI_eq t           -> (pp_type t) ^ ".eq"
  | WI_if (b1, b2)    -> "if" ^ nl ^
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

let () =
  let oc = open_out "out.wat" in
    Printf.fprintf oc "%s\n" (pp_module example_module);
    close_out oc;
