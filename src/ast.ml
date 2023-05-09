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

  | WI_local_get of int                         (* read local variable *)
  | WI_local_set of int                          (* write local variable *)
  | WI_global_get of int                         (* read global variable *)
  | WI_global_set of int                         (* write global variable *)

  | WI_load of i_nn_type                        (* read memory at address *)
  | WI_store of i_nn_type                       (* write memory at address *)
  

type wasm_global = {
  gtype : i_nn_type;
  const : wasm_instruction list;
}

type wasm_func = {
  ftype : fun_type;
  locals : i_nn_type list;
  body : wasm_instruction list;
}

type wasm_module = {
  globals : wasm_global list;
  functions : wasm_func list;
}


let example_module: wasm_module = {
  globals = [];
  functions = [
    {
      ftype = FunType ([], []);
      locals = [];
      body = [WI_nop]
    }
  ]
}

let print_instruction (i : wasm_instruction) =
  match i with 
  | WI_nop -> "nop"
  | _ -> "TBD"

let print_function (f : wasm_func) =
  let print_locals (locals : i_nn_type list) = List.map (fun _ -> "i32")  locals
  and print_body (body: wasm_instruction list) = ""
in match f with
    | { ftype; locals; body } -> 
      "(func \n"
      ^ "(param " ^ (String.concat "" (print_locals locals))
      ^ "( " ^ (print_body body)



let print_module (m : wasm_module) = 
  "(module)"
  

let () =
  let oc = open_out "out.wat" in
    Printf.fprintf oc "%s\n" (print_module example_module);
    close_out oc;