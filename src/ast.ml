open Sec
open Util

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

let check_stack s1 s2 =
  assert (List.length s1 = List.length s2 && List.for_all2 (fun t1 -> fun t2 -> t1 == t2) s1 s2)

let pop (s1 : i_nn_type list) (s2 : i_nn_type list) =
  let l1 = List.length s1 in
  let l2 = List.length s2 in
  let l = min l1 l2 in 
  check_stack s1 (ListUtil.drop (l2 - l) s2);
  ListUtil.take (l2 - l) s2

let rec check_instr (i : wasm_instruction) (_ : i_nn_type list) : (i_nn_type list) * (i_nn_type list) =
  match i with 
  | WI_unreachable -> ([], [])
  | WI_nop -> ([], [])
  | WI_const (I32, _) -> ([], [I32])
  | WI_add I32-> ([I32; I32], [I32])
  | _ -> ([], [])
  

(* TODO: A non-lazy version of split-last *)
let rec check_seq (seq : wasm_instruction list) : i_nn_type list =
  match seq with
  | [] ->
    []
  | _ -> 
    match List.rev seq with 
    | [] -> 
      []
    | h :: seq' ->
      let stack = check_seq (List.rev seq') in
      let (ins, outs) = check_instr h stack in 
      (pop ins stack) @ outs



let type_check_function (f : wasm_func) = 
  let _ = check_seq f.body 
  in true
  
  

let type_check_module (m : wasm_module) = 
  List.for_all type_check_function m.functions

let example_module' = {
    globals = [];
    functions = [
      {
        ftype = FunType ([], []);
        locals = [];
        body = [WI_const (I32, 1); WI_const (I32, 1); WI_add I32; WI_const (I32, 40); WI_add I32]
      }
    ]
  }

let () =
  ignore (type_check_module example_module')

  (* let oc = open_out "out.wat" in
    ignore (type_check_module example_module);
    Printf.fprintf oc "%s\n" (print_module example_module);
    close_out oc; *)