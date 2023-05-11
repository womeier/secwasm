open Secwasm.Ast
open Secwasm.Type_check

let () =
  ignore (type_check_module example_module');
  let oc = open_out "out.wat" in
  Printf.fprintf oc "%s\n" (pp_module example_module);
  close_out oc
