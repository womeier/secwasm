open Secwasm.Ast

let () = 
  let oc = open_out "out.wat" in
  Printf.fprintf oc "%s\n" (pp_module example_module);
  close_out oc;
