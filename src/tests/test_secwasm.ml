open Secwasm.Ast
open Secwasm.Type_check

let print_test_result test_id result = 
  print_endline ("Test " ^ test_id ^ ": " ^ (if result then "Success" else "Failure"))

(*
  (module 
    (func 
      (result i32)
      i32.const 1
      i32.const 1
      i32.add
    )
  )
*)
let module1 = 
  {
    globals = [];
    functions = [
      {
        ftype = FunType ([], [I32]); 
        locals = []; 
        body = [WI_Const 1l; WI_Const 1l; WI_BinOp Add]; 
        export_name = None
      }
    ]
  }

let test1 = 
  fun () -> 
    let result = type_check_module module1 in 
    print_test_result "test1" result


let test2 = 
  fun () -> print_test_result "test2" false

let () = 
  test1();
  test2();
  ()


