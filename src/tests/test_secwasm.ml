open Secwasm.Ast
open Secwasm.Type_check

let next_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "%d" !counter

let print_test_result name result =
  let s = if result then "SUCCESS" else "FAIL" in
  print_endline ("Test" ^ next_id () ^ " [" ^ name ^ "]\t\t: " ^ s)

let test_check_module (name : string) (m : wasm_module) =
  print_test_result name (type_check_module m)

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
let module1_add_consts =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], [ I32 ]);
          locals = [];
          body = [ WI_Const 1l; WI_Const 1l; WI_BinOp Add ];
          export_name = None;
        };
      ];
  }

(*
  (module
    (func
      (local i32)
      i32.const 42
      local.set 0
    )
  )
*)
let module2_local_set =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], []);
          locals = [ I32 ];
          body = [ WI_Const 42l; WI_LocalSet 0l ];
          export_name = None;
        };
      ];
  }

let _ = test_check_module "add consts" module1_add_consts
let _ = test_check_module "local.set" module2_local_set
