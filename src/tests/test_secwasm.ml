open Secwasm.Ast
open Secwasm.Type_check

let next_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "%d" !counter

let print_test_result result =
  let s = if result then "SUCCESS" else "FAIL" in
  print_endline s

let test_check_module (name : string) (m : wasm_module) =
  let id = next_id () in
  let s = "Running test " ^ id ^ " [" ^ name ^ "] ... " in
  let padding = String.make (max 0 (50 - String.length s)) ' ' in
  print_string (s ^ padding);
  try print_test_result (type_check_module m)
  with exn ->
    print_endline "FAIL!";
    Printexc.print_backtrace stdout;
    print_endline (Printexc.to_string exn)

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
let module_add_consts =
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
let module_local_set =
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

(*
  (module
    (func
      nop
    )
  )
*)
let module_nop =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], []);
          locals = [ I32 ];
          body = [ WI_Nop ];
          export_name = None;
        };
      ];
  }

let _ = test_check_module "add consts" module_add_consts
let _ = test_check_module "nop" module_nop
let _ = test_check_module "local.set" module_local_set
