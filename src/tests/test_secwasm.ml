open Secwasm.Ast
open Secwasm.Type_check

let next_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "%d" !counter

let print_test_result (expect : bool) (result : bool) =
  let s =
    if result = expect then "SUCCESS"
    else Printf.sprintf "FAIL: Expected typable=%b, was %b" expect result
  in
  print_endline s

let test_check_module (expect : bool) (name : string) (m : wasm_module) =
  let id = next_id () in
  let s = "Running test " ^ id ^ " [" ^ name ^ "] ... " in
  let padding = String.make (max 0 (50 - String.length s)) ' ' in
  print_string (s ^ padding);
  try
    let _ = type_check_module m in
    print_test_result expect true (* m type checks *)
  with exn ->
    print_test_result expect false;
    if expect then (
      (* only print exceptions on positive tests *)
      Printexc.print_backtrace stdout;
      print_endline (Printexc.to_string exn))

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
      (result i32)
      i32.const 1
      i32.add
    )
  )
*)
let module_add_consts2 =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], [ I32 ]);
          locals = [];
          body = [ WI_Const 1l; WI_BinOp Add ];
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

(*
  (module
    (func
      unreachable
    )
  )
*)
let module_uncreachable =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], []);
          locals = [ I32 ];
          body = [ WI_Unreachable ];
          export_name = None;
        };
      ];
  }

(*
  (module
    (func
      i32.const 42
      drop
    )
  )
*)
let module_drop =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], []);
          locals = [ I32 ];
          body = [ WI_Const 42l; WI_Drop ];
          export_name = None;
        };
      ];
  }

(*
  (module
    (func
      drop
    )
  )
*)
let module_drop2 =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], []);
          locals = [ I32 ];
          body = [ WI_Drop ];
          export_name = None;
        };
      ];
  }

let _ = test_check_module true "add consts" module_add_consts
let _ = test_check_module false "add consts 2" module_add_consts2
let _ = test_check_module true "nop" module_nop
let _ = test_check_module true "unreachable" module_uncreachable
let _ = test_check_module true "drop" module_drop
let _ = test_check_module false "drop 2" module_drop2
let _ = test_check_module true "local.set" module_local_set
