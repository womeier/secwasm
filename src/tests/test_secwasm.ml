open Secwasm.Ast
open Secwasm.Type_check
open OUnit2

type res = exn option

let test_check_module (expect : res) (m : wasm_module) (_ : test_ctxt) =
  let f () = type_check_module m in
  match expect with
  | None -> assert_equal () (f ())
  | Some e -> assert_raises e f

let pos_test (m : wasm_module) = test_check_module None m
let neg_test (m : wasm_module) (e : exn) = test_check_module (Some e) m

(* ======= Modules under test  ========= *)

let test_list : test list ref = ref []
let ( ~+ ) t = test_list := !test_list @ [ t ]

(*
  Add two constants

  (module
    (func
      (result i32)
      i32.const 1
      i32.const 1
      i32.add
    )
  )
*)
let m_add_consts : wasm_module =
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

let _ = ~+("add consts" >:: pos_test m_add_consts)

(*
  Add is missing an operand

  (module
    (func
      (result i32)
      i32.const 1
      i32.add
    )
  )
*)
let m_add_consts2 : wasm_module =
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

let _ = ~+("add consts 2" >:: neg_test m_add_consts2 (TypingError err_msg_binop))

(*
  Nop is well-typed

  (module
    (func
      nop
    )
  )
*)
let m_nop : wasm_module =
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

let _ = ~+("nop" >:: pos_test m_nop)

(*
  Unreachable is well-typed

  (module
    (func
      unreachable
    )
  )
*)
let m_unreachable : wasm_module =
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

let _ = ~+("unreachable" >:: pos_test m_unreachable)

(*
  Push a constant, drop it again

  (module
    (func
      i32.const 42
      drop
    )
  )
*)
let m_drop : wasm_module =
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

let _ = ~+("drop" >:: pos_test m_drop)

(*
  Nothing to drop

  (module
    (func
      drop
    )
  )
*)
let m_drop : wasm_module =
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

let _ = ~+("drop 2" >:: neg_test m_drop (TypingError err_msg_drop))

(*
  Get a public local variable

  (module
    (func
      (local i32)
      local.get 0
    )
  )
*)
let m_local_get : wasm_module =
  {
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], []);
          locals = [ I32 ];
          body = [ WI_LocalGet 0l ];
          export_name = None;
        };
      ];
  }

let _ = ~+("local.get" >:: pos_test m_local_get)

(*
  Set a public local variable

  (module
    (func
      (local i32)
      i32.const 42
      local.set 0
    )
  )
*)
let m_local_set =
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

let _ = ~+("local.set" >:: pos_test m_local_set)

(*  ================= End of tests ================== *)
(*  Run suite! *)

let _ = run_test_tt_main ("SecMiniWasm" >::: !test_list)

(* TODO : Refactor example into test

   let example_module : wasm_module =
     {
       globals = [];
       functions =
         [
           {
             ftype = FunType ([ I32; Public; I32 ], []);
             locals = [ I32 ];
             body =
               [
                 WI_Nop;
                 WI_LocalGet 0l;
                 WI_LocalGet 1l;
                 WI_BinOp Add;
                 WI_Const 0l;
                 WI_BinOp Eq;
                 WI_If
                   ( [ WI_Nop; WI_Const 2l; WI_LocalSet 0l ],
                     [ WI_Const 42l; WI_LocalSet 0l ] );
               ];
             export_name = Some "hello";
           };
         ];
     }

   let example_module' =
     {
       globals =
         [
           { gtype = { t = I32; lbl = Secret }; const = [ WI_Const 40l ] };
           { gtype = { t = I32; lbl = Public }; const = [ WI_Const 0l ] };
         ];
       functions =
         [
           {
             ftype = FunType ([], []);
             locals = [];
             body =
               [
                 WI_Const 1l;
                 WI_Const 1l;
                 WI_BinOp Add;
                 WI_GlobalGet 0l;
                 WI_BinOp Add;
                 WI_GlobalSet 1l;
               ];
             export_name = None;
           };
         ];
     }
*)
