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
let neg_test (e : exn) (m : wasm_module) = test_check_module (Some e) m
let test_list : test list ref = ref []

let test (name : string) (t : wasm_module -> test_ctxt -> unit)
    (m : wasm_module) =
  test_list := !test_list @ [ name >:: t m ]

(* ======= Modules under test  ========= *)

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

let _ =
  test "add consts" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Const 1l; WI_Const 1l; WI_BinOp Add ];
            export_name = None;
          };
        ];
    }

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
let _ =
  test "add consts 2" (neg_test err_binop)
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Const 1l; WI_BinOp Add ];
            export_name = None;
          };
        ];
    }

(*
  Nop is well-typed

  (module
    (func
      nop
    )
  )
*)
let _ =
  test "nop" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [ { t = I32; lbl = Public } ];
            body = [ WI_Nop ];
            export_name = None;
          };
        ];
    }

(*
  Unreachable is well-typed

  (module
    (func
      unreachable
    )
  )
*)
let _ =
  test "unreachable" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Unreachable ];
            export_name = None;
          };
        ];
    }

(*
  Push a constant, drop it again

  (module
    (func
      i32.const 42
      drop
    )
  )
*)
let _ =
  test "drop" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [ { t = I32; lbl = Public } ];
            body = [ WI_Const 42l; WI_Drop ];
            export_name = None;
          };
        ];
    }

(*
  Nothing to drop

  (module
    (func
      drop
    )
  )
*)
let _ =
  test "drop 2" (neg_test err_drop)
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [ { t = I32; lbl = Public } ];
            body = [ WI_Drop ];
            export_name = None;
          };
        ];
    }

(*
  Get a public local variable

  (module
    (func
      (local i32)
      local.get 0
    )
  )
*)
let _ =
  test "local.get" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [ { t = I32; lbl = Public } ];
            body = [ WI_LocalGet 0l ];
            export_name = None;
          };
        ];
    }

(*
  Set a public global variable

  (module
    (global (mut i32) (i32.const 0))
    (func
      i32.const 42
      global.set 0
    )
  )
*)
let _ =
  test "global.set" pos_test
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Public };
            const = [ WI_Const 0l ];
            mut = true;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 42l; WI_GlobalSet 0l ];
            export_name = None;
          };
        ];
    }

(*
  Forbidden explicit flow from secret global variable to public global variable

  (module
    (global i32 (i32.const 0))       ;; secret
    (global (mut i32) (i32.const 0)) ;; public
    (func
      global.get 0
      i32.const 1
      i32.add
      global.set 1
    )
  )
*)
let _ =
  test "forbidden explicit flow (global vars)"
    (neg_test
       (PrivacyViolation
          "global.set expected pc \226\138\148 l \226\138\145 l' but was \
           pc=Public, l=Secret, l'=Public"))
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 0l ];
            mut = false;
          };
          {
            gtype = { t = I32; lbl = Public };
            const = [ WI_Const 0l ];
            mut = true;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [ WI_GlobalGet 0l; WI_Const 1l; WI_BinOp Add; WI_GlobalSet 1l ];
            export_name = None;
          };
        ];
    }

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
let _ =
  test "local.set" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [ { t = I32; lbl = Public } ];
            body = [ WI_Const 42l; WI_LocalSet 0l ];
            export_name = None;
          };
        ];
    }

(*
  Load from memory

  (module
    (memory 1)
    (func
      i32.const 0
      i32.load
    )
  )
*)
let _ =
  test "load" pos_test
    {
      memories = [ { min_size = 1l; max_size = None } ];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 0l; WI_Load Public ];
            export_name = None;
          };
        ];
    }

(*
  Store to memory

  (module
    (memory 1)
    (func
      i32.const 0
      i32.const 42
      i32.store
    )
  )
*)
let _ =
  test "store" pos_test
    {
      memories = [ { min_size = 1l; max_size = None } ];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 0l; WI_Const 42l; WI_Store Public ];
            export_name = None;
          };
        ];
    }

(*
  Forbidden implicit flow via load from secret address

  (module
    (memory 3)
    (global i32 (i32.const 0))       ;; secret
    (global (mut i32) (i32.const 0)) ;; public
    (func
      global.get 0    ;; secret address
      i32.load Public ;; value itself not secret
      global.set 1
    )
  )
*)
let _ =
  test "forbidden implicit flow via load from secret address"
    (neg_test
       (PrivacyViolation
          "global.set expected pc \226\138\148 l \226\138\145 l' but was \
           pc=Public, l=Secret, l'=Public"))
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 0l ];
            mut = false;
          };
          {
            gtype = { t = I32; lbl = Public };
            const = [ WI_Const 0l ];
            mut = true;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_GlobalGet 0l; WI_Load Public; WI_GlobalSet 1l ];
            export_name = None;
          };
        ];
    }

(*
  Simple block

  (module
    (func
      (block
        nop
      end)
    )
  )
*)
let _ =
  test "block 1" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Block (BlockType ([], []), [ WI_Nop ]) ];
            export_name = None;
          };
        ];
    }

(*
  Simple nested block

  (module
    (func
      (block
        (block
          nop
        end)
      end)
    )
  )
*)
let _ =
  test "nested block" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [
                WI_Block
                  ( BlockType ([], []),
                    [ WI_Block (BlockType ([], []), [ WI_Nop ]) ] );
              ];
            export_name = None;
          };
        ];
    }

(*
  Block with simple params

  (module
    (func
      i32.const 42
      (block i32 L ->
        drop
      end)
    )
  )
*)
let _ =
  test "block with simple param" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [
                WI_Const 42l;
                WI_Block
                  (BlockType ([ { t = I32; lbl = Public } ], []), [ WI_Drop ]);
              ];
            export_name = None;
          };
        ];
    }

(*
  Block with simple params is ill-typed since stack is empty

  (module
    (func
      (block i32 L ->
        nop
      end)
    )
  )
*)
let _ =
  test "block with simple param"
    (neg_test (err_block1 1 0))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [
                WI_Block
                  (BlockType ([ { t = I32; lbl = Public } ], []), [ WI_Nop ]);
              ];
            export_name = None;
          };
        ];
    }

(*
  Block with simple params is ill-typed since stack is empty

  (module
    (func
      (block i32 L ->
        drop
      end)
    )
  )
*)
let _ =
  test "block with simple param 2"
    (neg_test (err_block2 1 0))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [
                WI_Const 42l;
                WI_Block
                  ( BlockType
                      ( [ { t = I32; lbl = Public } ],
                        [ { t = I32; lbl = Public } ] ),
                    [ WI_Drop ] );
              ];
            export_name = None;
          };
        ];
    }

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
