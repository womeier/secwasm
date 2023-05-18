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
            body = [ WI_Const 1; WI_Const 1; WI_BinOp Add ];
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
            body = [ WI_Const 1; WI_BinOp Add ];
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
            body = [ WI_Const 42; WI_Drop ];
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
      drop
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
            body = [ WI_LocalGet 0; WI_Drop ];
            export_name = None;
          };
        ];
    }

(*
  Get non-existing local variable

  (module
    (func
      local.get 0
      drop
    )
  )
*)
let _ =
  test "local.get non-existing local"
    (neg_test (TypingError "expected local variable of index 0"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_LocalGet 0 ];
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
            const = [ WI_Const 0 ];
            mut = true;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 42; WI_GlobalSet 0 ];
            export_name = None;
          };
        ];
    }

(*
  Set a non-mut public global variable

  (module
    (global i32 (i32.const 0))
    (func
      i32.const 42
      global.set 0
    )
  )
*)
let _ =
  test "set non-mut global var"
    (neg_test (TypingError "global.set expected global var to be mutable"))
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Public };
            const = [ WI_Const 0 ];
            mut = false;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 42; WI_GlobalSet 0 ];
            export_name = None;
          };
        ];
    }

(*
  Set a non-existing public global variable

  (module
    (func
      i32.const 42
      global.set 0
    )
  )
*)
let _ =
  test "set non-existing global var"
    (neg_test (TypingError "expected global variable of index 0"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 42; WI_GlobalSet 0 ];
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
            const = [ WI_Const 0 ];
            mut = false;
          };
          {
            gtype = { t = I32; lbl = Public };
            const = [ WI_Const 0 ];
            mut = true;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_GlobalGet 0; WI_Const 1; WI_BinOp Add; WI_GlobalSet 1 ];
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
            body = [ WI_Const 42; WI_LocalSet 0 ];
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
      memories = [ { min_size = 1; max_size = None } ];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Const 0; WI_Load Public ];
            export_name = None;
          };
        ];
    }

(*
  Load no memory present

  (module
    (func
      i32.const 0
      i32.load
    )
  )
*)
let _ =
  test "load no memory present "
    (neg_test (TypingError "load expected memory in the context"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Const 0; WI_Load Public ];
            export_name = None;
          };
        ];
    }

(*
  Store no memory present

  (module
    (func
      i32.const 0
      i32.load
    )
  )
*)
let _ =
  test "store no memory present "
    (neg_test (TypingError "store expected memory in the context"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 0; WI_Const 0; WI_Store Public ];
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
      memories = [ { min_size = 1; max_size = None } ];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 0; WI_Const 42; WI_Store Public ];
            export_name = None;
          };
        ];
    }

(*
  Storing secret value in public memory is forbidden

  (module
    (memory 1)
    (global i32<Secret>)
    (func
      i32.const 0
      global.get 0
      i32.store Public
    )
  )
*)
let _ =
  test "store secret value in public memory"
    (neg_test
       (PrivacyViolation
          "store expected pc ⊔ la ⊔ lv ⊑ lm but was pc=Public, la=Public, \
           lv=Secret, lm=Public"))
    {
      memories = [ { min_size = 1; max_size = None } ];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 0 ];
            mut = false;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 0; WI_GlobalGet 0; WI_Store Public ];
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
      memories = [ { min_size = 1; max_size = None } ];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 0 ];
            mut = false;
          };
          {
            gtype = { t = I32; lbl = Public };
            const = [ WI_Const 0 ];
            mut = true;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_GlobalGet 0; WI_Load Public; WI_GlobalSet 1 ];
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
                WI_Const 42;
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
                WI_Const 42;
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

(*
  Block input stack has higher security level than block params security level

  (module
    (global i32<Secret> (i32.const 42))
    (func
      global.get 0
      (block i32<Public> -> ϵ
        drop
      end)
    )
  )
*)
let _ =
  test "block input stack incorrect security level"
    (neg_test
       (err_block3 [ { t = I32; lbl = Public } ] [ { t = I32; lbl = Secret } ]))
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 42 ];
            mut = false;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [
                WI_GlobalGet 0;
                WI_Block
                  (BlockType ([ { t = I32; lbl = Public } ], []), [ WI_Drop ]);
              ];
            export_name = None;
          };
        ];
    }

(*
  Block output stack has higher security level than block result security level

  (module
    (global i32<Secret> (i32.const 42))
    (func
      (block ϵ -> i32<Public>
        global.get 0
      end)
    )
  )
*)
let _ =
  test "block output stack incorrect security level"
    (neg_test
       (err_block4 [ { t = I32; lbl = Public } ] [ { t = I32; lbl = Secret } ]))
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 42 ];
            mut = false;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body =
              [
                WI_Block
                  ( BlockType ([], [ { t = I32; lbl = Public } ]),
                    [ WI_GlobalGet 0 ] );
              ];
            export_name = None;
          };
        ];
    }

(*
  Function output stack has different length than specified by function type

  (module
    (func (result i32<Public>)
      nop
    )
  )
*)
let _ =
  test "function output stack incorrect length"
    (neg_test (err_function1 1 0))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Nop ];
            export_name = None;
          };
        ];
    }

(*
  Function output stack has higher security level than result security level

  (module
    (func (result i32<Public>)
      nop
    )
  )
*)
let _ =
  test "function output stack incorrect security level"
    (neg_test
       (err_function2
          [ { t = I32; lbl = Public } ]
          [ { t = I32; lbl = Secret } ]))
    {
      memories = [];
      globals =
        [
          {
            gtype = { t = I32; lbl = Secret };
            const = [ WI_Const 42 ];
            mut = false;
          };
        ];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_GlobalGet 0 ];
            export_name = None;
          };
        ];
    }

(*
  Function body doesn't correspond to function type (too few return values)

  (module
    (func (result i32)
      nop
    )
  )
*)
let _ =
  test "function body has incorrect type (too few return values)"
    (neg_test (TypingError "function must leave 1 value on the stack (found 0)"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Nop ];
            export_name = None;
          };
        ];
    }

(*
  Function body doesn't correspond to function type (too many return values)

  (module
    (func
      i32.const 0
    )
  )
*)
let _ =
  test "function body has incorrect type (too many return values)"
    (neg_test (TypingError "function must leave 0 value on the stack (found 1)"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Const 0 ];
            export_name = None;
          };
        ];
    }

(*
  Test block typechecks

  (func
  (param i32) (result i32)
      local.get 0
      block (param i32) (result i32)
        nop
      end
  )
*)
let _ =
  test "block-1" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype =
              FunType
                ( [ { t = I32; lbl = Public } ],
                  Public,
                  [ { t = I32; lbl = Public } ] );
            locals = [ { t = I32; lbl = Public } ];
            body =
              [
                WI_LocalGet 0;
                WI_Block
                  ( BlockType
                      ( [ { t = I32; lbl = Public } ],
                        [ { t = I32; lbl = Public } ] ),
                    [ WI_Nop ] );
              ];
            export_name = None;
          };
        ];
    }

(*
  Test type mismatch at end of block, expected [] but got [i32]

  (func
  (param i32) (result i32)
      local.get 0
      block (param i32)
        nop
      end
  )
*)

let _ =
  test "type mismatch at end of block"
    (neg_test (TypingError "block must leave 0 values on the stack (found 1)"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype =
              FunType
                ( [ { t = I32; lbl = Public } ],
                  Public,
                  [ { t = I32; lbl = Public } ] );
            locals = [ { t = I32; lbl = Public } ];
            body =
              [
                WI_LocalGet 0;
                WI_Block
                  (BlockType ([ { t = I32; lbl = Public } ], []), [ WI_Nop ]);
              ];
            export_name = None;
          };
        ];
    }

(*
  Test type mismatch at end of function, expected [] but got [i32, i32]

  (func (result i32)
      i32.const 1
      i32.const 2
      i32.const 3
  )
*)

let _ =
  test "type mismatch at end of function"
    (neg_test (TypingError "function must leave 1 value on the stack (found 3)"))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
            locals = [];
            body = [ WI_Const 1; WI_Const 2; WI_Const 3 ];
            export_name = None;
          };
        ];
    }

(*
  Test func can get its arguments using local.get
  (module
    (func
    (param i32) (result i32)
      local.get 0
    )
  )
*)
let _ =
  test "func can get it's argument using local.get" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype =
              FunType
                ( [ { t = I32; lbl = Public } ],
                  Public,
                  [ { t = I32; lbl = Public } ] );
            locals = [];
            body = [ WI_LocalGet 0 ];
            export_name = None;
          };
        ];
    }

(*
  Test branch to end of current block

  (module
    (func
    (param i32) (result i32)
      block
        br 0
      end
      i32.const 32
    )
  )
*)
let _ =
  test "branch to end of current block" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype =
              FunType
                ( [ { t = I32; lbl = Public } ],
                  Public,
                  [ { t = I32; lbl = Public } ] );
            locals = [];
            body = [ WI_Block (BlockType ([], []), [ WI_Br 0 ]); WI_Const 42 ];
            export_name = None;
          };
        ];
    }

(*
  Test branch to the end of nested block typechecks

  (module
    (func
    (param i32) (result i32)
      block (result i32)
        block
          br 0
        end
        i32.const 42
      end
    )
  )
*)

let _ =
  test "br-2" pos_test
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype =
              FunType
                ( [ { t = I32; lbl = Public } ],
                  Public,
                  [ { t = I32; lbl = Public } ] );
            locals = [];
            body =
              [
                WI_Block
                  ( BlockType ([], [ { t = I32; lbl = Public } ]),
                    [ WI_Block (BlockType ([], []), [ WI_Br 0 ]); WI_Const 42 ]
                  );
              ];
            export_name = None;
          };
        ];
    }

(*
  Test type mismatch in br, expected [i32] but got []

  (module
    (func
    (param i32) (result i32)
      block (result i32)
        block
          br 1
        end
        i32.const 42
      end
    )
  )
*)

let _ =
  test "type mismatch in br"
    (neg_test (err_branch_stack_size 1 0))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype =
              FunType
                ( [ { t = I32; lbl = Public } ],
                  Public,
                  [ { t = I32; lbl = Public } ] );
            locals = [];
            body =
              [
                WI_Block
                  ( BlockType ([], [ { t = I32; lbl = Public } ]),
                    [ WI_Block (BlockType ([], []), [ WI_Br 1 ]); WI_Const 42 ]
                  );
              ];
            export_name = None;
          };
        ];
    }

(*
  Unconditionally branching outside a block,
  or in general to an index that is higher 
  than the nesting depth of blocks

  (module
    (func
      br 0
    )
  )
*)

let _ =
  test "unconditional branching outside block"
    (neg_test err_branch_outside_block)
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Br 0 ];
            export_name = None;
          };
        ];
    }

(*
  Unconditionally branching to an invalid index (negative)

  (module
    (func
      block
        br -1
      end
    )
  )
*)

let _ =
  test "unconditional branching to invalid index (negative)"
    (neg_test (err_branch_index (-1) 0))
    {
      memories = [];
      globals = [];
      functions =
        [
          {
            ftype = FunType ([], Public, []);
            locals = [];
            body = [ WI_Block (BlockType ([], []), [ WI_Br (-1) ]) ];
            export_name = None;
          };
        ];
    }

(*  ================= End of tests ================== *)
(*  Run suite! *)

let _ = run_test_tt_main ("SecMiniWasm" >::: !test_list)
