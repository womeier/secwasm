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

let _ = ~+("add consts 2" >:: neg_test m_add_consts2 err_binop)

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

let _ = ~+("drop 2" >:: neg_test m_drop err_drop)

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
    memories = [];
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
          locals = [ { t = I32; lbl = Public } ];
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

let _ = ~+("local.set" >:: pos_test m_local_set)

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
let m_load =
  {
    memories = [ { min_size = 1l; max_size = None } ];
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], Public, [ { t = I32; lbl = Public } ]);
          locals = [];
          body = [ WI_Const 0l; WI_Load Public ];
          export_name = None;
        };
      ];
  }

let _ = ~+("load" >:: pos_test m_load)

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
let m_store =
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

let _ = ~+("store" >:: pos_test m_store)

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
let m_block =
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

let _ = ~+("block 1" >:: pos_test m_block)

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
let m_block =
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

let _ = ~+("nested block" >:: pos_test m_block)

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
let m_block =
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

let _ = ~+("block with simple param" >:: pos_test m_block)

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
let m_block =
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

let _ = ~+("block with simple param neg 1" >:: neg_test m_block (err_block1 1 0))

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
let m_block =
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

let _ = ~+("block with simple param neg 2" >:: neg_test m_block (err_block2 1 0))

let m_block_input_stack_incorrect =
  let bt = BlockType ([ { t = I32; lbl = Public } ], []) in
  let bexps = [ WI_Drop ] in
  {
    memories = [];
    globals =
      [
        {
          gtype = { t = I32; lbl = Secret };
          const = [ WI_Const 42l ];
          mut = false;
        };
      ];
    functions =
      [
        {
          ftype = FunType ([], Public, []);
          locals = [];
          body = [ WI_GlobalGet 0l; WI_Block (bt, bexps) ];
          export_name = None;
        };
      ];
  }

let _ =
  ~+("block input stack incorrectly typed"
    >:: neg_test m_block_input_stack_incorrect
          (err_block3
             [ { t = I32; lbl = Public } ]
             [ { t = I32; lbl = Secret } ]))

let m_block_output_stack_incorrect =
  let bt = BlockType ([], [ { t = I32; lbl = Public } ]) in
  let bexps = [ WI_GlobalGet 0l ] in
  {
    memories = [];
    globals =
      [
        {
          gtype = { t = I32; lbl = Secret };
          const = [ WI_Const 42l ];
          mut = false;
        };
      ];
    functions =
      [
        {
          ftype = FunType ([], Public, []);
          locals = [];
          body = [ WI_Block (bt, bexps) ];
          export_name = None;
        };
      ];
  }

let _ =
  ~+("block input stack incorrectly typed"
    >:: neg_test m_block_output_stack_incorrect
          (err_block4
             [ { t = I32; lbl = Public } ]
             [ { t = I32; lbl = Secret } ]))

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
