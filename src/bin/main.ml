open Secwasm.Ast
open Secwasm.Type_check
open Secwasm.Dynamic_check
open Secwasm.Sec

let example1_module : wasm_module =
  {
    memory = None;
    globals = [];
    functions =
      [
        {
          ftype =
            FunType
              ( [ { t = I32; lbl = Public }; { t = I32; lbl = Public } ],
                Public,
                [] );
          locals = [ { t = I32; lbl = Public }; { t = I32; lbl = Public } ];
          body =
            [
              WI_Nop;
              WI_LocalGet 0;
              WI_LocalGet 1;
              WI_BinOp Add;
              WI_Const 0;
              WI_BinOp Eq;
              WI_LocalSet 0
              (* WI_If
                     ( FunType ([], Public, []),
                       [ WI_Nop; WI_Const 2; WI_LocalSet 0 ],
                       [ WI_Const 42; WI_LocalSet 0 ] );
                 ]; *);
            ];
          export_name = Some "hello";
        };
      ];
  }

(*
  (module
    (memory 1)
    (func (export "foo") (param i32) (result i32)
      i32.const STORE_ADDR
      local.get 0
      ;; store parameter at address 0
      store STORE_LABEL
      i32.const LOAD_ADDR
      ;; load it again
      load LOAD_LABEL
    )
  )
*)
let store_and_load_module (store_addr : int) (load_addr : int)
    (store_label : SimpleLattice.t) (load_label : SimpleLattice.t) : wasm_module
    =
  {
    memory = Some { size = 1 };
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
              WI_Const store_addr;
              WI_LocalGet 0;
              WI_Store store_label;
              WI_Const load_addr;
              WI_Load load_label;
            ];
          export_name = Some "foo";
        };
      ];
  }

let store_and_load_first_byte = store_and_load_module 0 0
let store_public_load_as_public = store_and_load_first_byte Public Public
let store_public_load_as_secret = store_and_load_first_byte Public Secret
let store_secret_load_as_public = store_and_load_first_byte Secret Public
let store_secret_load_as_secret = store_and_load_first_byte Secret Secret

(****** CMDLINE PARSING *******)

let pretty_print = ref false
let typecheck = ref false
let dynchecks = ref false
let output_file = ref ""
let wmodule = ref None

let set_module s =
  Printf.fprintf stdout "using test module: %s\n" s;
  match s with
  | "1" -> wmodule := Some example1_module
  | "2" -> wmodule := Some store_public_load_as_public (* Ok! *)
  | "3" -> wmodule := Some store_public_load_as_secret (* Ok! *)
  | "4" -> wmodule := Some store_secret_load_as_public (* Should trap! *)
  | "5" -> wmodule := Some store_secret_load_as_secret (* Ok! *)
  | "6" ->
      (* Should trap! *)
      wmodule := Some (store_and_load_module 0 1 Secret Public)
  | "7" ->
      (* Should trap! *)
      wmodule := Some (store_and_load_module 0 2 Secret Public)
  | "8" ->
      (* Should trap! *)
      wmodule := Some (store_and_load_module 0 3 Secret Public)
  | "9" ->
      (* Ok, we're readying 2nd word in memory! *)
      wmodule := Some (store_and_load_module 0 4 Secret Public)
  | "10" ->
      (* store and load at last available byte in 1 page memory *)
      wmodule := Some (store_and_load_module 65532 65532 Public Public)
  | "11" ->
      (* store and load at last available byte in 1 page memory *)
      wmodule := Some (store_and_load_module 65532 65532 Public Secret)
  | "12" ->
      (* store and load at first index out of bounds  *)
      wmodule := Some (store_and_load_module 65533 65533 Secret Secret)
  | _ -> ()

let speclist =
  [
    ("-example", Arg.String set_module, "Use secwasm example program <i>");
    ("-pp", Arg.Set pretty_print, "Pretty print given secwasm program");
    ("-typecheck", Arg.Set typecheck, "Typecheck given secwasm program");
    ( "-dynchecks",
      Arg.Set dynchecks,
      "Insert dynamic checks in given secwasm program" );
    ("-out", Arg.Set_string output_file, "Set output file name");
  ]

let usage_msg =
  {|
=========================================
  HELP
=========================================
  Things you can do at the moment:

  - typecheck example program
  - pretty print example program
  - insert dynamic checks and pretty print example program

  - run the test suite: make test

=========================================
  EXAMPLE

  ./main.exe -example 1 -pp -out example.wat
  ./main.exe -example 1 -typecheck
  ./main.exe -example 1 -dynchecks -out example.wat
=========================================
  The following commands are available:
|}

(*
  TODO: Read input .secwast file -> lex -> parse -> typecheck -> drop labels -> output .wast file

  *)

let output_module m =
  flush stdout;
  let oc = open_out !output_file in
  Printf.fprintf oc "%s\n" (pp_module m);
  close_out oc

(****** MAIN *******)
let () =
  print_endline "";
  Arg.parse speclist (fun _ -> ()) usage_msg;

  match !wmodule with
  | None ->
      failwith "No input program given, please specify one e.g. with -example 1"
  | Some m ->
      if !typecheck then (
        Printf.fprintf stdout "typechecking module\n";
        type_check_module m;
        (* Raises exception if module doesn't typecheck *)
        Printf.fprintf stdout "success!\n");
      if !pretty_print then
        if not (String.equal !output_file "") then (
          Printf.fprintf stdout "pretty printing module to [%s]\n" !output_file;
          output_module m)
        else
          Printf.fprintf stdout
            "missing -out option\n\
             usage: ./main.exe -example 1 -pp -out example1.wat\n";
      if !dynchecks then
        if not (String.equal !output_file "") then (
          Printf.fprintf stdout "translating module to insert dynamic checks\n";
          let m' = transform_module m in
          Printf.fprintf stdout "pretty printing module to [%s]\n" !output_file;
          output_module m')
        else
          Printf.fprintf stdout
            "missing -out option\n\
             usage: ./main.exe -example 1 -dynchecks -out example1-checks.wat\n"
