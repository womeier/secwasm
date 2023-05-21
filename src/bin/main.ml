open Secwasm.Ast
open Secwasm.Type_check
open Secwasm.Dynamic_check

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

let example2_module : wasm_module =
  {
    memory = Some { size = 1 };
    globals = [];
    functions =
      [
        {
          ftype = FunType ([], Public, []);
          locals = [ { t = I32; lbl = Public } ];
          body = [ WI_Const 0; WI_Const 42; WI_Store Public ];
          export_name = Some "foo";
        };
      ];
  }

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
  | "2" -> wmodule := Some example2_module
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
