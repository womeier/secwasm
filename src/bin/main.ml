open Secwasm.Ast
open Secwasm.Type_check

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

(****** CMDLINE PARSING *******)

let typecheck = ref false
let output_file = ref ""
let wmodule = ref None

let set_module s =
  match s with "1" -> wmodule := Some example1_module | _ -> ()

let speclist =
  [
    ("-example", Arg.String set_module, "Use secwasm example program <i>");
    ("-typecheck", Arg.Set typecheck, "Typecheck given secwasm program");
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

  - run the test suite: make test

=========================================
  EXAMPLE

  ./main.exe -example 1 -out example.wat
  ./main.exe -example 1 -typecheck
=========================================
  The following commands are available:
|}

(*
  TODO: Read input .secwast file -> lex -> parse -> typecheck -> drop labels -> output .wast file

  *)

let typecheck_module m =
  print_endline "Typechecking module...";
  flush stdout;
  type_check_module m

let output_module m =
  print_endline ("Printing to " ^ !output_file ^ "...");
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
      if !typecheck then typecheck_module m;
      if !output_file != "" then output_module m
