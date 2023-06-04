type 'a start = Module : Ast.wasm_module start

exception Syntax = Script.Syntax

let parse' name lexbuf start =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name };
  try start Lexer.token lexbuf
  with Syntax (region, s) ->
    let region' =
      if region <> Source.no_region then region
      else
        {
          Source.left = Lexer.convert_pos lexbuf.Lexing.lex_start_p;
          Source.right = Lexer.convert_pos lexbuf.Lexing.lex_curr_p;
        }
    in
    raise (Syntax (region', s))

let parse (type a) name lexbuf : a start -> a = function
  | Module -> parse' name lexbuf Parser.module1

let string_to_module s =
  let lexbuf = Lexing.from_string s in
  parse "string" lexbuf Module
