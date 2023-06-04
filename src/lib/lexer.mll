{
open Parser
open Operators

let convert_pos pos =
  { Source.file = pos.Lexing.pos_fname;
    Source.line = pos.Lexing.pos_lnum;
    Source.column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  }

let region lexbuf =
  let left = convert_pos (Lexing.lexeme_start_p lexbuf) in
  let right = convert_pos (Lexing.lexeme_end_p lexbuf) in
  {Source.left = left; Source.right = right}

let error lexbuf (msg: string) = failwith ("syntax error: " ^ msg ^ " at " ^ (Source.string_of_region (region lexbuf)))
let error_nest start lexbuf msg =
  lexbuf.Lexing.lex_start_p <- start;
  error lexbuf msg

let string s =
  let b = Buffer.create (String.length s) in
  let i = ref 1 in
  while !i < String.length s - 1 do
    let c = if s.[!i] <> '\\' then s.[!i] else
      match (incr i; s.[!i]) with
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      | '\\' -> '\\'
      | '\'' -> '\''
      | '\"' -> '\"'
      | 'u' ->
        let j = !i + 2 in
        i := String.index_from s j '}';
        let n = int_of_string ("0x" ^ String.sub s j (!i - j)) in
        let bs = Utf8.encode [n] in
        Buffer.add_substring b bs 0 (String.length bs - 1);
        bs.[String.length bs - 1]
      | h ->
        incr i;
        Char.chr (int_of_string ("0x" ^ String.make 1 h ^ String.make 1 s.[!i]))
    in Buffer.add_char b c;
    incr i
  done;
  Buffer.contents b

let value_type = function
  | "i32" -> Types.I32
  | "i64" -> failwith "NotImplemented"
  | "f32" -> failwith "NotImplemented"
  | "f64" -> failwith "NotImplemented"
  | _ -> assert false

(* todo
let label = function
  | "Public" -> Sec.Public
  | "Secret" -> Sec.Secret
  | _ -> assert false
  *)

let intop t i32 =
  match t with
  | "i32" -> i32
  | _ -> assert false

}

let sign = '+' | '-'
let digit = ['0'-'9']
let hexdigit = ['0'-'9''a'-'f''A'-'F']
let num = digit ('_'? digit)*
let hexnum = hexdigit ('_'? hexdigit)*

let letter = ['a'-'z''A'-'Z']
let symbol =
  ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`''.''\'']

let space = [' ''\t''\n''\r']
let ascii = ['\x00'-'\x7f']
let ascii_no_nl = ['\x00'-'\x09''\x0b'-'\x7f']
let utf8cont = ['\x80'-'\xbf']
let utf8enc =
    ['\xc2'-'\xdf'] utf8cont
  | ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xed'] ['\x80'-'\x9f'] utf8cont
  | ['\xe1'-'\xec''\xee'-'\xef'] utf8cont utf8cont
  | ['\xf0'] ['\x90'-'\xbf'] utf8cont utf8cont
  | ['\xf4'] ['\x80'-'\x8f'] utf8cont utf8cont
  | ['\xf1'-'\xf3'] utf8cont utf8cont utf8cont
let utf8 = ascii | utf8enc
let utf8_no_nl = ascii_no_nl | utf8enc

let escape = ['n''r''t''\\''\'''\"']
let character =
    [^'"''\\''\x00'-'\x1f''\x7f'-'\xff']
  | utf8enc
  | '\\'escape
  | '\\'hexdigit hexdigit
  | "\\u{" hexnum '}'

let nat = num | "0x" hexnum
let int = sign nat
let frac = num
let hexfrac = hexnum
let float =
    sign? num '.' frac?
  | sign? num ('.' frac?)? ('e' | 'E') sign? num
  | sign? "0x" hexnum '.' hexfrac?
  | sign? "0x" hexnum ('.' hexfrac?)? ('p' | 'P') sign? num
  | sign? "inf"
  | sign? "nan"
  | sign? "nan:" "0x" hexnum
let string = '"' character* '"'
let name = '$' (letter | digit | '_' | symbol)+

let ixx = "i" ("32" | "64")
let fxx = "f" ("32" | "64")
let nxx = ixx | fxx
let mixx = "i" ("8" | "16" | "32" | "64")
let mfxx = "f" ("32" | "64")
let sign = "s" | "u"
let mem_size = "8" | "16" | "32"

rule token = parse
  | "(" { LPAR }
  | ")" { RPAR }

  | "<" { LBRACK }
  | ">" { RBRACK }

  | nat as s { NAT s }
  | int as s { INT s }
  | float as s { FLOAT s }

  | string as s { STRING (string s) }
  | '"'character*('\n'|eof) { error lexbuf "unclosed string literal" }
  | '"'character*['\x00'-'\x09''\x0b'-'\x1f''\x7f']
    { error lexbuf "illegal control character in string literal" }
  | '"'character*'\\'_
    { error_nest (Lexing.lexeme_end_p lexbuf) lexbuf "illegal escape" }

  | (nxx as t) { VALUE_TYPE (value_type t) }
  | (nxx)".const"
    { let open Source in
      CONST (fun s -> let n = Int32.to_int (Int32.of_string s.it)in
          i32_const (n @@ s.at), Values.I32 n)
    }
  | "funcref" { FUNCREF }
  | "mut" { MUT }

  | "Public" { PUBLIC }
  | "Secret" { SECRET }

  | "nop" { NOP }
  | "unreachable" { UNREACHABLE }
  | "drop" { DROP }
  | "block" { BLOCK }
  | "loop" { LOOP }
  | "end" { END }
  | "br" { BR }
  | "br_if" { BR_IF }
  | "return" { RETURN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "call" { CALL }

  | "local.get" { LOCAL_GET }
  | "local.set" { LOCAL_SET }
  | "global.get" { GLOBAL_GET }
  | "global.set" { GLOBAL_SET }

  | (nxx as t)".load"
    { if t <> "i32" then failwith "NotImplemented" else LOAD }
  | (nxx as t)".store"
    { if t <> "i32" then failwith "NotImplemented" else STORE }


  | (ixx as t)".add" { BINARY (intop t i32_add) }
  | (ixx as t)".sub" { BINARY (intop t i32_sub) }
  | (ixx as t)".mul" { BINARY (intop t i32_mul) }
  | (ixx as t)".and" { BINARY (intop t i32_and) }
  | (ixx as t)".or" { BINARY (intop t i32_or) }
  | (ixx as t)".shl" { BINARY (intop t i32_shl) }
  | (ixx as t)".shr_u" { BINARY (intop t i32_shr_u) }

  | (ixx as t)".eq" { COMPARE (intop t i32_eq) }

  | (ixx as t)".lt_s" { COMPARE (intop t i32_lt_s) }
  | (ixx as t)".lt_u" { COMPARE (intop t i32_lt_u) }
  | (ixx as t)".le_s" { COMPARE (intop t i32_le_s) }
  | (ixx as t)".ge_s" { COMPARE (intop t i32_ge_s) }

  | "type" { TYPE }
  | "func" { FUNC }
  | "start" { START }
  | "param" { PARAM }
  | "result" { RESULT }
  | "local" { LOCAL }
  | "global" { GLOBAL }
  | "memory" { MEMORY }
  | "import" { IMPORT }
  | "export" { EXPORT }

  | "module" { MODULE }

  | name as s { VAR s }

  | ";;"utf8_no_nl*eof { EOF }
  | ";;"utf8_no_nl*'\n' { Lexing.new_line lexbuf; token lexbuf }
  | ";;"utf8_no_nl* { token lexbuf (* causes error on following position *) }
  | "(;" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | space#'\n' { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }

  | utf8 { error lexbuf "malformed operator" }
  | _ { error lexbuf "malformed UTF-8 encoding" }

and comment start = parse
  | ";)" { () }
  | "(;" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; comment start lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment start lexbuf }
  | eof { error_nest start lexbuf "unclosed comment" }
  | utf8 { comment start lexbuf }
  | _ { error lexbuf "malformed UTF-8 encoding" }
