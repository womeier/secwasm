%{
open Source
open Types
open Ast
open Operators

(* Error handling *)

let error at msg = raise (Script.Syntax (at, msg))

let parse_error msg =
  error Source.no_region
    (if msg = "syntax error" then "unexpected token" else msg)


(* Position handling *)

let position_to_pos position =
  { file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position1 position2 =
  { left = position_to_pos position1;
    right = position_to_pos position2
  }

let at () =
  positions_to_region (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
let ati i =
  positions_to_region (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)


(* Literals *)

let literal f s =
  try f s with Failure _ -> error s.at "constant out of range"

let nat s at =
  try
    let n = int_of_string s in
    if n >= 0 then n else raise (Failure "")
  with Failure _ -> error at "integer constant out of range"

(*
let nat32 s at =
  try Int32.of_string s with Failure _ -> error at "i32 constant out of range" *)


let name s at =
  try Utf8.decode s with Utf8.Utf8 -> error at "invalid UTF-8 encoding"

%}

%token NAT INT FLOAT STRING VAR VALUE_TYPE FUNCREF MUT LPAR RPAR LBRACK RBRACK
%token NOP DROP BLOCK END IF THEN ELSE LOOP BR BR_IF
%token CALL RETURN
%token PUBLIC SECRET
%token LOCAL_GET LOCAL_SET GLOBAL_GET GLOBAL_SET
%token LOAD STORE
%token CONST UNARY BINARY TEST COMPARE CONVERT
%token UNREACHABLE
%token FUNC START TYPE PARAM RESULT LOCAL GLOBAL
%token ELEM MEMORY DATA IMPORT EXPORT
%token MODULE BIN QUOTE
%token ASSERT_MALFORMED ASSERT_INVALID ASSERT_SOFT_INVALID ASSERT_UNLINKABLE
%token ASSERT_RETURN ASSERT_RETURN_CANONICAL_NAN ASSERT_RETURN_ARITHMETIC_NAN ASSERT_TRAP ASSERT_EXHAUSTION
%token INPUT OUTPUT
%token EOF

%token<string> NAT
%token<string> INT
%token<string> FLOAT
%token<string> STRING
%token<string> VAR
%token<Types.value_type> VALUE_TYPE
%token<string Source.phrase -> Ast.instr' * Values.value> CONST
%token<Ast.instr'> UNARY
%token<Ast.instr'> BINARY
%token<Ast.instr'> TEST
%token<Ast.instr'> COMPARE
%token<Ast.instr'> CONVERT

%nonassoc LOW
%nonassoc VAR

%start module1

%type<Ast.wasm_module> module1

%%

/* Auxiliaries */

name :
  | STRING { name $1 (at ()) }

string_list :
  | /* empty */ { "" }
  | string_list STRING { $1 ^ $2 }


/* Types */

value_type_list :
  | /* empty */ { [] }
  | VALUE_TYPE value_type_list { $1 :: $2 }

labeled_value_type:
  | VALUE_TYPE LBRACK PUBLIC RBRACK
    { $1, Sec.Public }
  | VALUE_TYPE LBRACK SECRET RBRACK
    { $1, Sec.Secret }

labeled_value_type_list :
  | /* empty */ { [] }
  | labeled_value_type labeled_value_type_list { $1 :: $2 }

elem_type :
  | FUNCREF { Types.FuncRefType }

global_type :
  | labeled_value_type { ($1, false) }
  | LPAR MUT labeled_value_type RPAR { ($3, true) }

def_type :
  | LPAR FUNC func_type RPAR { $3 }

func_type :
  | /* empty */
    { FunType ([], Public, []) }
  | LPAR RESULT labeled_value_type_list RPAR func_type
    { let FunType (ins, lbl, out) = $5 in
      if ins <> [] then error (at ()) "result before parameter";
      FunType (ins, lbl, $3 @ out) }
  | LPAR PARAM labeled_value_type_list RPAR func_type
    { let FunType (ins, lbl, out) = $5 in FunType ($3 @ ins, lbl, out) }

block_type :
  | /* empty */
    { BlockType ([], []) }
  | LPAR RESULT labeled_value_type_list RPAR block_type
    { let BlockType (ins, out) = $5 in
      if ins <> [] then error (at ()) "result before parameter";
      BlockType (ins, $3 @ out) }
  | LPAR PARAM labeled_value_type_list RPAR block_type
    { let BlockType (ins, out) = $5 in BlockType ($3 @ ins, out) }

type_use :
  | LPAR TYPE var RPAR { $3 }


/* Immediates */

literal :
  | NAT { $1 @@ at () }
  | INT { $1 @@ at () }
  | FLOAT { $1 @@ at () }

var :
  | NAT { $1 @@ at () }

/* Instructions & Expressions */

instr :
  | plain_instr { let at = at () in [$1 @@ at] }
  | block_instr { let at = at () in [$1 @@ at] }

plain_instr :
  | UNREACHABLE { unreachable }
  | NOP { nop }
  | DROP { drop }
  | BR NAT { br (nat $2 (at ())) }
  | BR_IF NAT { br_if (nat $2 (at ()))  }
  | CALL NAT { call (nat $2 (at ())) }
  | LOCAL_GET NAT { local_get (nat $2 (at ())) }
  | LOCAL_SET NAT { local_set (nat $2 (at ())) }
  | GLOBAL_GET NAT { global_get (nat $2 (at ())) }
  | GLOBAL_SET NAT {  global_set (nat $2 (at ())) }
  | LOAD {  i32_load }
  | STORE {  i32_store }
  | CONST literal {  fst (literal $1 $2) }
  | TEST {  $1 }
  | COMPARE {  $1 }
  | UNARY {  $1 }
  | BINARY {  $1 }
  | CONVERT { $1 }


block_instr :
  | BLOCK block_type instr_list END
    { block $2 $3 }
  | LOOP block_type instr_list END
    { loop $2 $3 }

instr_list :
  | /* empty */ { [] }
  | instr instr_list {  $1 @ $2 }

const_expr :
  | instr_list { let at = at () in $1 @@ at }


/* Functions */

func :
  | LPAR FUNC func_type func_body RPAR
    { let ft = $3 in let fb = $4 in
      { fb with ftype = ft } }
  | LPAR FUNC LPAR EXPORT STRING RPAR func_type func_body RPAR
    { let ft = $7 in let fb = $8 in
      { fb with ftype = ft; export_name = Some $5 } }

func_body :
  | instr_list
    { { empty_instr_list with body = $1 }  }
  | LPAR LOCAL labeled_value_type_list RPAR func_body
    { let fb = $5 in
      { empty_instr_list with locals = $3 @ fb.locals; body = fb.body} }

/* Tables, Memories & Globals */

memory :
  | LPAR MEMORY NAT RPAR
    { { size = nat $3 (at ()) } }

global :
  | LPAR GLOBAL global_fields RPAR
    { $3 }

global_fields :
  | global_type const_expr
    { {gtype = $1; const = ($2).it} }

/* Imports & Exports */

func_import_desc :
  | LPAR FUNC func_type RPAR  /* Sugar */
    { $3 }

import :
  | LPAR IMPORT STRING STRING func_import_desc RPAR
    { ( $3,  $4, $5) }


/* Modules */

module_fields :
  | /* empty */
    { empty_module }
  | module_fields1 { $1 }

module_fields1 :
  | global module_fields
    { let x = $1 in let mf = $2 in { mf with globals = x :: mf.globals} }
  | memory module_fields
    { let mf = $2 in match mf.memory with
      | None -> { mf with memory = Some $1}
      | _ -> error (at ()) "multiple memories not supported"  }
  | func module_fields
    { let x = $1 in let mf = $2 in { mf with functions = x :: mf.functions} }
  | import module_fields
    { let x = $1 in let mf = $2 in { mf with function_imports = x :: mf.function_imports} }

module_ :
  | LPAR MODULE module_fields RPAR
    { $3 }

const :
  | LPAR CONST literal RPAR { snd (literal $2 $3) @@ ati 3 }

const_list :
  | /* empty */ { [] }
  | const const_list { $1 :: $2 }

module1 :
  | module_ EOF { $1 }

%%
