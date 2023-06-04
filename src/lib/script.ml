type var = string Source.phrase

type definition = definition' Source.phrase

and definition' =
  | Textual of Ast.wasm_module Source.phrase
  | Encoded of string * string
  | Quoted of string * string

exception Syntax of Source.region * string
