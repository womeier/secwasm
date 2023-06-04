(* Types *)

type value_type = I32
type labeled_value_type = value_type * Sec.SimpleLattice.t
type elem_type = FuncRefType
type stack_type = labeled_value_type list
type func_type = FunType of stack_type * Sec.SimpleLattice.t * stack_type
type block_type = BlockType of stack_type * stack_type
type 'a limits = { min : 'a; max : 'a option }
type mutability = Immutable | Mutable
type table_type = TableType of Int32.t limits * elem_type
type memory_type = MemoryType of Int32.t limits
type global_type = GlobalType of value_type * mutability

type extern_type =
  | ExternFuncType of func_type
  | ExternTableType of table_type
  | ExternMemoryType of memory_type
  | ExternGlobalType of global_type

(* Attributes *)

let size = function I32 -> 4

(* Filters *)

let funcs =
  Lib.List.map_filter (function ExternFuncType t -> Some t | _ -> None)

let tables =
  Lib.List.map_filter (function ExternTableType t -> Some t | _ -> None)

let memories =
  Lib.List.map_filter (function ExternMemoryType t -> Some t | _ -> None)

let globals =
  Lib.List.map_filter (function ExternGlobalType t -> Some t | _ -> None)

(* String conversion *)
