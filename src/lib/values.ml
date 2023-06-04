(* Values and operators *)

type value = I32 of Int.t

(* Typing *)

let type_of = function I32 _ -> Types.I32
let default_value = function Types.I32 -> I32 Int.zero

(* Conversion *)

let value_of_bool b = I32 (if b then 1 else 0)
let string_of_value = function I32 i -> Int.to_string i

let string_of_values = function
  | [ v ] -> string_of_value v
  | vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"

(* Injection & projection *)

exception Value of Types.value_type

module type ValueType = sig
  type t

  val to_value : t -> value
  val of_value : value -> t (* raise Value *)
end

module I32Value = struct
  type t = Types.value_type

  let to_value i = I32 i
  let of_value = function I32 i -> i
end
