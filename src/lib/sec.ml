[@@@coverage exclude_file]

module type LATTICE = sig
  type t

  val leq : t -> t -> bool
  val lub : t -> t -> t
  val encode : t -> int
end

type 'a lattice = (module LATTICE with type t = 'a)
type simpleLatticeElement = Public | Secret

let str_l t = match t with Public -> "Public" | Secret -> "Secret"

module SimpleLattice : LATTICE with type t = simpleLatticeElement = struct
  type t = simpleLatticeElement

  let leq e1 e2 =
    match (e1, e2) with Public, _ -> true | _, Secret -> true | _ -> false

  let lub e1 e2 = match (e1, e2) with Public, e2 -> e2 | _ -> Secret

  let encode e =
    match e with
    | Public -> 0
    | _ ->
        (* 16843009
           = 0x01010101
           = 0b00000001000000010000000100000001
           = the 32-bit word with 1's in every byte *)
        16843009
end

(*
   let lub : ('a lattice) -> ('a lattice) =
     fun (type a) (lub : a lattice) (e1 : a) (e2 : a) ->
       let module Lub =
         (val lub : LATTICE with type = a) in
         Lub.lub e1 e2 *)
