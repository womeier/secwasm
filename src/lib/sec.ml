[@@@coverage exclude_file]

module type LATTICE = sig
  type t

  val leq : t -> t -> bool
  val lub : t -> t -> t
  val str : t -> string
end

type 'a lattice = (module LATTICE with type t = 'a)
type simpleLatticeElement = Public | Secret

module SimpleLattice : LATTICE with type t = simpleLatticeElement = struct
  type t = simpleLatticeElement

  let leq e1 e2 =
    match (e1, e2) with Public, _ -> true | _, Secret -> true | _ -> false

  let lub e1 e2 = match (e1, e2) with Public, e2 -> e2 | _ -> Secret

  let str (s : simpleLatticeElement) =
    match s with Public -> "Public" | Secret -> "Secret"
end

(*
   let lub : ('a lattice) -> ('a lattice) =
     fun (type a) (lub : a lattice) (e1 : a) (e2 : a) ->
       let module Lub =
         (val lub : LATTICE with type = a) in
         Lub.lub e1 e2 *)
