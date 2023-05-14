module type LATTICE = sig
  type t

  val leq : t -> t -> bool
  val lub : t -> t -> t
  val str : t -> string
  val bottom : t
  val top : t 
end

type 'a lattice = (module LATTICE with type t = 'a)

type 'a lattice_functions_type = {
  leq : 'a -> 'a -> bool; 
  lub : 'a -> 'a -> 'a;
  str : 'a -> string;
  bottom : 'a;
  top : 'a;
}

let lattice_functions : 'a lattice -> 'a lattice_functions_type =
  fun (type a) (lat : a lattice) -> 
    let module Lattice = (val lat : LATTICE with type t = a) in
    {
      leq = (fun e1 e2 -> Lattice.leq e1 e2); 
      lub = (fun e1 e2 -> Lattice.lub e1 e2);
      str = Lattice.str; 
      bottom = Lattice.bottom;
      top = Lattice.top;
    }

type simpleLatticeElement = Public | Secret

module SimpleLattice : LATTICE with type t = simpleLatticeElement = struct
  type t = simpleLatticeElement

  let leq e1 e2 =
    match (e1, e2) with Public, _ -> true | _, Secret -> true | _ -> false

  let lub e1 e2 = match (e1, e2) with Public, e2 -> e2 | _ -> Secret

  let str (s : simpleLatticeElement) =
    match s with Public -> "Public" | Secret -> "Secret"
  let bottom = Public
  let top = Secret
end

let simple_lat = (module SimpleLattice : LATTICE with type t = simpleLatticeElement)



