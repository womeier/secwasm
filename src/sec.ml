module Lattice = struct
  type elem = Public | Secret 
  let leq e1 e2 = match e1, e2 with
    | Public, _ -> true
    | _, Secret -> true
    | _ -> false
  let lub e1 e2 = if e1 = Public then e2 else Secret
end