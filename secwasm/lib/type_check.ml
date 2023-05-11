open Ast

type 'a context = {
  funcs : fun_type list; 
  globals : global_type list;
  locals : ('a labeled_value_type) list;
  labels : ('a labeled_value_type) list;
  return : ('a labeled_value_type) list;
}

type 'a pc_type = PC of 'a
type 'a stack_type = 'a labeled_value_type list
type 'a stack_of_stacks_type = (('a stack_type) * ('a pc_type))


let check_stack s1 s2 =
  assert (List.length s1 = List.length s2 && List.for_all2 (fun t1 -> fun t2 -> t1 == t2) s1 s2)

let pop (s1 : value_type list) (s2 : value_type list) =
  let l1 = List.length s1 in
  let l2 = List.length s2 in
  let l = min l1 l2 in 
  check_stack s1 (Util.List.drop (l2 - l) s2);
  Util.List.take (l2 - l) s2

let rec check_instr (i : wasm_instruction) (_ : value_type list) : (value_type list) * (value_type list) =
  match i with 
  | WI_Const v -> 
    let t = type_of_value v in
      ([], [t])

  | WI_BinOp bop -> 
      let t = type_of_binop bop in 
      ([t; t], [t])

  | _ -> ([], [])
  

let rec check_seq (seq : wasm_instruction list) : value_type list =
  match seq with
  | [] ->
    []
  | _ -> 
    let seq', i = Util.List.split_last seq in
    let stack = check_seq seq' in
      let (ins, outs) = check_instr i stack in 
      (pop ins stack) @ outs



let type_check_function (f : wasm_func) = 
  let _ = check_seq f.body 
  in true
  
  

let type_check_module (m : wasm_module) = 
  List.for_all type_check_function m.functions
