open Ast

let rec log2 x = match x with 1 -> 0 | _ -> 1 + log2 ((x + 1) / 2)

type context = { locals : labeled_value_type list }

let transform_instr (c : context) (i : wasm_instruction) :
    context * wasm_instruction =
  match i with
  | WI_Load _ -> (c, WI_Block (BlockType ([], []), []))
  | WI_Store _ ->
      ( c,
        WI_Block
          ( BlockType
              ([ { t = I32; lbl = Public }; { t = I32; lbl = Public } ], []),
            [ WI_Const 42 ] ) )
  | _ -> (c, i)

let rec transform_seq (c : context) (seq : wasm_instruction list) :
    context * wasm_instruction list =
  match seq with
  | [] -> (c, [])
  | i :: rest ->
      (* transform the instruction and get a new context with updated locals *)
      let c', i' = transform_instr c i in
      (* transform the rest and get a new context again *)
      let c'', rest' = transform_seq c' rest in
      (* return the newest context and transformed list of instructions *)
      (c'', i' :: rest')

let transform_func (f : wasm_func) : wasm_func =
  (* transform the body of f*)
  let ctxt = { locals = f.locals } in
  let ctxt', new_body = transform_seq ctxt f.body in
  { f with body = new_body; locals = ctxt'.locals }

let transform_module (m : wasm_module) : wasm_module =
  let mem' =
    (* double the size of the memory and make sure it's a multiple of 2
       This makes splitting an address up into low/ high addresses easier *)
    match m.memory with
    | None -> None
    | Some mem -> Some { size = log2 mem.size * 2 }
  in
  { m with memory = mem'; functions = List.map transform_func m.functions }
