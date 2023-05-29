open Ast
open Sec

type context = {
  noOfParams : int;
  locals : labeled_value_type list;
  memory : wasm_memory;
}

let push_bitmask0 (c : context) =
  [
    (* push 1111...111 *)
    WI_Const (-1);
    (* todo: explain why 18 works here *)
    WI_Const 18;
    WI_Const c.memory.size;
    (* compute 18 - (mem_size + 1)*)
    WI_BinOp Sub;
    (* shift 11111 right with 32 - (mem_size + 1)*)
    WI_BinOp Shr_u
    (* = 01111111 where 0 is at index mem_size (from the right) *);
  ]

let push_bitmask1 (c : context) =
  [
    (* Size of memory in bytes = mem_size * 64 * 2^10 = 2^(16+mem_size) *)
    WI_Const 1;
    WI_Const 14;
    WI_Const c.memory.size;
    WI_BinOp Add;
    WI_BinOp Shl
    (* = 2^(15+mem_size) = 100000 where 1 is at index k (from the right) *);
  ]

let translate_store (c : context) (encoded_lbl : int) :
    context * wasm_instruction =
  (* We extend the list of locals with two extra items,
         for saving the value to be stored and address to
         stored into *)
  let idx_val = List.length c.locals + c.noOfParams in
  let idx_addr = List.length c.locals + c.noOfParams + 1 in
  let new_ctxt =
    {
      c with
      locals =
        (* labels don't matter *)
        c.locals @ [ { t = I32; lbl = Secret }; { t = I32; lbl = Secret } ];
    }
  in
  ( new_ctxt,
    WI_Block
      ( BlockType
          (* labels don't matter *)
          ([ { t = I32; lbl = Secret }; { t = I32; lbl = Secret } ], []),
        [
          (* save value *)
          WI_LocalSet idx_val;
          (* save address *)
          WI_LocalSet idx_addr;
          (* === BEGIN STORE VALUE *)
          WI_LocalGet idx_addr;
        ]
        @ push_bitmask0 c
        @ [
            (* Top of the stack: 01111111 where 0 is at index mem_size (from the right) *)
            (* Next element    : addr *)
            (* Force addres into lower part of memory *)
            WI_BinOp And;
            (* Get value *)
            WI_LocalGet idx_val;
            (* Store value - label doesn't matter *)
            WI_Store Secret;
            (* === BEGIN STORE LABEL *)
            WI_LocalGet idx_addr;
          ]
        @ push_bitmask1 c
        @ [
            (* Top of the stack: 1000000 where 1 is at index mem_size (from the right) *)
            (* Next element    : addr *)
            (* Force address into upper part of memory *)
            WI_BinOp Or;
            (* Push label on stack *)
            WI_Const encoded_lbl;
            (* Store it - label doesn't matter *)
            WI_Store Secret;
          ] ) )

let translate_load_public (c : context) : context * wasm_instruction =
  (* We extend the list of locals with two extra items,
         for saving the value to be stored and address to
         stored into *)
  let idx_addr = List.length c.locals + c.noOfParams in
  let new_ctxt =
    {
      c with
      locals = (* labels don't matter *)
               c.locals @ [ { t = I32; lbl = Secret } ];
    }
  in
  ( new_ctxt,
    WI_Block
      (* labels don't matter *)
      ( BlockType ([ { t = I32; lbl = Secret } ], [ { t = I32; lbl = Secret } ]),
        [
          (* save address *)
          WI_LocalSet idx_addr;
          (* === BEGIN CHECK LABEL*)
          WI_Block
            ( BlockType ([], []),
              (* push address and 100000 *)
              (WI_LocalGet idx_addr :: push_bitmask1 c)
              @ [
                  (* Top of the stack: 1000000 where 1 is at index mem_size (from the right) *)
                  (* Next element    : addr *)
                  (* Force address into upper part of memory *)
                  WI_BinOp Or;
                  (* Load labels from memory (4 bytes, i.e. 4 labels) - label doesn't matter*)
                  WI_Load Secret;
                  (* Assert that all labels are 0 *)
                  WI_Const 0;
                  WI_BinOp Eq;
                  (* branch conditionally to the end of the block*)
                  WI_BrIf 0;
                  (* attempt to load secret into public value, trap! *)
                  WI_Unreachable;
                ] );
        ]
        (* === BEGIN LOAD VALUE *)
        @ (WI_LocalGet idx_addr :: push_bitmask0 c)
        @ [
            (* Top of the stack: 01111111 where 0 is at index mem_size (from the right) *)
            (* Next element    : addr *)
            (* Force addres into lower part of memory *)
            WI_BinOp And;
            (* load value - label doesn't matter *)
            WI_Load Secret;
          ] ) )

let translate_load_secret (c : context) : context * wasm_instruction =
  ( c,
    WI_Block
      (* labels don't matter *)
      ( BlockType ([ { t = I32; lbl = Secret } ], [ { t = I32; lbl = Secret } ]),
        (* addr is on the stack, force it down in lower half *)
        push_bitmask0 c
        @ [
            (* Top of the stack: 01111111 where 0 is at index mem_size (from the right) *)
            (* Next element    : addr *)
            (* Force addres into lower part of memory *)
            WI_BinOp And;
            (* Load labels from memory (4 bytes, i.e. 4 labels) - label doesn't matter*)
            WI_Load Secret;
          ] ) )

let transform_instr (c : context) (i : wasm_instruction) :
    context * wasm_instruction =
  match i with
  | WI_Load l -> (
      match l with
      | Public -> translate_load_public c
      | Secret -> translate_load_secret c)
  | WI_Store l -> translate_store c (SimpleLattice.encode l)
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

let transform_func (m : wasm_memory) (f : wasm_func) : wasm_func =
  let noOfParams =
    match f.ftype with FunType (st_in, _, _) -> List.length st_in
  in
  let ctxt = { noOfParams; locals = f.locals; memory = m } in
  (* transform the body of f*)
  let ctxt', new_body = transform_seq ctxt f.body in
  { f with body = new_body; locals = ctxt'.locals }

let transform_module (m : wasm_module) : wasm_module =
  let rec f x acc =
    if x <= acc then 2 * acc else match x with 0 -> 0 | _ -> f x (2 * acc)
  in
  match m.memory with
  (* if module doesn't use a memory it doesn't typecheck *)
  | None -> m
  | Some mem ->
      (* double the size of the memory and make sure it's a multiple of 2
         This makes splitting an address up into low/ high addresses easier *)
      let mem' = { size = f mem.size 1 } in
      {
        m with
        memory = Some mem';
        functions = List.map (transform_func mem') m.functions;
      }
