open Ast
open Sec
open Types

let rec log2 x = match x with 1 -> 0 | _ -> 1 + log2 ((x + 1) / 2)

let instrs is =
  List.map (fun i : wasm_instruction -> { it = i; at = Source.no_region }) is

type context = {
  locals : labeled_value_type list (* params and locals *);
  memory : wasm_memory;
}

let push_bitmask0 (c : context) : wasm_instruction list =
  instrs
    [
      (* push 1111...111 *)
      WI_Const (-1);
      WI_Const 16;
      WI_Const (log2 c.memory.size);
      (* compute 16 - (log mem_size) *)
      WI_BinOp Sub;
      (* shift 11111 right with 32 - (log mem_size) *)
      WI_BinOp Shr_u
      (* = 01111111 where 0 is at index mem_size + 1 (from the right) *);
    ]

let push_bitmask1 (c : context) : wasm_instruction list =
  instrs
    [
      (* Size of memory in bytes = mem_size * 64 * 2^10 = 2^(16 + log mem_size) *)
      WI_Const 1;
      WI_Const 16;
      WI_Const (log2 c.memory.size);
      WI_BinOp Add;
      WI_BinOp Shl
      (* = 100000 where 1 is at index log mem_size + 16 (from the right) *);
    ]

let translate_store (c : context) (encoded_lbl : int) :
    context * wasm_instruction =
  (* We extend the list of locals with two extra items,
         for saving the value to be stored and address to
         stored into *)
  let idx_val = List.length c.locals in
  let idx_addr = List.length c.locals + 1 in
  let new_ctxt =
    {
      c with
      locals =
        (* labels don't matter *)
        c.locals @ [ (I32, Secret); (I32, Secret) ];
    }
  in
  ( new_ctxt,
    {
      it =
        WI_Block
          ( BlockType
              (* labels don't matter *)
              ([ (I32, Secret); (I32, Secret) ], []),
            instrs
              [
                (* save value *)
                WI_LocalSet idx_val;
                (* save address *)
                WI_LocalSet idx_addr;
                (* === BEGIN STORE VALUE *)
                WI_LocalGet idx_addr;
              ]
            @ push_bitmask0 c
            @ instrs
                [
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
            @ instrs
                [
                  (* Top of the stack: 1000000 where 1 is at index mem_size (from the right) *)
                  (* Next element    : addr *)
                  (* Force address into upper part of memory *)
                  WI_BinOp Or;
                  (* Push label on stack *)
                  WI_Const encoded_lbl;
                  (* Store it - label doesn't matter *)
                  WI_Store Secret;
                ] );
      at = Source.no_region;
    } )

let translate_load_public (c : context) : context * wasm_instruction =
  (* We extend the list of locals with two extra items,
         for saving the value to be stored and address to
         stored into *)
  let idx_addr = List.length c.locals in
  let new_ctxt =
    { c with locals = (* labels don't matter *)
                      c.locals @ [ (I32, Secret) ] }
  in
  ( new_ctxt,
    {
      it =
        WI_Block
          (* labels don't matter *)
          ( BlockType ([ (I32, Secret) ], [ (I32, Secret) ]),
            instrs
              [
                (* save address *)
                WI_LocalSet idx_addr;
                (* === BEGIN CHECK LABEL*)
                WI_Block
                  ( BlockType ([], []),
                    (* push address and 100000 *)
                    (instrs [ WI_LocalGet idx_addr ] @ push_bitmask1 c)
                    @ instrs
                        [
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
            @ (instrs [ WI_LocalGet idx_addr ] @ push_bitmask0 c)
            @ instrs
                [
                  (* Top of the stack: 01111111 where 0 is at index mem_size (from the right) *)
                  (* Next element    : addr *)
                  (* Force addres into lower part of memory *)
                  WI_BinOp And;
                  (* load value - label doesn't matter *)
                  WI_Load Secret;
                ] );
      at = Source.no_region;
    } )

let translate_load_secret (c : context) : context * wasm_instruction =
  ( c,
    {
      it =
        WI_Block
          (* labels don't matter *)
          ( BlockType ([ (I32, Secret) ], [ (I32, Secret) ]),
            (* addr is on the stack, force it down in lower half *)
            push_bitmask0 c
            @ instrs
                [
                  (* Top of the stack: 01111111 where 0 is at index mem_size (from the right) *)
                  (* Next element    : addr *)
                  (* Force addres into lower part of memory *)
                  WI_BinOp And;
                  (* Load labels from memory (4 bytes, i.e. 4 labels) - label doesn't matter*)
                  WI_Load Secret;
                ] );
      at = Source.no_region;
    } )

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

and transform_instr (c : context) (i : wasm_instruction) :
    context * wasm_instruction =
  match i.it with
  | WI_Load l -> (
      match l with
      | Public -> translate_load_public c
      | Secret -> translate_load_secret c)
  | WI_Store l -> translate_store c (SimpleLattice.encode l)
  | WI_Block (bt, instr) ->
      let c', instr' = transform_seq c instr in
      (c', { i with it = WI_Block (bt, instr') })
  | WI_Loop (bt, instr) ->
      let c', instr' = transform_seq c instr in
      (c', { i with it = WI_Loop (bt, instr') })
  | _ -> (c, i)

let transform_func (m : wasm_memory) (f : wasm_func) : wasm_func =
  let params = match f.ftype with FunType (st_in, _, _) -> st_in in
  let ctxt = { locals = params @ f.locals; memory = m } in
  (* transform the body of f*)
  let ctxt', new_body = transform_seq ctxt f.body in
  { f with body = new_body; locals = ctxt'.locals }

let transform_module (m : wasm_module) : wasm_module =
  let rec f x acc =
    if x <= acc then acc else match x with 0 -> 0 | _ -> f x (2 * acc)
  in
  match m.memory with
  (* if module doesn't use a memory it doesn't typecheck *)
  | None -> m
  | Some mem ->
      (* Make sure the memory is a multiple of 2, for bitmasking operation to actually split the memory *)
      let nearest_power_of_two = f mem.size 1 in
      {
        m with
        memory = Some { size = nearest_power_of_two * 2 };
        functions =
          List.map (transform_func { size = nearest_power_of_two }) m.functions;
      }
