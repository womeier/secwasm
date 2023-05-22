module L = Sec
module A = Ast
module C = Constraints
module S = C.Solver

type label_var = C.label_var

type pc_type = label_var
type stack_type = label_var list
type stack_of_stacks_type = (stack_type * pc_type) list

type global = label_var * bool

type context = {
  memory : A.wasm_memory option; (* number of memories *)
  func_types : A.fun_type list;
  globals : global list;
  locals : label_var list;
  labels : A.labeled_value_type list list;
  return : A.labeled_value_type list;
}

let empty_context =
  {
    memory = None;
    func_types = [];
    globals = [];
    locals = [];
    labels = [];
    return = [];
  }

type 'a nonempty =
  | First of 'a
  | Cons of ('a * 'a nonempty)

let split_at_index n l =
  let rec f n l acc =
    if n <= 0 then (acc, l)
    else
      match l with
      | [] -> failwith "split_at_index: list is empty!"
      | x :: xs -> f (n - 1) xs (x :: acc)
  in
  let fst, lst = f n l [] in
  (List.rev_append fst [], lst)

  let str_l = L.str_l

  let return = C.SolverState.return

  let (let*) = C.SolverState.bind

  let (>>=) = C.SolverState.bind
  
  let (>>>) m k = C.SolverState.bind m (fun _ -> k)
  
  let (==?) l1 l2 = 
    S.assert_constraint C.Eq l1 l2
  
  let (<=?) l1 l2 =
    S.assert_constraint C.Leq l1 l2

  let lub = S.lub
  let leq l1 l2 = l1 ==? l2
  let geq = Fun.flip leq

  let rec fold_left f acc xs = match xs with
  | [] -> return acc
  | x :: xs' ->
    fold_left f (f acc x) xs' 

  let rec fold_right f xs acc = match xs with
    | [] -> return acc
    | x :: xs' ->
      fold_right f xs' acc >>= f x 

  let map f xs = 
    let f' x acc = f x >>= fun x' -> return (x' :: acc) in 
    fold_right f' xs []

  let rec fold_right_nonempty f g acc = match g with 
    | First (st, pc) -> 
      f (st, pc) acc >>= return
    | Cons ((st, pc), g') -> 
      f (st, pc) acc >>= fold_right_nonempty f g'


  let map_nonempty f g = 
    match g with 
    | First (st, pc) -> 
      let* (st', pc') = f (st, pc) in return (First (st', pc'))
    | Cons ((st, pc), g') -> 
      let f' (st, pc) acc = f (st, pc) >>= fun (st', pc') -> return (Cons ((st', pc'), acc)) in 
      let* st', pc' = f (st, pc) in
      fold_right_nonempty f' g' (First (st', pc'))  

  (* ======= Error handling ======= *)

  exception NotImplemented of string
  exception InternalError of string
  exception TypingError of string
  exception PrivacyViolation of string
  
  let t_err0 msg = raise (TypingError msg)
  
  let t_err2i msg (i1 : int) (i2 : int) =
    let error_msg = Printf.sprintf msg i1 i2 in
    raise (TypingError error_msg)
  
  let p_err3 msg l1 l2 l3 =
    let error_msg = Printf.sprintf msg (str_l l1) (str_l l2) (str_l l3) in
    raise (PrivacyViolation error_msg)
  
  let p_err4 msg l1 l2 l3 l4 =
    let error_msg =
      Printf.sprintf msg (str_l l1) (str_l l2) (str_l l3) (str_l l4)
    in
    raise (PrivacyViolation error_msg)
  
  (* error messages are checked in test-suite, don't inline *)
  
  let err_drop = TypingError "drop expected 1 value on the stack"
  let err_binop = TypingError "binop expected 2 values on the stack"
  
  let err_globalset2 l1 l2 l3 =
    PrivacyViolation
      (Printf.sprintf "global.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
         (str_l l1) (str_l l2) (str_l l3))
  
  let err_globalset3 = TypingError "global.set expected 1 value on the stack"
  
  let err_globalset_imut =
    TypingError "global.set expected global var to be mutable"
  
  let err_localset2 l1 l2 l3 =
    PrivacyViolation
      (Printf.sprintf "local.set expected pc ⊔ l ⊑ l' but was pc=%s, l=%s, l'=%s"
         (str_l l1) (str_l l2) (str_l l3))
  
  let err_localset3 = TypingError "local.set expected 1 value on the stack"
  let err_load_nomemory = TypingError "load expected memory in the context"
  let err_load_addrexists = TypingError "load expected 1 value on the stack"
  let err_store_nomemory = TypingError "store expected memory in the context"
  let err_store_addrexists = TypingError "store expected 1 value on the stack"
  
  let err_store2 l1 l2 l3 l4 =
    PrivacyViolation
      (Printf.sprintf
         "store expected pc ⊔ la ⊔ lv ⊑ lm but was pc=%s, la=%s, lv=%s, lm=%s"
         (str_l l1) (str_l l2) (str_l l3) (str_l l4))
  
  let err_block1 i1 i2 =
    TypingError
      (Printf.sprintf "block needs %d values on the stack (found %d)" i1 i2)
  
  let err_block2 i1 i2 =
    TypingError
      (Printf.sprintf "block must leave %d values on the stack (found %d)" i1 i2)  
  
  let err_call1 l1 l2 =
    return (TypingError
      (Printf.sprintf "can't call function labeled as %s (pc: %s)" (str_l l1)
         (str_l l2)))
  
  let err_call2 i1 i2 =
    TypingError
      (Printf.sprintf "call needs %d values on the stack (found %d)" i1 i2)

  let err_function1 i1 i2 =
    TypingError
      (Printf.sprintf "function must leave %d value on the stack (found %d)" i1 i2)
  
  let err_branch_outside_block =
    TypingError (Printf.sprintf "branching outside of a block")
  
  let err_branch_index idx max_idx =
    TypingError
      (Printf.sprintf "branching to index %i, valid indices range from 0 to %i"
         idx max_idx)
  
  let err_branch_stack_size i1 i2 =
    TypingError
      (Printf.sprintf "branching expected at least %i values on stack (found %i)"
         i1 i2)

(* ======= Typing ======= *)

let lookup_global (c : context) (idx : int) =
  if 0 <= idx && idx < List.length c.globals then List.nth c.globals idx
  else t_err0 ("did not find global variable of index " ^ Int.to_string idx)

let lookup_local (c : context) (idx : int) =
  if 0 <= idx && idx < List.length c.locals then List.nth c.locals idx
  else t_err0 ("did not find local variable of index " ^ Int.to_string idx)

let lookup_func_type (c : context) (idx : int) =
  if 0 <= idx && idx < List.length c.func_types then List.nth c.func_types idx
  else t_err0 ("did not find function of index " ^ Int.to_string idx)

let lookup_label (c : context) (idx : int) =
  if idx < List.length c.labels then List.nth c.labels idx
  else failwith ("expected label of index " ^ Int.to_string idx)


let lift_st l_lift st =
  map (lub l_lift) st

(*
let rec lift_g l_lift g =
  let lift_pair (st, pc) = 
    let* st' = lift_st l_lift st in 
    let* pc' = lub l_lift pc in 
    return (st', pc')
  in 
  map_nonempty lift_pair g
*)

let lift_g l_lift g = 
  let lift_pair (st, pc) = 
    let* st' = lift_st l_lift st in 
    let* pc' = lub l_lift pc in 
    return (st', pc')
  in 
  map lift_pair g 


let rec check_instr ((g, c) : stack_of_stacks_type * context)
  (i : A.wasm_instruction) : (stack_of_stacks_type * context) C.SolverState.t =
  match g with
  | [] -> raise (InternalError "stack-of-stacks ill-formed")
  | (st, pc) :: g' -> (
    match i with 
    | WI_Unreachable ->
      (* Note: We put no restrictions on the program context,
        i.e. our approach is termination insensitive! *)
      return ((st, pc) :: g', c)
    | WI_Nop -> return ((st, pc) :: g', c)
    | WI_Drop -> (
        match st with _ :: st' -> return ((st', pc) :: g', c) | _ -> raise err_drop)
    | WI_Const _ -> 
      let* l = S.fresh_label_var in
      l ==? pc >>> 
      return ((l :: st, pc) :: g', c)
    | WI_BinOp _ -> (
        match st with
        | l1 :: l2 :: st' ->
            (* Label should be lbl1 ⊔ lbl2 ⊔ pc *)
            let* l3 = lub l1 l2 >>= lub pc in
            return ((l3 :: st', pc) :: g', c)
        | _ -> raise err_binop)
    | WI_Call idx -> (
        match lookup_func_type c idx with
        | FunType (ft_in, lbl, ft_out) ->
            (* Check if entering function is allowed *)
            let* r = pc <=? (if lbl == L.Public then 0 else 1) in 
            if not r then 
              failwith "TODO";
            (* Check enough arguments *)
            if List.length ft_in > List.length st then
              raise (err_call2 (List.length ft_in) (List.length st));
            (* st' corresponds to st in the paper *)
            let _tau1, st' = split_at_index (List.length ft_in) st in
            let* tau2 = map (fun _ -> S.fresh_label_var) ft_out in  
            return ((tau2 @ st', pc) :: g', c))
    | WI_LocalGet idx ->
        let l = lookup_local c idx in
        let* l' = lub pc l in 
        return ((l' :: st, pc) :: g', c)
    | WI_LocalSet idx -> (
        match st with
        | l :: st' ->
            let l' = lookup_local c idx in
            (* Types are guaranteed to be I32, Check that pc ⊔ l ⊑ l' *)
            let* r = lub l pc >>= geq l' in 
            if not r then failwith "TODO";
            return ((st', pc) :: g', c)
        | _ -> raise err_localset3)
    | WI_GlobalGet idx ->
        let (l', _) = lookup_global c idx in
        let* l = lub pc l' in
        return ((l :: st, pc) :: g', c)
    | WI_GlobalSet idx -> (
        match st with
        | l :: st' ->
            let (l', mut) = lookup_global c idx in
            (* Type are guaranteed to be I32 thus equal, Check that var is mutable and pc ⊔ l ⊑ l' *)
            if not mut then raise err_globalset_imut;
            let* r = lub l pc >>= geq l' in 
            if not r then failwith "TODO";
            return ((st', pc) :: g', c)
        | _ -> raise err_globalset3)
    | WI_Load lm -> (
        match c.memory with
        | None -> raise err_load_nomemory
        | Some _ -> (
            match st with
            | la :: st ->
                let lm = if lm == L.Public then 0 else 1 in 
                (* l = l_a ⊔ l_m ⊔ pc *)
                let* l = lub la lm >>= lub pc in
                return ((l :: st, pc) :: g', c)
            | _ -> raise err_load_addrexists))
    | WI_Store lm -> (
        match c.memory with
        | None -> raise err_store_nomemory
        | Some _ -> (
            match st with
            | lv :: la :: st' ->
                let lm = if lm == L.Public then 0 else 1 in 
                (* Addr and val are known to be I32, check that pc ⊔ l_a ⊔ l_v ⊑ l_m *)
                let* r = lub pc la >>= lub lv >>= geq lm in 
                if not r then
                  failwith "TODO";
               return ((st', pc) :: g', c)
            | _ -> raise err_store_addrexists))
    | WI_Block (bt, exps) -> type_check_block (g, c) (bt, exps)
    | WI_Br i -> (
        (* Check that
          1. we're in a block
          2. the branching index is valid *)
        (match (i, List.length c.labels) with
        | _, 0 -> raise err_branch_outside_block
        | idx, labels_len ->
            if idx < 0 || idx >= labels_len then
              raise (err_branch_index idx (labels_len - 1)));
        (* Find the return type of the block that we're branching to *)
        let bt_out = lookup_label c i in
        (* Check that the top of the stack matches *)
        if List.length bt_out > List.length st then
          raise (err_branch_stack_size (List.length bt_out) (List.length st));
        let st, st' = split_at_index (List.length bt_out) st in
        (* Check that pc ⊑ st *)
        let* r = fold_right (fun l _ -> leq pc l) st true in 
        if not r then
          failwith "TODO";
        let* g' = map (
          fun (st, pc) -> 
            let* st' = map (fun _ -> S.fresh_label_var) st in 
            let* _ = fold_right (fun (l, l') _ -> leq l l') (List.combine st st') true in
            let* pc' = S.fresh_label_var in 
            let* _ = leq pc pc' in 
            return (st', pc')) g 
        in 
        (* g1 = g'[0 : i - 1], g2 = g'[i :] *)
        let g1, g2 = split_at_index (i - 1) g' in
        let* lifted = lift_g pc ((st @ st', pc) :: g1) in 
        match lifted with
        | [] -> raise (InternalError "stack-of-stacks ill-formed")
        | (st'', pc') :: g1' -> 
          return (((st'', pc') :: g1') @ g2, c))
    | WI_BrIf _ -> raise (NotImplemented "br_if"))

and check_seq ((g, c) : stack_of_stacks_type * context) seq : (stack_of_stacks_type * context) C.SolverState.t =
  List.fold_left (fun acc i -> let* (g, c) = acc in check_instr (g, c) i) (return (g, c)) seq

and type_check_block ((g, c) : stack_of_stacks_type * context)
    (BlockType (bt_in, bt_out), instrs) =
  match g with
  | [] -> raise (InternalError "blocks: stack-of-stacks ill-formed")
  | (stack, pc) :: g -> (
      let c' = { c with labels = bt_out :: c.labels } in
      if List.length bt_in > List.length stack then
        raise (err_block1 (List.length bt_in) (List.length stack));
      (* naming: bt_in and bt_out refer to tau1 and tau2 that are annotated
         tau1 refers to the type actually found, tau2 to the type that is computed based on the actual inputs
      *)
      let tau1, st = split_at_index (List.length bt_in) stack in
      (* actual input types <= labeled input types *)
      (* check expression with updated stack, context *)
      let* (g', _) = check_seq ((tau1, pc) :: (st, pc) :: g, c') instrs in 
      match g' with
      (* computed pc' is discarded *)
      | (tau2, _) :: (st', pc'') :: g' ->
          if List.length tau2 != List.length bt_out then
            raise (err_block2 (List.length bt_out) (List.length tau2));
          let* pc''' = lub pc pc'' in
          return ((tau2 @ st', pc''') :: g', c) 
      | _ -> raise (InternalError "blocks: stack-of-stacks ill-formed"))
