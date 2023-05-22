module C = Secwasm.Constraints
module L = Secwasm.Sec
module A = Secwasm.Ast
module TC = Secwasm.Typing
module S = C.Solver
open OUnit2

let (let*) = C.SolverState.bind
let (>>=) = C.SolverState.bind
let return = C.SolverState.return

let fold_left = C.fold_left
let fold_right = C.fold_right
let map = C.map

let public = 0
let secret = 1 

let test_list : test list ref = ref []

let ctx : TC.context = TC.empty_context
let gamma : TC.stack_of_stacks_type = []
let st : TC.stack_type = []

let output_eq (gamma1, globals1, locals1) (gamma2, globals2, locals2) =
  List.equal (fun (st1, pc1) (st2, pc2) -> List.equal (=) st1 st2 && pc1 == pc2) gamma1 gamma2
  && List.equal (=) globals1 globals2 
  && List.equal (=) locals1 locals2

let to_values ((gamma, ctx) : TC.stack_of_stacks_type * TC.context) =
  let* gamma' = map (fun (st, pc) -> 
    let* st_val = map S.get_value_of_variable st in 
    let* pc_val = S.get_value_of_variable pc in 
    return (st_val, pc_val)) gamma in 
  let* globals = map (fun (l,_) -> let* v = S.get_value_of_variable l in return v) ctx.globals in 
  let* locals = map S.get_value_of_variable ctx.locals in 
  return (gamma', globals, locals)

let test_instr name s =
  let init_solver_state = C.new_solver_state () in 
  let open C.SolverState in 
  let f _ = 
    let expected_output, got_output = snd (runState s init_solver_state) in
    assert_equal ?cmp:(Some output_eq) expected_output got_output
  in
  test_list := !test_list @ [name >:: f]

  let _ =
    test_instr "unreachable"  
    (let* pc = S.fresh_label_var in
    let expected_output = ((st, pc) :: gamma, ctx) in 
    let* output = TC.check_instr ((st, pc) :: gamma, ctx) A.WI_Unreachable in 
    let* eo = to_values expected_output in 
    let* o = to_values output in 
    return (eo, o));

    test_instr "nop" 
    (let* pc = S.fresh_label_var in
    let expected_output = ((st, pc) :: gamma, ctx) in 
    let* output = TC.check_instr ((st, pc) :: gamma, ctx) A.WI_Unreachable in 
    let* eo = to_values expected_output in 
    let* o = to_values output in 
    return (eo, o));

    test_instr "binop"
    (let* pc = S.fresh_label_var in 
    let* l1 = S.fresh_label_var in 
    let* l2 = S.fresh_label_var in
    let* lout = S.lub l1 l2 >>= S.lub pc in 
    let expected_output = ((lout :: st, pc ) :: gamma, ctx) in 
    let* output = TC.check_instr ((l1 :: l2 :: st, pc) :: gamma, ctx) (A.WI_BinOp A.Add) in 
    let* eo = to_values expected_output in 
    let* o = to_values output in 
    return (eo, o))

  let _ = run_test_tt_main ("SecMiniWasm Instruction Typing" >::: !test_list)