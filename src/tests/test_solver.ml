module C = Secwasm.Constraints
module L = Secwasm.Sec
module S = C.Solver
open OUnit2

let test_list : test list ref = ref []

let test name expected_output input =
  let init_solver_state = C.new_solver_state () in 
  let open C.SolverState in 
  let f _  = 
    let output = (snd (runState input init_solver_state)) in
    assert_equal output expected_output 
  in
  test_list := !test_list @ [name >:: f]

let test_exn name e input = 
  let init_solver_state = C.new_solver_state () in 
  let open C.SolverState in 
  let t _ = assert_raises e (fun _ -> runState input init_solver_state)
  in
  test_list := !test_list @ [name >:: t]

let (let*) = C.SolverState.bind

let (>>=) = C.SolverState.bind

let (>>>) m k = C.SolverState.bind m (fun _ -> k)

let (==?) l1 l2 = 
  S.assert_constraint C.Eq l1 l2

let (<=?) l1 l2 =
  S.assert_constraint C.Leq l1 l2

(* Least upper bound operator *)
  let (|.|) l1 l2 = 
    S.lub l1 l2 

(* First, some sanity checks *)

let _ = 
  let trivial_properties lname l = 
    test (lname ^ " == " ^ lname) true
    (let* l = l in l ==? l);

    test (lname ^ " <= " ^ lname) true
    (let* l = l in l <=? l);

    test (lname ^ " == " ^ lname ^ " |.| " ^ lname) true 
    (let* l = l in 
    let* l' = l |.| l in 
     l ==? l');
  in 
  trivial_properties "Public" S.public; 
  trivial_properties "Secret" S.secret;
  trivial_properties "l" S.fresh_label_var;

  let lub_properties l1name l1 l2name l2 = 
    test (l1name ^ " <= " ^ l1name ^ " |.| " ^ l2name) true
    (let* l1 = l1 in 
    let* l2 = l2 in 
    let* l' = l1 |.| l2 in 
    l1 <=? l');

    test (l1name ^ " <= " ^ l2name ^ " |.| " ^ l1name) true
    (let* l1 = l1 in 
    let* l2 = l2 in 
    let* l' = l2 |.| l1 in 
    l1 <=? l');

    test (l2name ^ " <= " ^ l1name ^ " |.| " ^ l2name) true
    (let* l1 = l1 in 
    let* l2 = l2 in 
    let* l' = l1 |.| l2 in 
    l2 <=? l');

    test (l2name ^ " <= " ^ l2name ^ " |.| " ^ l1name) true
    (let* l1 = l1 in 
    let* l2 = l2 in 
    let* l' = l2 |.| l1 in 
    l2 <=? l');
  in 
  lub_properties "Public" S.public "Secret" S.secret;
  lub_properties "l1" S.fresh_label_var "l2" S.fresh_label_var;

  test "!(Secret <= Public)" false 
  (let* secret = S.secret in 
  let* public = S.public in 
  secret <=? public);

  test "l1 == Public => label(l1) == Public" L.Public 
  (let* l1 = S.fresh_label_var in 
  let* public = S.public in 
  l1 ==? public >>>
  S.get_value_of_variable l1);

  test "l1 == Secret => label(l1) == Secret" L.Secret 
  (let* l1 = S.fresh_label_var in 
  let* secret = S.secret in 
  l1 ==? secret >>>
  S.get_value_of_variable l1);

  test "!(l1 == Public /\ l1 == Secret)" false 
  (let* l1 = S.fresh_label_var in 
  let* public = S.public in 
  let* secret = S.secret in 
  (l1 ==? public) 
  >>> (l1 ==? secret));

  test "l1 == Public /\ l1 == l2 /\ l2 == l3 => label(l3) == Public" L.Public
  (let* l1 = S.fresh_label_var in 
  let* l2 = S.fresh_label_var in 
  let* l3 = S.fresh_label_var in 
  let* public = S.public in 
  (l1 ==? public)
  >>> (l1 ==? l2)
  >>> (l2 ==? l3)
  >>> S.get_value_of_variable l3)

let _ = run_test_tt_main ("SecMiniWasm Solver" >::: !test_list)