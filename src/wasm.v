Inductive type :=
  | I32
  | I64.

Inductive function_type :=
  | F_ty : list type -> list type -> function_type.

Inductive wasm_instr :=
  | WI_unreachable : wasm_instr                               (* trap unconditionally *)
  | WI_nop : wasm_instr                                       (* do nothing *)
  | WI_comment : string -> wasm_instr                         (* do nothing, not part of the spec *)
  | WI_if : list wasm_instr -> list wasm_instr -> wasm_instr  (* conditional *)
  | WI_return : wasm_instr                                    (* break from function body *)
  | WI_call : nat -> wasm_instr                               (* call function *)
  | WI_local_get : nat -> wasm_instr                          (* read local variable *)
  | WI_local_set : nat -> wasm_instr                          (* write local variable *)
  | WI_global_get : nat -> wasm_instr                         (* read global variable *)
  | WI_global_set : nat -> wasm_instr                         (* write global variable *)
  | WI_load : type -> wasm_instr                              (* read memory at address *)
  | WI_store : type -> wasm_instr                             (* write memory at address *)
  | WI_const : nat -> type -> wasm_instr                      (* constant *)
  | WI_add : type -> wasm_instr                               (* add, a bit different in the spec *)
  | WI_eq : type -> wasm_instr.                               (* equality check, a bit different in the spec *)

Record wasm_function :=
  { idx : nat
  ; export_name : string
  ; function_type : function_type
  ; locals : list (nat * type)
  ; body : list wasm_instr
  }.

Record wasm_module :=
  { functions : list wasm_function
  ; memory : nat                                              (* size *)
  ; global_vars : list (nat * type * nat)                     (* var, type, init_value *)
  ; function_imports : list (string * nat * list type)        (* namespace, variable (name), parameter types, currently no return type needed *)
  ; comment : string
  }.

Definition quote : string := "\"".

Definition type_show (t : type) :=
  match t with
  | I32 => "i32"
  | I64 => "i64"
  end.

Definition var_show (n : nat) :=
  show n.

Definition instr_list_show' (indent : nat) (show : nat -> wasm_instr -> string) (l : list wasm_instr) : string
  := (fold_left (fun _s i => _s ++ show indent i) l "").


Fixpoint spaces (n : nat) : string :=
  match n with
  | 0 => ""
  | S n' => " " ++ spaces n'
  end.

Fixpoint instr_show (indent : nat) (e : wasm_instr) : string :=
  (spaces indent) ++
  (match e with
  | WI_unreachable => "unreachable"
  | WI_nop  => "nop"
  | WI_comment s => nl ++ (spaces indent) ++ ";; " ++ s
  | WI_return => "return"
  | WI_local_get x => "local.get " ++ var_show x
  | WI_local_set x => "local.set " ++ var_show x
  | WI_global_get x => "global.get " ++ var_show x
  | WI_global_set x => "global.set " ++ var_show x
  | WI_if thenBranch elseBranch => "if" ++ nl ++
                                      instr_list_show' (2 + indent) instr_show thenBranch ++ nl ++ (spaces indent) ++
                                   "else" ++ nl ++
                                      instr_list_show' (2 + indent) instr_show elseBranch ++ nl ++ (spaces indent) ++
                                   "end"
  | WI_call f => "call " ++ var_show f
  | WI_load t => type_show t ++ ".load"
  | WI_store t => type_show t ++ ".store"
  | WI_const n t => type_show t ++ ".const " ++ var_show n
  | WI_add t => type_show t ++ ".add"
  | WI_eq t => type_show t ++ ".eq"
  end) ++ nl.

Definition instr_list_show (indent : nat) (l : list wasm_instr) : string
  := (fold_left (fun _s i => _s ++ instr_show indent i) l "").

Definition parameters_show (prefix : string) (l : list (nat * type)) : string :=
  fold_left (fun _s p =>
    let name := var_show (fst p) in
    let type := type_show (snd p) in
      _s ++ " (" ++ prefix ++ " " ++ name ++ " " ++ type ++ ")") l "".

Definition function_show (f : wasm_function) : string :=
  let ret_type := match f.(ret_type) with
                  | None => ""
                  | Some t => "(result " ++ type_show t ++ ")"
                  end
  in
  "(func " ++ var_show f.(name) ++ (if f.(export)
                                    then " (export " ++ quote ++ var_show f.(name) ++ quote ++ ")"
                                    else "") ++ nl
    ++ parameters_show "param" f.(args) ++ " " ++ ret_type ++ nl
    ++ parameters_show "local" f.(locals) ++ nl
    ++ instr_list_show 2 f.(body) ++ ")" ++ nl.

Definition global_vars_show (prefix : string) (l : list (nat * type * var)) : string :=
  fold_left (fun _s p =>
    let '(v, t, i) := p in
    let name := var_show v in
    let type := type_show t in
    let init := (match t with
                 | I32 => "i32.const " ++ var_show i
                 | I64 => "i64.const " ++ var_show i
                 end) in
      _s ++ "(" ++ prefix ++ " " ++ name ++ " (mut " ++ type  ++ ") (" ++ init ++ ")" ++ ") ") l "".

Definition function_imports_show (fns : list (string * nat * list type)) :=
  fold_left (fun _s f =>
    let '(namespace, name, arg_types) := f in
    let func := "(func " ++ var_show name ++ " (param" ++ (fold_left (fun _s' t => _s' ++ " " ++ type_show t) arg_types "") ++ "))"
    in
    _s ++ nl ++ "(import " ++ quote ++ namespace ++ quote ++ " " ++ quote ++ var_show name ++ quote ++ func ++ ")"
  ) fns "".

Definition wasm_module_show (m : wasm_module) : string :=
  "(module" ++ nl ++ ";;" ++ nl ++
  ";; " ++ m.(comment) ++ nl ++
  function_imports_show m.(function_imports) ++ nl ++
  "(memory " ++ var_show m.(memory) ++ ") ;; * 64 KB" ++ nl ++
  global_vars_show "global" m.(global_vars) ++ nl ++
  (fold_left (fun _s f => _s ++ nl ++ function_show f) m.(functions) "") ++ ")".
