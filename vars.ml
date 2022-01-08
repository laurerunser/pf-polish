(* This module makes a static analysis of the program to see if some variables are accessed before being initiliazed (with READ or :=) *)
open Polish
open Printf

let rec find_vars_expr e vars vars_ok vars_not_ok = 
  match e with
  | Num(n) -> vars, vars_ok, vars_not_ok
  | Var(n) -> 
    let vars_not_ok = if Names.mem n vars_ok then vars_not_ok else Names.add n vars_not_ok in
    (Names.add n vars), vars_ok, vars_not_ok
  | Op(op, e1, e2) -> union_vars_2_expr e1 e2 vars vars_ok vars_not_ok
and union_vars_2_expr e1 e2 vars vars_ok vars_not_ok =
  let vars1, _, vars_not_ok1 = find_vars_expr e1 vars vars_ok vars_not_ok in
  let vars2, _, vars_not_ok2 = find_vars_expr e2 vars vars_ok vars_not_ok in
  (Names.union vars1 vars2), vars_ok, (Names.union vars_not_ok1 vars_not_ok2)

let find_vars_cond c vars vars_ok vars_not_ok = 
  let e1, comp, e2 = c in
  union_vars_2_expr e1 e2 vars vars_ok vars_not_ok

let rec find_vars (p:program) vars vars_ok vars_not_ok =
  match p with
  | [] -> vars, vars_ok, vars_not_ok
  | (i, Set(n, e))::xs -> find_vars xs (Names.add n vars) (Names.add n vars_ok) vars_not_ok
  | (i, Print(e))::xs -> 
    let vars, vars_ok, vars_not_ok = find_vars_expr e vars vars_ok vars_not_ok in
    find_vars xs vars vars_ok vars_not_ok
  | (i, Read(n))::xs -> find_vars xs (Names.add n vars) (Names.add n vars_ok) vars_not_ok
  | (i, If(c, b, b2))::xs ->
      let vars, vars_ok, vars_not_ok = find_vars_if c b b2 vars vars_ok vars_not_ok in
      find_vars xs vars vars_ok vars_not_ok
  | (i, While(c, b))::xs -> 
    let vars, vars_ok, vars_not_ok = find_vars_while c b vars vars_ok vars_not_ok in
    find_vars xs vars vars_ok vars_not_ok
and find_vars_if c b b2 vars vars_ok vars_not_ok =
  let vars, vars_ok, vars_not_ok = find_vars_cond c vars vars_ok vars_not_ok in
  let vars1, vars_ok1, vars_not_ok1 = find_vars b vars vars_ok vars_not_ok in
  let vars2, vars_ok2, vars_not_ok2 = find_vars b2 vars vars_ok vars_not_ok in
  (Names.union vars1 vars2), (Names.inter vars_ok1 vars_ok2), (Names.union vars_not_ok1 vars_not_ok2)
and find_vars_while c b vars vars_ok vars_not_ok =
  let vars, vars_ok, vars_not_ok = find_vars_cond c vars vars_ok vars_not_ok in
  let vars, _, vars_not_ok = find_vars b vars vars_ok vars_not_ok in
  vars, vars_ok, vars_not_ok

let rec print_vars_list vars =
  match vars with
  | [] -> ()
  | x::xs -> print_string x; print_string " "; print_vars_list xs

let print_vars p =
  let vars, vars_ok, vars_not_ok = find_vars p Names.empty Names.empty Names.empty in
  print_vars_list (Names.elements vars);
  print_newline ();
  print_vars_list (Names.elements vars_not_ok);
  print_newline ()