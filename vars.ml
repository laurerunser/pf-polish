(* This module makes a static analysis of the program to see if some variables are accessed before being initiliazed (with READ or :=) *)
open Polish
open Printf

let rec find_vars_expr e vars = 
  match e with
  | Num(n) -> vars
  | Var(n) -> Names.add n vars
  | Op(op, e1, e2) -> Names.union vars (Names.union (find_vars_expr e1 vars) (find_vars_expr e2 vars))

let find_vars_cond c vars = 
  let e1, comp, e2 = c in
  Names.union vars (Names.union (find_vars_expr e1 vars) (find_vars_expr e2 vars))

let rec find_vars (p:program) vars vars_ok =
  match p with
  | [] -> vars, vars_ok
  | (i, Set(n, e))::xs -> find_vars xs (Names.add n vars) (Names.add n vars_ok)
  | (i, Print(e))::xs -> find_vars xs (find_vars_expr e vars) vars_ok
  | (i, Read(n))::xs -> find_vars xs (Names.add n vars) (Names.add n vars_ok)
  | (i, If(c, b, b2))::xs -> 
      let vars, vars_ok = find_vars_if c b b2 vars vars_ok in
      find_vars xs vars vars_ok
  | (i, While(c, b))::xs -> find_vars xs (find_vars_cond c vars) vars_ok
  and find_vars_if c b b2 vars vars_ok =
    let vars = find_vars_cond c vars in
    let vars1, vars_ok1 = find_vars b vars vars_ok in
    let vars2, vars_ok2 = find_vars b2 vars vars_ok in
    (Names.union vars1 vars2), (Names.inter vars_ok1 vars_ok2)

let rec print_vars_list vars =
  match vars with
  | [] -> ()
  | x::xs -> print_string x; print_string " "; print_vars_list xs

let print_vars p =
  let vars, vars_ok = find_vars p Names.empty Names.empty in
  print_vars_list (Names.elements vars);
  print_newline ();
  let vars_not_ok = Names.diff vars vars_ok in
  print_vars_list (Names.elements vars_not_ok);
  print_newline ()