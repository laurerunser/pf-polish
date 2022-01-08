open Polish
open Printf

(* In this module, you can often find these variables :
- vars : the set of all the variables that have already appeared in the program
- vars_ok : the set of variables that we are sure have been initialized in the program
- vars_not_ok : the set of variables that have been accessed before in the program, but
  were maybe not initialized before
*)

(* *find_vars_expr* takes the expression and the current sets
of variables. It returns the updated sets of variables :
- it adds all the variables of the expression to the *vars* set
- it adds all the variables that are used but are not present in the *vars_ok* set into the 
  *vars_not_ok* set.
*)
let rec find_vars_expr e vars vars_ok vars_not_ok = 
  match e with
  | Num(n) -> vars, vars_ok, vars_not_ok
  | Var(n) -> 
    let vars_not_ok = if Names.mem n vars_ok then vars_not_ok else Names.add n vars_not_ok in
    (Names.add n vars), vars_ok, vars_not_ok
  | Op(op, e1, e2) -> union_vars_2_expr e1 e2 vars vars_ok vars_not_ok
(* *union_vars_2_expr takes 2 expressions and the current sets of variables. 
It then computes for each expression the new sets of variables, and combines them into one. *)
  and union_vars_2_expr e1 e2 vars vars_ok vars_not_ok =
  let vars1, _, vars_not_ok1 = find_vars_expr e1 vars vars_ok vars_not_ok in
  let vars2, _, vars_not_ok2 = find_vars_expr e2 vars vars_ok vars_not_ok in
  (Names.union vars1 vars2), vars_ok, (Names.union vars_not_ok1 vars_not_ok2)

(* *find_vars_cond* takes a condition and the current sets of variables. It looks into
each expression of the condition and returns the updated sets of variables. *)
  let find_vars_cond c vars vars_ok vars_not_ok = 
  let e1, comp, e2 = c in
  union_vars_2_expr e1 e2 vars vars_ok vars_not_ok

(* *find_vars* takes a program (or a block) and the current sets of variables. 
It looks through the entire program and returns the updates sets of variables. *)
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
(* *find_vars_if* takes a condition and 2 blocks (the info inside a If() instruction) and
the current sets of variables.
It first looks at the condition to see if any variable is accessed that has not been initialized
  yet. 
Then it looks through both blocks to find the updated sets of variables :
- *vars* will contain all the variable that appear in one (or both) blocks
- *vars_ok* will contain all the variables that were initialized before the If() and all the 
  variables that are initialized inside BOTH blocks
- *vars_not_ok* will contain all the variables that are accessed in at least one block
  but have not been initialized before. *)
    and find_vars_if c b b2 vars vars_ok vars_not_ok =
  let vars, vars_ok, vars_not_ok = find_vars_cond c vars vars_ok vars_not_ok in
  let vars1, vars_ok1, vars_not_ok1 = find_vars b vars vars_ok vars_not_ok in
  let vars2, vars_ok2, vars_not_ok2 = find_vars b2 vars vars_ok vars_not_ok in
  (Names.union vars1 vars2), (Names.inter vars_ok1 vars_ok2), (Names.union vars_not_ok1 vars_not_ok2)
(* *find_vars_while* takes a condition and a block (the info inside a While() instruction) and the
current sets of variables. 
First it looks at the condition to see if it accesses a variable that has not been initialized yet. 
Then it looks at the instructions inside the block and updates the sets of variables :
- *vars* will contain all the variables that appear in the program before and in the while block 
- *vars_ok* will contain all the variables that were intialized before the while (none of the variables
  inside the while will be added to this set)
- *vars_not_ok* will contain all the variables used inside the block that have not been initialized.
  It will not contain the variables that are initialized inside the while and used afterwards (even
  though they will not be added to the *vars_ok* set). *)
  and find_vars_while c b vars vars_ok vars_not_ok =
  let vars, vars_ok, vars_not_ok = find_vars_cond c vars vars_ok vars_not_ok in
  let vars, _, vars_not_ok = find_vars b vars vars_ok vars_not_ok in
  vars, vars_ok, vars_not_ok

(* *print_vars_list* takes a string list and prints each element, seperated by spaces. *)
  let rec print_vars_list vars =
  match vars with
  | [] -> ()
  | x::xs -> print_string x; print_string " "; print_vars_list xs

(* *print_vars* launches the analysis of the variables of the program and prints 2 lines :
- the names of all the variables that appear in the program
- the names of all the variables that are used in the program, but were maybe not initialized before. *)
  let print_vars p =
  let vars, vars_ok, vars_not_ok = find_vars p Names.empty Names.empty Names.empty in
  print_vars_list (Names.elements vars);
  print_newline ();
  print_vars_list (Names.elements vars_not_ok);
  print_newline ()