open Polish

val eval_block : program -> int NameTable.t -> int NameTable.t
(* [eval_block p env] evaluates the program p with the current environment
variables in env. It returns the updated environment variables. *)


val eval_expr : expr -> int NameTable.t -> int
(* [eval_expr e env]  evaluates the expression e with the current environment
variables in env and returns the result as an int.
Fails if it tries to access a variable that has ot been initialized yet, 
or if it tries to compute a division by 0. *)


val eval_cond : cond -> int NameTable.t -> bool                       
(* [eval_cond c env] evaluates the condition c with the current environment
variables in env and returns the result as a boolean.
Fails if the expressions in the condition cannot be evaluated (see eval_expr) *)                                             
                                                 
(*
For reference : the other functions in that module are described below :
                              
val print_value_expr : expr -> int NameTable.t -> unit

val read_value : name -> int NameTable.t -> int NameTable.t
*)
