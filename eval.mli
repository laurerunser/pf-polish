open Polish

module NameTable : Map.S with type key = name
(* The environment variables will be stored in a NameTable which is an assoc
table indexed by strings. This is more efficient than simple assoc lists. *)

val eval_expr : expr -> int NameTable.t -> int
(* [eval_expr e env]  evaluates the expression e with the current environment
variables in env and returns the result as an int.
Fails if it tries to access a variable that has ot been initialized yet, 
or if it tries to compute a division by 0. *)
                                   
val eval_cond : cond -> int NameTable.t -> bool                    
(* [eval_cond c env] evaluates the condition c with the current environment
variables in env and returns the result as a boolean.
Fails if the expressions in the condition cannot be evaluated (see eval_expr) *)

val print_value_expr : expr -> int NameTable.t -> unit
(* [print_value_expr e env] evaluates the expression e with the current
environment variables in env and prints the result to stdout. Then also prints
a newline.
Fails if the expression cannot be evaluated (see eval_expr) *)

val read_value : name -> int NameTable.t -> int NameTable.t
(* [read_value n env] :
- prints to stdout the name of the variable, a space, '?' and another space
- waits for user input
- if the user input is an int, adds it into the NameTable with the key n
and returns the updated NameTable
- if the user input is not an int, starts again until a valid input is entered *)

val eval_block : program -> int NameTable.t -> int NameTable.t
                                                                                   
