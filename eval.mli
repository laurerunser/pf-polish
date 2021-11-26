open Polish

module NameTable : Map.S with type key = name
(* The environment variables will be stored in a NameTable which is an assoc
table indexed by strings. This is more efficient than simple assoc lists. *)

val eval_expr : expr -> NameTable -> int
(* [eval_expr e env]  evaluates the expression e with the current environment
variables in env and returns the result as an int.
Fails if it tries to access a variable that has ot been initialized yet, 
or if it tries to compute a division by 0. *)
                                   
val eval_cond : cond -> NameTable.key -> bool                    
(* [eval_cond c env] evaluates the condition c with the current environment
variables in env and returns the result as a boolean.
Fails if the expressions in the condition cannot be evaluated (see eval_expr) *)

val print_value_expr : expr -> NameTable.key -> unit
(* [print_value_expr e env] evaluates the expression e with the current
environment variables in env and prints the result to stdout. Then also prints
a newline.
Fails if the expression cannot be evaluated (see eval_expr) *)

val read_value : name -> NameTable.key -> NameTable.key
(* [read_value n env] :
- prints to stdout the name of the variable, a space, '?' and another space
- waits for user input
- if the user input is an int, adds it into the NameTable with the key n
and returns the updated NameTable
- if the user input is not an int, starts again until a valid input is entered *)

val eval : program -> NameTable.key -> NameTable.key
                                                                                   
