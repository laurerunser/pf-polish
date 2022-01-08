(* Vars module : static analysis of the program to determine if some variables are used before initialization. *)

open Polish

val print_vars : program -> unit
(*[print_vars p] launches the static analysis and prints 2 lines :
- on the first one, the names of all the variables that are used in the program
- on the second one, the names of all the variables that might be used before their initialization *)