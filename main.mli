(* Module Main : the entry point of the project. 
This module receives the CLI arguments and launches the corresponding methods.*)

open Polish

val read_polish : string -> program
(* [read_polish filename] will open and parse the file into the OCAML
representation of this file.
Fails if the file cannot be opened or if it is not a valid polish file. *)

val print_polish : program -> unit
(* [print_polish p] prints the program p to stdout.
This printing method removes all of the COMMENT lines. *)

val eval_polish : program -> unit
(* [eval_polish p] evaluates the program and prints its result to stdout *)

val simplify_polish : program -> program
(* [simplify_polish p] simplifies the code of the polish program and returns the
updated program *)

val analyse_vars : program -> unit
(* [analyse_vars p] performs a static analysis of the program p and prints 2 lines :
- the names of all the variables that appear in the program
- the names of all the variables that are accessed in the program but have maybe not be 
initialized beforehand. *)

val main : unit -> unit
(* Parses the CLI arguments and launches the appropriate functions *)                     
