(* Module Main : the entry point of the project. 
This module receives the CLI arguments and launches the corresponding methods.

See README for a complete explanation of the arguments. *)
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

val main : unit -> unit
(* Parses the CLI arguments and launches the appropriate functions *)                     
