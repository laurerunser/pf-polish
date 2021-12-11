(* Module Parse
This module handles the parsing of a .p file into a program.
 *)

open Polish (* needed for types definitions *)

val parse_file : string -> program
(* [parse_file filename] parses the file into a polish program. *)



(* For reference : the other methods of this module are described below :


val parse_instruction : position -> name list -> (position * (name list)) list
                        -> instr * (position * (name list)) list

val parse_block : (position * (name list)) list -> int
                  -> (position * instr) list
                  -> block * (position * (name list)) list

val parse_expr : (name list) -> expr * (name list)

val parse_cond : (name list) -> cond
 
val read_all_lines :  string -> string list

val lines_to_words : (position * name) list -> (position * (name list)) list

val remove_comments : (position * (name list)) list -> (position * (name list)) list

val compute_indent : (name list) -> int

val find_cmp_and_partition : (name list) -> comp * (name list) * (name list)
                                                                     
val next_line_is_else : (position * (name list)) list -> bool
*)
