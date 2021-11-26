(* Module Parse
This module handles the parsing of a .p file into a program.
 *)

open Polish (* needed for types definitions *)

val parse_file : string -> program
(* [parse_file filename] parses the file into a polish program. *)

val parse_instruction : position -> name list -> (position * (name list)) list
                        -> instr * (position * (name list)) list
(* [parse_instruction position s lines] parses the next instruction that starts
with the line s at the position position. 
If needed, it will parse the rest of the lines to complete the instruction 
(in the case of an IF or WHILE instruction).

Returns the instruction and the list of lines that were not parsed. *)

val parse_block : (position * (name list)) list -> int
                  -> (position * instr) list
                  -> block * (position * (name list)) list
(* [parse_block lines ident acc] parses the block starting from the first
element of lines. 
It parses all the instructions of the block, until the indent of the line goes
back up (= it is the end of the block).
All the instructions are stored into the acc argument, along with their line position.

Returns the list of instructions and the list of lines that were not parsed. *)
  
val parse_expr : (name list) -> expr * (name list)
(* [parse_expr words] parses the list of words into an expression.
Fails if the expression cannot be parsed. *)                                    

val parse_cond : (name list) -> cond
(* [parse_cond words] parses the list of word into a condition *)  
  
val read_all_lines :  string -> string list
(* [read_all_lines filename] reads all the lines from that file 
and returns them in a list.
This methods does NOT handle errors in opening the file, it assumes
the file exists and can be opened. *)                                  

val lines_to_words : (position * name) list -> (position * (name list)) list
(* [lines_to_words lines] splits each line into words.
- lines is a list that associate each line number with its line
- the empty words (if there are 2 spaces next to each other) are not removed *)

val remove_comments : (position * (name list)) list -> (position * (name list)) list
(* [remove_comments lines] removes all the lines that contain the
word "COMMENT" from the list (even if it is not the first word of the line). 

Possible amelioration : only remove the line if "COMMENT" is the first word.
But that would add a lot of work for nothing. I think it is better to treat
"COMMENT" as a restricted keyword of the language. *)

val compute_indent : (name list) -> int
(* [compute_indent words] computes the identation of a list of words.
The identation is half of the number of empty words at the beginning of the list
(nb empty words / 2). *)

val find_cmp_and_partition : (name list) -> comp * (name list) * (name list)
(* [find_cmp_and_partition words] finds the comparaison operation in the list
of words. Then it partitions the list to get 2 sublists :
- the first one with all the elements that come before the operator in the list
- the second one with all the elements that come after the operator in the list
The operator is in neither of the two new sublists.

Fails if it cannot find an operator in the words list. *)
                                                                       
val next_line_is_else : (position * (name list)) list -> bool
(* [next_line_is_else lines] returns true if the first line of lines
is an "ELSE" instruction. *)
                                                        
