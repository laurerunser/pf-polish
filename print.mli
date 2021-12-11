(* Print module : this module handles the printing of a polish program.*)
open Polish

val print_block : block -> int -> unit
(* [print_block b i] prints the instructions of the block b with an indent of i. 
   The indent is half of the number of spaces to print (an indent of 3 = 6 spaces). *)


(* For reference, the other functions in that module are described below : 

val print_line : (int * instr) -> int -> unit

val print_cond : cond -> unit

val print_expr : expr -> unit

val print_comp : comp -> unit

val print_op : op -> unit

val print_indent : int -> unit
 *)
