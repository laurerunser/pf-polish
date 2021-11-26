(* Print module : this module handles the printing of a polish program.*)
open Polish

val print_block : block -> int -> unit
(* [print_block b i] prints the instructions of the block b with an indent of i. 
   The indent is half of the number of spaces to print (an indent of 3 = 6 spaces). *)

val print_line : (int * instr) -> int -> unit
(* [print_line (p, ins) i] prints the instruction ins with an indent of i. 
It prints a newline at the end of the line *)

val print_cond : cond -> unit
(* [print_cond c] prints the condition c.
It prints a space between each value, variable name and operator.
It doesn't print a newline at the end of the condition. *)

val print_expr : expr -> unit
(* [print_expr e] prints the expressions e.
It doesn't print a newline at the end of the expression. *)

val print_comp : comp -> unit
(* [print_comp c] prints the symbol corresponding to the comparaison operator c.
It prints a space before and after the symbol. *)

val print_op : op -> unit
(* [print_op o] prints the symbol corresponding to the operator o.
It prints a space after the symbol, but not before. *)

val print_indent : int -> unit
(* [print_indent i] prints twice as many space as i.
For example, if i = 3, then 6 spaces will be printed. *)                            
