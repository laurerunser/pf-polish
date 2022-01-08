open Polish
open Printf
   
(* [print_indent i] prints twice as many space as i.
For example, if i = 3, then 6 spaces will be printed. *)                            
let rec print_indent i =
  match i with
  | 0 -> printf ""
  | n -> printf "  "; print_indent (n-1)

(* [print_op o] prints the symbol corresponding to the operator o.
It prints a space after the symbol, but not before. *)
let print_op op = match op with
  | Add -> printf "+ "
  | Sub -> printf "- "
  | Mul -> printf "* "
  | Div -> printf "/ "
  | Mod -> printf "%% " (* % is a special char, need to escape it *)

(* [print_comp c] prints the symbol corresponding to the comparaison operator c.
It prints a space before and after the symbol. *)
let print_comp comp = match comp with
  | Eq -> printf " = "
  | Ne -> printf " <> "
  | Lt -> printf " < "
  | Le -> printf " <= "
  | Gt -> printf " > "
  | Ge -> printf " >= "

(* [print_expr e] prints the expressions e.
It doesn't print a newline at the end of the expression. *)
let rec print_expr expr=
  match expr with
  | Num(i) -> printf "%d" i;
  | Var(n) -> printf "%s" n;
  | Op(op, e1, e2) -> print_op op; print_expr e1; printf " ";
                      print_expr e2

(* [print_cond c] prints the condition c.
It prints a space between each value, variable name and operator.
It doesn't print a newline at the end of the condition. *)
let print_cond condition =
  let e1, comp, e2 = condition in
  print_expr e1; print_comp comp; print_expr e2                   

(* [print_block b i] prints the instructions of the block b with an indent of i. 
   The indent is half of the number of spaces to print (an indent of 3 = 6 spaces). *)
let rec print_block block indent =
  match block with
  | [] -> printf ""
  | i::xs -> print_line i indent; print_block xs indent
(* [print_line (p, ins) i] prints the instruction ins with an indent of i. 
It prints a newline at the end of the line *)
and print_line line indent =
  print_indent indent;
  let position, instruction = line in
  match instruction with
  | Set(name, expr) -> printf "%s := " name; print_expr expr; printf "\n"
  | Read(name) -> printf "READ %s\n" name
  | Print(expr) -> printf "PRINT "; print_expr expr; printf "\n"
  | If(condition, block, block2) ->
     printf "IF "; print_cond condition; printf "\n";
     print_block block (indent+1);
     if block2 <> [] then
      print_indent indent; printf "ELSE \n"; print_block block2 (indent+1)
  | While(condition, block) ->
     printf "WHILE "; print_cond condition; printf "\n";
     print_block block (indent+1)
