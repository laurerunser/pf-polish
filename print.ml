open Polish
open Printf
   

let rec print_indent i =
  match i with
  | 0 -> printf ""
  | n -> printf "  "; print_indent (n-1)

let print_op op = match op with
  | Add -> printf "+ "
  | Sub -> printf "- "
  | Mul -> printf "* "
  | Div -> printf "/ "
  | Mod -> printf "%% " (* % is a special char, need to escape it *)

let print_comp comp = match comp with
  | Eq -> printf " = "
  | Ne -> printf " <> "
  | Lt -> printf " < "
  | Le -> printf " <= "
  | Gt -> printf " > "
  | Ge -> printf " >= "
         
let rec print_expr expr=
  match expr with
  | Num(i) -> printf "%d" i;
  | Var(n) -> printf "%s" n;
  | Op(op, e1, e2) -> print_op op; print_expr e1; printf " ";
                      print_expr e2

let print_cond condition =
  let e1, comp, e2 = condition in
  print_expr e1; print_comp comp; print_expr e2

  (*let rec print_expr expr newline =
  match expr with
  | Num(i) -> printf "%d\n" i;
  | Var(n) -> printf "%s\n" n;
  | Op(op, e1, e2) -> print_op op; print_expr e1 false; printf " ";
                      print_expr e2 false;
  if newline then printf "\n"

let print_cond condition newline =
  let e1, comp, e2 = condition in
  print_expr e1 false; print_comp comp; print_expr e2 false;
  if newline then printf "\n"*)
                        
                        
let rec print_block block indent =
  match block with
  | [] -> printf ""
  | i::xs -> print_line i indent; print_block xs indent
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
       printf "ELSE\n"; print_block block2 (indent+1)
  | While(condition, block) ->
     printf "WHILE "; print_cond condition; printf "\n";
     print_block block (indent+1)
