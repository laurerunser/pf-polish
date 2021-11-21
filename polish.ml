(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)
open List
open Printf
   
(* Opens the file `filename` and reads all the lines.
Returns a list that associates each line with its line number. *)  
let read_all_lines (filename:string) : (int * string) list =
  let in_chan = open_in filename in
  let try_read = try Some (input_line in_chan) with End_of_file -> None in
  let rec read_lines acc p = match try_read with
    | Some s -> read_lines ((p, s)::acc) (p+1)
    | None -> close_in in_chan; List.rev acc in
  read_lines [] 1 (* the first line is number 1 *)

(* changes the list from (int*string) to (int*(string list)) by seperating
the line into words. Doesn't remove the empty strings (in between the spaces 
if there are several next to each other) *)
let lines_to_words lines =
  let to_words l =
    let position, str = l in
    position, String.split_on_char ' ' str in
  map (fun l -> to_words l) lines

(* remove all the lines that have the COMMENT key-word inside *)
let remove_comments lines =
  filter (fun l -> let p,w = l in not (mem "COMMENT" w)) lines


(* computes the indentation of a line of words.
The identation is the number of spaces / 2 at the beginning of the line *)
let compute_indent (words:string list) : int =
  let rec loop w acc = match w with
    | [] -> acc
    | ""::xs -> loop xs (acc+1)
    | x::xs -> acc
  in (loop words 0) / 2


(* parse the list of words to get the arithmetic expression
Returns the expr and a list of the rest of the words  *)
let rec parse_expr words =
  let s = hd words in
  let ops = ["+"; "-"; "*"; "/"; "%"] in
  if (mem s ops) then
    let e, rest = parse_expr (tl words) in
    let e2, _ = parse_expr rest in
    match s with
    | "+" -> Op(Add, e, e2), []
    | "-" -> Op(Sub, e, e2), []
    | "*" -> Op(Mul, e, e2), []
    | "/" -> Op(Div, e, e2), []
    | "%" -> Op(Mod, e, e2), []
    | _ -> raise (Failure "Invalid operator in expr")
  else
    try Num(int_of_string s), tl words
    with Failure _ -> Var(s), tl words            

(* parse the list of words to get the condition and return it *)
let parse_cond words =
  let e1, rest = parse_expr words in
  let cmp_word = hd rest in
  let e2, rest = parse_expr (tl rest) in
  let cmp = 
    match cmp_word with
    | "=" -> Eq
    | "<>" -> Ne
    | "<" -> Lt
    | "<=" -> Le
    | ">" -> Gt
    | ">=" -> Ge
    | _ -> raise (Failure "Invalid comparaison operator in condition") in
  e1, cmp, e2

(* returns true if the next line begins by ELSE, false otherwise *)
let next_line_is_else lines =
  if lines = [] then false
  else
    let _, s = hd lines in
    let s = filter (fun k -> k <> "") s in
    if s = [] then false
    else if hd s = "ELSE" then true
    else false
                        
let rec parse_instruction position s lines =
  let fail = ksprintf failwith "Invalid line %d" position in
  let indent = compute_indent s in
  let words = filter (fun k -> k <> "") s in
  match words with
  | "READ"::_ -> if length words = 2 then Read(nth words 2), tl lines
              else fail
  | "PRINT"::_ -> if length words < 2 then fail
               else let e, _ = parse_expr (tl words) in
                    Print(e), tl lines
  | "IF"::_ ->
     let condition = parse_cond (tl words) in
     let block, rest_of_lines = parse_block (tl lines) (indent+1) [] in
     if next_line_is_else rest_of_lines then
       let block2, rest_of_lines2= parse_block (tl rest_of_lines) (indent+1) [] in
       If(condition, block, block2), rest_of_lines2
     else If(condition, block, []), rest_of_lines
  | "WHILE"::_ -> let condition = parse_cond (tl words) in
               let block, rest_of_lines = parse_block (tl lines) (indent+1) [] in
               While(condition, block), rest_of_lines
  | x::":="::_ ->
           let e, _ = parse_expr (tl (tl words)) in
           Set(x, e), tl lines
  | _ -> fail
and parse_block lines indent acc =
  let position, s = hd lines in
  let i2 = compute_indent s in
  if i2 = indent then
    let new_instr, rest_of_lines = parse_instruction position s lines in
    let acc = (position, new_instr)::acc in
    parse_block rest_of_lines indent acc
  else List.rev acc, lines

      
let read_polish (filename:string) : program = 
  (* read all the lines and associate them with their line numbers *)
  let lines = read_all_lines filename in
  let lines_words = lines_to_words lines in
  let lines_clean = remove_comments lines_words in
  let prgm, _ = parse_block lines_clean 0 [] in
  prgm

(**********************************************************************************)
let rec print_indent i =
  match i with
  | 0 -> printf "\n"
  | n -> printf "  "; print_indent (n-1)

let print_op op = match op with
  | Add -> printf "+ "
  | Sub -> printf "- "
  | Mul -> printf "* "
  | Div -> printf "/ "
  | Mod -> printf "%%" (* % is a special char, need to escape it *)

let print_comp comp = match comp with
  | Eq -> printf " = "
  | Ne -> printf " <> "
  | Lt -> printf " < "
  | Le -> printf " <= "
  | Gt -> printf " > "
  | Ge -> printf " >= "
         
let rec print_expr expr =
  match expr with
  | Num(i) -> printf "%d\n" i
  | Var(n) -> printf "%s\n" n
  | Op(op, e1, e2) -> print_op op; print_expr e1; printf " ";
                      print_expr e2; printf "\n"

let print_cond condition =
  let e1, comp, e2 = condition in
  print_expr e1; print_comp comp; print_expr e2; printf "\n"
                        
                        
let rec print_block block indent =
  match block with
  | [] -> printf "\n"
  | i::xs -> print_line i indent; print_block xs indent
and print_line line indent =
  print_indent indent;
  let position, instruction = line in
  match instruction with
  | Set(name, expr) -> printf "%s := " name; print_expr expr;
  | Read(name) -> printf "READ %s\n" name
  | Print(expr) -> printf "PRINT "; print_expr expr
  | If(condition, block, block2) ->
     printf "IF "; print_cond condition;
     print_block block (indent+1);
     if block2 <> [] then
       printf "ELSE\n"; print_block block2 (indent+1)
  | While(condition, block) ->
     printf "WHILE "; print_cond condition;
     print_block block (indent+1)

let print_polish (p:program) : unit = print_block p 0

(**********************************************************************************)
let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
