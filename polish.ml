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
(* Opens the file `filename` and reads all the lines.
Returns a list that associates each line with its line number. *)  
let read_all_lines (filename:string) : (int * string) list =
  let in_chan = open_in filename in
  let try_read = try Some (input_line in_chan) with End_of_file -> None in
  let rec read_lines acc p = match try_read with
    | Some s -> read_lines ((p, s)::acc) (p+1)
    | None -> close_in in_chan; List.rev acc in
  read_lines [] 1 (* the first line is number 1 *)

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
                        


(* *)
let read_polish (filename:string) : program = 
  (* read all the lines and associate them with their line numbers *)
  let lines_list = read_all_lines filename in

                                            
let print_polish (p:program) : unit = failwith "TODO"

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
