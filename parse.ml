open Polish

open List
open Printf
   
(* Opens the file `filename` and reads all the lines.
Returns a list that associates each line with its line number. *)  
let read_all_lines (filename:string) : (string) list =
(*  let in_chan = open_in filename in
  let try_read = try Some (input_line in_chan) with End_of_file -> None in
  let rec read_lines acc p = match try_read with
    | Some s -> read_lines ((p, s)::acc) (p+1)
    | None -> close_in in_chan; List.rev acc in
    printf "read all the lines\n";
  read_lines [] 1 (* the first line is number 1 *) *)
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


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


let rec print_arr w =
  match w with
  | [] -> print_string "ok\n"
  | x::xs -> printf "%s " x; print_arr xs       
 

(* parse the list of words to get the arithmetic expression
Returns the expr and a list of the rest of the words  *)
let rec parse_expr words =
  if words = [] then failwith "Invalid line"
  else
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
      let i = try Num(int_of_string s)
              with Failure _ -> Var(s) in
      i, (tl words)



(* Finds the comparaison operator in the list of words.
Returns the operator and 2 lists :
- the first one contains the elements before the operator in the original list
- the second one contains the elements after the operator in the original list*)
let find_cmp_and_partition words =
  let rec loop acc l =
    match l with
    | [] -> failwith "No comparaison operator in the condition\n"
    | x::xs ->
       match x with
       | "=" -> Eq, List.rev acc, xs
       | "<>" -> Ne, List.rev acc, xs
       | "<" -> Lt, List.rev acc, xs
       | "<=" -> Le, List.rev acc, xs
       | ">" -> Gt, List.rev acc, xs
       | ">=" -> Ge, List.rev acc, xs
       | _ -> loop (x::acc) xs
  in loop [] words

(* Parses the list of words into a condition*)
let parse_cond words =
  let cmp, l1, l2 = find_cmp_and_partition words in
  let e1, _ = parse_expr l1 in
  let e2, _ = parse_expr l2 in
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
  let fail p = ksprintf failwith "Invalid line %d" p in
  let indent = compute_indent s in
  let words = filter (fun k -> k <> "") s in
  match words with
  | "READ"::_ -> if length words = 2 then Read(nth words 1), tl lines
              else fail position
  | "PRINT"::_ -> if length words < 2 then fail position
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
  | "COMMENT"::_ ->
     let new_p, new_s = hd lines in
     parse_instruction new_p new_s (tl lines)
  | _ -> fail position
and parse_block lines indent acc =
  if lines = [] then List.rev acc, []
  else 
    let position, s = hd lines in
    let i2 = compute_indent s in
    if i2 = indent then
      let new_instr, rest_of_lines = parse_instruction position s lines in
      let acc = (position, new_instr)::acc in
      parse_block rest_of_lines indent acc
    else List.rev acc, lines

      
let parse_file (filename:string) : program =
  (* read all the lines and associate them with their line numbers *)
  let lines = read_all_lines filename in
  let lines_with_positions =
    mapi (fun k str -> (k, str)) lines in
  let lines_words = lines_to_words lines_with_positions in
  let lines_clean = remove_comments lines_words in
  let prgm, _ = parse_block lines_clean 0 [] in
  prgm

