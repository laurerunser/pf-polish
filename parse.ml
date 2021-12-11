open Polish
open List
open Printf

(* [read_all_lines filename] reads all the lines from that file 
and returns them in a list.
This methods does NOT handle errors in opening the file, it assumes
the file exists and can be opened. *)                                  
let read_all_lines (filename:string) : (string) list =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
  

(* [lines_to_words lines] splits each line into words.
- lines is a list that associate each line number with its line
- the empty words (if there are 2 spaces next to each other) are not removed *)
let lines_to_words lines =
  let to_words l =
    let position, str = l in
    position, String.split_on_char ' ' str in
  map (fun l -> to_words l) lines


(* [remove_comments lines] removes all the lines that contain the
word "COMMENT" from the list (even if it is not the first word of the line). 

Possible amelioration : only remove the line if "COMMENT" is the first word.
But that would add a lot of work for nothing. I think it is better to treat
"COMMENT" as a restricted keyword of the language. *)
let remove_comments lines =
  filter (fun l -> let p,w = l in not (mem "COMMENT" w)) lines


(* [compute_indent words] computes the identation of a list of words.
The identation is half of the number of empty words at the beginning of the list
(nb empty words / 2). *)
let compute_indent (words:string list) : int =
  let rec loop w acc = match w with
    | [] -> acc
    | ""::xs -> loop xs (acc+1)
    | x::xs -> acc
  in (loop words 0) / 2


(* [parse_expr words] parses the list of words into an expression.
Fails if the expression cannot be parsed. *)                                    
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


(* [find_cmp_and_partition words] finds the comparaison operation in the list
of words. Then it partitions the list to get 2 sublists :
- the first one with all the elements that come before the operator in the list
- the second one with all the elements that come after the operator in the list
The operator is in neither of the two new sublists.

Fails if it cannot find an operator in the words list. *)
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


(* [parse_cond words] parses the list of word into a condition *)  
let parse_cond words =
  let cmp, l1, l2 = find_cmp_and_partition words in
  let e1, _ = parse_expr l1 in
  let e2, _ = parse_expr l2 in
  e1, cmp, e2


(* [next_line_is_else lines] returns true if the first line of lines
is an "ELSE" instruction. *)
let next_line_is_else lines =
  if lines = [] then false
  else
    let _, s = hd lines in
    let s = filter (fun k -> k <> "") s in
    if s = [] then false
    else if hd s = "ELSE" then true
    else false


(* [parse_instruction position s lines] parses the next instruction that starts
with the line s at the position position. 
If needed, it will parse the rest of the lines to complete the instruction 
(in the case of an IF or WHILE instruction).

Returns the instruction and the list of lines that were not parsed. *)
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
(* [parse_block lines ident acc] parses the block starting from the first
element of lines. 
It parses all the instructions of the block, until the indent of the line goes
back up (= it is the end of the block).
All the instructions are stored into the acc argument, along with their line position.

Returns the list of instructions and the list of lines that were not parsed. *)
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
  let lines = read_all_lines filename in (* read the lines *)
  let lines_with_positions = (* associate them with their line numbers *)
    mapi (fun k str -> (k, str)) lines in
  let lines_words = lines_to_words lines_with_positions in (* splits them into words *)
  let lines_clean = remove_comments lines_words in (* remove the comments *)
  let prgm, _ = parse_block lines_clean 0 [] in (* parse into a program *)
  prgm

