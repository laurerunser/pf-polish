open Polish
open Parse
open Print
open Eval
open Simpl
open Printf

let read_polish filename = Parse.parse_file filename

let print_polish p = print_block p 0

let simplify_polish p = 
  simplify_block p (NameTable.empty)

let eval_polish p =
  let env = eval_block p (NameTable.empty) in
  (* eval return type is not unit, so needs an empty print statement to compile *)
  ()

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: option filename \n";
  print_string "options supported :\n";
  print_string "-reprint : parse the polish file and reprint it\n";
  print_string "-eval : evaluate the polish file\n";
  print_string "-simpl : simplify the polish file and print the result\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simplify_polish (read_polish file))
  | _ -> usage ()

let () = main ()
