open Polish
open Parse
open Print
open Eval
open Simpl
open Printf
open Vars
open Sign

let read_polish filename = Parse.parse_file filename

let print_polish p = print_block p 0

let simplify_polish p = 
  simplify_block p (NameTable.empty)

let eval_polish p =
  let env = eval_block p (NameTable.empty) in
  (* eval return type is not unit, so needs an empty print statement to compile *)
  ()
  
let analyse_vars p = print_vars p

let analyse_signs p = () 
(* first simplify the program !! *)

let usage () =
  print_string "Polish : static analysis of a mini language\n";
  print_string "usage: option filename \n";
  print_string "Supported option:\n";
  print_string "-reprint : parse the polish file and reprint it\n";
  print_string "-eval : evaluate the polish file\n";
  print_string "-simpl : simplify the polish file and print the result\n";
  print_string "-vars : performs a static analysis of the variables and prints :\n-the names of all the variables";
  print_string "in the program\n-the name of all the variables that might have been accessed before being initialized\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simplify_polish (read_polish file))
  | [|_;"-vars";file|] -> analyse_vars (read_polish file)
  | [|_;"-sign";file|] -> test ()
  | _ -> usage ()

let () = main ()
