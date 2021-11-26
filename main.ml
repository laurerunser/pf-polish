open Polish
open Parse
open Print

let read_polish filename = Parse.parse_file filename

let print_polish p = print_block p 0

let eval_polish p = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish  (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

let () = main ()
