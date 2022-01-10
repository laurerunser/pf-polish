open Polish
open Eval
open Simpl
let test () = ()

(* Reverse the comparaison operator to go from "a comp b" to "b comp a" *)
let reverse_comp c =
  match c with
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Ge
  | Le -> Gt
  | Ge -> Lt
  | Gt -> Le
let reverse_sign s =
  match s with
  | Pos -> Neg
  | Neg -> Pos
  | s -> s

let remove_duplicates l =
  let add_zero = if List.mem Zero l then [Zero] else [] in
  let add_neg = if List.mem Neg l then Neg::add_zero else add_zero in
  let add_pos = if List.mem Pos l then Pos::add_neg else add_neg in
  if List.mem Error l then Error::add_pos else add_pos
  

let sign_add s1 s2 =
    match s1, s2 with
    | Zero, Zero -> [Zero]
    | Zero, Pos | Pos, Zero -> [Pos]
    | Neg, Zero | Zero, Neg -> [Neg]
    | Pos, Pos -> [Pos]
    | Neg, Neg -> [Neg]
    | Pos, Neg | Neg, Pos -> [Neg; Zero; Pos]
    | Error, _ | _, Error -> [Error]

let sign_sub s1 s2 =
  let s1 = reverse_sign s1 in
  let s2 = reverse_sign s2 in
  sign_add s1 s2

let sign_mul s1 s2 =
  match s1, s2 with
  | Error, _| _, Error -> [Error]
  | _, Zero -> [Zero]
  | Zero, _ -> [Zero]
  | Pos, Neg | Neg, Pos -> [Neg]
  | Pos, Pos | Neg, Neg -> [Pos]

let sign_div s1 s2 =
  match s1, s2 with
  | Error, _| _, Error | _, Zero -> [Error]
  | Zero, _ -> [Zero]
  | Pos, Neg | Neg, Pos -> [Neg]
  | Pos, Pos | Neg, Neg -> [Pos]
(* We could have combined both sign_mul and sign_div into a single function, with a
boolean to know which case we were on. 
But then we would not have been able to use those functions as first-class arguments 
in the find_sign function because all the functions to find  the sign would not have 
had the same type.
I though this solution was the best one, and the one that required the least amount of code *)
  
let sign_mod s1 s2 =
  match s1, s2 with
  | Error, _ | _, Error | _, Zero ->  [Error]
  | s1, s2 -> [s1]
  (* The Pervasives documentation states that : "Note that x mod y is negative only if x < 0."
    So the mod is always the same sign than the dividend (unless y=0, then error) *)

let signs_num n =
  if n = 0 then [Zero]
  else if n > 0 then [Pos]
  else [Neg]

let signs_var n signs =
  let signs_list = Signs.find_opt n signs in
  if Option.is_none signs_list then [Error] (* This var was not initialized before use *)
  else Option.get signs_list (* The var was already initialized so we return its sign list*)

let rec loop_expr e signs_list signs =
  match e with
  | Num(n) -> signs_num n
  | Var(n) -> signs_var n signs
  | Op(op, e1, e2) ->
    let l1 = loop_expr e1 signs_list signs in
    let l2 = loop_expr e2 signs_list signs in
    match op with
    | Add -> find_sign l1 l2 sign_add signs_list signs []
    | Sub -> find_sign l1 l2 sign_sub signs_list signs []
    | Mul -> find_sign l1 l2 sign_mul signs_list signs []
    | Div -> find_sign l1 l2 sign_div signs_list signs []
    | Mod -> find_sign l1 l2 sign_mod signs_list signs []
and find_sign l1 l2 sign_fun signs_list signs acc =
  let rec loop s1 l2 acc =
    match l2 with
    | [] -> acc
    | s2::xs -> 
      loop s1 xs (acc @ (sign_fun s1 s2))
 in match l1 with
 | [] -> remove_duplicates acc
 | y::ys -> find_sign ys l2 sign_fun signs_list signs (loop y l2 acc)

let find_signs_expr n e (signs:sign list Signs.t) =
  let list_e = Signs.find_opt n signs in
  loop_expr e list_e signs

(********************************************)
let add_signs var s_to_add signs_list signs =
  let new_s = if List.mem Error signs_list then s_to_add@[Error] else s_to_add in
  Signs.add var new_s (Signs.remove var signs)

let remove_signs var s_to_remove signs_list signs =
  let s1 = if List.mem Zero signs_list then [] else [Zero] in
  let s2 = if List.mem Neg signs_list then s1 else Neg::s1 in
  let s3 = if List.mem Pos signs_list then s2 else Pos::s2 in
  let s_to_add = if List.mem Error signs_list then s3 else Error::s3 in
  add_signs var s_to_add signs_list signs

let rec compare_var_and_num var n comp signs =
  let signs_list = signs_var var signs in
  if signs_list = [Error] then false, signs (* impossible *)
  else if n = 0 then
    match comp with
    | Eq -> if List.mem Zero signs_list then true, add_signs var [Zero] signs_list signs 
            else false, signs
    | Ne -> if signs_list = [Zero] || signs_list = [Zero; Error] || signs_list = [Error; Zero] 
              then false, signs (* impossible *)
            else if List.mem Zero signs_list (* possible but cannot be Zero *)
              then true, remove_signs var [Zero] signs_list signs
            else true, signs (* true *)
    | Lt -> if List.mem Neg signs_list then true, add_signs var [Neg] signs_list signs (* ok, and the var is now Neg (or Error) only *)
            else false, signs (* impossible *)
    | Gt -> if List.mem Pos signs_list then true, add_signs var [Pos] signs_list signs (* ok, and the var is now Pos (or Error) only *)
            else false, signs (* impossible *)
    | Le -> if List.mem Zero signs_list || List.mem Neg signs_list then true, add_signs var [Neg; Zero] signs_list signs
            else false, add_signs var [Pos] signs_list signs
    | Ge -> if List.mem Zero signs_list || List.mem Pos signs_list then true, add_signs var [Pos; Zero] signs_list signs
            else false, add_signs var [Neg] signs_list signs
  else if n > 0 then
    match comp with
    | Eq | Gt | Ge -> compare_var_and_num var 0 Gt signs (* var > 0 *)
    | Ne | Lt | Le -> true, signs (* possible, no new info about signs *)
  else (* n < 0 *)
    match comp with
    | Eq | Lt | Le-> compare_var_and_num var 0 Lt signs
    | Ne | Gt | Ge -> true, signs (* no new info about signs *) 

(*let rec are_expr_equal e1 e2 =
  match e1, e2 with
  | Num(a), Num(b) when a = b -> true
  | Var(n), Var(m) when n = m -> true
  | Op(op1, a, b), Op(op2, c, d) ->
    if op1 != op2 then false
    else are_expr_equal a c && are_expr_equal b d
  | _ -> false*)

let rec compare_2_signs s1 s2 comp =
  match s1, s2 with
  | Error, _ | _ , Error -> false
  | Zero, Zero ->
    (match comp with
    | Eq | Le | Ge -> true
    | Ne | Lt | Gt -> false)
  | Zero, Neg ->
    (match comp with
    | Ne | Gt | Ge -> true
    | Eq | Le | Lt -> false)
  | Neg, Zero -> compare_2_signs s2 s1 (reverse_comp comp)
  | Zero, Pos | Neg, Pos -> 
    (match comp with
    | Eq | Ge | Gt -> false
    | Ne | Le | Lt -> true) 
  | Pos, Zero -> compare_2_signs s2 s1 (reverse_comp comp)
  | Pos, Pos | Neg, Neg -> true
  | Pos, Neg -> compare_2_signs s2 s1 (reverse_comp comp)

let rec compare_2_expr e1 e2 comp signs =
  let signs_e1 = signs_var e1 signs in
  let signs_e2 = find_signs_expr e1 (Var(e1)) signs in
  let rec loop_e2 s1 l2 b acc =
    match l2 with
    | [] -> b, acc
    | x::xs -> 
      let b2 = compare_2_signs s1 x comp in
      if b2 then loop_e2 s1 xs true (x::acc)
      else loop_e2 s1 xs b acc
    in
  let rec loop_e1 l1 b acc1 acc2 =
    match l1 with
    | [] -> b, acc1, acc2
    | x::xs ->
      let b2, acc_e = loop_e2 x signs_e2 false [] in
      if b2 then loop_e1 xs b2 (remove_duplicates (x::acc1)) (remove_duplicates acc_e@acc2)
      else loop_e1 xs b acc1 acc2
    in
  loop_e1 signs_e1 false [] []


let rec sign_cond c signs = 
  let e1, comp, e2 = c in
  match e1, e2 with
  | Num(a), Num(b) -> if a > b then true, signs else false, signs
  | Var(var), Num(n) -> compare_var_and_num var n comp signs
  | Num(n), Var(var) -> compare_var_and_num var n (reverse_comp comp) signs
  | Var(var), e2 -> 
    let b, signs_var, signs_e = compare_2_expr var e2 comp signs in
    let signs = Signs.add var signs_var (Signs.remove var signs) in
    (* updates the signs of the var *)
    (* TODO ?? maybe should also determine stuff about the vars inside the expr (if any)
       with the new info we got about the sign of the expression *)
    b, signs
  | e1, Var(var) -> sign_cond (Var(var), reverse_comp comp, e1) signs
  | _ -> failwith ""


(**********************************************)

let rec signs_program p signs =
  match p with
  | [] -> signs
  | (i, Print(e))::xs  -> signs_program xs signs
  | (i, Read(n))::xs -> signs_program xs(Signs.add n [Neg; Zero; Pos] signs)
  | (i, Set(n, e))::xs -> signs_program xs (Signs.add n (find_signs_expr n e signs) signs)
  | (i, If(c, b, b2))::xs -> signs_program xs (signs_if c b b2 signs)
  | (i, While(c, b))::xs -> signs_program xs (signs_while c b signs)
and signs_if c b b2 signs = 

  signs
and signs_while c b signs = signs


let print_signs p =
  let signs = signs_program p Signs.empty in
  let rec print_list s_list =
    match s_list with
    | [] -> print_newline()
    | x::xs -> 
      match x with
      | Neg -> print_string "-"; print_list xs
      | Pos -> print_string "+"; print_list xs
      | Zero -> print_string "0"; print_list xs
      | Error -> print_string "!"; print_list xs
  in
  Signs.iter (fun x signs_list -> 
              print_string x; print_string " : "; 
              print_list signs_list) signs
(* TODO : make sure the signs in the sign list come in the right order *)