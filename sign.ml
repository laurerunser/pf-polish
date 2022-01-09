open Polish
open Eval
open Simpl
let test () = ()

let revert_sign s =
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
  let s1 = revert_sign s1 in
  let s2 = revert_sign s2 in
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
  let list_e = Signs.find n signs in
  loop_expr e list_e signs

(********************************************)
let prop_cond e1 e2 comp signs =
  

  None, []

let rec sign_cond c env signs = 
  let e1, comp, e2 = c in
  match e1, e2 with
  | Num(a), Num(b) -> if eval_cond c env then Some(true), signs else Some(false), signs
  | Num(a), e2 -> prop_cond e1 e2 comp signs
  | _ -> failwith ""
  ;;


(**********************************************)

let rec signs_program p env signs =
  match p with
  | [] -> signs
  | (i, Print(e))::xs  -> signs_program xs env signs
  | (i, Read(n))::xs -> signs_program xs env (Signs.add n [Neg; Zero; Pos] signs)
  | (i, Set(n, e))::xs -> signs_program xs (NameTable.add n e env) (Signs.add n (find_signs_expr n e signs) signs)
  | (i, If(c, b, b2))::xs -> signs_program xs env (signs_if c b b2 env signs)
  | (i, While(c, b))::xs -> signs_program xs env (signs_while c b env signs)
and signs_if c b b2 env signs = 

  signs
and signs_while c b env signs = signs