open Polish
let test () = ()

type sign = Neg | Zero | Pos | Error

let revert_sign s =
  match s with
  | Pos -> Neg
  | Neg -> Pos
  | s -> s

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

let rec remove_duplicates l =
  let add_zero = if List.mem Zero l then [Zero] else [] in
  let add_neg = if List.mem Neg l then Neg::add_zero else add_zero in
  let add_pos = if List.mem Pos l then Pos::add_neg else add_neg in
  if List.mem Error l then Error::add_pos else add_pos

let rec sign_expr e env =
  match e with
  | Num(_) | Var(_) -> env
  | Op(op, e1, e2) ->
    let l1 = sign_expr e1 env in
    let l2 = sign_expr e2 env in
    match op with
    | Add -> find_sign l1 l2 sign_add env []
    | Sub -> find_sign l1 l2 sign_sub env []
    | Mul -> find_sign l1 l2 sign_mul env []
    | Div -> find_sign l1 l2 sign_div env []
    | Mod -> find_sign l1 l2 sign_mod env []
and find_sign l1 l2 sign_fun env acc =
  let rec loop s1 l2 acc =
    match l2 with
    | [] -> acc
    | s2::xs -> 
      loop s1 xs (acc @ (sign_fun s1 s2))
 in match l1 with
 | [] -> remove_duplicates acc
 | y::ys -> find_sign ys l2 sign_fun env (loop y l2 acc)
