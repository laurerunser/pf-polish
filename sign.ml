open Polish

(********************************************)
(* BASIC HELPER FUNCTIONS *)

(* Reverse the comparaison operator comp to go from "a comp b" to "b comp a" *)
let reverse_comp comp =
  match comp with
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Ge
  | Le -> Gt
  | Ge -> Lt
  | Gt -> Le

(* Returns the opposite of the sign s 
(Neg->Pos, Pos->Neg, the rest stays the same)*)
let reverse_sign s =
  match s with
  | Pos -> Neg
  | Neg -> Pos
  | s -> s

(* [remove_duplicates l] builds and returns a new sign list
that contains the same elements that l but only once. 
This is a rather "dirty" inefficient function, but it really simplified
the code, and it doesn't take too much time because there are only 4
possible values, and l stays rather low in size. *)
let remove_duplicates l =
  let add_zero = if List.mem Zero l then [Zero] else [] in
  let add_neg = if List.mem Neg l then Neg::add_zero else add_zero in
  let add_pos = if List.mem Pos l then Pos::add_neg else add_neg in
  if List.mem Error l then Error::add_pos else add_pos


(* FUNCTION TO COMPUTE A POSSIBLE SIGN FROM THE OPERATOR, AND A SIGN
   FROM EACH SIDE OF THE EXPRESSION *)

(* We could have combined both sign_mul and sign_div into a single function, with a
boolean to know which case we were on.
In the same way, only sign_div needs to have current_line and error_line as arguments
because it is the only function in which we might want to raise an error. 
But then we would not have been able to use those functions as first-class arguments 
in the find_sign function because all the functions to find the sign would not have 
had the same type.
I though this solution was the best one, and the one that required the least amount 
of code. 
I decided to split all the functions so that they are more readable and easier to
debug. *)

(* All of the functions below take as argument :
- s1 : a sign for the left side of the expression 
- s2 : a sign for the right side of the expression
- current_line : the current line number
- error_line : the error line number 
The operator they process is indicated in their names (add -> Add, ...)
They return a list of the possible signs of the expression. *)

let sign_add s1 s2 current_line error_line =
  let signs = 
    match s1, s2 with
    | Zero, Zero -> [Zero]
    | Zero, Pos | Pos, Zero -> [Pos]
    | Neg, Zero | Zero, Neg -> [Neg]
    | Pos, Pos -> [Pos]
    | Neg, Neg -> [Neg]
    | Pos, Neg | Neg, Pos -> [Neg; Zero; Pos]
    | Error, _ | _, Error -> [Error]
  in signs, error_line

let sign_sub s1 s2 current_line error_line =
  let s1 = reverse_sign s1 in
  let s2 = reverse_sign s2 in
  sign_add s1 s2 current_line error_line

let sign_mul s1 s2 current_line error_line =
  let signs = 
    match s1, s2 with
    | Error, _| _, Error -> [Error]
    | _, Zero -> [Zero]
    | Zero, _ -> [Zero]
    | Pos, Neg | Neg, Pos -> [Neg]
    | Pos, Pos | Neg, Neg -> [Pos]
  in signs, error_line

let sign_div s1 s2 current_line error_line =
  match s1, s2 with
  | Error, _| _, Error -> [Error], error_line
  | _, Zero -> 
    [Error], if error_line = -1 then current_line else error_line
    (* Error : if the error_line is -1, then this is the first 
      possible division by 0 of the program. In this case, return
      the current line number. 
      Otherwise, this is not the first error, so return error_line
      that is already set to the first line where a division by 0 
      occurs.
    *)
  | Zero, _ -> [Zero], error_line
  | Pos, Neg | Neg, Pos -> [Neg], error_line
  | Pos, Pos | Neg, Neg -> [Pos], error_line
  
let sign_mod s1 s2 current_line error_line =
  match s1, s2 with
  | Error, _ | _, Error | _, Zero ->  [Error], error_line
  | s1, s2 -> [s1], error_line
  (* The Pervasives documentation states that : "Note that x mod y is negative only if x < 0."
    So the mod is always the same sign than the dividend (unless y=0, then error) *)

(********************************************)
(* FUNCTIONS TO COMPUTE THE SIGN OF AN EXPRESSION *)

(* [sign_num n] takes an integer and returns its sign. *)
let sign_num n =
  if n = 0 then [Zero]
  else if n > 0 then [Pos]
  else [Neg]

(* [signs_var var signs] 
- var : the name of the variable
- signs : the current signs environement
This function returns the signs of the variable in the current environment,
or [Neg; Zero; Pos] if it doesn't have any. 
We decided that the Error sign only detects the division by zero errors, and 
not variable initialization problems. *)
let signs_var var signs =
  let signs_list = Signs.find_opt var signs in
  if Option.is_none signs_list then [Neg; Zero; Pos] (* the var was not initialized before use *)
  else Option.get signs_list (* the var was already initialized so we return its sign list *)

(* [loop_expr e signs_list signs current_line error_line] :
- e : the expression we are trying to find the signs of
- signs : the current sign environment
- current_line, error_line : the current and error line numbers
The function returns a list of the possible signs of the expression 
and the error line number.
It recursively matches on the expression until it finds either a variable or a number
to find the base signs. Then it go back up and combines the different signs with
the help of the find_sign function. *)
let rec find_signs_expr e signs current_line error_line =
  match e with
  | Num(n) -> sign_num n, error_line
  | Var(n) -> signs_var n signs, error_line
  | Op(op, e1, e2) ->
    let l1, error_line1 = find_signs_expr e1 signs current_line error_line in
    let l2, error_line2 = find_signs_expr e2 signs current_line error_line in
    let error_line = if error_line1 = -1 then error_line2 else min error_line1 error_line2 in
    let f = (match op with
    | Add -> sign_add
    | Sub -> sign_sub
    | Mul -> sign_mul
    | Div -> sign_div
    | Mod -> sign_mod)
    in find_sign l1 l2 f signs [] current_line error_line
(* [find_sign l1 l2 sign_fun signs acc current_line error_line]
- l1 : the list of possible signs for the left side of the expression
- l2 : the list of possible signs for the right side of the expression
- sign_fun : a function that takes 2 signs and returns the possible signs
            There is a function for each operator, above in the file
- signs : the current signs environment
- acc : an accumulator to store the possible signs
- current_line, error_line : the current and error line numbers
This function takes a list of possible signs for each side of an expression,
and returns the possible signs of the complete expression, and the error line 
number. *)
and find_sign l1 l2 sign_fun signs acc current_line error_line =
  let rec loop s1 l2 acc error_line =
    match l2 with
    | [] -> acc, error_line
    | s2::xs -> 
      let acc2, error_line = sign_fun s1 s2 current_line error_line in
      loop s1 xs (acc @ acc2) error_line
 in match l1 with
 | [] -> remove_duplicates acc, error_line
 | y::ys -> 
    let acc2, error_line = loop y l2 acc error_line in
    find_sign ys l2 sign_fun signs acc2 current_line error_line

(********************************************)
(* FUNCTIONS FOR CONDITIONS*)

(*[add_signs var s_to_add signs_list signs] returns the updated signs environment
where the sign list for the variable var is s_to_add.
If the initial signs_list of the variable contained [Error], then it is added too. *)
let set_signs var s_to_add signs_list signs =
  let new_s = if List.mem Error signs_list then s_to_add@[Error] else s_to_add in
  Signs.add var new_s (Signs.remove var signs)

(*[remove_zero var s_to_remove signs_list signs] returns the signs environment
where the sign list for the variable var doesn't contains Zero. *)
  let remove_zero var s_to_remove signs_list signs =
  let s1 = if List.mem Zero signs_list then [] else [Zero] in
  let s2 = if List.mem Neg signs_list then s1 else Neg::s1 in
  let s3 = if List.mem Pos signs_list then s2 else Pos::s2 in
  let s_to_add = if List.mem Error signs_list then s3 else Error::s3 in
  set_signs var s_to_add signs_list signs

(* Compares the signs of a variable and a number, depending on the comparaison
operator. Returns 2 things :
- false if the condition cannot be satisfied, true otherwise
- the signs environment updated with the propagation of the condition *)
let rec compare_var_and_num var n comp signs =
  let signs_list = signs_var var signs in
  if signs_list = [Error] then false, signs (* impossible *)
  else if n = 0 then
    match comp with
    | Eq -> if List.mem Zero signs_list then true, set_signs var [Zero] signs_list signs 
            else false, signs
    | Ne -> if signs_list = [Zero] || signs_list = [Zero; Error] || signs_list = [Error; Zero] 
              then false, signs (* impossible *)
            else if List.mem Zero signs_list (* possible but cannot be Zero *)
              then true, remove_zero var [Zero] signs_list signs
            else true, signs (* true *)
    | Lt -> if List.mem Neg signs_list then true, set_signs var [Neg] signs_list signs (* ok, and the var is now Neg (or Error) only *)
            else false, signs (* impossible *)
    | Gt -> if List.mem Pos signs_list then true, set_signs var [Pos] signs_list signs (* ok, and the var is now Pos (or Error) only *)
            else false, signs (* impossible *)
    | Le -> if List.mem Zero signs_list && List.mem Neg signs_list then true, set_signs var [Neg; Zero] signs_list signs
            else if List.mem Zero signs_list then true, set_signs var [Zero] signs_list signs
            else if List.mem Neg signs_list then true, set_signs var [Neg] signs_list signs
            else false, set_signs var [Pos] signs_list signs
    | Ge -> if List.mem Zero signs_list && List.mem Pos signs_list then true, set_signs var [Zero; Pos] signs_list signs
            else if List.mem Zero signs_list then true, set_signs var [Zero] signs_list signs
            else if List.mem Pos signs_list then true, set_signs var [Pos] signs_list signs
            else false, set_signs var [Neg] signs_list signs
  else if n > 0 then
    match comp with
    | Eq | Gt | Ge -> compare_var_and_num var 0 Gt signs (* var > 0 *)
    | Ne | Lt | Le -> true, signs (* possible, no new info about signs *)
  else (* n < 0 *)
    match comp with
    | Eq | Lt | Le-> compare_var_and_num var 0 Lt signs
    | Ne | Gt | Ge -> true, signs (* no new info about signs *) 

(* Ths method compares 2 signs with the comparaison operator comp and
return false if the condition can never be satisfied, true otherwise
(this doesn't mean the condition can be satisfied, we just can't say
that it can't)*)
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

(* This compares the signs of the 2 expressions and returns :
- the updated signs environment (after the propagation of the condition)
- the error line number *)
let compare_2_expr e1 e2 comp signs current_line error_line =
  let signs_e1, error_line1 = find_signs_expr e1 signs current_line error_line in
  let signs_e2, error_line2 = find_signs_expr e2 signs current_line error_line in
  let error_line = if error_line1 = -1 then error_line2 else min error_line1 error_line2 in
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
  loop_e1 signs_e1 false [] [], error_line


(* This function finds the possible signs for the condition c,
and returns :
- false if the condition can never be satisfied, true otherwise
- the updated signs environment (with condition propagation)
- the error line number *)
let rec sign_cond c signs current_line (error_line:int) = 
  let e1, comp, e2 = c in
  match e1, e2 with
  | Num(a), Num(b) -> if a > b then true, signs, error_line else false, signs, error_line
  | Var(var), Num(n) -> 
      let b, signs = compare_var_and_num var n comp signs in b, signs, error_line
  | Num(n), Var(var) -> 
    let b, signs = compare_var_and_num var n (reverse_comp comp) signs in b, signs, error_line
  | Var(var), e2 -> 
    let (b, signs_var, signs_e), error_line = compare_2_expr e1 e2 comp signs current_line error_line in
    let signs = Signs.add var signs_var (Signs.remove var signs) in
    b, signs, error_line
  | e1, Var(var) -> sign_cond (Var(var), reverse_comp comp, e1) signs current_line error_line
  | e1, e2 ->
    let (_, _, _), error_line = compare_2_expr e1 e2 comp signs current_line error_line in 
    true, signs, error_line 
  (* The last line compares Op and Num, or 2 Op => it is very difficult to process
     (need to solve equations) and I don't have time to implement it. 
     Because I already simplified the program before launching the sign analysis, 
     the most basic cases are already caught in the other patterns. 
     So I decided to say the expression is possibly true, and to return the signs 
     with no changes. I do launch the comparaison of the two to make sure I catch
     any division by 0 errors in the expressions. *)

(**********************************************)
(* GENERAL FUNCTIONS *)

(* This function finds the signs of all the variables in the program/block p,
and returns :
- the signs environement
- the number of the first line to have a division by zero error 
(or -1 if there is none) *)
let rec signs_program p signs error_line =
  match p with
  | [] -> signs, error_line
  | (i, Print(e))::xs  -> signs_program xs signs error_line
  | (i, Read(n))::xs -> signs_program xs(Signs.add n [Neg; Zero; Pos] signs) error_line
  | (i, Set(n, e))::xs -> 
      let signs_list, error_line = find_signs_expr e signs i error_line in
      signs_program xs (Signs.add n signs_list signs) error_line
  | (i, If(c, b, b2))::xs -> 
      let signs, error_line = signs_if c b b2 signs i error_line in
      signs_program xs signs error_line
  | (i, While(c, b))::xs -> 
      let signs, error_line = signs_while c b signs i error_line in
      signs_program xs signs error_line
and signs_if c b b2 signs current_line error_line = 
  (* See if the condition can be satisfied and compute the signs propagation *)
  let res1, signs1, error_line = sign_cond c signs current_line error_line in
  let e1, comp, e2 = c in
  let res2, signs2, error_line = sign_cond (e1, reverse_comp comp, e2) signs current_line error_line in
  (* If the condition can be satisfied, compute the signs of the block *)
  let signs1, error_line1 = if res1 then signs_program b signs1 error_line else Signs.empty, error_line in 
  let signs2, error_line2 = if res2 then signs_program b2 signs2 error_line else Signs.empty, error_line in
  let error_line = if error_line1 = -1 then error_line2 else min error_line1 error_line2 in
  (* Join both environments *)
  Signs.merge (fun var x y -> 
                if Option.is_none x then y
                else if Option.is_none y then x
                else Some(remove_duplicates (Option.get x)@(Option.get y))) 
              signs1 signs2
  , error_line
and signs_while c b signs current_line error_line = 
  let e1, comp, e2 = c in
  (* loop_while computes the fixed-point for the signs*)
  let rec loop_while signs0 current_line error_line =
    let res, signs1, error_line1 = sign_cond c signs current_line error_line in 
    let signs1, error_line2 = signs_program b signs1 error_line in 
    let error_line = if error_line1 = -1 then error_line2 else min error_line1 error_line2 in
    if signs1 = signs0 then signs0
    else loop_while signs1 current_line error_line
  in 
  (* propagating the opposite condition into the fixed-point signs.
     This is because when we get out of the while loop, it means 
     we didn't satisfy the condition anymore. *)
  let res2, signs2, error_line = 
    sign_cond (e2, reverse_comp comp, e1) signs current_line error_line in 
  signs2, error_line

(* This functions lanches the sign analysis for the variables in p. 
Then it prints the result for each variable. 
On the last line, it will also print "safe" it there is no possibility to get an
error because of a division by 0; or "divbyzero N" where N is the number of the
first line that can potentially cause a division by zero. *)
  let print_signs p =
  let signs, error_line = signs_program p Signs.empty (-1) in
  let error_str = if error_line = -1 then "safe\n" else "divbyzero" 
                  ^ string_of_int error_line ^ "\n" in
  let rec print_list s_list =
    match s_list with
    | [] -> print_newline()
    | x::xs -> (* TODO : make sure the signs in the sign list come in the right order *)
      match x with
      | Neg -> print_string "-"; print_list xs
      | Pos -> print_string "+"; print_list xs
      | Zero -> print_string "0"; print_list xs
      | Error -> print_string "!"; print_list xs
  in
  Signs.iter (fun x signs_list -> 
              print_string x; print_string " : "; 
              print_list (remove_duplicates signs_list)) signs;
  print_string error_str
