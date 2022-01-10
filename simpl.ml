open Polish
open Eval

(*[simplify_expr e env] returns the expression simplified using
the values in the env map and basic arithmetic simplfications. *)
let rec simplify_expr e env = 
  match e with
  | Var(n) ->
    (try Num(eval_expr (Var(n)) env) with _ -> Var(n))
  | Op(op, e1, e2) -> 
    let e1 = simplify_expr e1 env in
    let e2 = simplify_expr e2 env in
    (match e1, e2 with
    | Num(a), Num(b) ->
      (try Num(eval_expr e env)
      with _ -> Op(op, Num(a), Num(b)))
    | Num(0), e2 | Num(1), e2 -> basic_simpl op e1 e2 true env
    | e1, Num(0) | e1, Num(1) -> basic_simpl op e2 e1 false env
    | e1, e2 -> Op(op, e1, e2))
  | e -> e
(*[basic_simpl op n e first env] :
  - op : the operator
  - n : the number to simplify with == Num(a) where a must be either 0 or 1
  - e : the other expression
  - first : if true, then the complete arithmetic expression that is being simplified
            is Op(op, Num(a), e)
            if false, it is Op(op, e, Num(a))
  - env : the current values for the variables that have been set with an int
This function simplifies the arithmetic expression (described in the line for first)
because one of its terms is either Num(0) or Num(1). 
It does all the basic simplfications :
- 0 + e or e + 0 or e - 0 = e
- 1 * e or e * 1 = e
- 0 / e or 0 * e or e * 0 or 0 % e = 0 
Otherwise (there is a problem : a is neither 0 nor 1), returns the expression 
Op(op, Num(a), e simplfied) *)
and basic_simpl op n e first env =
    match op, n with
    | Add, Num(0) | Mul, Num(1) -> simplify_expr e env
    | Mul, Num(0) -> Num(0)
    | Div, Num(1) | Mod, Num(1) | Sub, Num(0) ->
      if not first then simplify_expr e env
      else Op(op, n, e)
    | Div, Num(0) | Mod, Num(0) ->
      if first then Num(0)
      else Op(op, e, Num(0))
    | _ -> if first then Op(op, n, simplify_expr e env) 
          else Op(op, simplify_expr e env, n)
    
 
(* [simplify_cond (e1, comp, e2) env] returns the condition with both 
expressions simplified *)
let simplify_cond cond env =
  let e1, c, e2 = cond in
  let e1 = simplify_expr e1 env in
  let e2 = simplify_expr e2 env in
  e1, c, e2

(* [simplify_bloc p env] simplifies the program/block p with the current 
environment env.
It simplifies all the expressions, and removes the dead code :
- WHILE completely disappears if the condition cannot be satisfied
- IF is replaced by one branche if the condition is satisfied, or the 
  other if it is not *)
let rec simplify_block (p:program) env =
  match p with
  | [] -> []
  | (i, Print(e))::xs  -> (i, Print(simplify_expr e env))::(simplify_block xs env)
  | (i, Read(n))::xs -> (i, Read(n))::simplify_block xs (NameTable.remove n env)
  | (i, Set(n, e))::xs ->
      let e2 = simplify_expr e env in
      (i, Set(n, e2))::simplify_block xs
        (* try adding to the Nametable if the expr can be calculated *)
        (try NameTable.add n (eval_expr e2 env) env with _ -> env)
  | (i, If(c, b, b2))::xs -> (simplify_if c b b2 env i)@(simplify_block xs env)
  | (i, While(c, b))::xs -> (simplify_while c b env i)@(simplify_block xs env)
(*[simplify_if c b b2 env pos] tries to evaluate the condition c and :
- if it can't be evaluated returns If(c, b,b2) where c, b and b2 have been simplified
- if c is true, returns only the block b (simplified)
- if c is false, returns only the block b2 (simplified) *)
  and simplify_if c b b2 env (pos : int) =
  let e1, comp, e2 = simplify_cond c env in
  match e1, e2 with
  | Num(x), Num(y) -> if eval_cond (e1, comp, e2) env then simplify_block b env else simplify_block b2 env 
  | _, _ -> [(1, If((e1, comp, e2), (simplify_block b env), (simplify_block b2 env)))]
(*[simplify_while c b env pos] tries to evaluate the condition c and :
- if it can't be evaluated or is true, returns While(c, b) where b is simplified, but not c
- if it is false, returns an empty block *)
  and simplify_while c b env pos =
  let e1, comp, e2 = simplify_cond c env in
  match e1, e2 with
  | Num(x), Num(y) -> if not (eval_cond (e1, comp, e2) env) then [] else [(pos, While(c, (simplify_block b env)))]
  | _ -> [(pos, While(c, (simplify_block b env)))]
(* The condition is kept intact in the final program, 
   in case the variables in it are updated inside.
   Simplifying the condition only helps to decides if we should get rid 
   of the 'while' (in case we never enter the loop). *)