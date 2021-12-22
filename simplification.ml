open Polish
open Eval

let rec simplify_expr e env = 
  match e with
  | Op(op, Num(a), Num(b)) -> Num(eval_expr e env)
  | Op(op, e1, e2) -> 
    if e1 = Num(0) || e1 = Num(1) then
      basic_simpl op e1 e2 true env
    else if e2 = Num(0) || e2 = Num(1) then 
      basic_simpl op e2 e1 false env
    else e
  | e -> e
and basic_simpl op n e first env =
    match op, n with
    | Add, Num(0) | Sub, Num(0) | Mul, Num(1) -> simplify_expr e env
    | Div, Num(1) | Mod, Num(1) ->
      if not first then simplify_expr e env
      else Op(op, n, e)
    | Div, Num(0) | Mod, Num(0) ->
      if first then Num(0)
      else Op(op, e, Num(0))
    | _ -> failwith "There was a problem"

let simplify_cond cond env =
  let e1, c, e2 = cond in
  let e1 = simplify_expr e1 env in
  let e2 = simplify_expr e2 env in
  e1, c, e2

let rec simplify_block (p:program) env =
  match p with
  | [] -> []
  | (i, Print(e))::xs  -> (i, Print(e))::(simplify_block xs env)
  | (i, Read(n))::xs -> (i, Read(n))::simplify_block xs env
  | (i, Set(n, e))::xs -> (i, Set(n, e))::simplify_block xs (NameTable.add n (eval_expr e env) env)
  | (i, If(c, b, b2))::xs -> (simplify_if c b b2 env i)@(simplify_block xs env)
  | (i, While(c, b))::xs -> (simplify_while c b env i)@(simplify_block xs env)
and simplify_if c b b2 env (pos : int) =
  let e1, c, e2 = simplify_cond c env in
  match e1, e2 with
  | Num(x), Num(y) -> if eval_cond (e1, c, e2) env then simplify_block b env else simplify_block b2 env 
  | _, _ -> [(1, If((e1, c, e2), (simplify_block b env), (simplify_block b2 env)))]
and simplify_while c b env pos =
  let e1, c, e2 = simplify_cond c env in
  match e1, e2 with
  | Num(x), Num(y) -> if not (eval_cond (e1, c, e2) env) then [] else [(pos, While((e1, c, e2), (simplify_block b env)))]
  | _ -> [(pos, While((e1, c, e2), (simplify_block b env)))]