(* Module Eval : this module evaluates a polish program. *)
open Polish
open Printf
open List

module NameTable = Map.Make(String)

let eval_expr e env =
  let rec eval e = match e with
    | Num(n) -> n
    | Var(n) ->
       (try NameTable.find n env
       with Not_found -> ksprintf failwith "unknown variable %s" n)
    | Op(op, e1, e2) ->
       match op with
       | Add -> (eval e1) + (eval e2)
       | Sub -> (eval e1) - (eval e2)
       | Mul -> (eval e1) * (eval e2)
       | Div ->
          (try (eval e1) / (eval e2)
           with Division_by_zero -> failwith "division by zero")
       | Mod -> (eval e1) mod (eval e2)
  in eval e

let eval_cond c env =
  let e1, comp, e2 = c in
  let v1 = eval_expr e1 env in
  let v2 = eval_expr e2 env in
  match comp with
  | Eq -> v1 = v2
  | Ne -> v1 <> v2
  | Lt -> v1 < v2
  | Le -> v1 <= v2
  | Gt -> v1 > v2
  | Ge -> v1 >= v2

let print_value_expr e env =
  let v = eval_expr e env in
  print_int v;
  print_string "\n"

let rec read_value n env =
  printf "%s ? " n;
  try NameTable.add n (read_int()) env
  with Failure _ -> printf "Not an int ! Please rety \n"; read_value n env            

let rec eval_block (p:program) env =
  match p with
  | [] -> env
  | (i, Set(n, e))::xs -> eval_block xs (NameTable.add n (eval_expr e env) env)
  | (i, Print(e))::xs -> print_value_expr e env; eval_block xs env
  | (i, Read(n))::xs -> eval_block xs (read_value n env)
  | (i, If(c, b, b2))::xs -> eval_block xs (eval_if c b b2 env)
  | (i, While(c, b))::xs -> eval_block xs (eval_while c b env)
and eval_if c b b2 env =
  if eval_cond c env then eval_block b env
  else eval_block b2 env
and eval_while c b env =
  if eval_cond c env then
    let env2 = eval_block b env in
    eval_while c b env2
  else env
   
