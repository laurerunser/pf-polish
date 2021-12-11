(* Module Eval : this module evaluates a polish program. *)
open Polish
open Printf
open List

module NameTable = Map.Make(String)

(* [eval_expr e env]  evaluates the expression e with the current environment
variables in env and returns the result as an int.
Fails if it tries to access a variable that has ot been initialized yet, 
or if it tries to compute a division by 0. *)
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


(* [eval_cond c env] evaluates the condition c with the current environment
variables in env and returns the result as a boolean.
Fails if the expressions in the condition cannot be evaluated (see eval_expr) *)
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


(* [print_value_expr e env] evaluates the expression e with the current
environment variables in env and prints the result to stdout. Then also prints
a newline.
Fails if the expression cannot be evaluated (see eval_expr) *)
let print_value_expr e env =
  let v = eval_expr e env in
  print_int v;
  print_string "\n"



(* [read_value n env] :
- prints to stdout the name of the variable, a space, '?' and another space
- waits for user input
- if the user input is an int, adds it into the NameTable with the key n
and returns the updated NameTable
- if the user input is not an int, starts again until a valid input is entered *)
let rec read_value n env =
  printf "%s ? " n;
  try NameTable.add n (read_int()) env
  with Failure _ -> printf "Not an int ! Please rety \n"; read_value n env            


(* [eval_block p env] evaluates the program p with the current environment
variables in env. It returns the updated environment variables. *)                     
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
   
