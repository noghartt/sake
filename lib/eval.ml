open Expr

exception RuntimeError of string

let definitions = ref []

let define x e = definitions := (x, e) :: !definitions

let error msg  = raise (RuntimeError msg)

let print_eval e = e |> string_of_expr |> print_endline

let lookup x env =
  try List.assoc x env
  with Not_found -> (
    try List.assoc x !definitions
    with Not_found -> error ("unbound variable: " ^ x)
  )

let rec eval env = function
  | Symbol s -> lookup s env
  | (Int _ | Builtin _ | True | False) as e -> e
  | Nil -> error "missing expression"
  | Pair (e1, e2) ->
    match eval env e1 with
    | Builtin (_, f) -> f (eval_args env e2)
    | e -> error ("not a function: " ^ string_of_expr e)

and expand_env args e env =
  match (args, e) with
  | [], Nil -> env
  | x :: xs, Pair (e1, e2) -> expand_env xs e2 ((x, e1) :: env)
  | _, _ -> error "wrong number of arguments"

and eval_args env = function
  | Nil -> Nil
  | Pair (e1, e2) -> Pair (eval env e1, eval_args env e2)
  | _ -> error "malformed arguments"

let arg1 = function
| Pair (e, Nil) -> e
| _ -> error "wrong number of arguments"

let arg2 = function
  | Pair (e, Pair (e1, Nil)) -> (e, e1)
  | _ -> error "wrong number of arguments"

let binary_operation e op acc =
  let rest = flat_int_pair e in
  if List.length (List.filter (fun i -> is_int i <> true) rest) = 0
  then List.fold_left op acc (List.map int_of_Int rest) |> fun i -> Int i
  else
    error "wrong type"
;;

define "+" (Builtin ("+", fun e -> binary_operation e ( + ) 0));;
define "-" (Builtin ("-", fun e -> binary_operation e ( - ) 0));;
define "*" (Builtin ("*", fun e -> binary_operation e ( * ) 1));;
