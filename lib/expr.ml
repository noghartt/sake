type expr =
  | Symbol of string
  | Int of int

  | Pair of (expr * expr)
  | Builtin  of string * (expr -> expr)

  | True
  | False
  | Nil

let rec flat_int_pair pair =
  match pair with
  | Pair (Int i, Nil) -> [ Int i ]
  | Pair (Int i, Pair (i', r)) ->  Int i :: flat_int_pair (Pair (i', r))
  | _ -> failwith "error to parse pair"

let is_int e = match e with Int _ -> true | _ -> false

let int_of_Int = function Int i -> i | _ -> failwith "not Int"

let rec string_of_expr =
  function
  | Builtin (b, _) -> "#<primitive-procedure " ^ b ^ ">"
  | Symbol s -> s
  | Int i -> string_of_int i
  | True -> "#t"
  | False -> "#f"
  | Nil -> "()"
  | Pair (a, b) -> "(" ^ string_of_pair a b ^ ")"

and string_of_pair e =
  function
  | Nil -> string_of_expr e
  | Pair (a, b) -> string_of_expr e ^ " " ^ (string_of_pair a b)
  | b -> string_of_expr e ^ " " ^ string_of_expr b
