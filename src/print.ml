open Type
open Lexer

let rec exp_to_str = function
  (*Specific cases*)
  | Let (s,App(Op "opfix",Fun(_,e1)),e2) ->
                   "let rec " ^ s ^ " = " ^ exp_to_str e1
                   ^ " in " ^ exp_to_str e2
  | App (Op "opif",Pair(e1,Pair(Fun(_,e2),Fun(_,e3)))) ->
                   "if " ^ exp_to_str e1 ^ " then " ^ exp_to_str e2
                   ^ " else " ^ exp_to_str e3
  | App (Op s,e) when List.mem s infix_operators ->
                   s ^ " " ^ exp_to_str e
  | App (Op s,Pair(e1,e2)) -> "(" ^ exp_to_str e1 ^ " " ^ s
                              ^ " " ^ exp_to_str e2 ^ ")"
  (*General cases*)
  | Var s -> s
  | Number n -> string_of_int n
  | Op s -> s
  | Fun (s,e') -> "fun " ^ s ^ " -> " ^ exp_to_str e'
  | App (e1,e2) -> "(" ^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^ ")"
  | Pair (e1,e2) -> "(" ^ exp_to_str e1 ^ ","
                    ^ exp_to_str e2 ^ ")"
  | Let (s,e1,e2) -> "let " ^ s ^ " = " ^ exp_to_str e1
                     ^ " in " ^ exp_to_str e2
  | True -> "true"
  | False -> "false"

let print_expression e =
  print_string @@ exp_to_str e
