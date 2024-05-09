open Parser
open Lexer
open Eval
open Print

let run s =
  eval @@ parse @@ lex s

let run_debug s =
  let t0 = 1000. *. Sys.time () in
  let lexed_input = lex s in
  let t1 = 1000. *. Sys.time () in
  let parsed_input = parse lexed_input in
  let t2 = 1000. *. Sys.time () in
  print_string "Evaluation:\n";
  let res = eval parsed_input in
  let t3 = 1000. *. Sys.time () in
  print_string "\nResult:\n";
  print_expression res;
  print_endline "\n\nTime:";
  Printf.printf "Lexing: %fms\nParsing: %fms\nEval: %fms\nTotal: %f ms\n"
  (t1-.t0) (t2-.t1) (t3-.t2) (t3-.t0)
