open Parser
open Lexer
open Eval
open Print

let read_file f =
  In_channel.with_open_bin f In_channel.input_all

let run s =
  eval @@ parse @@ lex s

let run_file f =
  run @@ read_file f

let () =
  init_lexer ();
  init_parser ();
  if Array.length Sys.argv >= 2 then
    if Sys.file_exists Sys.argv.(1) then
      let input = read_file Sys.argv.(1) in
      let parsed_input = parse @@ lex input in
      print_expression @@ eval parsed_input;
      print_newline ()
