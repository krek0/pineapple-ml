open Parser
open Lexer
open Eval

let read_file f =
  In_channel.with_open_bin f In_channel.input_all

let run s =
  eval @@ evalue @@ make_tree @@ analyse s
  
let run_file f =
  run @@ read_file f

let () =
  init_lexer ();
  init_parser ()
