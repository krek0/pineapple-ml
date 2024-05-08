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

let debug = ref false
let filename = ref ""
let parse_args () =
  let usage_msg = "A mini ml interpreter\n\n" ^ 
                  "Usage: pml [-debug] <file>\n\n" ^
                  "Arguments:" in
  let speclist = [("--debug", Arg.Set debug, "Output debug information")] in
  let get_filename name =
    filename := name in
  Arg.parse speclist get_filename usage_msg

let () =
  init_lexer ();
  init_parser ();

  (*arg*)
  parse_args ();
  if !filename = "" then
    (print_string "pml: missing file operand\n";
    print_string "Try `pml --help` for more information.\n")
  else
  (
  let input = read_file !filename in
  if !debug then
    let t0 = 1000. *. Sys.time () in
    let lexed_input = lex input in
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
  else
    ignore @@ eval @@ parse @@ lex input
  )
