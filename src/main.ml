open Run
open Lexer
open Parser

let read_file f =
  In_channel.with_open_bin f In_channel.input_all

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
  if !filename <> "" then
  (
    let input = read_file !filename in
    if !debug then
      run_debug input
    else
      ignore @@ run input
  )
  else
  (
    print_string "pml: missing file operand\n";
    print_string "Try `pml --help` for more information.\n"
  )
