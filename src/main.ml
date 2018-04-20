open Lexing

let filename = Sys.argv.(1)

let () =
  let input = open_in filename in
  let filebuf = from_channel input in
  try
    Printf.printf "%s\n" (Syntax.show_xmodule (Parser.prog_module Lexer.read filebuf))
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error";
  close_in input