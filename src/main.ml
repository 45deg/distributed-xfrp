open Lexing

let filename = Sys.argv.(1)

let main () =
  let input = open_in filename in
  let filebuf = from_channel input in
  try
    (Parser.prog_module Lexer.read filebuf); 
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error"
  ;
  close_in input

let _ = main ()