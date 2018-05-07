open Lexing

let filename = Sys.argv.(1)

let () =
  let input = open_in filename in
  let filebuf = from_channel input in
  try
    let main = Parser.prog_module Lexer.read filebuf in
    let ti = Typing.type_module main in
    print_endline (Codegen.of_xmodule main ti)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error";
  close_in input 