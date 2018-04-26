open Lexing

let filename = Sys.argv.(1)

let () =
  let input = open_in filename in
  let filebuf = from_channel input in
  try
    let main = Parser.prog_module Lexer.read filebuf in
    Syntax.pp_module main
    (*print_endline (Type.string_of_type (Typing.infer (Inter.gen_env main) 0 (Inter.to_let main)))*)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error";
  close_in input