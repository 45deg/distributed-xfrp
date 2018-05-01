open Lexing

let filename = Sys.argv.(1)

let () =
  let input = open_in filename in
  let filebuf = from_channel input in
  try
    let main = Parser.prog_module Lexer.read filebuf in
    print_endline "==========================";
    Syntax.pp_module main;
    print_endline "==========================";
    print_endline (Env.string_of_env (Typing.type_module main));
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error";
  close_in input