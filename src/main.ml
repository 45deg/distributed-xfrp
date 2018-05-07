open Lexing

let filename = Sys.argv.(1)

let () =
  let input = open_in filename in
  let lexbuf = from_channel input in
  try
    let main = Parser.prog_module Lexer.read lexbuf in
    let ti = Typing.type_module main in
    print_endline (Codegen.of_xmodule main ti)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s." msg;
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax Error at Line %d, Char %d." pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
  close_in input 