open Lexing

exception CommandError of string
exception CompileError of string

let output_file = ref None
let input_file  = ref None

let speclist = [
  ("-o", Arg.String(fun s -> output_file := Some(s)), "Write output to file.");
]

let parse in_c =
  let lexbuf = from_channel in_c in
  try
    let main = Parser.prog_module Lexer.read lexbuf in
    let ti = Typing.type_module main in
    (main, ti)
  with 
  | Lexer.Error msg ->
    raise (CompileError("Lexing rrror: " ^ msg))
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    raise (CompileError(Printf.sprintf "Syntax error at Line %d, Char %d." pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))
  | Typing.TypeError s ->
    raise (CompileError("Type error: " ^ s))

let () =
  Arg.parse speclist (fun s -> input_file := Some(s)) "Usage:";
  try
    let input = open_in (match !input_file with 
      | Some s -> s
      | None -> raise (CommandError("Specify an input file."))) in
    let (main, ti) = parse input in
    match !output_file with
      | Some(file) -> 
        let oc = open_out file in
        output_string oc (Codegen.of_xmodule main ti);
        close_out oc;
      | None ->
        print_string (Codegen.of_xmodule main ti);
    close_in input
  with
  | CommandError msg ->
    Printf.eprintf "Command Error: %s." msg;
  | CompileError msg -> Printf.eprintf "%s" msg;