open Lexing

exception CommandError of string
exception CompileError of string

let output_file = ref None
let input_file  = ref None
let template_file = ref None

let speclist = [
  ("-o", Arg.String(fun s -> output_file := Some(s)), "Write output to file.");
  ("-t", Arg.String(fun s -> template_file := Some(s)), "Template for I/O functions.")
]

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let compile in_c =
  let template = match !template_file with
    | Some(s) -> Some(Bytes.to_string (load_file s))
    | None -> None
  in
  let lexbuf = from_channel in_c in
  try
    let main = Parser.prog_module Lexer.read lexbuf in
    let ti = Typing.type_module main in
    Codegen.of_xmodule main ti template
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
    let result = compile input in
    match !output_file with
      | Some(file) -> 
        let oc = open_out file in
        output_string oc result;
        close_out oc;
      | None ->
        print_string result;
    close_in input
  with
  | CommandError msg ->
    Printf.eprintf "Command Error: %s" msg;
  | CompileError msg -> Printf.eprintf "%s" msg;