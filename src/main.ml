open Lexing

exception CommandError of string
exception CompileError of string

type mode =
  Erlang | Dot

let output_file = ref None
let input_file  = ref None
let template_file = ref None
let debug_mode = ref false

let opt = let open Codegen in ref default_config

let mode = ref Erlang

let speclist = [
  ("-o", Arg.String(fun s -> output_file := Some(s)), " [file] Write output to file.");
  ("-t", Arg.String(fun s -> template_file := Some(s)), " [file] Template for I/O functions.");
  ("-dot", Arg.Unit(fun _ -> mode := Dot), "Output the dependency graph.");
  ("-debug", Arg.Unit(fun _ -> opt := { !opt with debug = true }), "Output function trace (experimental)");
  ("-mess", Arg.Int(fun n -> opt := { !opt with mess = Some(n) }), " [N] Let sending messages delayed randomly up to N ms (experimental)");
  ("-drop", Arg.Float(fun n -> opt := { !opt with drop = Some(n) }), " [P (0~1)] Let messages dropped with the probability of P (experimental)");
  ("-driver", Arg.String(function 
    | "simple" -> opt := { !opt with driver = Codegen.Simple }
    | "actor"  -> opt := { !opt with driver = Codegen.Actor }
    | _        -> opt := { !opt with driver = Codegen.default_config.driver }
  ), "[simple|actor] Change I/O behavior. ")
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
    let program = Parser.prog_module Lexer.read lexbuf in
    let xmod = Module.of_program program in
    match !mode with
      | Erlang ->
        let ti = Typing.type_module xmod in
        Codegen.of_xmodule xmod ti template !opt
      | Dot -> 
        Graphviz.of_xmodule xmod
  with 
  | Lexer.Error msg ->
    raise (CompileError("Lexing error: " ^ msg))
  | Syntax.InvalidId(id) ->
    let pos = lexbuf.lex_curr_p in
    raise (CompileError(Printf.sprintf "Id \"%s\" is reserved at Line %d, Char %d." id pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))
  | Codegen.InfiniteLoop(loops) ->
    raise (CompileError("Loop detected: " ^
      String.concat ", " (List.map (fun loop -> 
        String.concat " -> " (loop @ [List.hd loop])
      ) loops) 
    ))
  | Codegen.UnknownId(id) ->
    raise (CompileError("Not found id \"" ^ id ^ "\""))
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    raise (CompileError(Printf.sprintf "Syntax error at Line %d, Char %d." pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))
  | Typing.TypeError s ->
    raise (CompileError("Type error: " ^ s))
  | Dependency.InvalidAtLast ss ->
    raise (CompileError("Invalid usage of @last: \n" ^ String.concat "\n" (List.map (fun s -> "\t" ^ s) ss)  ))

let () =
  Arg.parse speclist (fun s -> input_file := Some(s)) "Usage:";
  if !debug_mode then Printexc.record_backtrace true;
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
    Printf.eprintf "Command Error: %s\n" msg;
  | CompileError msg -> Printf.eprintf "%s\n" msg;