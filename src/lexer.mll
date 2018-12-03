{
open Parser
open Lexing
exception Error of string

let lexicon : (string, token) Hashtbl.t = Hashtbl.create 17

let () =
  List.iter (fun (key, builder) -> Hashtbl.add lexicon key builder)
  [
    "module", MODULE;
    "in", IN;
    "out", OUT;
    "use", USE;
    "const", CONST;
    "node", NODE;
    "init", INIT;
    "fun", FUN;
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "let", LET;
    "case", CASE;
    "of", OF;
    "last", LAST;
    "True", TRUE;
    "False", FALSE;
    "unify", UNIFY; 
    "async", ASYNC;
  ]

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let unescape = function
    'n' -> '\n'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 't' -> '\t'
  | c   -> c

}

let space = [' ' '\r' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let digits  = ['0'-'9']+
let fdigits = (['0'-'9']+ '.' ['0'-'9']* | '.' ['0'-'9']+) (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule read = parse
  | space            { read lexbuf }
  | newline          { next_line lexbuf; read lexbuf }
  | '#'              { read_comment lexbuf; read lexbuf }
  | ','              { COMMA }
  | '['              { LBRACKET }
  | ']'              { RBRACKET }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | ':'              { COLON }
  | "::"             { COLON2 }
  | ';'              { SEMICOLON }
  | '@'              { AT }
  | "->"             { ARROW }
  | '~'              { TILDE }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '%'              { PERCENT }
  | '*'              { ASTERISK }
  | '/'              { SLASH }
  | '^'              { XOR }     
  | "||"             { OR2 } 
  | "&&"             { AND2 } 
  | '|'              { OR }
  | '&'              { AND }
  | "=="             { EQUAL2 }
  | "!="             { NEQ }
  | "<<"             { LSHIFT }
  | "<="             { LTE }
  | '<'              { LT }
  | ">>"             { RSHIFT }
  | ">="             { GTE }
  | '>'              { GT }
  | '!'              { BANG }
  | '='              { EQUAL }
  | '\'' ([^ '\\' '\r' '\n'] as c) '\''
    { CHAR(c) }
  | '\'' '\\' (['b' 'f' 'n' 'r' 't'] as c) '\''
    { CHAR (unescape c) }
  | '{' ([^ '{' '}']+ as h) '}'
    { HOST h }
  | id+
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find lexicon s
          with Not_found ->
            ID s }
  | fdigits          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | digits           { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof              { EOF }
  | _                { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and read_comment = parse
  | '\n'             { new_line lexbuf }
  | eof              { () }
  | _                { read_comment lexbuf }