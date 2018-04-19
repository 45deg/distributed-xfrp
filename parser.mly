%{
open Syntax
%}

%token
  MODULE IN OUT USE CONST NODE INIT FUN IF THEN ELSE
  LET CASE OF LAST TRUE FALSE

%token
  COMMA LBRACKET RBRACKET UNIT LPAREN RPAREN COLON
  SEMICOLON AT ARROW TILDE PLUS MINUS PERCENT ASTERISK
  SLASH XOR OR2 AND2 OR AND EQUAL2 NEQ LSHIFT LTE LT
  RSHIFT GTE GT BANG EQUAL

%token <char> CHAR
%token <string> ID
%token <float> FLOAT
%token <int> INT 
%token EOF

%start <unit> prog_module

%%

prog_module:
  | MODULE modulename = ID
    IN innodes = separated_list(COMMA, id_and_type)
    OUT outnodes = separated_list(COMMA, id_and_type)
    USE modules = separated_list(COMMA, ID)
    defs = nonempty_list(definition)
    EOF
    { {
      name = modulename;
      in_node = innodes;
      out_node = outnodes;
      use = modules
      definition = defs
    } }

definition:
  | CONST it = id_and_type_opt EQUAL e = expr
    { Const(it, e) }
  | NODE init = option(INIT LBRACKET e = expr RBRACKET { e }) it = id_and_type_opt EQUAL e = expr
    { Node(init,it,e) }
  | FUN id = ID LPAREN a = fargs RPAREN t = option(COLON type_spec { $2 }) EQUAL e = expr
    { Fun({
      name: id,
      args: a,
      type_spec: Option.default TNone t
      body: e 
    }) }

expr:
  | constant { $1 }

fargs:
  | separated_list(COMMA, id_and_type_opt) { $1 }

constant:
  | UNIT { CUnit }
  | TRUE { CBool(true) }
  | FALSE { CBool(false) }
  | c = CHAR { CChar(c) }
  | n = INT { CInt(n) }
  | x = FLOAT { CFloat(x) }

id_and_type:
  | i = ID COLON t = type_spec
    { (i, t) }

id_and_type_opt:
  | i = ID COLON t = type_spec
    { (i, t) }
  | i = ID
    { (i, TNone) }

type_spec:
  | t = prim_type_spec
    { t }
  | LPAREN tpl = separated_nonempty_list(COMMA, prim_type_spec) RPAREN
    { TTuple(tpl) }

prim_type_spec:
  | t = ID
    { match t with
      | "Unit"  -> TUnit
      | "Bool"  -> TBool
      | "Char"  -> TChar
      | "Int"   -> TInt
      | "Float" -> TFloat
      | _ -> TNone }
