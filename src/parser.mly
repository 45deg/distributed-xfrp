%{
open Syntax
open Type

type definition
  = Const of const_t
  | Node of node_t
  | Fun of fundef_t

let collect_defs defs = 
  List.fold_right
  (fun d ({ const = c; func = f; node = n } as r) -> match d with
  | Const (it, e) -> { r with const = (it, e) :: c }
  | Fun (it, e) -> { r with func = (it, e) :: f }
  | Node (it, init, e) -> { r with node = (it, init, e) :: n })
  defs
  { const = []; func = []; node = [] }

%}

%token
  MODULE IN OUT USE CONST NODE INIT FUN IF THEN ELSE
  LET CASE OF LAST TRUE FALSE

%token
  COMMA LBRACKET RBRACKET LPAREN RPAREN COLON
  SEMICOLON AT ARROW TILDE PLUS MINUS PERCENT ASTERISK
  SLASH XOR OR2 AND2 OR AND EQUAL2 NEQ LSHIFT LTE LT
  RSHIFT GTE GT BANG EQUAL

%token <char> CHAR
%token <string> ID
%token <float> FLOAT
%token <int> INT 
%token EOF

%start <Syntax.xmodule> prog_module

%right prec_let
%right prec_if
%left OR2
%left AND2
%left OR
%left XOR
%left AND
%left EQUAL2 NEQ
%left LT LTE GT GTE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left ASTERISK SLASH PERCENT
%right prec_uni

%%

prog_module:
  | MODULE id = ID
    IN innodes = separated_list(COMMA, id_and_type)
    OUT outnodes = separated_list(COMMA, id_and_type)
    USE modules = separated_list(COMMA, ID)
    defs = nonempty_list(definition)
    EOF
    { {
      id = id;
      in_node = innodes;
      out_node = outnodes;
      use = modules;
      definition = collect_defs defs
    } }

definition:
  | CONST it = id_and_type_opt EQUAL e = expr
    { Const(it, e) }
  | NODE init = option(INIT LBRACKET e = expr RBRACKET { e }) it = id_and_type_opt EQUAL e = expr
    { Node(it, init, e) }
  | FUN id = ID LPAREN a = fargs RPAREN t_opt = option(COLON type_spec { $2 }) EQUAL e = expr
    { let (ai, at) = List.split a in Fun((id, (at, t_opt)), EFun(ai, e)) }

expr:
  | constant { EConst($1) }
  | ID       { EId($1) }
  | id = ID AT a = annotation { EAnnot(id, a) }
  | id = ID LPAREN args = args RPAREN { EApp(id, args) }
  | expr binop expr { EBin($2, $1, $3) }
  | uniop expr %prec prec_uni { EUni($1, $2) }
  | LPAREN xs = args RPAREN 
    { match xs with
        | []   -> EConst(CUnit)
        | [x]  -> x
        | _    -> ETuple(xs)
    }
  | IF c = expr THEN a = expr ELSE b = expr
    %prec prec_if
    { EIf(c, a, b) }
  | LET bs = separated_nonempty_list(SEMICOLON, binder) IN e = expr
    %prec prec_let
    { ELet(bs, e) }
  | CASE e = expr OF bs = nonempty_list(case_body)
    { ECase(e, bs) }

%inline
binop:
  | ASTERISK { BMul }
  | SLASH    { BDiv }
  | PERCENT  { BMod }
  | PLUS     { BAdd }
  | MINUS    { BSub }
  | LSHIFT   { BShL } 
  | RSHIFT   { BShR }
  | LT       { BLt }
  | LTE      { BLte }
  | GT       { BGt }
  | GTE      { BGte }
  | EQUAL2   { BEq } 
  | NEQ      { BNe }
  | AND      { BAnd }
  | XOR      { BXor }
  | OR       { BOr }
  | AND2     { BLAnd }
  | OR2      { BLOr }

%inline
uniop:
  | MINUS    { UNeg }
  | TILDE    { UInv }
  | BANG     { UNot }
 
args:
  | separated_list(COMMA, expr) { $1 }

fargs:
  | separated_list(COMMA, id_and_type_opt) { $1 }

annotation:
  | LAST { ALast }

constant:
  | TRUE { CBool(true) }
  | FALSE { CBool(false) }
  | CHAR { CChar($1) }
  | INT { CInt($1) }
  | FLOAT { CFloat($1) }

id_and_type:
  | i = ID COLON t = type_spec
    { (i, t) }

id_and_type_opt:
  | i = ID COLON t = type_spec
    { (i, Some t) }
  | i = ID
    { (i, None) }

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
      | _ -> assert false }

binder:
  | it = id_and_type_opt EQUAL e = expr
    { let (i,t) = it in (i,e,t) }

case_body:
  | p = pattern ARROW e = expr SEMICOLON
    { (p, e) }

pattern:
  | prim_pattern { $1 }
  | LPAREN ps = separated_list(COMMA, prim_pattern) RPAREN
    { match ps with
      | [] -> PConst(CUnit)
      | _  -> PTuple(ps)
    }

prim_pattern:
  | ID
    { match $1 with
      | "_" -> PWild
      | _   -> PVar($1)
    }
  | constant { PConst($1) }