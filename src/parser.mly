%{
open Syntax
open Type

module S = Set.Make(String);;
module HM = Map.Make(struct type t = host let compare = compare end);;

type innodes = 
  | InNode of id_and_type
  | InUnify of id list

let in_split ls =
  let f (xs, ys) = function
  | InNode(idt) -> (idt :: xs, ys)
  | InUnify(ids) -> (xs, ids :: ys) in 
  let (ls, rs) = List.fold_left f ([],[]) ls in
  (List.rev ls, List.rev rs)

let reserved_word = S.of_list [
  "in";
  "out"
]

let hostinfo = ref HM.empty
let last_host = ref Localhost

let update id host = 
  last_host := host;
  hostinfo := HM.update host (function 
  | Some(l) -> Some(id :: l)
  | None    -> Some([id])) !hostinfo

%}

%token
  MODULE IN OUT USE CONST NODE INIT FUN IF THEN ELSE
  LET CASE OF LAST TRUE FALSE UNIFY EXTERN

%token
  COMMA LBRACKET RBRACKET LPAREN RPAREN COLON COLON2
  SEMICOLON AT ARROW TILDE PLUS MINUS PERCENT ASTERISK
  SLASH XOR OR2 AND2 OR AND EQUAL2 NEQ LSHIFT LTE LT
  RSHIFT GTE GT BANG EQUAL ASYNC

%token <char> CHAR
%token <string> ID
%token <string> HOST
%token <float> FLOAT
%token <int> INT 
%token EOF

%start <Syntax.program> prog_module

%right prec_let
%right prec_if
%left OR2
%left AND2
%left OR
%left XOR
%left AND
%left EQUAL2 NEQ
%left LT LTE GT GTE
%right COLON2
%left LSHIFT RSHIFT
%left PLUS MINUS
%left ASTERISK SLASH PERCENT
%right prec_uni

%%

prog_module:
  | MODULE id = ID
    IN innodes = separated_list(COMMA, in_nodes)
    OUT outnodes = separated_list(COMMA, id_and_type)
    USE modules = separated_list(COMMA, ID)
    defs = nonempty_list(definition)
    EOF
    { 
    let (inode, iunis) = in_split innodes in   
    {
      id = id;
      in_node = inode;
      in_unify = iunis;
      out_node = outnodes;
      use = modules;
      definition = defs;
      hostinfo = HM.bindings !hostinfo;
    } }

in_nodes:
  | HOST id_and_type 
    { update (fst $2) (Host $1); InNode($2) }
  | id_and_type 
    { update (fst $1) !last_host; InNode($1) }
  | UNIFY LPAREN ids = separated_nonempty_list(COMMA, ID) RPAREN
    { InUnify(ids) } 

definition:
  | CONST it = id_and_type_opt EQUAL e = expr
    { Const(it, e) }
  | host = HOST async = option(ASYNC) NODE init = option(INIT LBRACKET e = expr RBRACKET { e }) it = id_and_type_opt EQUAL e = expr
    { update (fst it) (Host host); Node(it, init, e, match async with | None -> false | _ -> true) }
  | async = option(ASYNC) NODE init = option(INIT LBRACKET e = expr RBRACKET { e }) it = id_and_type_opt EQUAL e = expr
    { update (fst it) !last_host; Node(it, init, e, match async with | None -> false | _ -> true) }
  | FUN id = ID LPAREN a = fargs RPAREN t_opt = option(COLON type_spec { $2 }) EQUAL e = expr
    { let (ai, at) = List.split a in Fun((id, (at, t_opt)), EFun(ai, e)) }
  | EXTERN FUN id = ID LPAREN arg_t = separated_list(COMMA, type_spec) RPAREN COLON ret_t = type_spec
    { Extern(id, (arg_t, ret_t)) }

expr:
  | constant { EConst($1) }
  | ID       { if S.mem $1 reserved_word then (raise (InvalidId($1))) else EId($1) }
  | id = ID AT a = annotation { EAnnot(id, a) }
  | id = ID LPAREN args = args RPAREN { EApp(id, args) }
  | expr binop expr { EBin($2, $1, $3) }
  | uniop expr %prec prec_uni { EUni($1, $2) }
  | LBRACKET args = args RBRACKET { EList(args) }
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
  | COLON2   { BCons }
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
  | LPAREN tpl = separated_nonempty_list(COMMA, type_spec) RPAREN
    { TTuple(tpl) }
  | LBRACKET t = type_spec RBRACKET
    { TList(t) }

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
  | hd = pattern COLON2 tl = pattern { PCons(hd, tl) }
  | LPAREN ps = separated_list(COMMA, pattern) RPAREN
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
  | LBRACKET RBRACKET { PNil }