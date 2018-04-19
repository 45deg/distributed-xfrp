type id = string
type moduleid = string

type id_and_type = id * Type.t
type fundef = {
  name: id;
  args: id_and_type list;
  t: Type.t;
  body: expr
}

type definition
  = Const of id_and_type * expr
  | Node of id_and_type * expr option * expr
  | Fun of fundef

type const 
  = CUnit
  | CBool of bool
  | CChar of char
  | CInt of int
  | CFloat of float

type annot
  = ALast

type uniop
  = UNot | UNeg | UInv

type binop 
  = BMul | BDiv | BMod
  | BAdd | BSub
  | BShL | BShR
  | BLt | BLte | BGt | BGte
  | BEq | BNe
  | BAnd
  | BXor
  | BOr
  | BLAnd
  | BLOr

type pattern
  = PWild
  | PConst of const
  | PVar of id
  | PTuple of pattern list

type expr 
  = EConst of const
  | EId of id
  | EAnnot of id * annot
  | EApp of id * expr list
  | EBin of binop * expr * expr
  | EUni of uniop * expr
  | ETuple of expr list
  | EIf of expr * expr * expr
  | ELet of (id * expr * Type.t) list * (Expr a)
  | ECase of (pattern * expr) list

type xmodule
  = {
    id: moduleid;
    in_node: id_and_type list;
    out_node: id_and_type list;
    use: moduleid list;
    definition: definition list
  }