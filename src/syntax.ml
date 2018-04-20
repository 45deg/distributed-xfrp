type id = string
[@@deriving show]
type moduleid = string
[@@deriving show]

type id_and_type = id * Type.t
[@@deriving show]

type const 
  = CUnit
  | CBool of bool
  | CChar of char
  | CInt of int
  | CFloat of float
[@@deriving show]

type annot
  = ALast
[@@deriving show]

type uniop
  = UNot | UNeg | UInv
[@@deriving show]

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
[@@deriving show]

type pattern
  = PWild
  | PConst of const
  | PVar of id
  | PTuple of pattern list
[@@deriving show]

type expr 
  = EConst of const
  | EId of id
  | EAnnot of id * annot
  | EApp of id * expr list
  | EBin of binop * expr * expr
  | EUni of uniop * expr
  | ETuple of expr list
  | EIf of expr * expr * expr
  | ELet of (id * expr * Type.t) list * expr
  | ECase of expr * (pattern * expr) list
[@@deriving show]

type fundef = {
  name: id;
  args: id_and_type list;
  t: Type.t;
  body: expr
}
[@@deriving show]

type definition
  = Const of id_and_type * expr
  | Node of id_and_type * expr option * expr
  | Fun of fundef
[@@deriving show]

type xmodule
  = {
    id: moduleid;
    in_node: id_and_type list;
    out_node: id_and_type list;
    use: moduleid list;
    definition: definition list
  }
[@@deriving show]