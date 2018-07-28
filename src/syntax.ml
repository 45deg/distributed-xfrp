type id = string
type moduleid = string

type host = Host of string | Localhost

type id_and_type = id * Type.t

type id_and_type_opt = id * Type.t option

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
  | BCons

type pattern
  = PWild
  | PNil
  | PConst of const
  | PVar of id
  | PCons of pattern * pattern
  | PTuple of pattern list

type expr 
  = EConst of const
  | EId of id
  | EAnnot of id * annot
  | EApp of id * expr list
  | EBin of binop * expr * expr
  | EUni of uniop * expr
  | ETuple of expr list
  | EList of expr list
  | EIf of expr * expr * expr
  | ELet of (id * expr * Type.t option) list * expr
  | ECase of expr * (pattern * expr) list
  | EFun of id list * expr

type definition
  = Const of id_and_type_opt * expr
  | Node of id_and_type_opt * expr option * expr
  | Fun of (id * (Type.t option list * Type.t option)) * expr

type program = {
  id: moduleid;
  in_node: id_and_type list;
  out_node: id_and_type list;
  use: moduleid list;
  definition: definition list;
  hostinfo: (host * id list) list;
}

exception InvalidId of string

let rec string_of_const = function
  | CUnit -> "()"
  | CBool b -> string_of_bool b
  | CInt i  -> string_of_int i
  | CFloat f  -> string_of_float f
  | CChar c -> String.make 1 c

let rec string_of_pat = function
  | PNil -> "[]"
  | PWild -> "_"
  | PConst c -> string_of_const c
  | PVar id -> id
  | PCons(hd, tl) -> string_of_pat hd ^ "::" ^ string_of_pat tl
  | PTuple t -> List.map string_of_pat t |> String.concat ","

let rec string_of_expr = function
  | EConst c -> string_of_const c
  | EId id -> id
  | EAnnot (id, ALast) -> id ^ "@last"
  | EApp (id, es) -> id ^ "(" ^ String.concat "," (List.map string_of_expr es) ^ ")"
  | EBin (op, e1, e2) -> string_of_expr e1 ^ " " ^ (match op with
      | BMul -> "*"  | BDiv -> "/"   | BMod -> "%"
      | BAdd -> "+"  | BSub -> "-"   | BShL -> ">>"
      | BShR -> "<<" | BLt -> "<"    | BLte -> "<="
      | BGt -> ">"   | BGte -> ">="  | BEq -> "=="
      | BNe -> "!="  | BAnd -> "&"   | BXor -> "^"
      | BOr -> "|"   | BLAnd -> "&&" | BLOr -> "||" 
      | BCons -> "::") ^ " " ^ string_of_expr e2
  | EUni (op, e) -> (match op with 
    | UNot -> "!" 
    | UNeg -> "-" 
    | UInv -> "~") ^ string_of_expr e
  | ELet (binders, e) ->
    "let " ^ String.concat ", "
              (List.map (fun (i,e,_) -> i ^ " = " ^ string_of_expr e) binders)
    ^ " in " ^ string_of_expr e
  | EIf(c, a, b) ->
    "if " ^ string_of_expr c ^ " then " ^ string_of_expr a ^ " else " ^ string_of_expr b
  | EList es ->
    "[" ^ String.concat "," (List.map string_of_expr es) ^ "]"
  | ETuple es ->
    "(" ^ String.concat "," (List.map string_of_expr es) ^ ")"
  | EFun (args, e) ->
    "fun (" ^ String.concat ", " args ^ ") -> " ^ string_of_expr e
  | ECase(m, cls) -> 
    let f (p, e) = string_of_pat p ^ " -> "^ string_of_expr e ^ "; " in
    "case " ^ string_of_expr m ^ " of " ^ String.concat "" (List.map f cls)

let string_of_host = function
  | Host(h) -> h
  | Localhost -> "localhost"

let string_of_definition defs = 
  let open Type in
  let str_ty = function | Some (t) -> string_of_type t
                        | None -> "?" in
  let str_def = function
    | Const((i,t),e) -> Printf.sprintf "const %s : %s = %s" i (str_ty t) (string_of_expr e)
    | Fun((i,(at,rt)),EFun(ai, e)) ->
      Printf.sprintf "function %s(%s): %s = %s" i (List.map2 (fun i t -> i ^ ":" ^ str_ty t) ai at |> String.concat ",") 
                                                (str_ty rt) (string_of_expr e)
    | Fun(_,_) -> assert false
    | Node((i,t), init, e) -> Printf.sprintf "node %s = %s" i (string_of_expr e) in
  String.concat "\n" (List.map str_def defs)