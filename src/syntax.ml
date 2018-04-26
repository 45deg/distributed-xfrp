type id = string
type moduleid = string

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
  | ELet of (id * expr * Type.t option) list * expr
  | ECase of expr * (pattern * expr) list
  | EFun of id list * expr

type definition
  = Const of id_and_type_opt * expr
  | Node of id_and_type_opt * expr option * expr
  | Fun of (id * (Type.t option list * Type.t option)) * expr
and def_record = {
  const: definition list;
  func: definition list;
  node: definition list
}

type xmodule = {
  id: moduleid;
  in_node: id_and_type list;
  out_node: id_and_type list;
  use: moduleid list;
  definition: def_record;
}

let rec string_of_expr = function
  | EConst CUnit -> "()"
  | EConst (CBool b) -> string_of_bool b
  | EConst (CInt i)  -> string_of_int i
  | EConst (CFloat f)  -> string_of_float f
  | EConst (CChar c) -> String.make 1 c
  | EId id -> id
  | EAnnot (id, ALast) -> id ^ "@last"
  | EApp (id, es) -> id ^ "(" ^ String.concat "," (List.map string_of_expr es) ^ ")"
  | EBin (op, e1, e2) -> string_of_expr e1 ^ " " ^ (match op with
      | BMul -> "*"  | BDiv -> "/"   | BMod -> "%"
      | BAdd -> "+"  | BSub -> "-"   | BShL -> ">>"
      | BShR -> "<<" | BLt -> "<"    | BLte -> "<="
      | BGt -> ">"   | BGte -> ">="  | BEq -> "=="
      | BNe -> "!="  | BAnd -> "&"   | BXor -> "^"
      | BOr -> "|"   | BLAnd -> "&&" | BLOr -> "||" ) ^ " " ^ string_of_expr e2
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
  | ETuple es ->
    "(" ^ String.concat "," (List.map string_of_expr es) ^ ")"
  | EFun (args, e) ->
    "fun (" ^ String.concat ", " args ^ ") -> " ^ string_of_expr e
  | ECase(e, list) -> "[NOT IMPLEMENTED]"

[@@@ocaml.warning "-8"]
let string_of_definition { const = c; func = f; node = n } = 
  let open Type in
  let str_ty = function | Some (t) -> string_of_type t
                        | None -> "?" in
  let cs (Const((i,t),e)) = Printf.sprintf "const %s : %s = %s" i (str_ty t) (string_of_expr e) in
  let fs (Fun((i,(at,rt)),EFun(ai, e))) = 
    Printf.sprintf "function %s(%s): %s = %s" i (List.map2 (fun i t -> i ^ ":" ^ str_ty t) ai at |> String.concat ",") 
                                                (str_ty rt) (string_of_expr e) in
  let ns (Node((i,t), init, e)) =
    Printf.sprintf "node %s = %s" i (string_of_expr e) in
  String.concat "\n" ((List.map cs c) @ (List.map fs f) @ (List.map ns n))

let pp_module = function
  | {
    id = id;
    in_node = ins;
    out_node = outs;
    use = mods;
    definition = defs;
  } -> 
    Printf.printf "MODULE: %s\nIN: %s\nOUT: %s\nUSE: %s\nDEFS:\n%s\n"
      id
      (List.map fst ins |> String.concat ",")
      (List.map fst outs |> String.concat ",")
      (String.concat "," mods)
      (string_of_definition defs)