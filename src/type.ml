type t = 
    TUnit
  | TBool
  | TChar
  | TInt
  | TFloat
  | TTuple of t list
  | TList of t
  | TFun of t list * t
  | TVar of tvar ref
and tvar =
  | TVGeneric of int
  | TVFree of int * int
  | TVBound of t
[@@deriving show]

let rec type_id i = 
  if i < 26 then 
    String.make 1 (Char.chr (i + 97)) 
  else
    type_id (i / 26) ^ type_id (i mod 26)

let string_of_type ty = 
  let tbl = Hashtbl.create 17 in
  let counter = ref 0 in
  let var_s id = 
    try type_id (Hashtbl.find tbl id) with
    | Not_found -> 
      let i = !counter in 
        Hashtbl.add tbl id i; 
        incr counter; type_id i
  in
  let rec f = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TChar -> "Char"
  | TInt -> "Int"
  | TFloat -> "Float"
  | TList(t) -> "[" ^ f t ^ "]"
  | TTuple(ts) -> "(" ^ String.concat ", " (List.map f ts) ^ ")"
  | TFun(args, ret) -> "(" ^ String.concat ", " (List.map f args) ^ ") -> " ^ f ret
  | TVar {contents = TVBound ty}
    -> f ty
  | TVar {contents = TVGeneric id}
    -> "'" ^ var_s id
  | TVar ({contents = TVFree(id, level)})
    -> "'" ^ var_s id ^ "@" ^ string_of_int level
  in f ty

let counter = ref 0

let gen_var level = 
  counter := !counter + 1; TVar(ref (TVFree(!counter, level)))
let gen_generic_var () = 
  counter := !counter + 1; TVar(ref (TVGeneric(!counter)))
let rec gen_var_list level = function
  | 0 -> []
  | n -> gen_var level :: (gen_var_list level (n - 1))
