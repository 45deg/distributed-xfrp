type t = 
    TUnit
  | TBool
  | TChar
  | TInt
  | TFloat
  | TTuple of t list
  | TFun of t list * t
  | TVar of tvar ref
and tvar =
  | TVGeneric of string
  | TVFree of string * int
  | TVBound of t
[@@deriving show]

let rec string_of_type = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TChar -> "Char"
  | TInt -> "Int"
  | TFloat -> "Float"
  | TTuple(ts) -> String.concat ", " (List.map string_of_type ts)
  | TFun(args, ret) -> "(" ^ String.concat ", " (List.map string_of_type args) ^ ") -> " ^ string_of_type ret
  | TVar {contents = TVBound ty}
    -> string_of_type ty
  | TVar {contents = TVGeneric id}
    -> id
  | TVar ({contents = TVFree(id, level)})
    -> id ^ "@" ^ string_of_int level

let counter = ref 0

let gen_var level = 
  counter := !counter + 1; TVar(ref (TVFree("?" ^ (string_of_int !counter), level)))
let gen_generic_var () = 
  counter := !counter + 1; TVar(ref (TVGeneric("?" ^ (string_of_int !counter))))
let rec gen_var_list level = function
  | 0 -> []
  | n -> gen_var level :: (gen_var_list level (n - 1))
