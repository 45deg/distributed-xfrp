type t = 
    TNone
  | TUnit
  | TBool
  | TChar
  | TInt
  | TFloat
  | TTuple of t list
  | TFun of t list * t
  | TVar of t option ref
