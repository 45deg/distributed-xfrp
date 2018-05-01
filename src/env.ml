module M = Map.Make (struct
                      type t = Syntax.id
                      let compare = compare
                    end)
type env = Type.t M.t
let empty: env = M.empty
let extend env id ty = M.add id ty env
let extend_all env ids tys = List.fold_left2 extend env ids tys 
let lookup env id = 
  try Some(M.find id env)
  with | Not_found -> None
let rec string_of_env env = 
  M.bindings env 
  |> List.map (fun (a, b) -> "(" ^ a ^ ", " ^ Type.string_of_type b ^ ")")
  |> String.concat ","
  |> fun s -> "{" ^ s ^ "}"