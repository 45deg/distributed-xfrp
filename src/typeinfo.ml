module M = Map.Make (struct
                      type t = Syntax.id
                      let compare = compare
                    end)
type ti = Type.t M.t
let empty: ti = M.empty
let extend ti id ty = M.add id ty ti
let extend_all ti ids tys = List.fold_left2 extend ti ids tys 
let for_all = M.for_all
let find = M.find
let lookup ti id = 
  try Some(M.find id ti)
  with | Not_found -> None
let rec string_of_ti ti = 
  M.bindings ti 
  |> List.map (fun (a, b) -> "(" ^ a ^ ", " ^ Type.string_of_type b ^ ")")
  |> String.concat ","
  |> fun s -> "{" ^ s ^ "}"
