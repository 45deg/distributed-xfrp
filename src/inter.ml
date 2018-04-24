open Syntax
open Env

let rec collect ((cs, fs, ns) as triple) = function
  | [] -> triple
  | Const ((i, t), e) :: xs -> 
    collect ((i, e, t) :: cs, fs, ns) xs
  | Fun ((i, (arg_t, ret_t)), e) :: xs -> 
    (*let fn_t = Type.TFun(arg_t, ret_t) in*)
    collect (cs, ((i, e, None)) :: fs, ns) xs
  | Node ((i, t), init, e) :: xs ->
    collect (cs, fs, (i, e, t) :: ns) xs

let to_let { definition = defs; out_node = os; _ } = 
  (* divides definitions into groups *)
  let (cs, fs, ns) = collect ([], [], []) defs in
  ELet(cs, ELet(fs, ELet(ns, ETuple(List.map (fun (i, _) -> EId i) os))))
  

let gen_env = function
  | { in_node = i; out_node = o; _ } ->
      let io = i @ o in
      List.fold_left (fun m (i, t) -> Env.extend m i t) Env.empty io