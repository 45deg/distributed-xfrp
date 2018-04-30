open Type
open Syntax

exception TypeError of string

let rec adjust_level id level = function
  | TVar {contents = TVBound ty}
    -> adjust_level id level ty
  | TVar {contents = TVGeneric _}
    -> raise (TypeError("generic"))
  | TVar ({contents = TVFree(other_id, other_level)} as other_ty)
    -> if other_id == id then
         raise (TypeError("recursive type"))
        else
          if other_level > level then
            other_ty := TVFree(other_id, level)
          else
            ()
  | TFun(args, ret)
    -> List.iter (adjust_level id level) args;
       adjust_level id level ret
  | TTuple(ts)
    -> List.iter (adjust_level id level) ts
  | _ -> ()

let rec unify t1 t2 = 
  let rec unify t1 t2 =
  if t1 == t2 then ()
  else match t1, t2 with
    | TUnit, TUnit
    | TBool, TBool
    | TChar, TChar
    | TInt, TUnit
    | TFloat, TFloat -> ()
    | TTuple(ts1), TTuple(ts2)
      -> List.iter2 unify ts1 ts2
    | TFun(a1, r1), TFun(a2, r2)
      -> List.iter2 unify a1 a2;
         unify r1 r2
    | TVar {contents = TVBound t1}, t2
    | t1, TVar {contents = TVBound t2}
      -> unify t1 t2
    | TVar ({contents = TVFree(id, level)} as tv), t
    | t, TVar ({contents = TVFree(id, level)} as tv) ->
        adjust_level id level t;
        tv := TVBound(t)
    | _, _ -> raise (TypeError("unification failed with " ^ string_of_type t1 ^ " and " ^ string_of_type t2))
  in 
    print_string ("\tUNIFY (" ^ string_of_type t1 ^ "," ^ string_of_type t2 ^ ")");
    unify t1 t2; 
    print_endline (" +-> (" ^ string_of_type t1 ^ "," ^ string_of_type t2 ^ ")")

let rec generalize level = function
  | TFun(args, ret) ->
    TFun(List.map (generalize level) args, generalize level ret)
  | TTuple(ts) ->
    TTuple(List.map (generalize level) ts)
  | TVar {contents = TVFree(id, other_level)} as t 
    -> if other_level > level
        then TVar (ref (TVGeneric id))
        else t
  | TVar {contents = TVBound ty}
    -> generalize level ty
  | _ as t -> t

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty = match ty with
    | TVar {contents = TVBound ty} -> f ty
    | TVar {contents = TVGeneric id} -> begin
        try
          Hashtbl.find id_var_map id
        with Not_found ->
          let fresh_var = gen_var level in
          Hashtbl.add id_var_map id fresh_var;
          fresh_var
      end
    | TVar {contents = TVFree _} -> ty
    | TFun(args, ret) ->
        TFun(List.map f args, f ret)
    | TTuple(xs) ->
        TTuple(List.map f xs)
    | _ -> ty
  in
  f ty

let rec match_fun n = function
  | TFun(args, ret) ->
      if List.length args <> n then
        raise (TypeError ("unexpected number of arguments"))
      else
        args, ret
  | TVar {contents = TVBound ty} -> match_fun n ty
  | TVar ({contents = TVFree(id, level)} as tv) ->
      let args = gen_var_list level n in
      let ret = gen_var level in
      tv := TVBound (TFun(args, ret));
      args, ret
  | _ -> raise (TypeError("expected a function"))

let split3 list =
  let rec f ls (xs, ys, zs) =
    match ls with
    | [] -> (xs, ys, zs)
    | (x, y, z) :: rest -> 
      f rest (x :: xs, y :: ys, z :: zs)
  in f list ([], [], [])


let rec infer env level e = 
  let f = function
  | EConst(c) ->
    (match c with
    | CUnit -> TUnit
    | CBool _ -> TBool
    | CChar _ -> TChar
    | CInt _ -> TInt
    | CFloat _ -> TFloat)
  | EId(id) | EAnnot(id, _) ->
    (match (TypeInfo.lookup env id) with
    | Some(t) -> instantiate level t
    (*| None    -> gen_var level)*)
    | None -> raise (TypeError("unknown variable: " ^ id)))
  | ELet(binds, body) ->
    let (ids, es, tanots) = split3 binds in
    let var_ts = List.map (infer env (level + 1)) es in
    List.iter2 (fun t1 t2 -> match t1, t2 with
      | t1, Some(t2) -> unify t1 t2
      | _, None -> ()) var_ts tanots;
    let gen_ts = List.map (generalize level) var_ts in
    infer (TypeInfo.extend_all env ids gen_ts) level body
  | EApp(f_id, args) ->
    let args_t, ret_t =
      match_fun (List.length args) (infer env level (EId(f_id)))
    in
    List.iter2
      (fun arg_t arg -> unify arg_t (infer env level arg))
      args_t args
    ;
    ret_t
  | EBin(b, e1, e2) ->
    let t1 = infer env level e1 in
    let t2 = infer env level e2 in
    unify t1 t2; 
    (match b with
      | BLt | BLte | BGt | BGte | BEq | BNe | BLAnd | BLOr -> TBool
      | _ -> t1)
  | EUni(o, e) ->
    infer env level e
  | ETuple(es) ->
    TTuple(List.map (infer env level) es)
  | EIf(c, a, b) ->
    unify (infer env level c) TBool;
    let a_t = infer env level a in
    let b_t = infer env level b in
    unify a_t b_t; a_t
  | EFun(args, body) ->
      let args_t = gen_var_list level (List.length args) in
      let fn_env = List.fold_left2 TypeInfo.extend env args args_t in
      let ret_t = infer fn_env level body in
      TFun(args_t, ret_t)
  | ECase(m, bodies) -> assert false
  in 
  let fe = f e in 
    print_endline ("INFER " ^ TypeInfo.string_of_ti env ^
                   " (" ^ string_of_int level ^ "," ^ string_of_expr e ^ ") |- " ^ string_of_type fe);
    fe

let type_module ({ definition = defs; in_node = i; out_node = o; _ }) = 
  let default = function 
  | Some(t) -> t
  | None    -> gen_var 0 in
  let rec collect (is, ts, es) = function 
  | [] -> (is, ts, es)
  | (Const ((i, t), e)) :: xs -> collect (i :: is, default t :: ts, e :: es) xs
  | (Node ((i, t), _, e)) :: xs -> collect (i :: is, default t :: ts, e :: es) xs
  | (Fun ((i, (at, rt)), e)) :: xs -> 
    let t = TFun(List.map default at, default rt) in
    collect (i :: is, t :: ts, e :: es) xs
  in
  let (ids, annots, exprs) = collect ([], [], []) defs in
  let ioenv = List.fold_left (fun m (i, t) -> TypeInfo.extend m i t) TypeInfo.empty (i @ o) in
  let env = List.fold_left2 TypeInfo.extend ioenv ids annots in
  let exprs_t = List.map (infer env 1) exprs in
  List.iter2 unify exprs_t annots;
  let gen_ts = List.map (generalize 0) exprs_t in
  TypeInfo.extend_all env ids gen_ts