open Type
open Syntax

exception TypeError of string

let iter2 f a b fa fb = 
  try 
    List.iter2 f a b
  with
  | Invalid_argument(_) -> 
    raise (TypeError("two types have different lengths: (" ^ 
                String.concat "," (List.map fa a) ^ ") and (" ^ 
                String.concat "," (List.map fb b) ^ ")"))
let iter2ty f a b = iter2 f a b (string_of_type) (string_of_type)

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
  | TList(t)
    -> adjust_level id level t
  | _ -> ()

let rec unify t1 t2 = 
  if t1 == t2 then ()
  else match t1, t2 with
    | TUnit, TUnit
    | TBool, TBool
    | TChar, TChar
    | TInt, TUnit
    | TFloat, TFloat -> ()
    | TTuple(ts1), TTuple(ts2)
      -> iter2ty unify ts1 ts2
    | TList(t1), TList(t2)
      -> unify t1 t2
    | TFun(a1, r1), TFun(a2, r2)
      -> iter2ty unify a1 a2;
         unify r1 r2
    | TVar {contents = TVBound t1}, t2
    | t1, TVar {contents = TVBound t2}
      -> unify t1 t2
    | TVar ({contents = TVFree(id, level)} as tv), t
    | t, TVar ({contents = TVFree(id, level)} as tv) ->
        adjust_level id level t;
        tv := TVBound(t)
    | _, _ -> raise (TypeError("unification failed with " ^ string_of_type t1 ^ " and " ^ string_of_type t2))
  
let rec generalize level = function
  | TFun(args, ret) ->
    TFun(List.map (generalize level) args, generalize level ret)
  | TTuple(ts) ->
    TTuple(List.map (generalize level) ts)
  | TList(t) ->
    TList(generalize level t)
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
    | TList(t) ->
      TList(f t)
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

let type_of_const = function
    | CUnit -> TUnit
    | CBool _ -> TBool
    | CChar _ -> TChar
    | CInt _ -> TInt
    | CFloat _ -> TFloat

let rec infer env level = function
  | EConst(c) -> type_of_const c
  | EId(id) | EAnnot(id, _) ->
    (match (Typeinfo.lookup env id) with
    | Some(t) -> instantiate level t
    (*| None    -> gen_var level)*)
    | None -> raise (TypeError("unknown variable: " ^ id)))
  | ELet(binds, body) ->
    let (ids, es, tanots) = split3 binds in
    let var_ts = List.map (infer env (level + 1)) es in
    iter2 (fun t1 t2 -> match t1, t2 with
      | t1, Some(t2) -> unify t1 t2
      | _, None -> ()) var_ts tanots
      string_of_type (function | Some(t) -> string_of_type t | None -> "?");
    let gen_ts = List.map (generalize level) var_ts in
    infer (Typeinfo.extend_all env ids gen_ts) level body
  | EApp(f_id, args) ->
    let args_t, ret_t =
      match_fun (List.length args) (infer env level (EId(f_id)))
    in
    iter2ty unify args_t (List.map (infer env level) args)
    ;
    ret_t
  | EBin(b, e1, e2) ->
    let t1 = infer env level e1 in
    let t2 = infer env level e2 in
    (match b with
      | BCons ->
        unify (TList(t1)) t2; t2
      | BLt | BLte | BGt | BGte | BEq | BNe | BLAnd | BLOr -> 
        unify t1 t2; TBool
      | _ -> 
        unify t1 t2; t1)
  | EUni(o, e) ->
    infer env level e
  | EList(es) ->
    TList(List.fold_left (fun acc e -> 
            unify acc (infer env level e); acc
          ) (gen_var level) es)
  | ETuple(es) ->
    TTuple(List.map (infer env level) es)
  | EIf(c, a, b) ->
    unify (infer env level c) TBool;
    let a_t = infer env level a in
    let b_t = infer env level b in
    unify a_t b_t; a_t
  | EFun(args, body) ->
    let args_t = gen_var_list level (List.length args) in
    let fn_env = List.fold_left2 Typeinfo.extend env args args_t in
    let ret_t = infer fn_env level body in
    TFun(args_t, ret_t)
  | ECase(m, bodies) -> 
    let rec type_of_pattern = function
    | PWild -> (gen_var (level + 1), [])
    | PConst c -> (type_of_const c, [])
    | PVar v -> let vt = gen_var (level + 1) in (vt, [(v, vt)])
    | PTuple ps -> 
      let (ts, binds) = List.split (List.map type_of_pattern ps) in
      (TTuple ts, List.flatten binds)
    in
    let rec infer_bodies mt = function
    | [] -> TUnit
    | (p, e) :: rest ->
      let (pt, binds) = type_of_pattern p in
      unify mt pt; (* check: pattern *)
      let nenv = List.fold_left (fun e (i, t) -> Typeinfo.extend e i t) env binds in
      let et = infer nenv level e in 
      match rest with 
        | [] -> et
        | _  -> unify et (infer_bodies mt rest); et (* check: body *)
    in
    infer_bodies (infer env level m) bodies

let rec is_concrete = function
  | TVar {contents = TVBound ty}
    -> is_concrete ty
  | TVar {contents = TVGeneric _}
  | TVar ({contents = TVFree(_,_)})
    -> false
  | TFun(args, ret)
    (*-> List.for_all is_concrete args && is_concrete ret*)
    -> true (* ignore function type *)
  | TTuple(ts)
    -> List.for_all is_concrete ts
  | TList(t)
    -> is_concrete t
  | _ -> true

let infer_defs defs env =
  let (ids, exprs) = List.split defs in
  let exprs_t = List.map (fun (id,t) -> 
    try infer env 1 t
    with | TypeError(s) -> 
      raise (TypeError("For " ^ id ^ ", " ^ s))) defs in
  let annots = List.map (fun id -> (id,Module.M.find id env)) ids in
  iter2 (fun (id,annot) t -> 
    try
      unify annot t
    with | TypeError(s) -> 
      raise (TypeError("For " ^ id ^ ", " ^ s))
  ) annots exprs_t
    (fun (_, t) -> string_of_type t) (string_of_type);
  let gen_ts = List.map (generalize 0) exprs_t in
  Typeinfo.extend_all env ids gen_ts

let make_env default =
  let open Module in
  M.map (function 
  | TAConst (Some (t)) -> t
  | TAConst None       -> gen_var 1
  | TAFun (at, rt)     ->
    let default = function | Some(t) -> t
                           | None    -> gen_var 1 in
    TFun(List.map default at, default rt)
  | TANode  (Some (t)) -> t
  | TANode  None       -> gen_var 1
  ) default

let check_init env n =
  List.iter (fun (id, init_opt, _) ->
    match init_opt with 
    | Some (init) -> begin
      let ty = Typeinfo.find id env in
      let init_ty = infer env 1 init in
      try
        unify ty init_ty
      with | TypeError(s) -> 
        raise (TypeError("For " ^ id ^ ", " ^
          "the type of init value does not match: "
          ^ string_of_type init_ty ^ " and " ^ string_of_type ty))
      end
    | None -> ()
  ) n

let type_module program = 
  let open Module in
  let env = make_env program.typeinfo in
  let { const = c; func = f; node = n } = program in
  let result = env |>
  infer_defs c |>
  infer_defs f |>
  infer_defs (List.map (fun (i,_,e) -> (i,e)) n) in
  check_init result n;
  if List.for_all (fun (i, _, _) -> is_concrete (Typeinfo.find i result)) n then
    (* check all nodes are concrete type. *)
    result
  else 
    raise (TypeError("Generic types remain: " ^ 
      (n |> List.map (fun (i, _, _) -> (i, Typeinfo.find i result))
         |> List.filter (fun (_, t) -> not (is_concrete t))
         |> List.map (fun (i,t) -> "(" ^ i ^ ":" ^ string_of_type t ^ ")")
         |> String.concat ", ")
    ))