open Syntax
open Module

type element = 
    Current of id
  | Last of id

module ES = Set.Make(struct
                      type t = element
                      let compare x y = match x, y with
                        | Current a, Current b 
                        | Last a, Last b
                          -> compare a b
                        | Current _, Last _ -> 1
                        | Last _, Current _ -> 1
                    end);;
module S = Set.Make(String);;
module M = Map.Make(String);;

type dependency = {
  input_current: id list;
  input_last: id list;
  root: id list;
  root_group: (id * id list * id list) list;
  output: id list;
  is_output: bool
}

exception InvalidAtLast of string list

let string_of_graph graph = 
  graph |>
  M.bindings |>
  List.map (fun (k, r) -> "" ^ k ^ ": in_c(" ^ (String.concat ", " r.input_current) ^ ")" 
                                 ^ ": in_l(" ^ (String.concat ", " r.input_last) ^ ")"
                                 ^ ": out(" ^ (String.concat ", " r.output) ^ ")"
                                 ^ ": root({" ^ (String.concat ", " (
                                    List.map (fun (i, c, l) -> i ^ " -> (" ^ String.concat "," c ^ ";"
                                                                           ^ String.concat "," l ^ ")") r.root_group
                                 )) ^ "})") |>
  String.concat "\n"

let get_graph xmodule = 
  let extract nodes = 
    let rec f = function
      | EConst _ -> ES.empty
      | EId id -> if S.mem id nodes then ES.singleton (Current id) else ES.empty
      | EAnnot (id, _) -> ES.singleton (Last id) (* TODO: Check id is in nodes *)
      | EApp (id, es) -> List.map f es |> List.fold_left ES.union ES.empty
      | EBin (_, e1, e2) -> ES.union (f e1) (f e2)
      | EUni (op, e) -> f e
      | ELet (binders, expr) ->
        let (outer, inner) = List.fold_left (fun (outer, inner) (i, e, _) -> 
          (ES.union (f e) outer, ES.remove (Current i) inner)
        ) (ES.empty, f expr) binders 
        in ES.union outer inner
      | EIf(c, a, b) -> ES.union (f c) (ES.union (f a) (f b))
      | EList es | ETuple es -> 
        List.map f es |> List.fold_left ES.union ES.empty
      | EFun (args, e) -> 
        ES.diff (f e) (ES.of_list (List.map (fun a -> Current a) args))
      | ECase(m, list) -> (f m :: List.map (fun (_, e) -> f e) list) |> List.fold_left ES.union ES.empty
    in f
  in
  let inv_map m =
    M.fold (fun src -> 
      ES.fold (function | Current dst | Last dst ->
        M.update dst (function
          | Some(s) -> Some(S.add src s)
          | None    -> Some(S.singleton src)
        )
      )
    ) m M.empty
  in
  let partition = 
    let rec f (cs, ls) = function
      | [] -> (cs, ls)
      | Current i :: xs -> f (i :: cs, ls) xs
      | Last i :: xs -> f (cs, i :: ls) xs
    in f ([], [])
  in
  let unified name = 
      try Module.M.find name xmodule.unified_group with | Not_found -> name
  in
  let nodes = List.fold_left (fun m (i, _, e) -> M.add i e m) M.empty xmodule.node in
  let in_ids = S.of_list (List.map fst (M.bindings nodes) @ xmodule.source) in
  let ins = M.map (extract in_ids) nodes in
  let inv = inv_map ins in
  let root = List.fold_left (fun m in_name ->
    let ancestors name = 
      let rec f visited name =
        if S.mem name visited then S.empty else
        try 
          let out = (M.find name inv) in
          S.fold (fun n -> 
            S.union (f (S.add name visited) n)
          ) out out
        with Not_found -> 
          S.empty
      in f S.empty name
    in
    S.fold (fun node -> 
      M.update node (function
        | Some(s) -> Some(S.add (unified in_name) s)
        | None    -> Some(S.singleton (unified in_name))
      )
    ) (ancestors in_name) m
  ) M.empty xmodule.source
  in
  List.fold_left (fun m i -> M.add i ES.empty m) ins xmodule.source |>
  M.mapi (fun k i -> partition (ES.elements i)) |>
  M.mapi (fun k (cur, last) ->
    let common_root r ids = List.filter (fun id -> 
      try S.mem r (M.find id root) with Not_found -> compare r (unified id) == 0
    ) ids in
    let roots = try S.elements (M.find k root) with Not_found -> [] in
    { 
      input_current = cur;
      input_last = last;
      output = (try S.elements (M.find k inv) with Not_found -> []);
      root = roots;
      root_group = List.map (fun r -> (r, common_root r cur, common_root r last)) roots;
      is_output = List.mem k xmodule.sink
    }
  )

let find_loop in_nodes dependency =
  let inv = Hashtbl.create 13 in
  M.iter (fun dst d -> 
    List.iter (fun src ->
      Hashtbl.add inv src dst
    ) d.input_current
  ) dependency;
  let rec dropwhile thunk a = function
    | [] -> None
    | x :: xs when compare x a == 0 -> Some(x :: thunk) 
    | x :: xs -> dropwhile (x :: thunk) a xs in
  let rec f trace nodes = 
    match nodes with
    | [] -> []
    | n :: ns ->
      match dropwhile [] n trace with
      | Some(xs) ->
        [xs]
      | None ->
        (f (n :: trace) (Hashtbl.find_all inv n)) @ (f trace ns)
  in
  List.concat (List.map (fun n -> f [] (Hashtbl.find_all inv n)) in_nodes)

let check_source_constraint graph = 
  let get_invalids dep = List.fold_left (fun acc (root_id, _, lasts) ->
    List.fold_left (fun m last_id -> 
      M.update last_id (function
        | Some l -> Some (root_id :: l)
        | None   -> Some [root_id]
      ) m
    ) acc lasts
  ) M.empty dep.root_group
  |> M.bindings 
  |> List.filter (fun (_, l) -> List.length l > 1) in
(*
  let get_invalids dep = List.map (fun (root_id, _, lasts) -> 
    let inv_map = List.fold_left (fun m last_id -> 
      M.update last_id (function
        | Some l -> Some (root_id :: l)
        | None   -> Some [root_id]
      ) m
    ) M.empty lasts in
    let invalids = M.filter (fun _ list -> List.length list > 0) inv_map in
    M.bindings invalids
  ) dep.root_group |> List.flatten in
*)
  let warns = List.fold_left (fun acc (nodeid,dep) ->
    List.map (fun (last_id, rootids) ->
      let n = string_of_int (List.length rootids) in
      "In node " ^ nodeid ^ ", node `" ^ last_id ^ "` has " ^ n ^ " roots: " ^ (String.concat ", " rootids) ^ ". Consider unify them."
    ) (get_invalids dep) @ acc
  ) [] (M.bindings graph) in
  match warns with
    | [] -> ()
    | _  -> raise (InvalidAtLast warns)