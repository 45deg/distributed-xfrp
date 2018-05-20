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
  output: id list
}

let string_of_graph graph = 
  graph |>
  M.bindings |>
  List.map (fun (k, r) -> "" ^ k ^ ": in_c(" ^ (String.concat ", " r.input_current) ^ ")" 
                                 ^ ": in_l(" ^ (String.concat ", " r.input_last) ^ ")"
                                 ^ ": out(" ^ (String.concat ", " r.output) ^ ")"
                                 ^ ": root(" ^ (String.concat ", " r.root) ^ ")") |>
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
      | ETuple es -> List.map f es |> List.fold_left ES.union ES.empty
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
  let nodes = List.fold_left (fun m (i, _, e) -> M.add i e m) M.empty xmodule.node in
  let in_ids = S.of_list (List.map fst (M.bindings nodes) @ xmodule.input) in
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
        | Some(s) -> Some(S.add in_name s)
        | None    -> Some(S.singleton in_name)
      )
    ) (ancestors in_name) m
  ) M.empty xmodule.input
  in
  List.fold_left (fun m i -> M.add i ES.empty m) ins xmodule.input |>
  M.mapi (fun k i -> 
    let (cur, last) = partition (ES.elements i) in
    let output = try S.elements (M.find k inv) with Not_found -> [] in
    { 
      input_current = cur;
      input_last = last;
      output = output;
      root = (try S.elements (M.find k root) with Not_found -> [])
    }
  )