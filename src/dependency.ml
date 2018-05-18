open Syntax

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
  output: id list;
  is_lazy: bool
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
  let rec extract = function
    | EConst _ -> ES.empty
    | EId id -> ES.singleton (Current id)
    | EAnnot (id, _) -> ES.singleton (Last id)
    | EApp (id, es) -> List.map extract es |> List.fold_left ES.union ES.empty
    | EBin (_, e1, e2) -> ES.union (extract e1) (extract e2)
    | EUni (op, e) -> extract e
    | ELet (binders, expr) ->
      let (outer, inner) = List.fold_left (fun (outer, inner) (i, e, _) -> 
        (ES.union (extract e) outer, ES.remove (Current i) inner)
      ) (ES.empty, extract expr) binders 
      in ES.union outer inner
    | EIf(c, a, b) -> ES.union (extract c) (ES.union (extract a) (extract b))
    | ETuple es -> List.map extract es |> List.fold_left ES.union ES.empty
    | EFun (args, e) -> 
      ES.diff (extract e) (ES.of_list (List.map (fun a -> Current a) args))
    | ECase(m, list) -> (extract m :: List.map (fun (_, e) -> extract e) list) |> List.fold_left ES.union ES.empty
  in
  let rec collect result = function
    | [] -> result
    | (Node ((dst, _), _, e)) :: xs -> 
      collect (M.add dst ((extract e)) result) xs 
    | _ :: xs -> collect result xs
  in
  let rev_map m =
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
  let ins = collect M.empty xmodule.definition in
  let rev = rev_map ins in
  let root = List.fold_left (fun m (in_name, _) ->
    let ancestors name = 
      let rec f visited name =
        if S.mem name visited then S.empty else
        try 
          let out = (M.find name rev) in
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
  ) M.empty xmodule.in_node
  in
  List.fold_left (fun m (i, _) -> M.add i ES.empty m) ins xmodule.in_node |>
  M.mapi (fun k i -> 
    let (cur, last) = partition (ES.elements i) in
    let output = try S.elements (M.find k rev) with Not_found -> [] in
    { 
      input_current = cur;
      input_last = last;
      output = output;
      root = (try S.elements (M.find k root) with Not_found -> []);
      is_lazy = match cur with
        | [] -> true
        | _ -> false
    }
  )