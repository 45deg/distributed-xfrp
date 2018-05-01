open Syntax

module S = Set.Make(String);;
module M = Map.Make(String);;

type dependency = {
  ins: S.t;
  outs: S.t
}

let string_of_graph graph = 
  graph |>
  M.bindings |>
  List.map (fun (k, r) -> "" ^ k ^ ": in(" ^ (S.elements r.ins |> String.concat ", ") ^ ")" 
                                 ^ ": out(" ^ (S.elements r.outs |> String.concat ", ") ^ ")") |>
  String.concat "\n"

let get_graph xmodule = 
  let rec extract = function
    | EConst _ -> S.empty
    | EId id | EAnnot (id, _) -> S.singleton id
    | EApp (id, es) -> S.singleton id
    | EBin (_, e1, e2) -> S.union (extract e1) (extract e2)
    | EUni (op, e) -> extract e
    | ELet (binders, e) ->
      let ids = List.map (fun (a, _, _) -> a) binders in
      S.diff (extract e) (S.of_list ids)
    | EIf(c, a, b) -> S.union (extract c) (S.union (extract a) (extract b))
    | ETuple es -> List.map extract es |> List.fold_left S.union S.empty
    | EFun (args, e) -> 
      S.diff (extract e) (S.of_list args)
    | ECase(e, list) -> assert false
  in
  let rec collect result = function
    | [] -> result
    | (Node ((dst, _), _, e)) :: xs -> 
      collect (M.add dst (extract e) result) xs 
    | _ :: xs -> collect result xs
  in
  let filter ins m =
    let nodeids = M.bindings m |> List.map fst |> S.of_list in
    let keys = S.union nodeids (S.of_list (List.map fst ins)) in
    M.map (fun s -> S.inter keys s) m
  in
  let rev_map m =
    M.fold (fun src -> 
      S.fold (fun dst ->
        M.update dst (function
          | Some(s) -> Some(S.add src s)
          | None    -> Some(S.singleton src)
        )
      )
    ) m M.empty
  in
  collect M.empty xmodule.definition |>
  filter xmodule.in_node |>
  (fun m -> M.merge (fun k ao bo -> match ao, bo with
    | Some(a), Some(b) -> Some({ ins = a; outs = b})
    | None, Some(b) -> Some({ ins = S.empty; outs = b})
    | Some(a), None -> Some({ ins = a; outs = S.empty})
    | _, _ -> None
  ) m (rev_map m))