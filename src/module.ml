open Syntax

module M = Map.Make(String);;

type tannot
   = TAConst of Type.t option
   | TANode of Type.t option
   | TAFun of Type.t option list * Type.t option

type t = {
  id: moduleid;
  source: id list;
  unified_group: id M.t;
  sink: id list;
  const: (id * expr) list;
  func: (id * expr) list;
  node: (id * expr option * expr) list;
  typeinfo: tannot M.t;
  hostinfo: (host * id list) list;
}

let collect defs = 
  let (a, b, c) = List.fold_left (fun (cs, fs, ns) -> function
    | Const ((i, _), e) -> ((i, e) :: cs, fs, ns)
    | Fun ((i, _), e)  -> (cs, (i, e) :: fs, ns)
    | Node ((i, _), init, e) -> (cs, fs, (i, init, e) :: ns)
  ) ([], [], []) defs in
  (List.rev a, List.rev b, List.rev c)

let make_type program = 
  let in_t  = List.fold_left (fun m (i,t) -> M.add i (TANode (Some t)) m) M.empty program.in_node in
  let def_t = List.fold_left (fun m -> function
    | (Const ((i, t), _)) -> M.add i (TAConst t) m
    | (Fun ((i, (ta,tr)), _)) -> M.add i (TAFun (ta, tr)) m
    | (Node ((i, t), _, _)) -> M.add i (TANode t) m
  ) in_t program.definition in
  let out_t = List.fold_left (fun m (i,t) -> M.add i (TANode (Some t)) m) def_t program.out_node in
  out_t

let unify_source unifies source host =
  let r = ref M.empty in
  let rhost = ref host in
  List.iteri (fun i xs ->
    let nodename = ("unified@" ^ string_of_int i) in
    (* wip-impl. *)
    rhost := List.map (fun (h, ns) ->
      if List.mem (List.hd xs) ns then (h,nodename::ns) else (h, ns)
    ) !rhost;
    List.iter (fun x ->
      r := M.add x nodename !r
    ) xs
  ) unifies;
  (!r, !rhost)

let of_program program =
  let (const, func, node) = collect program.definition in
  let source = List.map fst program.in_node in
  let (ug, uh) = unify_source program.in_unify source program.hostinfo in
  {
    id = program.id;
    source = source;
    unified_group = ug;
    sink = List.map fst program.out_node;
    const = const;
    node = node;
    func = func;
    typeinfo = make_type program;
    hostinfo = uh
  }