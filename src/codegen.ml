open Syntax
open Module
open Dependency

module S = Set.Make(String);;
module M = Map.Make(String);;

exception UnknownId of string
exception AtLastError of string
exception InfiniteLoop of string list list

type driver =
  Simple | Actor

type code_option = {
  debug: bool;
  mess: int option;
  drop: float option;
  driver: driver
}

let default_config = { debug = false; mess = None; drop = None; driver = Simple }
let config = ref default_config

let try_find id m = begin
  try M.find id m with 
    Not_found -> (raise (UnknownId(id)))
end

type erl_id = 
  | EIConst of string 
  | EIFun of string * int
  | EIExtern of string * int
  | EIVar of string
  | EISigVar of string
  | EILast of erl_id

let string_of_eid ?(raw=true) = function
  | EIConst(id) -> "(const_" ^ id ^ "())"
  | EIFun(id, n) -> (if raw then "fun_" ^ id
                            else "fun ?MODULE:fun_" ^ id ^ "/" ^ string_of_int n)
  | EIExtern(id, n) -> (if raw then id
                            else "fun ?MODULE:" ^ id ^ "/" ^ string_of_int n)
  | EIVar(id) -> (match id with
    | "_" -> "_"
    | s -> "V" ^ s)
  | EISigVar(id) -> "S" ^ id
  | EILast(EISigVar(id)) -> "LS" ^ id
  | EILast(EIConst(id)) | EILast(EIFun(id, _)) | EILast(EIExtern(id, _)) | EILast(EIVar(id)) ->
    (raise (AtLastError(id ^ " is not node")))
  | EILast(EILast _) ->
    (raise (AtLastError("@last operator cannot be applied twice, make another delay node")))

let get_target hi i = match i with
    | "out_node" -> "out_node"
    | id -> let (host, _) = List.find (fun (_, ids) -> List.mem id ids) hi in
        match host with 
          | Host(h)   -> "{" ^ id ^ ",'" ^ h ^ "'}" 
          | Localhost -> id

let send hi i e = 
  let target = get_target hi i in
  let expr = match !config.mess with
    | None   -> target ^ " ! " ^ e
    | Some n -> "timer:send_after(rand:uniform(" ^ string_of_int n ^ "), " ^ target ^ ", " ^ e ^ ")" in
  match !config.drop with
    | None -> expr
    | Some n -> "(case (rand:uniform() > " ^ string_of_float n ^ ") of true -> "
                   ^ expr ^ "; false -> " ^ 
                    (if !config.debug then
                      "io:format(standard_error, \"Dropped ~p~n\", [" ^ e ^ "])"
                    else
                      "false")
                   ^ " end)"

let concat_map s f l = String.concat s (List.map f l)
let indent n s = String.make n '\t' ^ s

let erlang_of_expr env e = 
  let rec f env = function
    | EConst CUnit -> "void"
    | EConst (CBool b) -> string_of_bool b
    | EConst (CInt i)  -> string_of_int i
    | EConst (CFloat f)  -> Printf.sprintf "%f" f
    | EConst (CChar c) -> string_of_int (int_of_char c)
    | EId id -> string_of_eid ~raw:false (try_find id env)
    | EAnnot (id, ALast) -> string_of_eid (EILast(try_find id env))
    | EApp (id, es) -> (* workaround *)
      string_of_eid (try_find id env) ^ "(" ^ (concat_map "," (f env) es) ^ ")"
    | EBin (BCons, hd, tl) ->
      "[" ^ f env hd ^ "|" ^ f env tl ^ "]"
    | EBin (op, e1, e2) -> "(" ^ f env e1 ^ (match op with
        | BMul -> " * "   | BDiv -> " / "        | BMod -> " rem "
        | BAdd -> " + "   | BSub -> " - "        | BShL -> " bsl "
        | BShR -> " bsr " | BLt -> " < "         | BLte -> " =< "
        | BGt -> " > "    | BGte -> " >= "       | BEq -> " == "
        | BNe -> " /= "   | BAnd -> " band "     | BXor -> " bxor "
        | BOr -> " bor "  | BLAnd -> " andalso " | BLOr -> " orelse " | _ -> "") ^ f env e2 ^ ")"
    | EUni (op, e) -> (match op with 
      | UNot -> "(not " ^ f env e ^ ")"
      | UNeg -> "-" ^ f env e
      | UInv -> "(bnot " ^ f env e ^ ")") 
    | ELet (binders, e) -> 
      let bid = List.map (fun (i,_,_) -> string_of_eid (EIVar(i))) binders in
      let bex = List.map (fun (_,e,_) -> f env e) binders in
      let newenv = List.fold_left (fun env (i,_,_) -> M.add i (EIVar(i)) env) env binders in
      "(case {" ^ String.concat "," bex ^ "} of " ^ 
      "{" ^ String.concat "," bid ^ "} -> " ^ f newenv e ^ " end)"
    | EIf(c, a, b) -> 
      "(case " ^ f env c ^ " of true -> " ^ f env a ^ "; false -> " ^ f env b ^ " end)"
    | EList es ->
      "[" ^ (concat_map "," (f env) es) ^ "]"
    | ETuple es ->
      "{" ^ (concat_map "," (f env) es) ^ "}"
    | EFun (args, e) ->
      let newenv = List.fold_left (fun env i -> M.add i (EIVar(i)) env) env args in
      "(" ^ concat_map "," (fun i -> string_of_eid (EIVar i)) args ^ ") -> " ^ f newenv e
    | ECase(m, list) -> 
      let rec pat = function
        | PWild -> ("_", [])
        | PNil  -> ("[]", [])
        | PConst c -> (f env (EConst c), [])
        | PVar v -> (string_of_eid (EIVar v), [v])
        | PTuple ts -> 
          let (s, vs) = List.split (List.map pat ts) in
          ("{" ^ (String.concat "," s) ^ "}", List.flatten vs) 
        | PCons (hd, tl) ->
          let (hdt, hdbinds) = pat hd in
          let (tlt, tlbinds) = pat tl in
          ("[" ^ hdt ^ "|" ^ tlt ^ "]", hdbinds @ tlbinds) in
      let body (p, e) =
        let (ps, pvs) = pat p in
        let newenv = List.fold_left (fun e i -> M.add i (EIVar i) e) env pvs in
        ps ^ " -> " ^ f newenv e
      in
      "(case " ^ f env m ^ " of " ^
        concat_map "; " body list ^
      " end)"
  in f env e

let main deps xmod inits env = 
  let init i = try_find i inits |> erlang_of_expr env in
  let init_map node = "#{" ^ 
    concat_map ", " (fun i -> "{last, " ^ i ^ "} => " ^ init i) node.input_last
  ^ "}" in
  let spawn id =
    if List.exists (fun i -> compare i id == 0) xmod.source then
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ id ^ ", [0])),\n"
    else if Dependency.M.exists (fun _ v -> compare v id == 0) xmod.unified_group then
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ id ^ ", [0, #{}])),\n"
    else
      let node = Dependency.M.find id deps in
      let (init, fun_id) = (init_map node, id) in
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ fun_id ^ ", [#{" ^
        concat_map ", " (fun s -> "{" ^ s ^ ", 0} => " ^ init) node.root
      ^ "}, #{}, []])),\n"
  in
  List.map (fun (host, ids) ->
    let self = match !config.driver with
      | Simple -> ""
      | Actor  -> ", self()"
    in
    let hostname = string_of_host host in
    let whosts = (* The hosts to be waited *)
      List.filter (fun (h, _) -> compare h host != 0) xmod.hostinfo |>
      List.map (fun (h,_) -> string_of_host h)
      (* this implementation is optimized but it works incorrectly
      let sinks = List.map (fun id -> (Dependency.M.find id deps).output) ids |>
      List.flatten |> List.sort_uniq compare in
      List.filter (fun (h, _) -> compare h host != 0) xmod.hostinfo |>
      List.filter (fun (_,is) -> List.exists (fun i -> List.mem i sinks) is) |>
      List.map (fun (h,_) -> string_of_host h)
      *)
    in
    "start('" ^ hostname ^ "') -> \n" ^
    concat_map "" (fun id -> spawn id) ids ^
    (if List.exists (fun i -> List.mem i xmod.sink) ids then (* contains output node *)
      indent 1 "register(out_node,spawn(?MODULE, out_node, ['" ^ hostname ^ "'" ^ self ^ "])),\n" else "") ^
    indent 1 "wait([" ^ (concat_map "," (fun s -> "'" ^ s ^ "'") whosts) ^ "]),\n" ^
    indent 1 "in('" ^ hostname ^ "');"
  ) xmod.hostinfo @ ["start(_) -> erlang:error(badarg)."]
  |> String.concat "\n"

let in_node deps hi id unify_node = 
  let outputs = (try_find id deps).output in
  id ^ "(Version) ->\n" ^ 
  concat_map "\n" (indent 1) [
  "receive";
  "Value ->";
  (match unify_node with
    | Some(node) ->
      send hi node ("{" ^ id ^ ", [" ^ (concat_map "," (get_target hi) outputs) ^ "], Value}")
    | None ->
      concat_map ",\n\t" (fun s -> indent 1 (send hi s ("{" ^ id ^ ", Value, {" ^ id ^ ", Version}}"))) outputs);
  "end,";
  id ^ "(Version + 1)."]

let unify_node ug id =
  let num = Dependency.M.cardinal @@ Dependency.M.filter (fun _ v -> compare v id == 0) ug in
  id ^ "(Version, Last) ->\n" ^ 
  concat_map "\n" (indent 1) [
    "Elements = receive";
    indent 1 "{Source, Targets, Value} -> Last#{Source => {Targets, Value}}";
    "end,";
    "case maps:size(Elements) of";
    indent 1 (string_of_int num) ^ " -> ";
    indent 2 "maps:map(fun (Source, {Targets, Value}) -> ";
    indent 3 "lists:foreach(fun (Target) -> Target ! {Source, Value, {" ^ id ^ ", Version}} end, Targets)";
    indent 2 "end, Elements),";
    indent 2 id ^ "(Version + 1, Elements);";
    indent 1 "_ -> " ^ id ^ "(Version, Elements)";
    "end."
  ]

let out_node host outputs = String.concat "\n" @@
  let host = string_of_host host in
  let (arg, call_out) = match !config.driver with
    | Simple -> ("'" ^ host ^ "'", fun id -> "out(" ^ id ^ ", Value)")
    | Actor  -> ("'" ^ host ^ "', Pid", fun id -> "Pid ! {" ^ id ^ ", Value}")
  in
  ("out_node(" ^ arg ^ ") ->") :: [
    indent 1 "receive";
    (concat_map ";\n" (fun i -> indent 2 "{" ^ i ^ ", Value, _} -> " ^ call_out i) outputs);
    indent 1 "end,";
    indent 1 "out_node(" ^ arg ^ ");"
  ]
let out_nodes hosts outputs = String.concat "\n" @@
  (List.map (fun (host, ids) -> (host, List.filter (fun s -> List.mem s outputs) ids)) hosts |>
  List.filter (fun (_, ids) -> List.length ids > 0) |>
  List.map (fun (host, ids) -> out_node host ids)) @
  [match !config.driver with
      | Simple -> "out_node(_) -> erlang:error(badarg)."
      | Actor  -> "out_node(_,_) -> erlang:error(badarg)."]

let def_node deps hi env (id, init, expr, async) =
  let dep = try_find id deps in
  let bind (cs, ls) = "#{" ^ String.concat ", " (
    List.map (fun id -> id ^ " := " ^ string_of_eid (EISigVar id)) cs @
    List.map (fun id -> "{last, " ^ id ^ "} := " ^ string_of_eid (EILast (EISigVar id))) ls)
  ^ "}" in
  let update n = 
    (if async then indent n "spawn(fun () -> \n" else "") ^
    match (if dep.is_output then "out_node" :: dep.output else dep.output) with
      | [] -> indent n "% nothing to do\n" (* TODO: Should output some warning *)
      | output ->
        indent n "Curr = " ^ erlang_of_expr env expr ^ ",\n" ^
        indent n "lists:foreach(fun (V) -> \n" ^
        concat_map ",\n" (indent (n + 1)) (
          List.map (fun i -> send hi i ("{" ^ id ^ ", Curr, V}")) output
        ) ^ "\n" ^
        indent n "end, [Version|Deferred])" ^
        (if async then "end),\n" else ",\n")
  in
  (* main node function *)
  id ^ "(Buffer0, Rest0, Deferred0) ->\n" ^ 
  (if !config.debug then indent 1 "io:format(standard_error, \"" ^ id ^ "(~p,~p,~p)~n\", [Buffer0, Rest0, Deferred0]),\n" else "") ^
  indent 1 "HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),\n" ^
  indent 1 "{NBuffer, NRest, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Deferred}) -> \n" ^
  indent 2 "case E of\n" ^
  concat_map "" (fun (root, currents, lasts) ->
    let other_vars =
      let sub l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
      (sub dep.input_current currents, sub dep.input_last lasts)
    in
    indent 3 "{ {" ^ root ^ ", _} = Version, " ^ bind (currents, lasts) ^ " = Map} ->\n" ^
    match other_vars with
    | ([],[]) -> 
      update 4 ^
      indent 4 "{maps:remove(Version, Buffer), maps:merge(Rest, Map), []};\n"
    | others ->
      indent 4 "case Rest of\n" ^
      indent 5 (bind others) ^ " -> \n" ^
      update 6 ^
      indent 6 "{maps:remove(Version, Buffer), maps:merge(Rest, Map), []};\n" ^
      indent 5 "_ -> {maps:remove(Version, Buffer), maps:merge(Rest, Map), [Version|Deferred]}\n" ^
      indent 4 "end;\n"
  ) dep.root_group ^
  indent 3 "_ -> {Buffer, Rest, Deferred}\n" ^
  indent 2 "end\n" ^
  indent 1 "end, {Buffer0, Rest0, Deferred0}, HL),\n" ^
  indent 1 "Received = receive {_,_,{_, _}} = M -> M end,\n" ^
  (if !config.debug then indent 1 "io:format(standard_error, \"" ^ id ^ " receives (~p)~n\", [Received]),\n" else "") ^
  indent 1 id ^ "(buffer_update([" ^ String.concat ", " dep.input_current ^"], [" ^ String.concat ", " dep.input_last ^
            "], Received, NBuffer), NRest, NDeferred).\n"

let def_const env (id, e) =
  (string_of_eid (EIConst id)) ^ " -> " ^ erlang_of_expr env e ^ "."

let def_fun env (id, body) = match body with
  | InternFun (EFun(a, _) as expr) ->
    (string_of_eid (EIFun (id, -1))) ^ erlang_of_expr env expr ^ "."
  | ExternFun _ -> ""
  | _ -> assert false

let init_values x ti =
  let rec of_type = let open Type in function
    | TUnit -> EConst(CUnit)
    | TBool -> EConst(CBool(false))
    | TInt  -> EConst(CInt(0))
    | TFloat -> EConst(CFloat(0.0))
    | TChar -> EConst(CBool(false))
    | TTuple ts -> ETuple(List.map of_type ts)
    | TList t -> EList([])
    | _ -> assert false in
  let node_init = List.fold_left (fun m -> function 
    | (id, Some(init), _, _) -> M.add id init m
    | (id, None, _, _) -> M.add id (of_type (Typeinfo.find id ti)) m) M.empty x.node in
  List.fold_left (fun m id -> M.add id (of_type (Typeinfo.find id ti)) m) node_init x.source

let lib_funcs = String.concat "\n" [
  (* sort function *)
  "-define(SORTBuffer, fun ({{K1, V1}, _}, {{K2, V2}, _}) -> if";
  indent 1 "V1 == V2 -> K1 < K2;";
  indent 1 "true -> V1 < V2";
  "end end).";
  (* update buffer *)
  "buffer_update(Current, Rest, {Id, RValue, {RVId, RVersion}}, Buffer) ->";
  indent 1 "H1 = case lists:member(Id, Current) of";
  indent 2 "true  -> maps:update_with({RVId, RVersion}, fun(M) -> M#{ Id => RValue } end, #{ Id => RValue }, Buffer);";
  indent 2 "false -> Buffer";
  indent 1 "end,";
	indent 1 "case lists:member(Id, Rest) of";
  indent 2 "true  -> maps:update_with({RVId, RVersion + 1}, fun(M) -> M#{ {last, Id} => RValue } end, #{ {last, Id} => RValue }, H1);";
  indent 2 "false -> H1";
  indent 1 "end.";
  (* wait *)
  "wait(Hosts) -> ";
  indent 1 "case lists:all(fun (N) -> net_kernel:connect_node(N) end, Hosts) of";
  indent 2 "true -> ok;";
  indent 2 "false -> timer:sleep(1000), wait(Hosts)";
  indent 1 "end."
]

let in_func input hosts = 
  let fn (host, ids) =
    let hostname = string_of_host host in
    ("in('" ^ hostname ^ "') ->") :: List.map (indent 1) (
      ("% fill here for " ^ hostname) ::
      List.map (fun i -> "% " ^ i ^ " ! sth") (List.filter (fun i -> List.mem i ids) input) @
      ["in('" ^ hostname ^ "');"]
    ) |> String.concat "\n"
  in
  (List.map fn hosts) @ ["in(_) -> erlang:error(badarg)."] |> String.concat "\n"

let out_func output = String.concat "\n" @@
  List.map (fun id -> "out(" ^ id ^", Value) -> void; % replace here") output @
  ["out(_, _) -> erlang:error(badarg)."]

let of_xmodule x ti template opt = 
  config := opt;
  let dep = Dependency.get_graph x in
  (match Dependency.find_loop x.source dep with
    | [] -> ()
    | loops -> raise (InfiniteLoop(loops)));
  Dependency.check_source_constraint dep;
  let user_funs = 
     List.map (fun (i,_) -> (i, EIConst i)) x.const @
     List.map (function
               | (i, InternFun EFun(args, _)) -> (i, EIFun (i, List.length args))
               | (i, ExternFun (arg_t, _)) -> (i, EIExtern (i, List.length arg_t))
               | _ -> assert false) x.func in
  let unify_nodes = List.sort_uniq compare (List.map snd (Dependency.M.bindings x.unified_group)) in
  let attributes = 
    [[("start", 1)]; 
      (match !config.driver with
        | Simple -> [("out", 2); ("out_node", 1)]
        | Actor  -> [("out_node", 2)])
      @ [("in", 1); ("wait", 1)];
     List.map (fun (_, e) -> (string_of_eid e,
                match e with | EIFun(_, n) -> n
                             | EIExtern(_, n) -> n
                             | _ -> 0 )) user_funs;
     List.map (fun i -> (i, 1)) x.source;
     List.map (fun i -> (i, 2)) unify_nodes;
     List.map (fun (i, _, _, _) -> (i, 3)) x.node]
  in
  let exports = (concat_map "\n" (fun l -> "-export([" ^ 
      (concat_map "," (fun (f,n) -> f ^ "/" ^ string_of_int n) l)
    ^ "]).") attributes) in
  let env = List.fold_left (fun m (i,e) -> M.add i e m) M.empty user_funs in
  let env_with_nodes =
    List.map (fun (i, _, _, _) -> i) x.node @ x.source |>
    List.fold_left (fun m i -> M.add i (EISigVar i) m) env in
  let inits = (init_values x ti) in
  String.concat "\n\n" (
    ("-module(" ^ String.lowercase_ascii x.id ^ ").") ::
    exports ::
    (* concat_map "\n" (fun s -> "%" ^ s) (String.split_on_char '\n' (string_of_graph dep)) :: *)
    lib_funcs ::
    List.map (def_const env) x.const
    @ List.map (def_fun env) x.func
    @ main dep x inits env ::
     (match template with
       | Some s -> [s]
       | None   -> match !config.driver with
          | Simple -> [in_func x.source x.hostinfo; out_func x.sink]
          | Actor  -> [in_func x.source x.hostinfo]
    )
    (* outfunc *)
    @ (List.map (fun i -> in_node dep x.hostinfo i (Dependency.M.find_opt i x.unified_group)) x.source)
    @ List.map (unify_node x.unified_group) unify_nodes
    @ out_nodes x.hostinfo x.sink
    :: List.map (def_node dep x.hostinfo env_with_nodes) x.node
  )
