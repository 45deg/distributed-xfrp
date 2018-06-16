open Syntax
open Module
open Dependency

module S = Set.Make(String);;
module M = Map.Make(String);;

exception UnknownId of string
exception AtLastError of string
exception InfiniteLoop of string list list

type code_option = {
  debug: bool;
  mess: int option;
  drop: float option;
  bufsize: int option;
}

let config = ref { debug = false; mess = None; drop = None; bufsize = None }

let try_find id m = begin
  try M.find id m with 
    Not_found -> (raise (UnknownId(id)))
end

type erl_id = 
  | EIConst of string 
  | EIFun of string * int
  | EIVar of string
  | EISigVar of string
  | EILast of erl_id

let string_of_eid ?(raw=true) = function
  | EIConst(id) -> "(const_" ^ id ^ "())"
  | EIFun(id, n) -> (if raw then "fun_" ^ id
                            else "fun ?MODULE:fun_" ^ id ^ "/" ^ string_of_int n)
  | EIVar(id) -> (match id with
    | "_" -> "_"
    | s -> "V" ^ s)
  | EISigVar(id) -> "S" ^ id
  | EILast(EISigVar(id)) -> "LS" ^ id
  | EILast(EIConst(id)) | EILast(EIFun(id, _)) | EILast(EIVar(id)) ->
    (raise (AtLastError(id ^ " is not node")))
  | EILast(EILast _) ->
    (raise (AtLastError("@last operator cannot be applied twice, make another delay node")))

let send i e = 
  let expr = match !config.mess with
    | None   -> i ^ " ! " ^ e
    | Some n -> "timer:send_after(rand:uniform(" ^ string_of_int n ^ "), " ^ i ^ ", " ^ e ^ ")" in
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
    | EConst (CChar c) -> "?" ^ Char.escaped c
    | EId id -> string_of_eid ~raw:false (try_find id env)
    | EAnnot (id, ALast) -> string_of_eid (EILast(try_find id env))
    | EApp (id, es) -> (* workaround *)
      string_of_eid (try_find id env) ^ "(" ^ (concat_map "," (f env) es) ^ ")"
    | EBin (BCons, hd, tl) ->
      "[" ^ f env hd ^ "|" ^ f env tl ^ "]"
    | EBin (op, e1, e2) -> "(" ^ f env e1 ^ (match op with
        | BMul -> " * "   | BDiv -> " / "        | BMod -> " rem "
        | BAdd -> " + "   | BSub -> " - "        | BShL -> " bsl "
        | BShR -> " bsr " | BLt -> " < "         | BLte -> " <= "
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
  let spawn = concat_map "" (fun (id, node) ->
    if List.exists (fun i -> compare i id == 0) xmod.source then
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ id ^ ", [0])),\n"
    else
      let (init, fun_id) = (init_map node, id) in
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ fun_id ^ ", [#{" ^
        concat_map ", " (fun s -> "{" ^ s ^ ", 0} => " ^ init) node.root
      ^ "}, #{}, [], #{}])),\n"
  ) (M.bindings deps) in
  "main() -> \n" ^
  spawn ^
  indent 1 "register(out, spawn(?MODULE, out, [])),\n" ^
  indent 1 "in()."

let in_node deps id = 
  id ^ "(Version) ->\n" ^ 
  concat_map "\n" (indent 1) [
  "receive";
  "Value ->";
    (try_find id deps).output |>
    concat_map ",\n\t" (fun s -> indent 1 (send s ("{" ^ id ^ ", Value, {" ^ id ^ ", Version}}")));
  "end,";
  id ^ "(Version + 1)."]

let def_node deps env (id, init, expr) =
  let dep = try_find id deps in
  let bind (cs, ls) = "#{" ^ String.concat ", " (
    List.map (fun id -> id ^ " := " ^ string_of_eid (EISigVar id)) cs @
    List.map (fun id -> "{last, " ^ id ^ "} := " ^ string_of_eid (EILast (EISigVar id))) ls)
  ^ "}" in
  let update n = 
    match (if dep.is_output then "out" :: dep.output else dep.output) with
      | [] -> indent n "% nothing to do\n" (* TODO: Should output some warning *)
      | output ->
        indent n "Curr = " ^ erlang_of_expr env expr ^ ",\n" ^
        indent n "lists:foreach(fun (V) -> \n" ^
        concat_map ",\n" (indent (n + 1)) (
          List.map (fun i -> send i ("{" ^ id ^ ", Curr, V}")) output
        ) ^ "\n" ^
        indent n "end, [Version|Deferred]),\n"
  in
  (* main node function *)
  id ^ "(Heap0, Last0, Deferred0, Latest0) ->\n" ^ 
  (if !config.debug then indent 1 "io:format(standard_error, \"" ^ id ^ "(~p,~p,~p, ~p)~n\", [Heap0, Last0, Deferred0, Latest0]),\n" else "") ^
  indent 1 "HL = trim(Heap0, Last0, Latest0),\n" ^
  indent 1 "{NHeap, NLast, NDeferred} = lists:foldl(fun (E, {Heap, Last, Deferred}) -> \n" ^
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
      indent 4 "{maps:remove(Version, Heap), maps:merge(Last, Map), []};\n"
    | others ->
      indent 4 "case Last of\n" ^
      indent 5 (bind others) ^ " -> \n" ^
      update 6 ^
      indent 6 "{maps:remove(Version, Heap), maps:merge(Last, Map), []};\n" ^
      indent 5 "_ -> {maps:remove(Version, Heap), maps:merge(Last, Map), [Version|Deferred]}\n" ^
      indent 4 "end;\n"
  ) dep.root_group ^
  indent 3 "_ -> {Heap, Last, Deferred}\n" ^
  indent 2 "end\n" ^
  indent 1 "end, {maps:from_list(HL), Last0, Deferred0}, HL),\n" ^
  indent 1 "{Received,{RvId,RvVer}} = receive {_,_,V} = M -> {M,V} end,\n" ^
  (if !config.debug then indent 1 "io:format(standard_error, \"" ^ id ^ " receives (~p)~n\", [Received]),\n" else "") ^
  indent 1 "case maps:get(RvId, Latest0, 0) of\n" ^
  indent 2 "MaxVer when ?IS_OLD(MaxVer,RvVer) ->\n" ^
  indent 3 id ^ "(NHeap, NLast, NDeferred, Latest0);\n" ^
  indent 2 "MaxVer -> \n" ^
  indent 3 id ^ "(heap_update([" ^ String.concat ", " dep.input_current ^"], [" ^ String.concat ", " dep.input_last ^
            "], Received, NHeap), NLast, NDeferred, Latest0#{ RvId => max(RvVer, MaxVer) })\n" ^
  indent 1 "end."

let def_const env (id, e) =
  (string_of_eid (EIConst id)) ^ " -> " ^ erlang_of_expr env e ^ "."

let def_fun env (id, body) = match body with
  | EFun(a, _) ->
    (string_of_eid (EIFun (id, -1))) ^ erlang_of_expr env body ^ "."
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
    | (id, Some(init), _) -> M.add id init m
    | (id, None, _) -> M.add id (of_type (Typeinfo.find id ti)) m) M.empty x.node in
  List.fold_left (fun m id -> M.add id (of_type (Typeinfo.find id ti)) m) node_init x.source

let lib_funcs () = 
  String.concat "\n" [
  (match !config.bufsize with
   | Some(n) -> "-define(IS_OLD(Vmax,V), Vmax - V < " ^ string_of_int n ^ ")."
   | None    -> "-define(IS_OLD(Vmax,V), false).");
  "trim(HL, Last, Latest, Thunk) ->";
  indent 1 "case HL of";
  indent 2 "[] -> Thunk;";
  indent 2 "[{{I,V}, Map} = X|Xs] ->";
  indent 3 "case ?IS_OLD(maps:get(I, Latest, 0), V) of";
  indent 4 "true -> [{{I,V}, maps:merge(Last, Map)}|Thunk];";
  indent 4 "false -> trim(Xs, Last, Latest, [X|Thunk])";
  indent 3 "end";
  indent 1 "end.";
  "trim(Heap, Last, Latest) ->";
  indent 1 "trim(lists:sort(fun ({{K1, V1}, _}, {{K2, V2}, _}) -> if";
  indent 2 "V1 == V2 -> K1 < K2;";
  indent 2 "true -> V1 > V2";
  indent 1 "end end, maps:to_list(Heap)), Last, Latest, []).";
  "heap_update(Current, Last, {Id, RValue, {RVId, RVersion}}, Heap) ->";
  indent 1 "H1 = case lists:member(Id, Current) of";
  indent 2 "true  -> maps:update_with({RVId, RVersion}, fun(M) -> M#{ Id => RValue } end, #{ Id => RValue }, Heap);";
  indent 2 "false -> Heap";
  indent 1 "end,";
	indent 1 "case lists:member(Id, Last) of";
  indent 2 "true  -> maps:update_with({RVId, RVersion + 1}, fun(M) -> M#{ {last, Id} => RValue } end, #{ {last, Id} => RValue }, H1);";
  indent 2 "false -> H1";
  indent 1 "end."
]

let in_func input = String.concat "\n" @@
  "in() ->" :: List.map (indent 1) (
    "%fill here" ::
    List.map (fun i -> "% " ^ i ^ " ! sth") input @
    ["in()."]
  )

let out_func output = String.concat "\n" @@
  "out() ->" :: List.map (indent 1) [
    "receive";
    (concat_map ";\n" (fun i -> indent 1 "{" ^ i ^ ", Value, Version} -> io:format(\"~p @ ~p~n\", [Value, Version])") output);
    "end,";
    "out()."
  ]

let of_xmodule x ti template opt = 
  config := opt;
  let dep = Dependency.get_graph x in
  (match Dependency.find_loop x.source dep with
    | [] -> ()
    | loops -> raise (InfiniteLoop(loops)));
  let user_funs = 
     List.map (fun (i,_) -> (i, EIConst i)) x.const @
     List.map (function | (i, EFun(args, _)) -> (i, EIFun (i, List.length args))
               | _ -> assert false) x.func in
  let attributes = 
    [[("main", 0)]; [("out", 0); ("in", 0)];
     List.map (fun (_, e) -> (string_of_eid e,
                match e with | EIFun(_, n) -> n
                             | _ -> 0 )) user_funs;
     List.map (fun i -> (i, 1)) x.source;
     List.map (fun (i, _, _) -> (i, 4)) x.node]
  in
  let exports = (concat_map "\n" (fun l -> "-export([" ^ 
      (concat_map "," (fun (f,n) -> f ^ "/" ^ string_of_int n) l)
    ^ "]).") attributes) in
  let env = List.fold_left (fun m (i,e) -> M.add i e m) M.empty user_funs in
  let env_with_nodes =
    List.map (fun (i, _, _) -> i) x.node @ x.source |>
    List.fold_left (fun m i -> M.add i (EISigVar i) m) env in
  let inits = (init_values x ti) in
  String.concat "\n\n" (
    ("-module(" ^ String.lowercase_ascii x.id ^ ").") ::
    exports ::
    (* concat_map "\n" (fun s -> "%" ^ s) (String.split_on_char '\n' (string_of_graph dep)) :: *)
    lib_funcs () ::
    List.map (def_const env) x.const
    @ List.map (def_fun env) x.func
    @ main dep x inits env ::
     (match template with
       | Some s -> [s]
       | None   -> [in_func x.source; out_func x.sink])
    (* outfunc *)
    @ (List.map (in_node dep) x.source)
    @ List.map (def_node dep env_with_nodes) x.node
  )
