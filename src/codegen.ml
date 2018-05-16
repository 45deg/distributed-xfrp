open Syntax
open Dependency

module S = Set.Make(String);;
module M = Map.Make(String);;


exception UnknownId of string
let try_find id m = begin
  try M.find id m with Not_found -> (raise (UnknownId(id)))
end

let var = function
  | "_" -> "_"
  | s -> "V" ^ s
let sig_var s = "S" ^ s
let last_sig_var s = "LS" ^ s
let at_last var = if var.[0] == 'S' then "L" ^ var else var

let concat_map s f l = String.concat s (List.map f l)
let indent n s = String.make n '\t' ^ s

let const s = String.uppercase_ascii s

let erlang_of_expr env e = 
  let rec f env = function
    | EConst CUnit -> "void"
    | EConst (CBool b) -> string_of_bool b
    | EConst (CInt i)  -> string_of_int i
    | EConst (CFloat f)  -> Printf.sprintf "%f" f
    | EConst (CChar c) -> "?" ^ Char.escaped c
    | EId id -> try_find id env
    | EAnnot (id, ALast) -> try_find id env
    | EApp (id, es) -> (* workaround *)
      id ^ "(" ^ (concat_map "," (f env) es) ^ ")"
    | EBin (op, e1, e2) -> "(" ^ f env e1 ^ (match op with
        | BMul -> " * "   | BDiv -> " / "        | BMod -> " rem "
        | BAdd -> " + "   | BSub -> " - "        | BShL -> " bsl "
        | BShR -> " bsr " | BLt -> " < "         | BLte -> " <= "
        | BGt -> " > "    | BGte -> " >= "       | BEq -> " == "
        | BNe -> " /= "   | BAnd -> " band "     | BXor -> " bxor "
        | BOr -> " bor "  | BLAnd -> " andalso " | BLOr -> " orelse " ) ^ f env e2 ^ ")"
    | EUni (op, e) -> (match op with 
      | UNot -> "(not " ^ f env e ^ ")"
      | UNeg -> "-" ^ f env e
      | UInv -> "(bnot " ^ f env e ^ ")") 
    | ELet (binders, e) -> 
      let bid = List.map (fun (i,_,_) -> var i) binders in
      let bex = List.map (fun (_,e,_) -> f env e) binders in
      let newenv = List.fold_left (fun env (i,_,_) -> M.add i (var i) env) env binders in
      "(case {" ^ String.concat "," bex ^ "} of " ^ 
      "{" ^ String.concat "," bid ^ "} -> " ^ f newenv e ^ " end)"
    | EIf(c, a, b) -> 
      "(case " ^ f env c ^ " of true -> " ^ f env a ^ "; false -> " ^ f env b ^ " end)"
    | ETuple es ->
      "{" ^ (concat_map "," (f env) es) ^ "}"
    | EFun (args, e) ->
      let newenv = List.fold_left (fun env i -> M.add i (var i) env) env args in
      "(" ^ concat_map "," var args ^ ") -> " ^ f newenv e
    | ECase(m, list) -> 
      let rec pat = function
        | PWild -> ("_", [])
        | PConst c -> (f env (EConst c), [])
        | PVar v -> (var v, [v])
        | PTuple ts -> 
          let (s, vs) = List.split (List.map pat ts) in
          ("{" ^ (String.concat "," s) ^ "}", List.flatten vs) in
      let body (p, e) =
        let (ps, pvs) = pat p in
        let newenv = List.fold_left (fun e i -> M.add i (var i) e) env pvs in
        ps ^ " -> " ^ f newenv e
      in
      "(case " ^ f env m ^ " of " ^
        concat_map "; " body list ^
      " end)"
  in f env e

let main deps xmod inits env = 
  let init i = try_find i inits |> erlang_of_expr env in
  let init_map node = "#{" ^ 
    concat_map ", " (fun i -> i ^ " => " ^ init i) node.input_last
  ^ "}" in
  let spawn = concat_map "" (fun (id, node) ->
    if List.exists (fun (i, _) -> compare i id == 0) xmod.in_node then
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ id ^ ", [0])),\n"
    else
      let init = init_map node in
      indent 1 "register(" ^ id ^ ", " ^
      "spawn(?MODULE, " ^ id ^ ", [#{" ^
        concat_map ", " (fun s -> "{" ^ s ^ ", 0} => " ^ init) node.root
      ^ "}, #{}, []])),\n"
  ) (M.bindings deps) in
  "main() -> \n" ^
  spawn ^
  indent 1 "register(out, spawn(?MODULE, out, [])),\n" ^
  indent 1 "in()."

let in_node deps (id, _) = 
  id ^ "(Version) ->\n" ^ 
  concat_map "\n" (indent 1) [
  "receive";
  "Value ->";
    (try_find id deps).output |>
    concat_map ",\n\t" (fun s -> indent 1 s ^ " ! {" ^ id ^ ", Value, {" ^ id ^ ", Version}}");
  "end,";
  id ^ "(Version + 1)."]

let def_node xmod deps renv debug_flg  = 



  let env = !renv in function
  | Node ((id, _), init, expr) -> 
    let dep = try_find id deps in
    let root_group = 
      let is_root root id =
        match (try_find id deps).root with
        | [] -> compare root id == 0 (* source itself *)
        | xs -> List.mem root xs
      in
      List.map (fun r -> (r, List.filter (is_root r) dep.input_current
                           , List.filter (is_root r) dep.input_last)) dep.root in
    let output = if List.exists (fun (i, _) -> compare i id == 0) xmod.out_node then "out" :: dep.output
                 else dep.output in
    let newenv = env |> 
      (fun e -> List.fold_left (fun m i -> M.add i (sig_var i) m) e dep.input_current) |>
      (fun e -> List.fold_left (fun m i -> M.add i (last_sig_var i) m) e dep.input_last) in
    id ^ "(Heap, Last, Deferred) ->\n" ^ 
    (if debug_flg then indent 1 "io:format(\"" ^ id ^ "(~p,~p,~p)~n\", [Heap, Last, Deferred]),\n" else "") ^
    indent 1 "receive {Id,RValue,{RVId, RVersion}} ->\n" ^
    (if debug_flg then indent 2 "io:format(\"" ^ id ^ " receives (~p)~n\", [{Id,RValue,{RVId, RVersion}}]),\n" else "") ^
    indent 2 "NewHeap = maps:update_with(case Id of \n" ^
      (concat_map ";\n" (indent 3) (
        List.map (fun id -> id ^ " -> {RVId, RVersion}") dep.input_current @
        List.map (fun id -> id ^ " -> {RVId, RVersion + 1}") dep.input_last
      )) ^ "\n" ^
    indent 3 "end,\n" ^
    indent 3 "fun(M) -> M#{ Id => RValue } end,\n" ^
    indent 3 "#{ Id => RValue }, Heap),\n" ^
    indent 2 "HL = lists:sort(?SORTHEAP, maps:to_list(NewHeap)),\n" ^
    indent 2 "case lists_loop(fun (L) -> case L of\n" ^
    concat_map "" (fun (root, currents, lasts) ->
      let bind (cs, ls) = "#{" ^ String.concat ", " (
        List.map (fun id -> id ^ " := " ^ sig_var id) cs @
        List.map (fun id -> id ^ " := " ^ last_sig_var id) ls)
      ^ "}" in
      let other_vars =
        let sub l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
        (sub dep.input_current currents, sub dep.input_last lasts)
      in
      let body n =
        indent n "Curr = " ^ erlang_of_expr newenv expr ^ ",\n" ^
        concat_map "" (indent n) (
          List.map (fun i -> 
            (* i ^ " ! {" ^ id ^ ", Curr, Version},\n" ^ *)
            "lists:foreach(fun (V) -> " ^ i ^ " ! {" ^ id ^ ", Curr, V} end, [Version|Deferred]),\n" ) output
        ) ^
        indent n "{break, {Version, Map}};\n"
      in
      indent 3 "{ {" ^ root ^ ", _} = Version, " ^ bind (currents, lasts) ^ " = Map} ->\n" ^
      match other_vars with
      | ([],[]) -> body 4
      | others ->
        indent 4 "case Last of\n" ^
				indent 5 (bind others) ^ " -> \n" ^
        body 6 ^
        indent 5 "_ -> {break, {pend, Version, Map}}\n" ^
        indent 4 "end;\n"
    ) root_group ^
    indent 3 "_ -> continue\n" ^
    indent 2 "end end, HL) of\n" ^
    indent 3 "false -> " ^ id ^ "(NewHeap, Last, Deferred);\n" ^
    indent 3 "{pend,V,M} -> " ^ id ^ "(maps:remove(V, NewHeap), maps:merge(Last, M), [V|Deferred]);\n" ^
    indent 3 "{V,M} -> " ^ id ^ "(maps:remove(V, NewHeap), maps:merge(Last, M), [])\n" ^
    indent 2 "end\n" ^
    indent 1 "end."
  | Const ((id, _), e) -> 
    renv := M.add id ("?" ^ const id) env;
    "-define(" ^ const id ^ ", " ^ erlang_of_expr env e ^ ")."
  | Fun ((id, _), e) ->
    id ^ erlang_of_expr env e ^ "."

let init_values x ti =
  let rec of_type = let open Type in function
    | TUnit -> EConst(CUnit)
    | TBool -> EConst(CBool(false))
    | TInt  -> EConst(CInt(0))
    | TFloat -> EConst(CFloat(0.0))
    | TChar -> EConst(CBool(false))
    | TTuple ts -> ETuple(List.map of_type ts)
    | _ -> assert false in
  let collect m = function 
    | Node((i, _), Some(init), _) -> M.add i init m
    | Node((i, _), None, _) -> M.add i (of_type (Typeinfo.find i ti)) m
    | _ -> m
  in
  let ins = List.fold_left (fun m (i,_) -> M.add i (of_type (Typeinfo.find i ti)) m) M.empty x.in_node in
  List.fold_left collect ins x.definition

let list_func = String.concat "\n" [
  "lists_loop (Fun, [H|L]) ->";
  indent 1 "case Fun(H) of";
  indent 2 "{break, V} -> V;";
  indent 2 "continue -> lists_loop(Fun, L)";
  indent 1 "end;";
  "lists_loop (_, []) -> false."
]

let sort_func = String.concat "\n" [
  "-define(SORTHEAP, fun ({{K1, V1}, _}, {{K2, V2}, _}) -> if";
  indent 1 "V1 == V2 -> K1 < K2;";
  indent 1 "true -> V1 < V2";
  "end end)."
]

let in_func x = String.concat "\n" @@
  "in() ->" :: List.map (indent 1) (
    "%fill here" ::
    List.map (fun (i, _) -> "% " ^ i ^ " ! sth") x.in_node @
    ["in()."]
  )

let out_func x = String.concat "\n" @@
  "out() ->" :: List.map (indent 1) [
    "receive";
    (concat_map ";\n" (fun (i, _) -> indent 1 "{" ^ i ^ ", Value, Version} -> Value") x.out_node);
    "end,";
    "out()."
  ]

let of_xmodule x ti template debug_flg = 
  let dep = Dependency.get_graph x in
  let attributes = 
    [[("main", 0)]; [("in", 0); ("out", 0)];
     List.map (fun (i,_) -> (i, 1)) x.in_node;
     List.fold_left (fun l e -> match e with
     | Node ((id, _), _, _) -> (id, 3) :: l
     | Fun ((id, _), EFun(args, _))  -> (id, List.length args) :: l
     | _ -> l
     ) [] x.definition
     ]
  in
  let exports = (concat_map "\n" (fun l -> "-export([" ^ 
      (concat_map "," (fun (f,n) -> f ^ "/" ^ string_of_int n) l)
    ^ "]).") attributes) in
  let env = M.bindings dep
            |> List.fold_left (fun env (id, _) -> M.add id (last_sig_var id) env) M.empty
  in
  String.concat "\n\n" (
    ("-module(" ^ String.lowercase_ascii x.id ^ ").") ::
    exports ::
    sort_func ::
    list_func ::
    main dep x (init_values x ti) env ::
    (match template with
      | Some s -> [s]
      | None   -> [in_func x; out_func x])
    (* outfunc *)
    @ (List.map (in_node dep) x.in_node)
    @ (let renv = ref env in List.map (def_node x dep renv debug_flg) x.definition)
  )
