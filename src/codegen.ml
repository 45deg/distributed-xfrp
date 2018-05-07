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
  let nodes = M.bindings deps |> List.map fst in
  let spawn = concat_map "" (fun n ->
    if List.exists (fun (i, _) -> i == n) xmod.in_node then (* refactor needed *)
      indent 1 "register(" ^ n ^ ", spawn(?MODULE, " ^ n ^ ", [])),\n"
    else
      let init i = try_find i inits |> erlang_of_expr env in
      let ins = S.elements (try_find n deps).ins in
      indent 1 "register(" ^ n ^ ", " ^
      "spawn(?MODULE, " ^ n ^ ", [{" ^ (concat_map "," init ins) ^ "}])),\n"
  ) nodes in
  "main() -> \n" ^
  spawn ^
  indent 1 "in()."

let in_node deps (id, _) = 
  id ^ "() ->\n" ^ 
  concat_map "\n" (indent 1) [
  "receive";
  "Value ->";
    S.elements ((try_find id deps).outs) |>
    concat_map ";\n" (fun s -> indent 1 s ^ " ! {" ^ id ^ ", Value}");
  indent 1 "end,";
  id ^ "()."]


let def_node deps renv = 
  let env = !renv in function
  | Node ((id, _), init, expr) -> 
    let dep = (try M.find id deps with Not_found -> (raise (UnknownId id))) in
    let in_ids = S.elements dep.ins in
    let out_ids = S.elements dep.outs in
    id ^ "({" ^ (concat_map "," last_sig_var in_ids) ^ "}) ->\n" ^
    indent 1"receive\n" ^
    (concat_map ";\n" (fun in_id ->
      let newenv = env |> M.add in_id (sig_var in_id) in
      indent 2 "{" ^ in_id ^ ", " ^ sig_var in_id ^ "} when " ^ sig_var in_id ^ " /= " ^ last_sig_var in_id ^ " ->\n" ^
      (concat_map ",\n" (indent 3) (
        ["Curr = " ^ erlang_of_expr newenv expr] @
        List.map (fun i -> i ^ " ! " ^ "{" ^ id ^ ", Curr}") out_ids @
        [id ^ "({" ^ (concat_map "," (fun i -> if i == id then "Curr" else try_find i newenv) in_ids) ^ "})"]
      ))
    ) in_ids) ^ "\n" ^
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
 
let in_func x = String.concat "\n" @@
  "in() ->" :: List.map (indent 1) (
    "%fill here" ::
    List.map (fun (i, _) -> "% " ^ i ^ " ! sth") x.in_node @
    ["in()."]
  )

let out_func x = String.concat "\n" @@
  "out() ->" :: List.map (indent 1) [
    "receive";
    (concat_map ";\n" (fun (i, _) -> indent 1 "{" ^ i ^ ", Value} -> Value") x.out_node);
    "end,";
    "out()."
  ]

let of_xmodule x ti = 
  let dep = Dependency.get_graph x in
  let attributes = 
    [[("main", 0)]; [("in", 0); ("out", 0)];
     List.map (fun (i,_) -> (i, 0)) x.in_node;
     List.fold_left (fun l e -> match e with
     | Node ((id, _), _, _) -> (id, 1) :: l
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
    main dep x (init_values x ti) env ::
    in_func x ::
    out_func x ::
    (* outfunc *)
    (List.map (in_node dep) x.in_node)
    @ (let renv = ref env in List.map (def_node dep renv) x.definition)
  )
