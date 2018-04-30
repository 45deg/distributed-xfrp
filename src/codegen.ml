open Syntax
open Dependency

module S = Set.Make(String);;
module M = Map.Make(String);;

let var s = "V" ^ s
let sig_var s = "S" ^ s
let last_sig_var s = "LS" ^ s
let at_last var = if var.[0] == 'S' then "L" ^ var else var

let const s = String.uppercase_ascii s

exception NotImplemented

let erlang_of_expr env e = 
  let rec f env = function
    | EConst CUnit -> "()"
    | EConst (CBool b) -> string_of_bool b
    | EConst (CInt i)  -> string_of_int i
    | EConst (CFloat f)  -> Printf.sprintf "%f" f
    | EConst (CChar c) -> "?" ^ Char.escaped c
    | EId id -> M.find id env
    | EAnnot (id, ALast) -> at_last (M.find id env)
    | EApp (id, es) -> (* workaround *)
      id ^ "(" ^ String.concat "," (List.map (f env) es) ^ ")"
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
      let vars = List.map (fun (i,e,_) -> var i ^ "=" ^ f env e) binders |> String.concat "," in
      let newenv = List.fold_left (fun env (i,_,_) -> M.add i (var i) env) env binders in
      vars ^ "," ^ f newenv e
    | EIf(c, a, b) -> 
      "(case " ^ f env c ^ " of true -> " ^ f env a ^ "; false -> " ^ f env b ^ " end)"
    | ETuple es ->
      "{" ^ String.concat "," (List.map (f env) es) ^ "}"
    | EFun (args, e) ->
      "(" ^ String.concat ", " args ^ ") -> \n" ^ f env e
    | ECase(e, list) -> raise NotImplemented
  in f env e

let main deps xmod inits env = 
  let nodes = M.bindings deps |> List.map fst in
  let spawn = List.map (fun n ->
    if List.exists (fun (i, _) -> i == n) xmod.in_node then (* refactor needed *)
      "\tregister(" ^ n ^ ", spawn(?MODULE, " ^ n ^ ", [])),\n"
    else
      let init i = M.find i inits |> erlang_of_expr env in
      let ins = S.elements (M.find n deps).ins in
      "\tregister(" ^ n ^ ", " ^
      "spawn(?MODULE, " ^ n ^ ", [" ^ init n ^", {" ^ String.concat "," (List.map init ins) ^ "}])),\n"
  ) nodes |> String.concat "" in
  "main() -> \n" ^
  spawn ^
  "\tin()."

let in_node deps (id, _) = 
  id ^ "() ->\n" ^ 
  String.concat "\n" (List.map (fun s -> "\t" ^ s) [
  "receive";
  "Value ->";
    S.elements ((M.find id deps).outs) |>
    List.map (fun s -> "\t" ^ s ^ " ! {" ^ id ^ ", Value}") |>
    String.concat ";\n";
  "\tend,";
  id ^ "()."])

let def_node deps env = function
  | Node ((id, _), init, expr) ->
    let dep = M.find id deps in
    let in_ids = S.elements dep.ins in
    let out_ids = S.elements dep.outs in
    id ^ "(" ^ last_sig_var id ^ ",{" ^ String.concat "," (List.map last_sig_var in_ids) ^ "}) ->\n" ^
    "\treceive\n" ^
    String.concat ";\n" (List.map (fun in_id ->
      let newenv = env |> M.add in_id (sig_var in_id) in
      "\t\t{" ^ in_id ^ ", " ^ sig_var in_id ^ "} when " ^ sig_var in_id ^ " /= " ^ last_sig_var in_id ^ " ->\n" ^
      String.concat ",\n" (List.map (fun s -> "\t\t\t" ^ s) (
        [sig_var id ^ " = " ^ erlang_of_expr newenv expr] @
        List.map (fun i -> i ^ "!" ^ "{" ^ id ^ "," ^ sig_var id ^ "}") out_ids @
        [id ^ "(" ^ sig_var id ^ ", {" ^ String.concat "," (List.map (fun i -> M.find i newenv) in_ids) ^ "})"]
      ))
    ) in_ids) ^
    "\n\tend."
  (*| Const ((id, _), e) -> 
    "-define(" ^ const id ^ ", " ^ erlang_of_expr e ^ ")."
  | Fun ((id, _), e) ->
    id ^ erlang_of_expr e ^ "."*)

let init_values x ti =
  let of_type = let open Type in function
    | TUnit -> EConst(CUnit)
    | TBool -> EConst(CBool(false))
    | TInt  -> EConst(CInt(0))
    | TFloat -> EConst(CFloat(0.0))
    | TChar -> EConst(CBool(false))
    | _ -> assert false in
  let collect m = function 
    | Node((i, _), Some(init), _) -> M.add i init m
    | Node((i, _), None, _) -> M.add i (of_type (TypeInfo.find i ti)) m
    | _ -> m
  in
  let ins = List.fold_left (fun m (i,_) -> M.add i (of_type (TypeInfo.find i ti)) m) M.empty x.in_node in
  List.fold_left collect ins x.definition
 
let of_xmodule x ti = 
  let dep = Dependency.get_graph x in
  let attributes = 
    [[("main", 0)]; [("in", 0); ("out", 0)];
     List.map (fun (i,_) -> (i, 0)) x.in_node;
     List.fold_left (fun l e -> match e with
     | Node ((id, _), _, _) -> (id, 2) :: l
     | Fun ((id, _), EFun(args, _))  -> (id, List.length args) :: l
     | _ -> l
     ) [] x.definition
     ]
  in
  let env = M.bindings dep
            |> List.fold_left (fun env (id, _) -> M.add id (last_sig_var id) env) M.empty
  in
  String.concat "\n\n" ([
    "-module(" ^ String.lowercase_ascii x.id ^ ")."; 
    String.concat "\n" 
      (List.map (fun l -> "-export([" ^ 
        String.concat "," (List.map (fun (f,n) -> f ^ "/" ^ string_of_int n) l)
       ^ "]).") attributes);
    main dep x (init_values x ti) env;
    (* infunc *)
    "in() -> \n" ^
    "\t%fill here\n" ^
    String.concat "" 
      (List.map (fun (i, _) -> "\t% " ^ i ^ " ! sth\n") x.in_node) ^
    "\tin().";
    (* outfunc *)
    "out() -> \n" ^
    "\treceive\n" ^
    String.concat ";\n"
      (List.map (fun (i, _) -> "\t\t{" ^ i ^ ", Value} -> Value") x.out_node) ^
    "\n\tend,\n" ^
    "\tout().";
  ] 
  @ (List.map (in_node dep) x.in_node)
  @ (List.map (def_node dep env) x.definition)
  )
