open Syntax
open Dependency

let proc_var s = "P" ^ s
let signal_var s = "S" ^ s

let main deps xmod = 
  let nodes = M.bindings deps |> List.map fst in
  let spawn = List.map (fun n ->
    let outs = S.elements (M.find n deps).outs in
    let nulls = List.map (fun _ -> "null") outs |> String.concat "," in
    "\t" ^ (proc_var n) ^ " = spawn(fun -> " ^ n ^ "(null, " ^
                                            "{" ^ nulls ^ "}, " ^
                                            "{" ^ nulls ^ "})),\n"
  ) nodes |> String.concat "" in
  let call = List.map (fun n ->
    let ins = S.elements (M.find n deps).ins in
    "\t" ^ (proc_var n) ^ " ! {'@set',{" ^ String.concat ", " (List.map proc_var ins)  ^ "},\n" 
  ) nodes |> String.concat "" in
  "main() -> \n" ^
  spawn ^ call ^
  "\tin({" ^ String.concat "," (List.map (fun (i, _) -> proc_var i) xmod.in_node) ^ "})."

let node (Node ((id, _), init, e)) dep = 
  let in_ids = S.elements dep.ins |> List.map signal_var in
  let out_ids = S.elements dep.outs |> List.map proc_var in
  id ^ "(Last, {" ^ String.concat ", " in_ids ^ "}, {" ^ String.concat ", " out_ids ^ "}) ->" ^
  "\treceive\n" ^
  "\t\t{'@set', Procs} -> " ^ id ^ "(Last, { " ^ String.concat "," in_ids ^ " }, Procs);" ^
  "\n\tend."

let node _ _ = assert false

let of_xmodule x = 
  let dep = get_graph x in
  let infunc = 
    let arg = String.concat "," (List.map (fun (i, _) -> proc_var i) x.in_node) in
    "in({" ^ arg ^ "}) -> \n\t%fill here\t\n\tin({" ^ arg ^ "})."
  in
  String.concat "\n\n" [
    "module " ^ x.id; 
    main dep x;
    infunc
  ]
