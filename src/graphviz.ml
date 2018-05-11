open Syntax

let concat_map = Codegen.concat_map
let indent = Codegen.indent

let of_xmodule xmod = 
  let deps = Dependency.M.bindings (Dependency.get_graph xmod |> Dependency.M.remove "out") in
  let (ins, outs) = (List.map fst xmod.in_node, List.map fst xmod.out_node) in
  let def (key, _) = 
    key ^ " [label=\"" ^ key ^ "\"" ^
    (if List.mem key ins then ", shape = \"invhouse\"" else "") ^
    (if List.mem key outs then 
     ", style = filled, shape = invtriangle, fillcolor = \"#e4e4e4\"" else "") ^
    "];" in
  let edge (key, dep) =
    List.map (fun i -> i ^ " -> " ^ key ^ ";") (Dependency.S.elements dep.Dependency.ins)
  in
  "digraph " ^ xmod.id ^ " {\n" ^
    concat_map "\n" (indent 1) (List.map def deps) ^ "\n\n" ^
    concat_map "\n" (indent 1) (List.map edge deps |> List.flatten) ^ "\n\n" ^
    indent 1 "{rank = source; " ^ String.concat ";" ins ^ "; }" ^
    indent 1 "{rank = sink; " ^ String.concat ";" outs ^ "; }" ^
  "\n}"