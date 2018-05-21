open Syntax
open Module

(* TODO: Fix this after `codegen` works.
let concat_map = Codegen.concat_map
let indent = Codegen.indent
*)
let concat_map s f l = String.concat s (List.map f l)
let indent n s = String.make n '\t' ^ s

let of_xmodule xmod = 
  let deps = Dependency.M.bindings (Dependency.get_graph xmod) in
  let def (key, _) = 
    key ^ " [label=\"" ^ key ^ "\"" ^
    (if List.mem key xmod.source then ", shape = \"invhouse\"" else "") ^
    (if List.mem key xmod.sink then 
     ", style = filled, shape = invtriangle, fillcolor = \"#e4e4e4\"" else "") ^
    "];" in
  let edge (key, dep) =
    List.map (fun i -> i ^ " -> " ^ key ^ ";") (dep.Dependency.input_current) @
    List.map (fun i -> i ^ " -> " ^ key ^ " [style = dashed];") (dep.Dependency.input_last)
  in
  "digraph " ^ xmod.id ^ " {\n" ^
    concat_map "\n" (indent 1) (List.map def deps) ^ "\n\n" ^
    concat_map "\n" (indent 1) (List.map edge deps |> List.flatten) ^ "\n\n" ^
    indent 1 "{ rank = source; " ^ String.concat "; " xmod.source ^ "; }\n" ^
    indent 1 "{ rank = sink; " ^ String.concat "; " xmod.sink ^ "; }" ^
  "\n}"