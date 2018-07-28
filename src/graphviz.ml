open Syntax
open Module

(* TODO: Fix this after `codegen` works.
let concat_map = Codegen.concat_map
let indent = Codegen.indent
*)
let concat_map s f l = String.concat s (List.map f l)
let indent n s = String.make n '\t' ^ s

let colors =
  ["#1f77b4"; "#ff7f0e"; "#2ca02c"; "#d62728"; "#9467bd"; "#8c564b"; "#e377c2"; "#7f7f7f"; "#bcbd22"; "#17becf"]

module PairS = Set.Make(struct type t = string * string let compare = compare end)

let loop_table loops = 
  List.fold_left (fun set loop ->
    List.fold_left2 (fun set s t -> PairS.add (s,t) set) set loop (List.tl loop @ [List.hd loop])
  ) PairS.empty loops

let of_xmodule xmod = 
  let graph = Dependency.get_graph xmod in
  let deps = Dependency.M.bindings graph in
  let loops = loop_table (Dependency.find_loop xmod.source graph) in
  let def key = 
    key ^ " [label=\"" ^ key ^ "\"" ^
    (if List.mem key xmod.source then ", shape = \"invhouse\"" else "") ^
    (if List.mem key xmod.sink then 
     ", style = filled, shape = invtriangle, fillcolor = \"#e4e4e4\"" else "") ^
    "];" in
  let def_subgraph key label color nodes = 
    ["subgraph " ^ key ^ " {"] @
    indent 1 ("label=\"" ^ label ^ "\"; color=\"" ^ color ^ "\"; fontcolor=\"" ^ color ^ "\";") ::
    List.map (indent 1) nodes @
    ["}"] in
  let edge (key, dep) =
    List.map (fun i -> 
      if (PairS.mem (i,key) loops) then
        i ^ " -> " ^ key ^ " [color = red];"
      else
        i ^ " -> " ^ key ^ ";"
    ) (dep.Dependency.input_current) @
    List.map (fun i -> i ^ " -> " ^ key ^ " [style = dashed];") (dep.Dependency.input_last)
  in
  "digraph " ^ xmod.id ^ " {\n" ^
    (* concat_map "\n" (indent 1) (List.map def deps) ^ "\n\n" ^ *)
    String.concat "\n" (List.mapi (fun i (host, ids) -> 
      concat_map "\n" (indent 1) @@
      def_subgraph
        ("cluster_" ^ string_of_int i) (string_of_host host)
        (List.nth colors (i mod 10)) (List.map def ids)
    ) xmod.hostinfo) ^ "\n" ^
    concat_map "\n" (indent 1) (List.map edge deps |> List.flatten) ^
    (*
    indent 1 "{ rank = source; " ^ String.concat "; " xmod.source ^ "; }\n" ^
    indent 1 "{ rank = sink; " ^ String.concat "; " xmod.sink ^ "; }" ^
    *)
  "\n}"