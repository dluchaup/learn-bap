open Core_kernel.Std
open Bap.Std
open Sys
open Format
open Graph


module MyVertex = struct (* see Sig.COMPARABLE *) 
(*  type t = string let compare = String.compare *)
  type t = int let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end;;

module MyEdge = struct (* see Sig.ORDERED_TYPE_DFT *) 
  type t = string 
  let compare =  String.compare
  let default = ""
end;;

module G = struct
  include Persistent.Digraph.ConcreteLabeled(MyVertex)(MyEdge)
  (*
  let of_raw_list proj raw_call_list = 
    List.map raw_call_list
      ~f:(fun (addr0,addr1,addr2) ->
          dbgprint3addr t addr0 addr1 addr2;
          ((containing_func proj addr0), addr1, (containing_func proj addr2)))
    *)  
    
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let get_subgraph  _ = None
  let default_edge_attributes _ = []
  let edge_attributes (_,l,_) = [`Label l] (* may want to use sprintf *)
  let graph_attributes _ = []
  (* let vertex_name v = sprintf "%S" v *)
  let vertex_name v = sprintf "%S" (Int.to_string v)
end;;

module DotCG = Graph.Graphviz.Dot(G);;

let to_dot filename g = Out_channel.with_file filename
    ~f:(fun out -> DotCG.output_graph out g)

module SccCG = Components.Make(G)

module MergeCG = Merge.P(G);;
(***********)

let g = G.empty
let g = G.add_vertex g 1
let g = G.add_edge_e g (G.E.create 1 "12" 2)
let g = G.add_edge_e g (G.E.create 2 "23" 3)
let g = G.add_edge_e g (G.E.create 1 "14" 4)
let g = G.add_edge_e g (G.E.create 1 "15" 5)
let g = G.add_edge_e g (G.E.create 3 "35" 5)
let g = G.add_edge_e g (G.E.create 4 "43" 3)
let g = G.add_edge_e g (G.E.create 4 "45" 5)


(* persistent g *)
    
let () = to_dot "res-try5-g.dot" g
  

let g3 = G.add_edge_e g (G.E.create 1 "16" 6)
let g3 = G.add_edge_e g3 (G.E.create 1 "106" 6)
    
let () = to_dot "res-try5-g3.dot" g3

module CG = Components.Make(G)

let scc2string g =
  let vertices = G.fold_vertex (fun v acc -> v::acc) g [] in
  let (num_scc, vertex2scc) = CG.scc g in
  let v2s_list = List.map vertices ~f:(fun v -> (v, vertex2scc v)) in
  "[#"^Int.to_string(num_scc)^"]"^
  List.to_string ~f:(fun (v,s) -> (Int.to_string(v)^":"^Int.to_string(s)))
    v2s_list
    
let () = print_endline ("SCC of g:"^(scc2string g))



let g4 = G.add_edge_e g3 (G.E.create 3 "31" 1) (* back edge *)
    
let () = to_dot "res-try5-g4.dot" g4

let () = print_endline ("SCC of g4:"^(scc2string g4))

module MPG = Merge.P(G);;
let mg12 = MPG.merge_vertex g [1;2];;

let () = to_dot "res-try5-mg.12.dot" mg12
