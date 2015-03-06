open Core_kernel.Std
open Bap.Std
open Program_visitor

module Callgraph = struct
  type t=project
  module V = struct
    type t = string
  end
  module E = struct
    type t = string*string
    let src = fst
    let dst = snd
  end
  (* ... all other functions. Example: *)
  let graph_attributes _ = []
                           
  (* let iter_vertex _ _ = () *)
  let iter_vertex f t = Table.iter t.symbols ~f
                        
  let iter_edges_e  _ _ = ()
  let vertex_name v =
    let quote = sprintf "%S" in
    quote v
  
                          
  (** Graph, vertex and edge attributes. *)
    
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
      
  let get_subgraph  _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []

end

let main p =
  print_endline "START";
  let module Dot = Graph.Graphviz.Dot(Callgraph) in
  Out_channel.with_file "callgraph.dot"
    ~f:(fun out -> Dot.output_graph out p);
  p

let () = register main
