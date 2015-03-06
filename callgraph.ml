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
                           
  let iter_vertex _ _ = ()
  let iter_edges_e  _ _ = ()
                          
  (** Graph, vertex and edge attributes. *)
    
  let default_vertex_attributes _ = []
  let vertex_name vt = vt
  let vertex_attributes _ = []
      
  let get_subgraph  _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []

end
