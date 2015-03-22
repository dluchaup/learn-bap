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
  (* module VertexMap = Map.Make(Int) *)
  module VSet = Set.Make(Int)
end;;

module MyEdge = struct (* see Sig.ORDERED_TYPE_DFT *) 
  type t = string 
  let compare =  String.compare
  let default = ""
end;;

module G = struct
  include Persistent.Digraph.ConcreteLabeled(MyVertex)(MyEdge)
  module VSet = MyVertex.VSet
  (** for dotty **)
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
  (**************)
  let succ_set t f =  VSet.of_list (succ t f) (* Set of g s.t. f->g *)
  let pred_set t f =  VSet.of_list (pred t f) (* Set of g s.t. g->f *)

  type direction = Forward | Backward
  (* Find all g such that f->g for some f in fset *)
  let get_set_targets t fset dir =
    let get_targets = match dir with
      | Forward -> succ_set | Backward -> pred_set in
    VSet.fold fset ~init:VSet.empty
      ~f:(fun  acc f -> VSet.union acc (get_targets t f))
      
  (* fset is a set of functions.
       Find a k+1-chain of sets fset=fset_0->fset_1->fset_2->...->fset_k
       For convenience, the first element is the original one.
       So, for k calls, the chain has k+1 elements
  *)
  let rec get_k_call_sets t k fset dir =
    if (k < 0 || VSet.is_empty fset) then []
    else if (k = 0) then  [fset]
    else let targets = get_set_targets t fset dir in
      if (VSet.is_empty targets) then []
      else
        let tail = get_k_call_sets t (k-1) targets dir in
        if (tail=[]) then []
        else let () = assert (List.length tail = k) in
          fset::tail

  (** debugging and printing **)
  let vset_list_to_sexp = <:sexp_of<(VSet.t list)>>;;
  let vset_list_to_string dag = Sexp.to_string (vset_list_to_sexp dag);;
  let vset_to_string vs = Sexp.to_string (VSet.sexp_of_t vs)
end;;

module DotCG = Graph.Graphviz.Dot(G);;

let to_dot filename g = Out_channel.with_file filename
    ~f:(fun out -> DotCG.output_graph out g)

module SccCG = Components.Make(G)

module MergeCG = Merge.P(G);;
