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
  module VMap = Map.Make(Int)
end;;

module MyEdge = struct (* see Sig.ORDERED_TYPE_DFT *) 
  type t = string with sexp
  let compare =  String.compare
  let default = ""
end;;

module G = struct
  include Persistent.Digraph.ConcreteLabeled(MyVertex)(MyEdge)
  module VSet = MyVertex.VSet
  module VMap = MyVertex.VMap

  type sedge = MyEdge.t with sexp
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

  (*************** helper functions ***************)
  let from_call_list cl =
    List.fold cl ~init:empty
      ~f:(fun acc y -> add_edge_e acc y);;
  
  (*************** helper functions ***************)
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

  (*************** k-strings ***************)
  (* Find a (k+1)-depth DAG of calls fset_1->fset_2->...->fset_k ->{f} *)
  (* We build it by using a backward and forward chain, and            *)
  (* by taking the intersection                                        *)
  let get_k_call_dag t k f =
    if k < 0 then []
    else
      let bkd = get_k_call_sets t k (VSet.singleton f) Backward in
      if (bkd = []) then []
      else let () = assert ((List.length bkd) = k + 1) in
        let rbkd = List.rev bkd in
        let hd = List.nth_exn rbkd 0 in
        let () = assert (not (Set.is_empty hd)) in
        let fwd = get_k_call_sets t k hd Forward in
        let () = assert (List.length fwd = List.length rbkd) in
        let extended_dag = List.map2_exn fwd rbkd ~f:Set.inter in
        (*let dag = match (extended_dag) with
          _hd::tail -> tail | [] -> failwith "Empty" (* should not happen *)
          in*)
        extended_dag

  (* A kstring is a list [i1; i2; i3]                                 *)
  (* A pstring is a pair (F,[kstring1;kstring2,...])                  *)
  (* A pstring list is a list of pstrings (F,[...]) w/o repeating F's *)
  (* This returns a pstrings list from dag                            *)
  let rec get_dag_pstrings t dag =
    match dag with
    | [] -> [] (* failwith("Unreachable: should not be empty"); *)
    | [fset] -> assert(1 = (Set.length fset));
      let f = Set.choose_exn fset in [(f,[[]])]
    | fset::tail ->
      let expand_src_dst src (dst, kstring_list) =
        let edges = find_all_edges t src dst  in
        List.fold edges ~init:[]
          ~f:(fun acc label ->
              List.append acc
                (List.fold kstring_list ~init:[]
                   ~f:(fun acc kstring -> (label::kstring)::acc))
            )
      in
      let expand_src src ps_tail =
        List.fold ps_tail ~init:[]
          ~f:(fun acc dst_pair ->
              List.append acc (expand_src_dst src dst_pair)
            )
      in
      let ps_tail = get_dag_pstrings t tail in
      Set.fold fset ~init:[]
        ~f:(fun acc src ->
            (src, (expand_src src ps_tail))::acc 
          )

  let get_k_call_strings t k f =
    if k = 0 then [[]]
    else
      let dag = get_k_call_dag t k f in
      List.fold (get_dag_pstrings t dag) ~init:[]
        ~f:(fun acc (_src, kstring_list)-> List.append acc kstring_list)
        
  let get_k_call_strings_map t k =
    fold_vertex (fun f acc -> VMap.add acc
                    ~key:f ~data:(get_k_call_strings t k f)
                )  t VMap.empty;;
  
  (** debugging and printing **)
  let sexp_of_edge e = sexp_of_sedge e
  let vset_list_to_sexp = <:sexp_of<(VSet.t list)>>;;
  let vset_list_to_string dag = Sexp.to_string (vset_list_to_sexp dag);;
  let vset_to_string vs = Sexp.to_string (VSet.sexp_of_t vs);;
  let kstrings_map_to_sexp = <:sexp_of<(edge list list VMap.t)>>;;
end;;

module DotCG = Graph.Graphviz.Dot(G);;

let to_dot filename g = Out_channel.with_file filename
    ~f:(fun out -> DotCG.output_graph out g)

module SccCG = Components.Make(G)

module MergeCG = Merge.P(G);;
