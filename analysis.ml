open Core_kernel.Std
open Bap.Std
open Program_visitor
open OUnit
    
module Analysis = struct
  type t=project

  (* type location = Addr.t *)
  (*
     Example1: Assume an executable which we want to analyze and where the
               following calls happen:
     i1: f->g // i.e. There is a "call g" at address i1 inside function f
     ...
     i2: f->h
     ...
     i3: f->g
     .......
     i4: h->g
     .......
     i5: g->g
     
     CALL LISTS:
     A call list is just a list of all calls; i.e. labeled edges in a call graph
     The call list for Example1 is:
     (f,g,i1)
     (f,h,i2)
     (f,g,i3)
     (h,g,i4)
     (g,g,i5)
     *)
  let call_compare (s1,d1,i1) (s2,d2,i2)=
    if(      (String.compare s1 s2) <> 0) then (String.compare s1 s2)
    else if ((String.compare d1 d2) <> 0) then (String.compare d1 d2)
    else (Addr.compare i1 i2)
  let determinize_call_list cl =
    (List.sort ~cmp:(call_compare) cl)
  let gather_call_list t =
    let call_list = ref ([]:(string*string*Addr.t) list) in
    call_list := []; (* TBD: use fold *)
    Table.iteri t.symbols ~f:(fun mem0 src ->
        let mseq =  Disasm.insns_at_mem t.program mem0 in
        Seq.iter mseq ~f:(fun (mem1, insn) ->
            Bil.iter (object inherit [unit] Bil.visitor
              method!enter_int addr () = if in_jmp then
                  match Table.find_addr t.symbols addr with
                  | None -> ()
                  | Some (mem2, dst) ->
                    if Addr.(Memory.min_addr mem2 = addr) then
                      call_list := List.append !call_list
                          [(src,dst, (Memory.min_addr mem1))]
            end) (Insn.bil insn)));
    (determinize_call_list !call_list)
      
    (*
     CALL GRAPH:
     We represent the call graph as a directed graph with a labeled edge for
     every call instruction
        i:f->g.
     This edge goes from f to g and is labeled with i= the PC of the call instr.
     We represent a directed labeled call graph via a mapping:
     {caller1: {
                (callee1: [list;of;call;locations]);(* label info for callee1 *)
                (callee2: [list;of;call;locations]) (* label info for callee2 *)
                ...
               }(* end of Adjacency Map for Caller1  *)
      caller2: {
                (callee1: [list;of;call;locations]);
                (callee3: [list;of;call;locations])
                ...
               }(* end of Adjacency Map for Caller2  *)
     ...
     }
     Example: the call graph for Example1 is:
      {f: { (g:[i1,i3]); (h:[i2]) }
       h: { (g:[i4]) }
       g: { (g:[i5]) }
      }
   *)
    
  (***********************************************************************
     Labeled Directed Graph
    ******************************************************************** *)
  module LDG = struct
    module AdjacencyInfo = struct
      (* t maps each neighbor to a list of labeled edges to that neighbor   *)
      type t = Addr.t list String.Map.t (* TBD: make Addr.t a param *)
      let empty:t = String.Map.empty
      let add_call t callee loc = String.Map.add t ~key:callee
          ~data:(loc::(Option.value (String.Map.find t callee) ~default:[]))
                      
      let l_to_string ll =
        (List.fold ~init:"[" ~f:(fun acc l -> acc^";"^(Addr.to_string l)) ll)^"]"
        
      let to_string ?(sep="") t = 
        (String.Map.fold t ~init:("{"^sep)
           ~f:(fun ~key:callee ~data:lst acc -> acc^callee^(l_to_string lst)^sep))^"}"
    end
    
    type t = AdjacencyInfo.t String.Map.t
    let empty:t = String.Map.empty

    let add_call t (s,d,l) =
      String.Map.add t ~key:s
        ~data:(AdjacencyInfo.add_call
                 (Option.value (String.Map.find t s)
                    ~default: AdjacencyInfo.empty)
                 d l);;

    let to_string ?(in_sep="") ?(out_sep="") t = 
      (String.Map.fold t ~init:("{"^out_sep)
         ~f:(fun ~key:caller ~data:ni acc ->
             acc^caller^(AdjacencyInfo.to_string ni ~sep:in_sep)^out_sep))^"}"
      
    let from_call_list cl =
      List.fold cl ~init:empty
        ~f:(fun acc y -> add_call acc y);;
    
    (* Find all g such that f->g *)
    let get_targets t f =
      let ei = (Option.value (String.Map.find t f) ~default:String.Map.empty) in
      String.Map.fold ei ~init:String.Set.empty
        ~f:(fun  ~key:target ~data:_ acc -> String.Set.add acc target)
       
    (* Find all g such that f->g for some f in fset *)
    let get_set_targets t fset =
      String.Set.fold fset ~init:String.Set.empty
        ~f:(fun  acc f -> String.Set.union acc (get_targets t f))
        
    (* fset is a set of functions.
       Find a k+1-chain of sets fset=fset_0->fset_1->fset_2->...->fset_k
       For convenience, the first element is the original one.
       So, for k calls, the chain has k+1 elements
    *)
    let rec get_k_call_sets t k fset =
      if (k < 0 || Set.is_empty fset) then []
      else if (k = 0) then  [fset]
      else let targets = get_set_targets t fset in
        if (Set.is_empty targets) then []
        else
          let tail = get_k_call_sets t (k-1) targets in
          if (tail=[]) then []
          else let () = assert (List.length tail = k) in
            fset::tail
  end

  
  (***********************************************************************
     Enhanced Call Graph
    ******************************************************************** *)
  module ECG = struct
    type t = {
      (* ignore functions that are neither callers or callees               *)
      nodes   : string list;                     (* call sources or targets *)
      edges   : (string * string * Addr.t) list; (* directed edges in cg    *)
      cg  : LDG.t;                               (* directed graph          *)
      roots : string list;                       (* nodes w/o callers in cg *)
      rcg : LDG.t;                               (* reverse of cg           *)
      (*
       REVERSE CALL GRAPH:
       This data structure is used for convenience. Structurally it is the call
       graph with the edges reversed (from callees to callers, same labels).
       Example: the reverse call graph for the above example is:
       {g:{(f:[i1,i3]);(h:[i4]);(g:[i5])}
        h:{f:[i2]}
       }
      *)
    }
    
    let from_call_list call_list =
      let call_list = determinize_call_list call_list in
      let (callers,callees) =
        (List.fold call_list
           ~init:(String.Set.empty,String.Set.empty)
           ~f:(fun (ss,sd) (s,d,_) -> ((Set.add ss s), (Set.add sd d))))
      in
      {
        edges = call_list;
        cg = LDG.from_call_list call_list;
        rcg = LDG.from_call_list (List.map call_list ~f:(fun (s,d,i)->(d,s,i)));
        nodes = Set.to_list (Set.union callers callees);
        roots = Set.to_list (Set.diff callers callees);
      }

    let from_project proj = from_call_list (gather_call_list proj)
        
    (* Find a k-depth DAG of calls fset_1->fset_2->...->fset_k *)
    (* We build it by using a backward and forward chain, and  *)
    (* by taking the intersection                              *)
    let get_k_call_dag t k f =
      if k < 0 then []
      else
        let bkd = LDG.get_k_call_sets t.rcg k (String.Set.add String.Set.empty f)
        in
        if (bkd = []) then []
        else let () = assert ((List.length bkd) = k + 1) in
          let rbkd = List.rev bkd in
          let hd = List.nth_exn rbkd 0 in
          let () = assert (not (Set.is_empty hd)) in
          let fwd = LDG.get_k_call_sets t.cg k hd in
          let () = assert (List.length fwd = List.length rbkd) in
          List.map2_exn fwd rbkd ~f:Set.inter

    let call_dag_to_string dag =
      "{"^
      (List.fold
         dag
         ~init:""
         ~f:(fun acc fset ->
             acc^"["^
               (List.fold
                  (List.sort ~cmp:String.compare (Set.to_list fset))(*determinize*)
                  ~init:""
                  ~f:( fun acc f -> acc^f^",")
               )^"]"
           )
      )^"}"
      
        
  end

  let print_call_list cl =
    List.iter (determinize_call_list cl)
      ~f:(fun (s,d,l) -> printf "0x%xd: %s -> %s\n"
             (ok_exn ((Addr.(to_int l)))) s d)

      
  let main_test t =
    let ecg = ECG.from_project t in
    print_call_list ecg.edges;
    print_endline (LDG.to_string ecg.cg ~in_sep:"\n" ~out_sep:"\n\t");
    print_endline (LDG.to_string ecg.rcg ~in_sep:"\n" ~out_sep:"\n\t");
    ()
    
  let unit_test _t =
    let cl_example1 = [("f","g",Addr.of_int ~width:32 1);
                       ("f","h",Addr.of_int ~width:32 2);
                       ("f","g",Addr.of_int ~width:32 3);
                       ("h","g",Addr.of_int ~width:32 4);
                       ("g","g",Addr.of_int ~width:32 5)
                      ]; in
    let ecg = ECG.from_call_list cl_example1 in
    print_call_list ecg.edges;
    print_endline (LDG.to_string ecg.cg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
    print_endline (LDG.to_string ecg.rcg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
    let dag_g_0 = ECG.get_k_call_dag ecg 0 "g"  in
    print_endline (ECG.call_dag_to_string dag_g_0);
    let dag_g_1 = ECG.get_k_call_dag ecg 1 "g"  in
    print_endline (ECG.call_dag_to_string dag_g_1);
    let dag_g_2 = ECG.get_k_call_dag ecg 2 "g"  in
    print_endline (ECG.call_dag_to_string dag_g_2);
    ()
    
end

module TestAnalysis = struct
  let test_fixture = "MyTests" >::: [
      "add dag_g_0" >:: (fun () ->
          assert (4 = ((+) 2 2));
          assert (5 = ((+) 2 3));
          assert (6 = ((+) 4 2))
        )
    ]

  let run () = run_test_tt ~verbose:true test_fixture

end


let main p =
  Analysis.unit_test p;
  p
  
let () = register main
    
