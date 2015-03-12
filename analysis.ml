open Core_kernel.Std
open Bap.Std
open Program_visitor

module Analysis = struct
  type t=project
  (* type location = Addr.t *)
  (*
     Assume an executable which we want to analyze and
     where the following calls happen:
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
     Example:
     (f,g,i1)
     (f,h,i2)
     (f,g,i3)
     (h,g,i4)
     (g,g,i5)

     CALL GRAPH:
     The call graph is a directed graph with an edge for every call instruction
        i:f->g.
     This edge goes from f to g and is labeled with i= the PC of the call instr.
     We represent the directed call graph as a mapping:
     {caller1: {(callee1: [list;of;call;locations]);
                (callee2: [list;of;call;locations])
                ...
               }
      caller2: {(callee1': [list;of;call;locations]);
                (callee2': [list;of;call;locations])
                ...
               }
     ...
     }
     Example: the call graph for the above example is:
      {f: { (g:[i1,i3]); (h:[i2]) }
       h: { (g:[i4]) }
       g: { (g:[i5]) }
      }

     REVERSE CALL GRAPH:
     This is data structure used for convenience. Structurally it is the call
     graph with the edges reversed.
     Example: the reverse call graph for the above example is:
     {g:{(f:[i1,i3]);(h:[i4]);(g:[i5])}
      h:{f:[i2]}
     }
   *)

    let gather_call_list t =
      let call_list = ref ([]:(string*string*Addr.t) list) in
      call_list := [];
      Table.iter t.symbols ~f:(fun s -> printf "Symbol %s\n" s);
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
      !call_list

    let print_call_list cl =
      List.iter cl
        ~f:(fun (s,d,l) -> printf "0x%xd: %s -> %s\n"
               (ok_exn ((Addr.(to_int l)))) s d)

 (* ********************************************************************** *)
  module CG = struct 
    module NodeInfo = struct
      type callee2locs = Addr.t list String.Map.t
      type t = callee2locs
      let empty:t = String.Map.empty
      let add_call t callee loc =
        match String.Map.find t callee with
          None -> String.Map.add t ~key:callee ~data:[loc]
        | Some l -> String.Map.add (String.Map.remove t callee) ~key:callee ~data:(loc::l)

      let l_to_string ll =
       (List.fold ~init:"[" ~f:(fun acc l -> acc^";"^(Addr.to_string l)) ll)^"]"

      let to_string ?(sep="") t = 
        (String.Map.fold t ~init:"{"
           ~f:(fun ~key:callee ~data:lst acc -> acc^callee^(l_to_string lst)^sep))^"}"
    end
    
    type t = NodeInfo.t String.Map.t
    let empty:t = String.Map.empty
                    
    let add_call t (s,d,l) =
      match String.Map.find t s with
        None -> String.Map.add t ~key:s ~data:(NodeInfo.add_call NodeInfo.empty d l)
      | Some ni -> String.Map.add (String.Map.remove t s)
                     ~key:s ~data:(NodeInfo.add_call ni d l);;

    let to_string ?(in_sep="") ?(out_sep="") t = 
      (String.Map.fold t ~init:"{"
         ~f:(fun ~key:caller ~data:ni acc ->
             acc^caller^(NodeInfo.to_string ni ~sep:in_sep)^out_sep))^"}"

    let from_call_list cl =
      List.fold cl ~init:empty
        ~f:(fun acc y -> add_call acc y);;

    let dbg_test () =
      print_endline "test";
  end

  let main t =
    let call_list = gather_call_list t in
    print_call_list call_list;
    let cg = CG.from_call_list call_list in
    print_endline (CG.to_string cg ~in_sep:"\n" ~out_sep:"\n\t");
    let rcg = CG.from_call_list (List.map call_list ~f:(fun (s,d,i)->(d,s,i))) in
    print_endline (CG.to_string rcg ~in_sep:"\n" ~out_sep:"\n\t");
    ()
    (* CG.dbg_test() *)

end

let main p =
  print_endline "START";
  let module Dot = Analysis in
  Dot.main p;
  p

let () = register main
