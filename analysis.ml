open Core_kernel.Std
open Bap.Std
open Program_visitor

module Callgraph = struct
  type t=project
  type node = Int32
  type location = Int32
  (* type info = node*node* location list  *)
  type call = node*node* location 

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
                        
  let iter_edges_e  f t =
    Table.iteri t.symbols ~f:(fun mem src ->
        Disasm.insns_at_mem t.program mem |>
        Seq.iter ~f:(fun (_mem, insn) ->
            Bil.iter (object inherit [unit] Bil.visitor
                
                method!enter_int addr () = if in_jmp then
                    match Table.find_addr t.symbols addr with
                    | None -> ()
                    | Some (mem, dst) ->
                      if Addr.(Memory.min_addr mem = addr) then f(src,dst)
                  
            end) (Insn.bil insn)))
                               
    
  let vertex_name v =
    let quote = sprintf "%S" in
    quote v
  
                          
  (** Graph, vertex and edge attributes. *)
    
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
      
  let get_subgraph  _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []

  let main0 t =
    Table.iter t.symbols ~f:(fun s -> printf "Symbol %s\n" s);
    Table.iteri t.symbols ~f:(fun mem0 src ->
        Disasm.insns_at_mem t.program mem0 |>
        Seq.iter ~f:(fun (mem1, insn) ->
            Bil.iter (object inherit [unit] Bil.visitor
                
                method!enter_int addr () = if in_jmp then
                    match Table.find_addr t.symbols addr with
                    | None -> ()
                    | Some (mem2, dst) ->
                      if Addr.(Memory.min_addr mem2 = addr) then
                        printf "src=%s,dst=%s, addr[target]=%s ..mem1[instr]=%s.. mem0[src]=%s\n" src dst
                          (Addr.(to_string addr))
                          (Addr.(to_string Memory.(min_addr mem1)))
                          (Addr.(to_string Memory.(min_addr mem0)))
            end) (Insn.bil insn)))
      
  let print_call_list cl =
    List.iter cl ~f:(fun (s,d,i) -> printf "src=%s,dst=%s, call instr=%xd\n" s d i)
      
  let gather_call_list t =
    let dummy_call = ("f","g",1) in
    let call_list = ref [dummy_call] in
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
                            [(src,dst,(ok_exn ((Addr.(to_int Memory.(min_addr mem1))))))]
            end) (Insn.bil insn)));
    !call_list
      
  let main t =
    let call_list = gather_call_list t in
    print_call_list call_list
      
end

let main p =
  print_endline "START";
  let module Dot = Callgraph in
  Dot.main p;
  p

let () = register main
