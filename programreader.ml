open Core_kernel.Std
open Bap.Std
open Program_visitor


let dbgprint t tag addr =
  printf "([%s] addr2str=%s;\t addr2int=0x%xd;\t addr2sym=%s)\n"
    tag
    (Addr.(to_string addr))
    (ok_exn ((Addr.(to_int addr))))
    (match (Table.find_addr t.symbols addr) with
     | None -> "NONE"
     | Some (mem,sym) -> (*((Memory.to_string mem)^":"^sym)) *)
       (Addr.(to_string (Memory.min_addr mem))^":"^sym))

let containing_func t addr =
  match (Table.find_addr t.symbols addr) with
  | None -> "<NONE>" | Some (_mem,sym) -> sym
    
let dbgprint3addr t addr0 addr1 addr2 =
  (dbgprint t "(src)mem0" addr0);
  (dbgprint t "(call)mem1" addr1);
  (dbgprint t "(dst)mem2" addr2);
  print_endline ""
    
let dbgprint3mem t mem0 mem1 mem2 =
  dbgprint3addr t
    (Memory.min_addr mem0)
    (Memory.min_addr mem1)
    (Memory.min_addr mem2)
    
let get_raw_call_list t =
  let call_list = ref ([]:(Addr.t*Addr.t*Addr.t) list) in
  call_list := []; (* TBD: use fold *)
  Table.iteri t.symbols ~f:(fun mem0 _src ->
      let mseq =  Disasm.insns_at_mem t.program mem0 in
      Seq.iter mseq ~f:(fun (mem1, insn) ->
          Bil.iter (object inherit [unit] Bil.visitor
            method!enter_int addr () = if in_jmp then
                match Table.find_addr t.symbols addr with
                | None -> ()
                  | Some (mem2, _dst) ->
                    if Addr.(Memory.min_addr mem2 = addr) then
                      (*dbgprint3mem t mem0 mem1 mem2;*)
                      call_list := List.append !call_list
                          [((Memory.min_addr mem0),
                            (Memory.min_addr mem1),
                            (Memory.min_addr mem2))]
          end) (Insn.bil insn)));
  !call_list

let get_call_list t =
  let raw_call_list = get_raw_call_list t in
  List.map
    raw_call_list
    ~f:(fun (addr0,addr1,addr2) ->
        (* dbgprint3addr t addr0 addr1 addr2;*)
        ((containing_func t addr0), addr1, (containing_func t addr2)))
    


let dbg_print_call_list_string cl =
  List.iter cl
    ~f:(fun (s,l,d) -> printf "0x%xd: %s -> %s\n"
           (ok_exn ((Addr.(to_int l)))) s d)

let dbg_print_call_list_addr cl =
  List.iter cl
    ~f:(fun (s,l,d) -> printf "0x%xd: 0x%xd -> 0x%xd\n"
           (ok_exn ((Addr.(to_int l))))
           (ok_exn ((Addr.(to_int s))))
           (ok_exn ((Addr.(to_int d)))))


let call_list_to_sexp = <:sexp_of<(string*Addr.t*string) list>>

let main_analysis t =
  let cl = get_call_list t in
  let cl_sexp = call_list_to_sexp cl in
  print_endline (Sexp.to_string cl_sexp)
    

let main p =
  print_endline "programreader";
  main_analysis p;
  p
  
let () = register main    

