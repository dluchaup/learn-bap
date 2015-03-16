open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis

let k = 3

module Analysis = Analysis(Addr)

let gather_call_list t = (* TBD: integrate this better with Analysis *)
  let call_list = ref ([]:(string*string*Analysis.location) list) in
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
  (Analysis.determinize_call_list !call_list)


let main_analysis t =
  let map_kstrings = Analysis.get_k_call_strings_map t gather_call_list k in
  let sexp_kstrings = Analysis.ECG.kstrings_map_to_sexp map_kstrings in
  let serialized_kstrings = Sexp.to_string sexp_kstrings in
  print_endline
    ("Table for k={"^Int.to_string(k)^"=\n"^serialized_kstrings^"}");
  output_string (open_out "kstrings.scm") serialized_kstrings;
  (*
  let map_kstrings2 = Analysis.ECG.kstrings_map_of_sexp sexp_kstrings in
  let sexp_kstrings2 = Analysis.ECG.kstrings_map_to_sexp map_kstrings2 in
  let serialized_kstrings2 = Sexp.to_string sexp_kstrings2 in
  (*assert (map_kstrings == map_kstrings2);*)
  output_string (open_out "kstrings2.scm") serialized_kstrings2;
 *)
  ()


let main p =
  main_analysis p;
  p
  
let () = register main    
