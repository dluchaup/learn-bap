open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis

let k = 3

let test t =
  let ecg = Analysis.ECG.from_project t in
  Analysis.print_call_list ecg.edges;
  print_endline (Analysis.LDG.to_string ecg.cg ~in_sep:"\n" ~out_sep:"\n\t");
  print_endline (Analysis.LDG.to_string ecg.rcg ~in_sep:"\n" ~out_sep:"\n\t");
  ()

let main_analysis t =
  let ecg = Analysis.ECG.from_project t in
  print_endline
    ("Table for k={"^Int.to_string(k)^"=\n"^
     (Sexp.to_string (Analysis.ECG.kstrings_map_to_sexp
                       (Analysis.ECG.get_k_call_strings_map ecg k)))^"}"
    );
    ()
    
let main p =
  main_analysis p;
  p
  
let () = register main
