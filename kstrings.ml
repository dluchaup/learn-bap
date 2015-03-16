open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis

let k = 3

module Analysis = Analysis(Addr)

let main_analysis t =
  let map_kstrings = Analysis.get_k_call_strings_map t k in
  let sexp_kstrings = Analysis.ECG.kstrings_map_to_sexp map_kstrings in
  let serialized_kstrings = Sexp.to_string sexp_kstrings in
  print_endline
    ("Table for k={"^Int.to_string(k)^"=\n"^serialized_kstrings^"}");
  output_string (open_out "kstrings.scm") serialized_kstrings;
  ()
  
let main p =
  main_analysis p;
  p
  
let () = register main
