open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis

let main_test t =
  let ecg = Analysis.ECG.from_project t in
  Analysis.print_call_list ecg.edges;
  print_endline (Analysis.LDG.to_string ecg.cg ~in_sep:"\n" ~out_sep:"\n\t");
  print_endline (Analysis.LDG.to_string ecg.rcg ~in_sep:"\n" ~out_sep:"\n\t");
  ()
  
let main p =
  main_test p;
  p
  
let () = register main
