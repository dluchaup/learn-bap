open Core_kernel.Std
open Bap.Std
open Program_visitor
open Cg

let k = 3
  
let call_list_to_sexp = <:sexp_of<(string*Addr.t*string) list>>

let main_analysis p =
  let cl = Programreader.get_call_list p in
  let g = G.from_call_list cl in
  let str = Sexp.to_string (G.kstrings_map_to_sexp
                              (G.get_k_call_strings_map g k)) in
  print_endline str;;

let main p =
  main_analysis p;
  p
  
let () = register main    
