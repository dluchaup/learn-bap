open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis

(* This is dumped in a file. I know the expected output, and use diff *)
let unit_test () =
  let cl_example1 = [("f","g",Addr.of_int ~width:32 1);
                     ("f","h",Addr.of_int ~width:32 2);
                     ("f","g",Addr.of_int ~width:32 3);
                     ("h","g",Addr.of_int ~width:32 4);
                     ("g","g",Addr.of_int ~width:32 5)
                    ]; in
  let ecg = Analysis.ECG.from_call_list cl_example1 in
  Analysis.print_call_list ecg.edges;
  print_endline (Analysis.LDG.to_string ecg.cg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
  print_endline (Analysis.LDG.to_string ecg.rcg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
  let dag_g_0 = Analysis.ECG.get_k_call_dag ecg 0 "g"  in
  print_endline ("0:"^(Analysis.ECG.call_dag_to_string dag_g_0));
  let dag_g_1 = Analysis.ECG.get_k_call_dag ecg 1 "g"  in
  print_endline ("1"^(Analysis.ECG.call_dag_to_string dag_g_1));
  let dag_g_2 = Analysis.ECG.get_k_call_dag ecg 2 "g"  in
  print_endline ("2"^(Analysis.ECG.call_dag_to_string dag_g_2));
  ()

let () = unit_test()

