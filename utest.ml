open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis
open OUnit2

module Analysis = Analysis(Int)
module LDG = LDG(Int)


let test_cl cl =
  let ecg = Analysis.ECG.from_call_list cl in
  Analysis.print_call_list ecg.edges;
  let show fn =
    print_endline (fn^"->"^(Sexp.to_string
                              (String.Set.sexp_of_t
                                 (Analysis.ECG.LDG.get_targets ecg.cg fn))));
    print_endline (fn^"->2"^(Sexp.to_string
                               (String.Set.sexp_of_t
                                  (Analysis.ECG.LDG.get_set_targets ecg.cg
                                     (Analysis.ECG.LDG.get_targets ecg.cg fn)))));
  in
  List.iter ecg.nodes ~f:show;
  print_endline (Analysis.ECG.LDG.to_string
                   ecg.cg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
  print_endline (Analysis.ECG.LDG.to_string
                   ecg.rcg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
  let show_k_dag k fn =
    let dag = Analysis.ECG.get_k_call_dag ecg k fn  in
    print_endline (fn^":"^Int.to_string(k)^":"^
                   (Analysis.ECG.call_dag_to_string dag))
  in
  List.iter ecg.nodes ~f:(fun f ->
      for k = 0 to 3 do
        show_k_dag k f;
      done
    );
  for k = 0 to 3 do
    print_endline
      ("Table for k="^Int.to_string(k)^"=\n"^
       Sexp.to_string (Analysis.ECG.kstrings_map_to_sexp
                         (Analysis.ECG.get_k_call_strings_map ecg k)))
  done;
  ()
  


(* This is dumped in a file. I know the expected output, and use diff *)
let test1 () =
  let cl = [("f","g",Int.of_int 1);
            ("f","h",Int.of_int 2);
            ("f","g",Int.of_int 3);
            ("h","g",Int.of_int 4);
            ("g","g",Int.of_int 5)
           ]; in
  test_cl cl

let test2 () =
  let cl = [("f1","f2",Int.of_int 1);
            ("f2","f3",Int.of_int 2);
            ("f3","f4",Int.of_int 3);
            ("f4","f5",Int.of_int 4);
            ("f5","f6",Int.of_int 5);
            ("f6","f7",Int.of_int 6);
           ];
  in
  test_cl cl;
  let cl1 =(("f2","f4",Int.of_int 7)::
            ("f3","f5",Int.of_int 8)::cl)
  in
  test_cl cl1;
  let cl2 = ("f6","f3",Int.of_int 100)::cl1 in test_cl cl2;    
  ()
  
let unit_test() = test2()

let () = unit_test()

