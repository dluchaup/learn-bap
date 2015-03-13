open Core_kernel.Std
open Bap.Std
open Program_visitor
open Analysis
open OUnit2


let test_cl cl =
  let ecg = Analysis.ECG.from_call_list cl in
  Analysis.print_call_list ecg.edges;
  (* print_endline "f->"^(Sexp.to_string (String.Set.sexp_of_t (get_targets ecg.cg "f"))); *)
  let show fn =
    print_endline (fn^"->"^(Sexp.to_string (String.Set.sexp_of_t (Analysis.LDG.get_targets ecg.cg fn))));
    print_endline (fn^"->2"^(Sexp.to_string (String.Set.sexp_of_t
                                               (Analysis.LDG.get_set_targets ecg.cg
                                                  (Analysis.LDG.get_targets ecg.cg fn)))));
  in
  List.iter ecg.nodes ~f:show;
  print_endline (Analysis.LDG.to_string ecg.cg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
  print_endline (Analysis.LDG.to_string ecg.rcg ~in_sep:"\n\t\t" ~out_sep:"\n\t");
  let show_k_dag k fn =
    let dag = Analysis.ECG.get_k_call_dag ecg k fn  in
    print_endline (fn^":"^Int.to_string(k)^":"^(Analysis.ECG.call_dag_to_string dag))
  in
  List.iter ecg.nodes ~f:(fun n->
      show_k_dag 0 n;
      show_k_dag 1 n;
      show_k_dag 2 n;
      show_k_dag 3 n);
  ()
  


(* This is dumped in a file. I know the expected output, and use diff *)
let test1 () =
  let cl = [("f","g",Addr.of_int ~width:32 1);
            ("f","h",Addr.of_int ~width:32 2);
            ("f","g",Addr.of_int ~width:32 3);
            ("h","g",Addr.of_int ~width:32 4);
            ("g","g",Addr.of_int ~width:32 5)
           ]; in
  test_cl cl

let test2 () =
  let cl = [("f1","f2",Addr.of_int ~width:32 1);
            ("f2","f3",Addr.of_int ~width:32 2);
            ("f3","f4",Addr.of_int ~width:32 3);
            ("f4","f5",Addr.of_int ~width:32 4);
            ("f5","f6",Addr.of_int ~width:32 5);
            ("f6","f7",Addr.of_int ~width:32 6);
           ];
  in
  test_cl cl;
  let cl1 =(("f2","f4",Addr.of_int ~width:32 7)::("f3","f5",Addr.of_int ~width:32 8)::cl)
  in
  test_cl cl1;
  let cl2 = ("f6","f3",Addr.of_int ~width:32 100)::cl1 in test_cl cl2;    
  ()
  
let unit_test() = test2()

let () = unit_test()

