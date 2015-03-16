(** A tool to automate testing  *)
open Core_kernel.Std
open OUnit2
open Bap.Std
open Analysis

type 'a task = {
  name : string;
  read : Sexp.t -> 'a;
}

let output_of_task task = task.name ^ ".scm"

module Analysis = Analysis(Addr)


(*let read_kstring _  = failwith "parse k-string" *)
let read_kstring  = Analysis.ECG.kstrings_map_of_sexp

let read_kstring1 _  = "XXX"
  
let read_astring _  = failwith "parse a-string"
let read_treenode _ = failwith "parse a node"

let kstrings = {
  name = "kstrings";
  read = read_kstring;
}

let astrings = {
  name = "astrings";
  read = read_astring;
}

let csforest = {
  name = "csforest";
  read = read_treenode;
}

(** [run task check input ctxt] run test for a given task on a the
    [input] file and apply [check] to verify the output *)
let run task check input ctxt : unit =
  let output = output_of_task task in
  assert_command  ~ctxt "bap-objdump" [input; "--use-ida"; "-l"; task.name];
  In_channel.with_file output ~f:(fun chan ->
      Sexp.scan_sexp(*s*)  (Lexing.from_channel chan)  |>
      (*List.map ~f:*)task.read |>  check)

(** {3 Actual testing}  *)

(* let dummy data = todo "test data" *)

(* let fake data = print_endline "FAKE OK" *)
let check_kstrings new_map_kstrings =
  let new_sexp_kstrings = Analysis.ECG.kstrings_map_to_sexp new_map_kstrings in
  let new_serialized_kstrings = Sexp.to_string new_sexp_kstrings in
  assert(new_serialized_kstrings = "((.__gmon_start__())(.__libc_start_main())(.init_proc())(.term_proc())(__do_global_dtors_aux())(__libc_csu_init())(__x86.get_pc_thunk.bx())(_start())(deregister_tm_clones())(f1())(f2((((z 0x804858A)(w 32)(signed false))((z 0x8048544)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))(((z 0x804858A)(w 32)(signed false))((z 0x8048554)(w 32)(signed false))((z 0x8048527)(w 32)(signed false)))(((z 0x804858A)(w 32)(signed false))((z 0x8048554)(w 32)(signed false))((z 0x8048516)(w 32)(signed false)))(((z 0x8048527)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))(((z 0x8048516)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))(((z 0x80484FC)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))(((z 0x8048554)(w 32)(signed false))((z 0x8048516)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))(((z 0x8048554)(w 32)(signed false))((z 0x8048527)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))(((z 0x8048544)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false))((z 0x80484FC)(w 32)(signed false)))))(f3())(frame_dummy())(ft1())(ft2((((z 0x80484A0)(w 32)(signed false))((z 0x8048487)(w 32)(signed false))((z 0x8048487)(w 32)(signed false)))(((z 0x8048487)(w 32)(signed false))((z 0x8048487)(w 32)(signed false))((z 0x8048487)(w 32)(signed false)))(((z 0x80484CA)(w 32)(signed false))((z 0x80484A0)(w 32)(signed false))((z 0x8048487)(w 32)(signed false)))(((z 0x80484BA)(w 32)(signed false))((z 0x8048487)(w 32)(signed false))((z 0x8048487)(w 32)(signed false)))))(ft3())(main())(register_tm_clones())(uf1())(uf2((((z 0x804842B)(w 32)(signed false))((z 0x8048412)(w 32)(signed false))((z 0x8048412)(w 32)(signed false)))(((z 0x8048412)(w 32)(signed false))((z 0x8048412)(w 32)(signed false))((z 0x8048412)(w 32)(signed false)))(((z 0x8048455)(w 32)(signed false))((z 0x804842B)(w 32)(signed false))((z 0x8048412)(w 32)(signed false)))(((z 0x8048445)(w 32)(signed false))((z 0x8048412)(w 32)(signed false))((z 0x8048412)(w 32)(signed false)))))(uf3()))")





let suite = "BAP Fun" >::: [
    "kstrings" >::: [
      "test4" >:: run kstrings check_kstrings "test4.32.x"
    ]
  ]

let () = run_test_tt_main suite
