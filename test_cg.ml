open Core_kernel.Std
open Bap.Std
open Sys
open Format
open Graph

open Cg

(***********)

let g = G.empty
let g = G.add_vertex g 1
let g = G.add_edge_e g (G.E.create 1 "12" 2)
let g = G.add_edge_e g (G.E.create 2 "23" 3)
let g = G.add_edge_e g (G.E.create 1 "14" 4)
let g = G.add_edge_e g (G.E.create 1 "15" 5)
let g = G.add_edge_e g (G.E.create 3 "35" 5)
let g = G.add_edge_e g (G.E.create 4 "43" 3)
let g = G.add_edge_e g (G.E.create 4 "45" 5)


(* persistent g *)
    
let () = to_dot "res-try5-g.dot" g
  

let g3 = G.add_edge_e g (G.E.create 1 "16" 6)
let g3 = G.add_edge_e g3 (G.E.create 1 "106" 6)
    
let () = to_dot "res-try5-g3.dot" g3

module CG = Components.Make(G)

let scc2string g =
  let vertices = G.fold_vertex (fun v acc -> v::acc) g [] in
  let (num_scc, vertex2scc) = CG.scc g in
  let v2s_list = List.map vertices ~f:(fun v -> (v, vertex2scc v)) in
  "[#"^Int.to_string(num_scc)^"]"^
  List.to_string ~f:(fun (v,s) -> (Int.to_string(v)^":"^Int.to_string(s)))
    v2s_list
    
let () = print_endline ("SCC of g:"^(scc2string g))



let g4 = G.add_edge_e g3 (G.E.create 3 "31" 1) (* back edge *)
    
let () = to_dot "res-try5-g4.dot" g4

let () = print_endline ("SCC of g4:"^(scc2string g4))

module MPG = Merge.P(G);;
let mg12 = MPG.merge_vertex g [1;2];;

let () = to_dot "res-try5-mg.12.dot" mg12
    
(*************)
    
let to_dotty name g =
  let dot_name = ("dot-try5-"^name^".dot") in
  let  () = to_dot dot_name g in
  Sys.command ("dotty "^dot_name)


let test_g name g =
  let () = print_endline ("\n>>>>>>>>>>>>>>>>>>>>>\n"^name^":Testing{") in
  let show v =
    print_endline ((G.vertex_name v)^"->"^(G.vset_to_string (G.succ_set g v)));
    print_endline ((G.vertex_name v)^"->2"^(G.vset_to_string (G.get_set_targets g (G.succ_set g v) G.Forward)));
    ()
  in
  let () = G.iter_vertex show g in
  let () = print_endline "}\n <<<<<<<<<<<<<<<<<<<<<<<<<\n...\n" in
  to_dotty name g
    

let _ = test_g "g" g
    

let _ = test_g "g4" g4

