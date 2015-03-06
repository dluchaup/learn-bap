open Core_kernel.Std
open Bap.Std
open Program_visitor

let main project =
  print_endline "hello, BAP";
  Table.iter project.symbols ~f:print_endline;
  project

let () = register main


