
(*
   Interface for Z3 smt solver
 *)


open Z3
open Z3.SMT

let parse_smtlibv2 () =
  print_endline "A";
  let ctx = (mk_context []) in 
  let exp = parse_smtlib2_string ctx "(xxx)" [] [] [] [] in
  ()
