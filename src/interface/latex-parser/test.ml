
open Sexplib

open Tex.Texeqparser
open Helper

(* 
  print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (parse_latexeq_eq [ "G"; "\\\\"; "land"; "Y"; "\\\\"; "leq"; "X"; "\\\\"; "land"; "Z"; "\\\\"; "lor"; "H"; "\\\\"; "lor"; "HH"] [])));
*)

let input = ref ""

let s1 =
  try while true do
    input := (input_line stdin) ^ !input
  done with End_of_file -> ()

let test_input = lex (explode !input)

let _ =
  print_endline (Sexp.to_string_hum ( sexp_of_tokens test_input ))
