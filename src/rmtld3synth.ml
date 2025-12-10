(* rmtld3synth tool *)

open Unix
open Sexplib
open Sexplib.Conv

(* internal modules *)
open Rmtld3
open Synthesis
open Synthesis.Simplify
open Synthesis.Smtlib2
open Synthesis.Cpp11
open Synthesis.Ocaml
open Synthesis.Spark2014
open Interface
open Interface.Z3solver
open Helper

(* Functors for synthesis *)
module Conv_cpp11 = Standard.Translate (Synthesis.Cpp11)
module Conv_ocaml = Standard.Translate (Synthesis.Ocaml)
module Conv_spark2014 = Standard.Translate (Synthesis.Spark2014)

(** Formulates and configures the synthesis of rmtld into smtlibv2. *)
let synth_sat_problem fmt =
  (* Functor to translate rmtld3 into smtlibv2 *)
  let module Smtlib = Standard.Translate (Smtlib2) in
  (* 'smtlib2_str' will contain the output of the translation *)
  let smtlib2_str_lst = synth_smtlib fmt Smtlib.synth Options.helper in
  (* smtlib2_str contains a list with a pair (filename, smtlibv2 content) *)
  let smtlib2_str = snd (List.hd smtlib2_str_lst) in
  if List.length smtlib2_str_lst > 1 then
    failwith
      "Multiple SMTLib2 files generated; solver mode only supports a single \
       file" ;
  (*let smtlib2_str = rmtld3synthsmt formula helper in*)
  if Smtlib2.isZ3SolverEnabled Options.helper then (
    verb (fun _ -> print_endline "Z3 solver enabled.") ;
    let ctx, exp = parse_smtlibv2 smtlib2_str in
    let out, solver = solve_ ctx exp in
    verb (fun _ -> print_endline ("Result: " ^ out)) ;
    if not !Options.get_schedule_flag then print_endline out ;
    if out = "satisfiable" then (
      let model = get_model ctx solver in
      if not !Options.get_schedule_flag then
        print_endline (string_of_z3model model) ;
      if !Options.get_schedule_flag then (
        let scheduler_trace = get_scheduler ctx model Options.helper in
        if !Options.trace_style = "tinterval" then
          let _, trc_str =
            List.fold_left
              (fun (cnt, a) b ->
                let cnte = cnt +. 1. in
                (cnte, a ^ " (\"" ^ b ^ "\",(" ^ string_of_float cnt ^ ")); ")
                )
              (0., "") scheduler_trace
          in
          print_endline trc_str
        else if !Options.trace_style = "tcum" then
          let _, trc_str =
            List.fold_left
              (fun (cnt, a) b ->
                let cnte = cnt +. 1. in
                ( cnte
                , a ^ " (\"" ^ b ^ "\","
                  ^ string_of_float (cnte -. cnt)
                  ^ "); " ) )
              (0., "") scheduler_trace
          in
          print_endline trc_str
        else
          print_endline
            (Sexp.to_string (sexp_of_trace_untimed scheduler_trace)) ;
        () ) ) )

(** rmtld3synth's command line interface *)
let _ =
  Options.default_settings Options.helper ;
  try Options.parse () with Exit -> exit 1
;;

(* version information *)
if get_setting_bool "version" Options.helper then (
  print_endline ("Git version " ^ Version.git) ;
  exit 0 ) ;
set_setting "version" (Txt Version.git) Options.helper ;
verb_m 2 (fun a -> print_endline (Version.git ^ "\n")) ;
verb_m 1 (fun _ ->
    print_endline "Current Configuration:" ;
    print_settings Options.helper ) ;
let expressions = get_all_setting_formula "input_exp" Options.helper in
if expressions = [] then (
  print_endline "no formula is available." ;
  exit 1 ) ;
verb_m 1 (fun _ ->
    print_endline "Expression(s) selected to encode:" ;
    List.iter (fun exp -> print_endline (string_of_rmtld_fm exp)) expressions ) ;
(* selects the type of the input formula *)
let input_fm =
  if expressions <> [] then List.hd expressions
  else (* there is no imput formula *)
    mfalse
in
(* prepare prety printers *)
let pp_endline = pp_endline Format.std_formatter in
(* simplify all formulas if needed *)
if Options.simplify Options.helper then (
  let input_lst = get_all_setting_formula "input_exp" Options.helper in
  (* apply simplify to all formulas if exists *)
  if List.length input_lst = 0 then
    failwith
      "No input formula provided! Please provide at least one formula with \
       the 'input_exp' tag."
  else
    let simplified_formulas =
      List.mapi
        (fun idx fm ->
          verbose pp_endline
            ( "Simplifying input formula "
            ^ string_of_int (idx + 1)
            ^ "/"
            ^ string_of_int (List.length input_lst) ) ;
          verbose pp_endline
            ( "Output formula from the simplification process:\n"
            ^ Sexp.to_string_hum (sexp_of_rmtld3_fm fm) ) ;
          simplify fm )
        input_lst
    in
    (* remove all formulas from settings *)
    remove_setting_every_occurence "input_exp" Options.helper ;
    (* add simplified formulas *)
    List.iter
      (fun fm -> set_setting "input_exp" (Fm fm) Options.helper)
      simplified_formulas ) ;
(* Selects synthesis for smtlibv2, ocaml, cpp or does simplification. *)
if Options.smtlibv2_lang Options.helper then (
  let fmt = Format.std_formatter in
  synth_sat_problem fmt ; Format.print_flush () )
else if Options.ocaml_lang Options.helper then (
  verb_m 1 (fun _ ->
      print_endline "Synthesis for Ocaml language" ;
      print_endline
        "--------------------------------------------------------------------------------\n" ) ;
  synth_ocaml Format.std_formatter Conv_ocaml.synth Options.helper ;
  Format.print_flush () )
else if Options.cpp11_lang Options.helper then (
  verb_m 1 (fun _ ->
      print_endline "Synthesis for C++11 language" ;
      print_endline
        "--------------------------------------------------------------------------------\n" ) ;
  Options.default_cpp11_settings Options.helper ;
  synth_cpp11 Format.std_formatter Conv_cpp11.synth Options.helper ;
  Format.print_flush () )
else if Options.spark2014_lang Options.helper then (
  verb_m 1 (fun _ ->
      print_endline "Synthesis for SPARK 2014 language" ;
      print_endline
        "--------------------------------------------------------------------------------\n" ) ;
  synth_spark2014 Format.std_formatter Conv_spark2014.synth Options.helper ;
  Format.print_flush () )
else if Options.simplify Options.helper then
  (* formulas are simplified just write all of them *)
  let input_exp_list = get_all_setting_formula "input_exp" Options.helper in
  let simplified_formulas =
    List.mapi
      (fun idx fm ->
        (string_of_int idx, Sexp.to_string_hum (sexp_of_rmtld3_fm fm)) )
      input_exp_list
  in
  to_multipart_message Format.std_formatter simplified_formulas
else if !Options.gen_rmtld_formula then
  let fm = gen_formula_default () in
  slatex_of_rmtld_fm fm |> print_endline
else if get_setting_bool "evaluate" Options.helper then
  (* get trc *)
  let json =
    get_setting_string "environment" Options.helper
    |> Yojson.Safe.from_string
  in
  let json_trc = json |> Yojson.Safe.Util.member "trc" in
  let json_t = json |> Yojson.Safe.Util.member "t" in
  let trc =
    if json_trc <> `Null then trace_of_yojson json_trc
    else failwith "No 'trc' available!"
  in
  let env = Rmtld3.environment trc in
  let lg_env = Rmtld3.lenv in
  let t = if json_t <> `Null then Rmtld3.time_of_yojson json_t else 0. in
  let res = Rmtld3.eval (env, lg_env, t) input_fm in
  res |> b3_to_string |> print_endline
else print_endline "Nothing to do. Type --help"
