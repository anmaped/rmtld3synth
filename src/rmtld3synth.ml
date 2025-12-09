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
let synth_sat_problem formula =
  (* settings *)
  if not (is_setting "rec_unrolling_depth" Options.helper) then (
    (* Calculate the upper time bound of the formula to determine unrolling depth.
       Defaults to 20 if the formula is unbounded. *)
    let bound = int_of_float (try calculate_t_upper_bound formula with Failure _ -> 20.) in
    set_setting "rec_unrolling_depth" (Num (bound + 1)) Options.helper ;
    verb (fun _ ->
    print_endline ("t_upper_bound" ^ string_of_int bound) ) ;
  ) ;
  (* Functor to translate rmtld3 into smtlibv2 *)
  let module Smtlib = Standard.Translate (Smtlib2) in
  (* 'smtlib2_str' will contain the output of the translation *)
  let smtlib2_str = synth_smtlib Smtlib.synth formula Options.helper in
  (*let smtlib2_str = rmtld3synthsmt formula helper in*)
  if Smtlib2.isZ3SolverEnabled Options.helper then (
    verb (fun _ -> print_endline "Z3 solver enabled.") ;
    let ctx, exp = parse_smtlibv2 smtlib2_str in
    let out, solver = solve_ ctx exp in
    verb (fun _ -> print_endline ("Result: " ^ out)) ;
    if not !Options.get_schedule_flag then print_endline out ;
    if out = "satisfiable" then (
      let model = get_model ctx solver in
      if not !Options.get_schedule_flag then print_endline (string_of_z3model model) ;
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
  try Options.parse () with
  | Exit -> exit 1 ;;
  (* version information *)
  if get_setting_bool "version" Options.helper then
    ( print_endline ("Git version " ^ Version.git) ;
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
      List.iter
        (fun exp -> print_endline (string_of_rmtld_fm exp))
        expressions ) ;
  (* selects the type of the input formula *)
  let input_fm =
    if expressions <> [] then List.hd expressions
    else (* there is no imput formula *)
      mfalse
  in
  let to_simplify fm : rmtld3_fm =
    if !Options.simplify_formula then (
      let smp = simplify fm in
      verb (fun _ ->
          print_endline "Output formula from the simplification process:\n" ;
          print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm smp)) ;
          print_endline
            "--------------------------------------------------------------------------------\n" ) ;
      smp )
    else fm
  in
  (* Selects synthesis for smtlibv2, ocaml, cpp or does simplification. *)
  if Options.smtlibv2_lang Options.helper then
    if input_fm <> mfalse then synth_sat_problem (to_simplify input_fm)
    else (
      verb (fun _ -> print_endline "Rmdsl parsing enabled.") ;
      let fm_lst =
        get_all_setting_formula "input_exp"
          Options.helper (* list of expressions with input_exp tag *)
      in
      verb (fun _ ->
          print_endline
            "--------------------------------------------------------------------------------\n" ;
          print_endline "rmtld3 formula(s): " ;
          print_endline
            ("Available goals: " ^ string_of_int (List.length fm_lst)) ) ;
      let _ =
        List.fold_left
          (fun a fm_goal ->
            synth_sat_problem (to_simplify fm_goal) ;
            a + 1 )
          1 fm_lst
      in
      () )
  else if Options.ocaml_lang Options.helper then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for Ocaml language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
    synth_ocaml Format.std_formatter Conv_ocaml.synth Options.helper ;
    Format.print_flush ()
    )
  else if Options.cpp11_lang Options.helper then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for C++11 language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
    Options.default_cpp11_settings Options.helper ;
    synth_cpp11 Format.std_formatter Conv_cpp11.synth Options.helper ;
    Format.print_flush ()
    )
  else if Options.spark2014_lang Options.helper then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for SPARK 2014 language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
    synth_spark2014 Format.std_formatter Conv_spark2014.synth Options.helper ;
    Format.print_flush ()
    )
  else if !Options.simplify_formula then
    let inn = input_fm in
    if inn <> mfalse then
      let smp = to_simplify inn in
      slatex_of_rmtld_fm smp |> print_endline
    else raise (Failure "Cannot simplify the specified input.")
  else if !Options.gen_rmtld_formula then
    let fm = gen_formula_default () in
    slatex_of_rmtld_fm fm |> print_endline
  else if get_setting_bool "evaluate" Options.helper then
    (* get trc *)
    let json =
      get_setting_string "environment" Options.helper |> Yojson.Safe.from_string
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
