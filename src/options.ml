(* options.ml *)

open Sexplib
open Sexplib.Conv
open Interface
open Dsl
open Helper

let helper = mk_helper

let smtlibv2_lang = ref false

let simplify_formula = ref false

let cpp11_lang = ref false

let ocaml_lang () = get_setting_bool "synth_ocaml" helper

let spark2014_lang = ref false

let smt_solver = ref ""

let solver_statistics_flag = ref false

let get_schedule_flag = ref false

let trace_style = ref ""

let gen_rmtld_formula = ref false

let rtmlib2_period = ref 0

(* default settings *)
let default_settings helper =
  let default_settings =
    "(rtm_period 200000)\n\
     (rtm_buffer_size 100)\n\
     (rtm_min_inter_arrival_time 1)\n\
     (rtm_max_period 2000000)\n\
     (rtm_monitor_name_prefix rtm_#_%)\n\
     (rtm_monitor_time_unit s)\n\
     (gen_tests false)"
  in
  (* apply settings *)
  apply_settings default_settings helper

(* setters for various options *)
let set_rtm_config_file v =
  set_setting "rtm_config_file" (Txt v) helper ;
  load_settings_from_file v helper

let set_smt_formula f = smtlibv2_lang := true

let set_simplify_formula f = simplify_formula := true

let set_ocaml_language f = set_setting "synth_ocaml" (Sel true) helper

let set_cpp_language f = cpp11_lang := true

let set_spark2014_language f = spark2014_lang := true

let set_solve_statistics f = solver_statistics_flag := true

let set_solve_z3 f = set_setting "solver" (Txt "z3") helper

let set_solve_cvc4 f = set_setting "solver" (Txt "cvc4") helper

let set_recursive_unrolling arg =
  (* check scope: auto or [0-9]+ *)
  if arg <> "auto" && not (Str.string_match (Str.regexp "[0-9]+") arg 0) then
    failwith ("Unrecognized recursive unrolling parameter '" ^ arg ^ "'.")
  else if Str.string_match (Str.regexp "[0-9]+") arg 0 then
    set_setting "rec_unrolling_depth" (Num (int_of_string arg)) helper ;
  set_setting "rec_unrolling" (Sel true) helper

let set_assumption_unary_sequence () =
  set_setting "assume_unary_sequence" (Sel true) helper

let set_get_schedule f = get_schedule_flag := true

let set_trace_style f = trace_style := f

let set_gen_rmtld_formula f = gen_rmtld_formula := true

let set_eval _ = set_setting "evaluate" (Sel true) helper

let set_env v =
  let json =
    (* check whether this is a filename or a json string *)
    let re_path_unix = Str.regexp {|^\([\.]?/[^/ ]*\)+/?$|} in
    let re_path_windows =
      Str.regexp
        {|^[a-zA-Z]:\(\\[a-zA-Z0-9_\.-]+\)*\([\\]\|[\.][a-zA-Z]+\)?$|}
    in
    if
      Str.string_match re_path_unix v 0
      || Str.string_match re_path_windows v 0
    then Yojson.Safe.from_file v
    else Yojson.Safe.from_string v
  in
  let x = json |> Yojson.Safe.to_string in
  set_setting "environment" (Txt x) helper

(* output settings *)
let set_out_file v = set_setting "out_file" (Txt v) helper

let set_out_dir v = set_setting "out_dir" (Txt v) helper ;
  if v <> "" then
        create_dir (get_setting_string "out_dir" helper)
  else failwith "Setting Output directory cannot be empty."

(* input settings *)
let set_exp v =
  set_setting "input_exp_sexp" (Txt v) helper ;
  set_setting "input_exp" (Fm (formula_of_sexp (Sexp.of_string v))) helper

let set_exp_dsl v =
  set_setting "input_exp_dsl" (Txt v) helper ;
  set_setting "input_exp"
    (Fm (Dsl.Load.parse_string v |> Dsl.TranslateToRmtld3.conv_fm))
    helper

let set_exp_ltxeq v =
  set_setting "input_exp_ltxeq" (Txt v) helper ;
  set_setting "input_exp" (Fm (Tex.Texeqparser.texeqparser v)) helper

let set_exp_rmdsl v =
  set_setting "input_exp_rmdsl" (Txt v) helper ;
  let lst =
    Rmdslparser.rmtld3_fm_lst_of_rmdsl_lst (Rmdslparser.rmdslparser v)
  in
  List.iter (fun a -> set_setting "input_exp" (Fm a) helper) lst

(* rtm settings *)
let set_rtm_period v = set_setting "rtm_period" (Num v) helper

let set_rtm_buffer_size v = set_setting "rtm_buffer_size" (Num v) helper

let set_rtm_min_inter_arrival_time v =
  set_setting "rtm_min_inter_arrival_time" (Num v) helper

let set_rtm_max_period v = set_setting "rtm_max_period" (Num v) helper

let set_rtm_event_type v = set_setting "rtm_event_type" (Txt v) helper

let set_rtm_event_subtype v = set_setting "rtm_event_subtype" (Txt v) helper

let set_rtm_monitor_name_prefix v =
  set_setting "rtm_monitor_name_prefix" (Txt v) helper

let set_rtm_monitor_time_unit v =
  if v = "ns" || v = "us" || v = "ms" || v = "s" then
    set_setting_replace "rtm_monitor_time_unit" (Txt v) helper
  else failwith "check if time units are 'ns', 'us', 'ms', or 's'."

(* general settings *)
let set_gen_tests v = set_setting "gen_tests" (Sel v) helper

let set_version () = set_setting "version" (Sel true) helper

(* ... other refs & setters ... *)

let apply_options_from_assoc_list assoc_list =
  List.iter
    (fun (key, value) ->
      match value with
      | `String s -> 
          if key = "input_exp_sexp" then
            set_exp s
          else if key = "input_exp_dsl" then
            set_exp_dsl s
          else if key = "input_exp_ltxeq" then
            set_exp_ltxeq s
          else if key = "input_exp_rmdsl" then
            set_exp_rmdsl s
          else failwith ("Unknown string option: " ^ key)
      | `Int n -> set_setting key (Num n) helper
      | `Bool b -> set_setting key (Sel b) helper
      | _ -> () )
    assoc_list

let speclist =
  [ (* action flags *)
    ( "--gen-rmtld-formula"
    , Arg.Unit set_gen_rmtld_formula
    , " Call `gen_formula_default` function" )
  ; ( "--synth-smtlibv2"
    , Arg.Unit set_smt_formula
    , " Enables synthesis for SMT-LIBv2 language\n\n\
      \ Flags for runtime monitoring (rtm) synthesis: " )
  ; ( "--synth-ocaml"
    , Arg.Unit set_ocaml_language
    , " Enables synthesis for Ocaml language" )
  ; ( "--synth-cpp11"
    , Arg.Unit set_cpp_language
    , " Enables synthesis for C++11 language" )
  ; ( "--synth-spark2014"
    , Arg.Unit set_spark2014_language
    , " Enables synthesis for Spark2014 language (Experimental)\n\n\
      \ Flags for solving: " )
  ; ( "--simpl-cad"
    , Arg.Unit set_simplify_formula
    , " Simplify quantified RMTLD formulas using CAD (Experimental)" )
  ; ( "--solver-z3"
    , Arg.Unit set_solve_z3
    , " Enables solving smtlibv2 problems using Z3 SMT solver" )
  ; ( "--solver-cvc4"
    , Arg.Unit set_solve_cvc4
    , " Enables solving smtlibv2 problems using cvc4 SMT solver" )
  ; ( "--rec-unrolling"
    , Arg.String set_recursive_unrolling
    , " Enables recursive unrolling with depth: auto, [0-9]+" )
  ; ( "--assume-unary-seq"
    , Arg.Unit set_assumption_unary_sequence
    , " Assume that the output sequence is unary." )
  ; ( "--solver-statistics"
    , Arg.Unit set_solve_statistics
    , " Enables printing the solver statistics" )
  ; ("--get-trace", Arg.Unit set_get_schedule, " Returns the schedule")
  ; ( "--trace-style"
    , Arg.String set_trace_style
    , " Sets the trace style\n\n Evaluation:" )
  ; (* evaluate formula on a given environment *)
    ( "--eval"
    , Arg.Unit set_eval
    , " Enables evaluation of a formula on an environment" )
  ; (* input environments *)
    ( "--include"
    , Arg.String set_env
    , " Includes a given environment (e.g., --include 'filename.env')\n\n\
      \ Input:" )
  ; (* input expressions *)
    ( "--input-sexp"
    , Arg.String set_exp
    , " Inputs sexp expression (RMTLD3 formula)" )
  ; ( "--input-dsl"
    , Arg.String set_exp_dsl
    , " Inputs dsl expression (RMTLD3 formula)" )
  ; ( "--input-latexeq"
    , Arg.String set_exp_ltxeq
    , " Inputs latex equation expressions (RMTLD3 formula) (Experimental)" )
  ; ( "--input-rmdsl"
    , Arg.String set_exp_rmdsl
    , " Inputs rmdsl expressions for schedulability analysis (Experimental)\n\n\
      \ Set runtime monitoring (rtm) settings:" )
  ; (* exclusively used for monitoring synthesis *)
    ( "--config-file"
    , Arg.String set_rtm_config_file
    , " Set settings from a file" )
  ; (* setup settings for rtm *)
    ("--set-monitor-period", Arg.Int set_rtm_period, " Set monitoring period")
  ; ("--set-buffer-size", Arg.Int set_rtm_buffer_size, " Set buffer size")
  ; ( "--set-min-inter-time"
    , Arg.Int set_rtm_min_inter_arrival_time
    , " Set minimum inter arrival time" )
  ; ("--set-max-period", Arg.Int set_rtm_max_period, " Set maximum period")
  ; ("--set-event-type", Arg.String set_rtm_event_type, " Set event type")
  ; ( "--set-event-subtype"
    , Arg.String set_rtm_event_subtype
    , " Set event subtype" )
  ; ( "--set-monitor-name-prefix"
    , Arg.String set_rtm_monitor_name_prefix
    , " Set monitor name prefix" )
  ; ( "--set-monitor-time-unit"
    , Arg.String set_rtm_monitor_time_unit
    , " Set monitor time units\n\n Output:" )
  ; (*output models *)
    ( "--out-file"
    , Arg.String set_out_file
    , " Set the output filename for synthesis" )
  ; ( "--out-src"
    , Arg.String set_out_dir
    , " Set the output directory for synthesis" )
  ; ( "--out-dir"
    , Arg.String set_out_dir
    , " Set the output directory for synthesis\n\n Options:" )
  ; ("--verbose", Arg.Set_int verb_mode, " Enables verbose mode")
  ; ("--version", Arg.Unit set_version, " Version and SW information\n") ]

let usage_msg =
  "rmtld3synth flags [options] input [output]\n\n Flags for synthesis: "

let parse () =
  try Arg.parse_argv Sys.argv (Arg.align speclist) print_endline usage_msg
  with Arg.Help msg | Arg.Bad msg -> print_endline msg
