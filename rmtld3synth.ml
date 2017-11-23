(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

open Sexplib
open Sexplib.Conv

open Rmtld3
open Rmtld3synth_helper


type call_body = string * string
module type Conversion =
sig
  val compute_tm_constant : value -> helper -> call_body
  val compute_tm_duration : call_body -> call_body -> helper -> call_body
  val compute_tm_plus : call_body -> call_body -> helper -> call_body
  val compute_tm_times : call_body -> call_body -> helper -> call_body
  val compute_fm_true : helper -> call_body
  val compute_fm_p : prop -> helper -> call_body
  val compute_fm_not : call_body -> helper -> call_body
  val compute_fm_or : call_body -> call_body -> helper -> call_body
  val compute_fm_less : call_body -> call_body -> helper -> call_body
  val compute_fm_uless : value -> call_body -> call_body -> helper -> call_body
  val compute_fm_ueq : value -> call_body -> call_body -> helper -> call_body
  val compute_fm_ulesseq : value -> call_body -> call_body -> helper -> call_body
end;;

module Conversion_cpp (Conv : Conversion) = struct
  (* Synthesis of the rmtld3 term *)
  let rec compute_term term helper =
    match term with
      | Constant value       -> Conv.compute_tm_constant value helper
      | Duration (di,phi)    -> Conv.compute_tm_duration (compute_term di helper) (compute phi helper) helper
      | FPlus (tr1,tr2)      -> Conv.compute_tm_plus (compute_term tr1 helper) (compute_term tr2 helper) helper
      | FTimes (tr1,tr2)     -> Conv.compute_tm_times (compute_term tr1 helper) (compute_term tr2 helper) helper
      | _                    -> raise (Failure "compute_terms: missing term")

  (* Synthesis of the rmtld3 formula *)
  and compute formula helper =
    match formula with
      | True()                  -> Conv.compute_fm_true helper
      | Prop p                  -> Conv.compute_fm_p p helper
      | Not sf                  -> Conv.compute_fm_not (compute sf helper) helper
      | Or (sf1, sf2)           -> Conv.compute_fm_or (compute sf1 helper) (compute sf2 helper) helper
      | Until (gamma, sf1, sf2)     -> if gamma > 0. then Conv.compute_fm_uless gamma (compute sf1 helper) (compute sf2 helper) helper
                                       else raise  (Failure "Gamma of U< operator is negative")
      | Until_eq (gamma, sf1, sf2)  -> if gamma > 0. then Conv.compute_fm_ueq gamma (compute sf1 helper) (compute sf2 helper) helper
                                       else raise  (Failure "Gamma of U= operator is negative")
      | Until_leq (gamma, sf1, sf2) -> if gamma > 0. then Conv.compute_fm_ulesseq gamma (compute sf1 helper) (compute sf2 helper) helper
                                       else raise  (Failure "Gamma of U<= operator is negative")
      | LessThan (tr1,tr2)      -> Conv.compute_fm_less (compute_term tr1 helper) (compute_term tr2 helper) helper
      | _                       -> raise (Failure ("synth_mon: bad formula "^( Sexp.to_string_hum (sexp_of_rmtld3_fm formula))))

end;;

(* rmtld3 synthesis interface *)

let config_file = ref ""
let rmtld_formula = ref ""
let rmtld_formula_ltxeq = ref ""
let expression_rmdsl = ref ""
let smtlibv2_formula = ref false
let simplify_formula = ref false
let out_file = ref ""
let out_dir = ref ""
let cpp11_lang = ref false
let ocaml_lang = ref false
let spark14_lang = ref false
let smt_solver = ref ""
let solver_statistics_flag = ref false
let get_schedule_flag = ref false
let gen_rmtld_formula = ref false

let set_config_file file = config_file := file
let set_formulas f = rmtld_formula := f
let set_formulas_ltxeq f = rmtld_formula_ltxeq := f
let set_exp_rmdsl f = expression_rmdsl := f
let set_smt_formula f = smtlibv2_formula := true
let set_simplify_formula f = simplify_formula := true
let set_out_file f = out_file := f
let set_out_dir f = out_dir := f
let set_ocaml_language f = ocaml_lang := true
let set_cpp_language f = cpp11_lang := true
let set_spark14_language f = spark14_lang := true
let set_solve_z3 f = smt_solver := "z3"
let set_solve_statistics f = solver_statistics_flag := true
let set_get_schedule f = get_schedule_flag := true
let set_gen_rmtld_formula f = gen_rmtld_formula := true

let isSolverEnabled () = !smt_solver = ""

open Batteries
open Unix
open Sexplib
open Sexplib.Conv


open Rmtld3synth_simplify
open Rmtld3synth_unittest
open Rmtld3synth_smt
open Rmtld3synth_cpp11
open Rmtld3synth_ocaml
open Z3solver_
open Rmtld3synth_helper


let chose_synthesis a b c =
  if !cpp11_lang then a () else if !ocaml_lang then b () else if !spark14_lang then c ()

let mon_gen fm =
  (* helper to support query's settings along execution *)
  let a,b,c = settings config_file in
  let c = if c <> [] then c else
    if fm <> mfalse then [("mon0",0,fm)]
    else []
  in
  let helper = set_parameters (a,b,c) mk_helper in
  let create_dir dir_name = try let state = Sys.is_directory dir_name in if state then () else  Unix.mkdir dir_name 0o666; with _ -> Unix.mkdir dir_name 0o666 in
  

  (* monitor synthesis settings *)
  (* buffer size *)
  let event_queue_size = 
      max (search_settings_int "buffer_size" helper)
      (* compute the buffer size based on inter-arrival time *)
      ((List.fold_left (fun v (_,_,formula) -> let value = int_of_float (calculate_t_upper_bound formula) in if value > v then value else v ) 0 (get_settings_monitor helper))
        / (search_settings_int "maximum_inter_arrival_time" helper))
  in
  verb (fun _ -> Printf.printf "Buffer is defined as length %d\n" event_queue_size);
  
  (* monitor cluster name *)
  let cluster_name = search_settings_string "cluster_name" helper in (* search 'cluster_name' setting in the global_string parameters *)
  
  let cluster_name = if !out_dir <> "" then Filename.basename !out_dir else cluster_name in

  verb_m 2 (fun _ -> print_endline ("cluster_name: "^cluster_name); );

  if !out_dir <> "" then create_dir !out_dir;

  (* create dir for storage of SMT benchmarks *)
  (* 
    'cluster_name' setting is the name for the synthesis group. However, in order
    to generate the unit tests for SMT solvers' benchmark, the synthesis
    of s-expressions into SMT-LIBv2 is available in the hardcoded sub directory
    "smt/".
  *)
  if !out_dir <> "" then create_dir (!out_dir^"/smt/");


  (* for cpp11 and ocaml synthesis *)
  let module Conv_cpp11 = Conversion_cpp(Rmtld3synth_cpp11) in
  let module Conv_ocaml = Conversion_cpp(Rmtld3synth_ocaml) in

  (*
   * External and other dependencies for monitors (may include the RV model)
   *)
  
  chose_synthesis (fun a ->
    begin
    (* c++ type templates with pattern 'evt_type < evt_subtype > '  *)
    (* evt_type is a class given by rtmlib *)
    let evt_type = search_settings_string "event_type" helper in
    (* evt_subtype can be a primitive type or a class *)
    let evt_subtype = search_settings_string "event_subtype" helper in
    set_event_type evt_type helper;
    set_event_subtype evt_subtype helper;

    if !out_dir <> "" then
    begin
    (* External dependencies and environment for cpp11 *)
    synth_cpp1_external_dep cluster_name helper;
    synth_cpp11_env cluster_name evt_subtype event_queue_size helper;
    end
    
    end
  )
  (fun _ -> ())
  (fun _ -> ())
  ;

  (* generate monitors *)
  List.fold_left (fun _ (monitor_name,monitor_period,formula) ->

    (*create_dir (cluster_name^"/"^monitor_name);*)

    chose_synthesis
    ( fun _ ->
      (* TODO (out_file,out_dir) *)
      (* monitor synthesis for cpp11 *)
      synth_cpp11_compute (!out_file,!out_dir) cluster_name monitor_name monitor_period formula (fun a b -> let x,_ = Conv_cpp11.compute a b in x) helper
    )
    ( fun _ ->
      (* monitor synthesis for ocaml *)
      synth_ocaml_compute (!out_file,!out_dir) cluster_name monitor_name monitor_period formula (Conv_ocaml.compute) helper
    )
    ( fun _ ->
      (* monitor synthesis for spark14 *)
      ()
    )
    ;

  ) () (get_settings_monitor helper);

  
  (* lets generate the tests *)
  if !out_dir <> "" && (search_settings_string "gen_tests" helper) = "true" then
  begin
    create_dir (!out_dir^"/tests");
    Rmtld3synth_unittest.test () cluster_name helper;

    Rmtld3synth_unittest.rmtld3_unit_test_generation () (fun a b -> let x,_ = Conv_cpp11.compute a b in x) helper cluster_name helper;
  end


let sat_gen formula =
begin
  let helper = mk_helper in
  let stmlibv2_str = rmtld3synthsmt formula helper in

  if !smt_solver = "z3" then
  begin
    verb (fun _ -> print_endline "Z3 solver enabled."; ) ;
    let ctx,exp = parse_smtlibv2 stmlibv2_str in
    let out,solver = solve_ ctx exp in
    verb (fun _ -> print_endline ("Result: "^out) ) ;
     
    if not !get_schedule_flag then print_endline out;

    if out = "satisfiable" then
    begin
      let model = get_model ctx solver in
      if not !get_schedule_flag then print_endline (string_of_z3model model) ;
      if !get_schedule_flag then
      begin
        let scheduler_trace = get_scheduler ctx model helper in
        print_endline (Sexp.to_string (sexp_of_trace_untimed scheduler_trace)) ;
        ()
      end;
    end


  end;

  let stmlibv2_str = stmlibv2_str^"(check-sat-using (then qe smt))

(get-model)

(get-info :all-statistics)
" in

  if String.exists (!out_file) ".smt2" then
    let stream = open_out (!out_file) in
    Printf.fprintf stream "%s\n" (stmlibv2_str);
    close_out stream;
    verb (fun _ -> print_endline ("SMTLIBv2 file "^(!out_file)^" saved.")) ;
  else
  begin
    (* do not print if solver is enabled *)
    if isSolverEnabled () then
    begin
      verb (fun _ ->
        print_endline "Synthesis for SMTLIBv2 \n" ;
        print_endline ("--------------------------------------------------------------------------------\n") ;
      ) ;
      print_endline stmlibv2_str ;
    end;
  end
end


open Version
open Rmdslparser
open Rmtld3synth_helper

(*
   Command Line Interface
 *)
let _ =

  let speclist = [
    (* action flags *)
    ("--gen-rmtld-formula", Arg.Unit (set_gen_rmtld_formula), " Call `gen_formula_default` function" );
    ("--synth-smtlibv2", Arg.Unit (set_smt_formula), " Enables synthesis for SMT-LIBv2 language");
    ("--synth-ocaml", Arg.Unit (set_ocaml_language), " Enables synthesis for Ocaml language");
    ("--synth-cpp11", Arg.Unit (set_cpp_language), " Enables synthesis for C++11 language");
    ("--synth-spark2014", Arg.Unit (set_spark14_language), " Enables synthesis for Spark2014 language (unsupported)\n\n Flags for solving: ");

    ("--simpl-cad", Arg.Unit (set_simplify_formula), " Simplify quantified RMTLD formulas using CAD (Experimental)");
    ("--solve-z3",  Arg.Unit (set_solve_z3), " Enables solving smtlibv2 problems using Z3 SMT solver");

    ("--solve-statistics",  Arg.Unit (set_solve_statistics), " Enables printing the solve statistics") ;
    ("--get-schedule",  Arg.Unit (set_get_schedule), " Returns the schedule\n\n Input:") ;
    

    (* input models *)
    ("--input-sexp", Arg.String (set_formulas), " Inputs sexp expression (RMTLD3 formula)");
    ("--input-latexeq", Arg.String (set_formulas_ltxeq), " Inputs latex equation expressions (RMTLD3 formula) (Experimental)");
    ("--input-rmdsl", Arg.String (set_exp_rmdsl), " Inputs rmdsl expressions for schedulability analysis (Experimental)");
    (* this is used only for monitoring synthesis and for automatic generation of some SMT-LIBv2 bechmark problems *)
    ("--config-file", Arg.String (set_config_file), " File containing synthesis settings\n\n Output:");

    (*output models *)
    ("--out-file", Arg.String (set_out_file), " Set the output filename for synthesis");
    ("--out-src", Arg.String (set_out_dir), " Set the output directory for synthesis\n\n Options:");
    
    ("--verbose", Arg.Set_int verb_mode, " Enables verbose mode");
    ("--version", Arg.Unit (fun () -> print_endline ("Git version "^(Version.git)); exit 0), " Version and SW information\n");
  ]
  in let usage_msg = "rmtld3synth flags [options] input [output]\n\n Flags for synthesis: "
  in Arg.parse_argv (Sys.argv) (Arg.align speclist) print_endline usage_msg;

  (* conversion for input formula *)
  let input_fm =
    (* rmtld_formula is undefined ? try rmtld_formula_ltxeq *)
    if !rmtld_formula <> "" then
      begin
      verb (fun _ -> print_endline "Sexp parsing enabled.");
      formula_of_sexp (Sexp.of_string !rmtld_formula)
      end
    else if !rmtld_formula_ltxeq <> "" then
      begin
      verb (fun _ -> print_endline "Latex Eq parsing enabled.");
      Texeqparser.texeqparser !rmtld_formula_ltxeq
      end
    else
      (* there is no imput formula *)
      mfalse
  in

  let to_simplify fm : rmtld3_fm =
    if !simplify_formula then
      let smp = simplify fm in
      verb (fun _ ->
        print_endline "Output formula from the simplification process:\n";
        print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm smp));
        print_endline "--------------------------------------------------------------------------------\n";
      );
      smp
    else fm
    in

  (* for sat case *)
  if !smtlibv2_formula <> false then
    begin
      if input_fm <> mfalse then
      begin
        sat_gen (to_simplify (input_fm));
      end
      else
        begin
          verb (fun _ -> print_endline "Rmdsl parsing enabled.");
          let ex = Rmdslparser.rmdslparser !expression_rmdsl in
          let fm_lst = Rmdslparser.rmtld3_fm_lst_of_rmdsl_lst ex in
          verb (fun _ ->
            print_endline "--------------------------------------------------------------------------------\n";
            print_endline "rmtld3 formula(s): ";
            print_endline ("Available goals: "^(string_of_int (List.length fm_lst)));
          );

          let _ = List.fold_left (fun a fm_goal ->
            sat_gen (to_simplify fm_goal) ;
            a + 1
          ) 1 fm_lst in

          ()

        end
    end

  (*else if !config_file <> "" then
    begin
      print_endline ("Default synthesis filename: " ^ !config_file);
      mon_gen input_fm;
    end*)

  else if !ocaml_lang then
    begin
      verb_m 1 (fun _ ->
        print_endline "Synthesis for Ocaml language" ;
        print_endline ("--------------------------------------------------------------------------------\n") ;
      ) ;
      mon_gen input_fm;
    end

  else if !cpp11_lang then
    begin
      verb_m 1 (fun _ ->
        print_endline "Synthesis for C++11 language" ;
        print_endline ("--------------------------------------------------------------------------------\n") ;
      ) ;
      mon_gen input_fm;
    end

   else if !simplify_formula then
    begin
      (*if !rmtld_formula <> "" then
      begin
        print_endline ("Processing formula: "^(!rmtld_formula));
        let fm = simplify (formula_of_sexp (Sexp.of_string !rmtld_formula)) in
        print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm fm));
      end
      else if !rmtld_formula_ltxeq <> "" then
      begin
        print_endline ("Processing latexeq formula: "^(!rmtld_formula_ltxeq));

        let fm = simplify (formula_of_sexp (Sexp.of_string !rmtld_formula)) in
        print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm fm));
      end
      else
        print_endline "No formula specified. Use --input-sexp"*)
      let inn = input_fm in
      if inn <> mfalse then
        let smp = to_simplify inn in
        print_latex_formula smp
      else raise (Failure ("cannot simplify the specified input."))
      
    end

  else if !gen_rmtld_formula then
    let fm = gen_formula_default () in
    print_latex_formula fm
  
  else
    print_endline "Nothing to do. Type --help"
  
