(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

open Sexplib
open Sexplib.Conv

open Rmtld3
open Rmtld3synth_helper

(*open Rmtld3synth_cpp11*) (* module for cpp11 synthesis *)
open Rmtld3synth_ocaml

(*let module M = (val m : Rmtld3synth_ocaml)*)
module type Conversion =
sig
  val compute_tm_constant : value -> helper -> string
  val compute_tm_duration : string -> string -> helper -> string
  val compute_tm_plus : string -> string -> helper -> string
  val compute_tm_times : string -> string -> helper -> string
  val compute_fm_p : prop -> helper -> string
  val compute_fm_not : string -> helper -> string
  val compute_fm_or : string -> string -> helper -> string
  val compute_fm_less : string -> string -> helper -> string
  val compute_fm_uless : value -> string -> string -> helper -> string
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
      | Prop p                  -> Conv.compute_fm_p p helper
      | Not sf                  -> Conv.compute_fm_not (compute sf helper) helper
      | Or (sf1, sf2)           -> Conv.compute_fm_or (compute sf1 helper) (compute sf2 helper) helper
      | Until (gamma, sf1, sf2) -> if gamma > 0. then Conv.compute_fm_uless gamma (compute sf1 helper) (compute sf2 helper) helper
                                   else raise  (Failure "Gamma of U operator is a non-negative value") 
      | LessThan (tr1,tr2)      -> Conv.compute_fm_less (compute_term tr1 helper) (compute_term tr2 helper) helper
      | _                       -> raise (Failure ("synth_mon: bad formula "^( Sexp.to_string_hum (sexp_of_rmtld3_fm formula))))

end;;

(* rmtld3 synthesis interface *)

let config_mon_filename = ref ""
let rmtld_formula = ref ""
let rmtld_formula_ltxeq = ref ""
let expression_rmdsl = ref ""
let smtlibv2_formula = ref false
let simplify_formula = ref false
let smt_out_dir = ref ""

let set_config_file file = config_mon_filename := file
let set_formulas f = rmtld_formula := f
let set_formulas_ltxeq f = rmtld_formula_ltxeq := f
let set_exp_rmdsl f = expression_rmdsl := f
let set_smt_formula f = smtlibv2_formula := true
let set_simplify_formula f = simplify_formula := true
let set_smt_out_dir f = smt_out_dir := f
let set_ocaml_language f = ()
let set_cpp_language f = ()
let set_spark14_anguage f = ()

open Batteries
open Unix
open Sexplib
open Sexplib.Conv


open Rmtld3synth_simplify
open Rmtld3synth_unittest
open Rmtld3synth_smt
open Rmtld3synth_cpp11


let mon_gen () =
  (* helper to support query's settings along execution *)
  let helper = (ref "", ref "", ref 0, (settings config_mon_filename), [(ref 0, Hashtbl.create 10); (ref 0, Hashtbl.create 10)]) in
  let create_dir dir_name = try let state = Sys.is_directory dir_name in if state then () else  Unix.mkdir dir_name 0o666; with _ -> Unix.mkdir dir_name 0o666 in

  (* c++ type templates with pattern 'evt_type < evt_subtype > '  *)
  (* evt_type is a class given by rtmlib *)
  let evt_type = search_settings_string "event_type" helper in
  (* evt_subtype can be a primitive type or a class *)
  let evt_subtype = search_settings_string "event_subtype" helper in

  set_event_type evt_type helper;
  set_event_subtype evt_subtype helper;

  (* monitor synthesis settings *)
  (* buffer size *)
  let event_queue_size = 
      max (search_settings_int "buffer_size" helper)
      (* compute the buffer size based on inter-arrival time *)
      ((List.fold_left (fun v (_,_,formula) -> let value = int_of_float (calculate_t_upper_bound formula) in if value > v then value else v ) 0 (get_settings_monitor helper))
        / (search_settings_int "maximum_inter_arrival_time" helper))
  in
  Printf.printf "Buffer is defined as length %d\n" event_queue_size;

  (* monitor cluster name *)
  let cluster_name = search_settings_string "cluster_name" helper in (* search 'cluster_name' setting in the global_string parameters *)
  create_dir cluster_name;

  (* create dir for storage of SMT benchmarks *)
  (* 
    'cluster_name' setting is the name for the synthesis group. However, in order
    to generate the unit tests for SMT solvers' benchmark, the synthesis
    of s-expressions into SMT-LIBv2 is available in the hardcoded sub directory
    "smt/".
  *)
  create_dir ("smt/"^cluster_name);


  (* for cpp11 *)
  let module Conv_cpp11 = Conversion_cpp(Rmtld3synth_cpp11) in
  let module Conv_ocaml = Conversion_cpp(Rmtld3synth_ocaml) in

  (* generate monitors *)
  List.fold_left (fun _ (monitor_name,monitor_period,formula) ->

    (*create_dir (cluster_name^"/"^monitor_name);*)

    (* monitor synthesis to cpp11 *)
    synth_cpp11_compute cluster_name monitor_name monitor_period formula (Conv_cpp11.compute) helper;

  ) () (get_settings_monitor helper);

  synth_cpp1_external_dep cluster_name helper;

  synth_cpp11_env cluster_name evt_subtype event_queue_size helper;

  
  (* lets generate the tests *)
  if (search_settings_string "gen_tests" helper) = "true" then
  begin
    create_dir (cluster_name^"/tests");
    Rmtld3synth_unittest.test () cluster_name helper;

    Rmtld3synth_unittest.rmtld3_unit_test_generation () (Conv_cpp11.compute) helper cluster_name helper;
  end


let sat_gen formula =
begin
  if String.exists (!smt_out_dir) ".smt2" then
    let stream = open_out (!smt_out_dir) in
    Printf.fprintf stream "%s\n" (rmtld3synthsmt formula);
    close_out stream;
    verb (fun _ -> print_endline ("SMTLIBv2 file "^(!smt_out_dir)^" saved."));
  else
  begin
    verb (fun _ -> print_endline "SMTLIBv2 file: \n");
    print_endline (rmtld3synthsmt formula);
  end
end


open Version
open Rmdslparser

(*
   Command Line Interface
 *)
let _ =

  let speclist = [
    (* action flags *)
    ("--synth-smtlibv2", Arg.Unit (set_smt_formula), " Enables synthesis for SMT-LIBv2 language");
    ("--synth-ocaml", Arg.Unit (set_ocaml_language)," Enables synthesis for Ocaml language");
    ("--synth-cpp11", Arg.Unit (set_cpp_language), " Enables synthesis for C++11 language");
    ("--synth-spark2014", Arg.Unit (set_spark14_anguage), " Enables synthesis for Spark2014 language (Experimental)");
    ("--simpl-cad", Arg.Unit (set_simplify_formula), " Simplify quantified RMTLD formulas using CAD (Experimental)\n\n Input:");

    

    (* input models *)
    ("--input-sexp", Arg.String (set_formulas), " Inputs sexp expression (RMTLD3 formula)");
    ("--input-latexeq", Arg.String (set_formulas_ltxeq), " Inputs latex equation expressions (RMTLD3 formula) (Experimental)");
    ("--input-rmdsl", Arg.String (set_exp_rmdsl), " Inputs rmdsl expressions for schedulability analysis (Experimental)");
    (* this is used only for monitoring synthesis and for automatic generation of some SMT-LIBv2 bechmark problems *)
    ("--config-file", Arg.String (set_config_file), " File containing synthesis settings\n\n Output:");

    (*output models *)
    ("--out-smt-file", Arg.String (set_smt_out_dir), " Set the output filename and directory for SMTLIBv2 file");
    ("--out-mon-folder", Arg.Unit(fun () -> ()), " Set the output folder for monitor synthesis\n\n Options:");
    
    ("--verbose", Arg.Set_int verb_mode, " Enables verbose mode");
    ("--version", Arg.Unit (fun () -> print_endline ("Git version "^(Version.git)); exit 0), " Version and SW information\n");
  ]
  in let usage_msg = "rmtld3synth flags [options] input [output]\n\n Flags: "
  in Arg.parse (Arg.align speclist) print_endline usage_msg;

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
      (* rmtld_formula is undefined ? try rmtld_formula_ltxeq *)
      if !rmtld_formula <> "" then
        begin
          (* get satisfability of a plain formula *)
          sat_gen (to_simplify (formula_of_sexp (Sexp.of_string !rmtld_formula)))
        end
      else if !rmtld_formula_ltxeq <> "" then
        begin
          verb (fun _ -> print_endline "Latex Eq parsing enabled.";
            Texeqparser.texeqparser !rmtld_formula_ltxeq;
            (* type convert to rmtld3_fm USE: sat_gen (to_simplify X) *)
          );
        end
      else
        begin
          verb (fun _ -> print_endline "Rmdsl parsing enabled.");
          let ex = Rmdslparser.rmdslparser !expression_rmdsl in
          let fm_lst = Rmdslparser.rmtld3_fm_of_rmdsl ex in
          verb (fun _ ->
            print_endline "--------------------------------------------------------------------------------\n";
            print_endline "rmtld3 formula(s): ";
            print_endline ("Available goals: "^(string_of_int (List.length fm_lst)));
          );

          let _ = List.fold_left (fun a b ->
            let ex,ex2 = b (mtrue,mtrue) mtrue in (* TODO skip ex2 *)
            verb (fun _ ->
              print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm ex));
              print_endline "--------------------------------------------------------------------------------\n";
            );
            sat_gen (to_simplify ex);
            a + 1
          ) 1 fm_lst in
          
          ()

        end
    end

  else if !config_mon_filename <> "" then
    begin
      print_endline ("Default synthesis filename: " ^ !config_mon_filename);
      mon_gen ();
    end

  else if !simplify_formula then
    begin
      if !rmtld_formula <> "" then
      begin
        print_endline ("Processing formula: "^(!rmtld_formula));
        let fm = simplify (formula_of_sexp (Sexp.of_string !rmtld_formula)) in
        print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm fm));
      end
      else
        print_endline "No formula specified. Use --input-sexp"
    end

  else
    print_endline "Nothing to do. Type --help"
  
