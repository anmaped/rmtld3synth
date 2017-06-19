(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

open Sexplib
open Sexplib.Conv

open Rmtld3synth_helper

(* function that pretty prints 'observation' function as a lambda functions in c++ *)
let synth_obs_function observation_funcname struct_name =
  (* *)
  let code = "
  // obs lambda function
  auto "^observation_funcname^" = []( struct "^struct_name^" &env, proposition p, timespan t) mutable -> three_valued_type
  {
    DEBUGV_RTEMLD3(\"  eval: %lu prop:%d\\n\", t, p);
    return b3_or ( env.trace->searchOForward(env.state, p, t), env.trace->searchOBackward(env.state, p, t) );
  };
  " in
  code

  (* function that prety prints 'environment' function as a lambda function in c++ *)
let synth_environment helper =
  let struct_name = "Environment" in
  let circular_buffer_vartype = "RMTLD3_reader< "^ get_event_type(helper) ^" >" in
  let observation_funcname = "__observation" in
  (* object to read traces containing current duration and element numbers (since is a circular buffer then size is constant)*)
  let code_init = "
  struct "^struct_name^" {
    std::pair <size_t, timespanw> state;
    "^circular_buffer_vartype^" * const trace;
    three_valued_type (*const evaluate)(struct Environment &, proposition, timespan);

    "^struct_name^"(
      std::pair <size_t, timespanw> st,
      "^circular_buffer_vartype^" * const t,
      three_valued_type (*const ev)(struct Environment &, proposition, timespan)
    ) :
      state(st),
      trace(t),
      evaluate(ev) {};
  };
  " in

  (* observation function where duration_varname is the name of the current duration variable *)
  let code_obs = synth_obs_function observation_funcname (struct_name) in
  let code_end = "
  " in
  (code_init^code_obs^code_end)



open Rmtld3

let compute_function_head = "[](struct Environment env, timespan t) -> three_valued_type";;

let compute_function_head_mutable = "[](struct Environment &env, timespan t) mutable -> three_valued_type";;

let compute_term_function_head = "[](struct Environment env, timespan t) -> duration";;

let trace_iterator helper = "TraceIterator< "^ get_event_type(helper) ^" >";;

(* Synthesis of the compute function  *)
let rec compute_term term helper =
  match term with
    | Constant value       -> "make_duration("^string_of_float value^",false)"
    | Duration (di,phi)    -> compute_term_duration (compute_term di helper) (compute phi helper) helper
    | FPlus (tr1,tr2)      -> "make_duration("^compute_term tr1 helper ^" + "^ compute_term tr2 helper^",false)"
    | FTimes (tr1,tr2)     -> "make_duration("^compute_term tr1 helper ^" * "^ compute_term tr2 helper^",false)"
    | _ -> raise (Failure "compute_terms: missing term")
and compute formula helper =
  match formula with
    | Prop p                  -> let tbl = get_proposition_hashtbl helper in
                                 let counter = get_proposition_counter helper in 
                                 compute_function_head_mutable^" { return env.evaluate(env, "^
                                  string_of_int (
                                    try Hashtbl.find tbl p with Not_found -> Hashtbl.add tbl p counter; counter 
                                                )^
                                 ", t); }"
    | Not sf                  -> compute_function_head_mutable^" { auto sf = "^ compute sf helper ^"(env,t); return b3_not (sf); }"
    | Or (sf1, sf2)           -> compute_function_head_mutable^" { auto sf1 = "^ compute sf1 helper ^"(env,t); auto sf2 = "^ compute sf2 helper ^"(env,t); return b3_or (sf1, sf2); }"
    | Until (gamma, sf1, sf2) -> if gamma > 0. then compute_uless gamma (compute sf1 helper) (compute sf2 helper) helper else raise  (Failure "Gamma of U operator is a non-negative value") 
    | LessThan (tr1,tr2)      -> compute_function_head_mutable^" { return "^compute_function_head^" { auto tr1 = "^ compute_term tr1 helper ^"; auto tr2 = "^ compute_term tr2 helper ^"; return b3_lessthan (tr1, tr2); }(env,t); }"
    | _ -> raise (Failure ("synth_mon: bad formula "^( Sexp.to_string_hum (sexp_of_rmtld3_fm formula))))
and compute_uless gamma sf1 sf2 helper =
  compute_function_head ^"
  {
    auto eval_fold = []( struct Environment env, timespan t, "^trace_iterator helper^" iter) -> four_valued_type
    {

      // eval_b lambda function
      auto eval_b = []( struct Environment env, timespan t, four_valued_type v ) -> four_valued_type
      {
        // eval_i lambda function
        auto eval_i = [](three_valued_type b1, three_valued_type b2) -> four_valued_type
        {
          return (b2 != T_FALSE) ? b3_to_b4(b2) : ( (b1 != T_TRUE && b2 == T_FALSE) ? b3_to_b4(b1) : FV_SYMBOL );
        };

        // change this (trying to get the maximum complexity)
        //if ( v == FV_SYMBOL )
        //{
          DEBUGV_RTEMLD3(\"  compute phi1\\n\");
          // compute phi1
          three_valued_type cmpphi1 = "^sf1^"(env, t);

          DEBUGV_RTEMLD3(\"  compute phi2\\n\");
          // compute phi2
          three_valued_type cmpphi2 = "^sf2^"(env, t);

          four_valued_type rs = eval_i(cmpphi1, cmpphi2);

          DEBUGV_RTEMLD3(\" phi1=%s UNTIL phi2=%s\\n\", out_p(cmpphi1), out_p(cmpphi2) );

        if ( v == FV_SYMBOL )
        {
          return rs;
        }
        else
        {
          return v;
        }
      };

      DEBUGV_RTEMLD3(\"BEGIN until_op.\\n\\n \");
      iter.debug();

      ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

      auto cos = iter.getBegin();

      four_valued_type s = std::accumulate(
        iter.begin(),
        iter.end(),
        std::pair<four_valued_type, timespan>(FV_SYMBOL, t),
          [&env, &cos, eval_b]( const std::pair<four_valued_type, timespan> a, "^ get_event_fulltype(helper) ^" e ) {
            
            DEBUGV_RTEMLD3(\"  until++ (%s)\\n\", out_fv(a.first));

            count_until_iterations += 1;

            /* update the new state based on the sub_k calculate iterator.
             * optimization step: update current index to avoid re-read/evaluate events several times
             */
            env.state = std::make_pair ( cos,  a.second);
            cos++;

            return std::make_pair( eval_b( env, a.second, a.first ), a.second + e.getTime() );
          }
      ).first;

      return s;
    };


    // sub_k function defines a sub-trace
    auto sub_k = []( struct Environment env, timespan t) -> "^trace_iterator helper^"
    {

      // use env.state to speedup the calculation of the new bounds
      "^trace_iterator helper^" iter = "^trace_iterator helper^" (env.trace, env.state.first, 0, env.state.first, env.state.second, 0, env.state.second );

      // to use the iterator for both searches we use one reference
      "^trace_iterator helper^" &it = iter;

      ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

      auto lower = env.trace->searchIndexForwardUntil( it, t);
      auto upper = env.trace->searchIndexForwardUntil( it, (t + "^string_of_float (gamma)^") - 1 );

      // set TraceIterator for interval [t, t+"^string_of_float (gamma)^"[
      it.setBound(lower, upper);

      // return iterator ... interval length may be zero
      return it;
    };

    "^trace_iterator helper^" subk = sub_k(env, t);

    four_valued_type eval_c = eval_fold(env, t, subk );

    DEBUGV_RTEMLD3(\"END until_op (%s) enough(%d) .\\n\\n \", out_fv(eval_c), subk.getEnoughSize() );
    
    return ( eval_c == FV_SYMBOL ) ?
      ( ( !subk.getEnoughSize() ) ? T_UNKNOWN : T_FALSE )
    :
      b4_to_b3(eval_c);
  }

  "
  and compute_term_duration di tf helper =
    compute_term_function_head^" {
    
    auto eval_eta =  [](struct Environment env, timespan t, timespan t_upper, "^trace_iterator helper^" iter) -> duration
    {
      auto indicator_function = [](struct Environment env, timespan t) -> duration {
        auto formula = "^tf^"(env, t);

        return (formula == T_TRUE)? std::make_pair (1,false) : ( (formula == T_FALSE)? std::make_pair (0,false) : std::make_pair (0,true)) ;

      };


      // compare if t is equal to the lower bound
      auto lower = iter.getLowerAbsoluteTime();
      // compare if t is equal to the upper bound
      auto upper = iter.getUpperAbsoluteTime();

      timespan val1 = ( t == lower )? 0 : t - lower;
      timespan val2 = ( t_upper == upper )? 0 : t_upper - upper;

      DEBUGV_RTEMLD3(\"dur lower(%ld) upper(%ld)\\n\", val1, val2);

      auto cum = lower;

      // lets do the fold over the trace
      return std::accumulate(
        iter.begin(),
        iter.end(),
        std::make_pair (make_duration (0, false), (timespan)lower), // initial fold data (duration starts at 0)
        [&env, val1, val2, &cum, t, t_upper, indicator_function]( const std::pair<duration,timespan> p, "^ get_event_fulltype(helper) ^" e )
        {
          auto d = p.first;

          auto t_begin = cum;
          auto t_end = t_begin + e.getTime();
          cum = t_end;

          auto cond1 = t_begin <= t && t < t_end;
          auto cond2 = t_begin <= t_upper && t_upper < t_end;

          auto valx = ((cond1)? val1 : 0 ) + ((cond2)? val2 : 0);

          auto x = indicator_function(env, p.second);

          DEBUGV_RTEMLD3(\"dur=%f bottom=%d\\n\", d.first + (x.first * ( e.getTime() - valx )), d.second || x.second);

          return std::make_pair (make_duration (d.first + (x.first * ( e.getTime() - valx )), d.second || x.second), p.second + e.getTime());
        }
      ).first;
      
    };

    // sub_k function defines a sub-trace
    auto sub_k = []( struct Environment env, timespan t, timespan t_upper) -> "^trace_iterator helper^"
    {

      // use env.state to speedup the calculation of the new bounds
      "^trace_iterator helper^" iter = "^trace_iterator helper^" ( env.trace, env.state.first, 0, env.state.first, env.state.second, 0, env.state.second );

      // to use the iterator for both searches we use one reference
      "^trace_iterator helper^" &it = iter;

      ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

      auto lower = env.trace->searchIndexForwardUntil( it, t);
      auto upper = env.trace->searchIndexForwardUntil( it, t_upper - 1 );

      // set TraceIterator for interval [t, t + di[
      it.setBound(lower, upper);

      // return iterator ... interval length may be zero
      return it;
    };

    auto t_upper = t + "^di^".first;

    return eval_eta(env, t, t_upper, sub_k(env, t, t_upper));

    /*let indicator_function (k,u) t phi = if compute (k,u,t) phi = True then 1. else 0. in
        let riemann_sum m dt (i,i') phi =
          (* dt=(t,t') and t in ]i,i'] or t' in ]i,i'] *)
          count_duration := !count_duration + 1 ;
          let t,t' = dt in
          if i <= t && t < i' then
            (* lower bound *)
            (i'-.t) *. (indicator_function m i' phi)
          else (
            if i <= t' && t' < i' then
              (* upper bound *)
              (t'-.i) *. (indicator_function m t' phi)
            else
              (i'-.i) *. (indicator_function m i' phi)
          ) in
        let eval_eta m dt phi x = fold_left (fun s (prop,(i,t')) -> (riemann_sum
        m dt (i,t') phi) +. s) 0. x in
        let t,t' = dt in
        eval_eta (k,u) dt formula (sub_k (k,u,t) t')*/


    }(env,t)"


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



  (* generate monitors *)
  List.fold_left (fun _ (monitor_name,monitor_period,formula) ->

    (*create_dir (cluster_name^"/"^monitor_name);*)

    (* monitor synthesis to cpp11 *)
    synth_cpp11_compute cluster_name monitor_name monitor_period formula compute helper;

  ) () (get_settings_monitor helper);

  synth_cpp1_external_dep cluster_name synth_environment helper;

  synth_cpp11_env cluster_name evt_subtype event_queue_size helper;

  
  (* lets generate the tests *)
  if (search_settings_string "gen_tests" helper) = "true" then
  begin
    create_dir (cluster_name^"/tests");
    Rmtld3synth_unittest.test () cluster_name helper;

    Rmtld3synth_unittest.rmtld3_unit_test_generation () compute helper cluster_name helper;
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
    ("--simplify", Arg.Unit (set_simplify_formula), " Simplify quantified RMTLD formulas using CAD");
    ("--smt-lib-v2", Arg.Unit (set_smt_formula), " Enables Satisfability problem encoding in SMT-LIBv2 language\n\n Input:");
    

    (* input models *)
    ("--input-sexp", Arg.String (set_formulas), " Inputs sexp expression (RMTLD3 formula)");
    ("--input-latexeq", Arg.String (set_formulas_ltxeq), " Inputs latex equation expressions (RMTLD3 formula)");
    ("--input-rmdsl", Arg.String (set_exp_rmdsl), " Inputs rmdsl expressions for schedulability analysis");
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
  
