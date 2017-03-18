(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

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
    | _ -> raise (Failure "compute: missing formula")
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


(* compute the temporal upper bound of a formula *)
let rec calculate_t_upper_bound formula =
  match formula with
    | Prop p                  -> 0.
    | Not sf                  -> calculate_t_upper_bound sf
    | Or (sf1, sf2)           -> max (calculate_t_upper_bound sf1) (calculate_t_upper_bound sf2)
    | Until (gamma, sf1, sf2) -> gamma +. max (calculate_t_upper_bound sf1) (calculate_t_upper_bound sf2)
    | LessThan (tr1,tr2)      -> max (calculate_t_upper_bound_term tr1) (calculate_t_upper_bound_term tr2)
    | _ -> raise (Failure "ERROR: Calculating bound for unsupported term.") 
and calculate_t_upper_bound_term term =
  match term with
    | Constant value       -> 0.
    | Duration (di,phi)    -> 0.
    | FPlus (tr1,tr2)      -> 0.
    | FTimes (tr1,tr2)     -> 0.
    | _ -> raise (Failure "ERROR: Calculating bound for unsupported term.")


(* rmtld3 synthesis interface *)

let verbose = ref false
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

open Unix
open Sexplib
open Sexplib.Conv

open Rmtld3synth_simplify
open Rmtld3synth_unittest
open Rmtld3synth_smt


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
    to generate the same unit tests as benchamarks for SMT solvers, the s-expressions
    synthesis to the SMT-LIBv2 is available in the sub directory "smt/".
  *)
  create_dir ("smt/"^cluster_name);



  (* generate monitors *)
  List.fold_left (fun _ (monitor_name,monitor_period,formula) ->

    (*create_dir (cluster_name^"/"^monitor_name);*)

    (* Synthesize ocaml evaluation algorithm into c++ *)

    (* monitor dependent c++ functions begin here *)

    (* Synthesize ocaml formula evaluation algorithm into c++ *)
    let stream = open_out (cluster_name^"/"^monitor_name^"_compute.h") in
    let code = "
  #ifndef _"^ String.uppercase_ascii (monitor_name^"_compute") ^"_H_
  #define _"^ String.uppercase_ascii (monitor_name^"_compute") ^"_H_

  #include \"rmtld3.h\"
  
  auto _"^monitor_name^"_compute = "^compute (formula) helper^";

  #endif //_"^ String.uppercase_ascii (monitor_name^"_compute") ^"_H_
    " in
    Printf.fprintf stream "%s\n" code;
    close_out stream;


    let stream = open_out (cluster_name^"/"^String.capitalize_ascii monitor_name^".h") in
    let code = "
  #ifndef MONITOR_"^String.uppercase_ascii monitor_name^"_H
  #define MONITOR_"^String.uppercase_ascii monitor_name^"_H

  #include \"Rmtld3_reader.h\"
  #include \"RTML_monitor.h\"

  #include \""^monitor_name^"_compute.h\"
  #include \""^ cluster_name ^".h\"

  class "^String.capitalize_ascii monitor_name^" : public RTML_monitor {

  private:
    RMTLD3_reader< "^ get_event_type(helper) ^" > trace = RMTLD3_reader< "^ get_event_type(helper) ^" >( __buffer_"^ cluster_name ^".getBuffer(), "^ string_of_float (calculate_t_upper_bound formula) ^" );

    struct Environment env;

  protected:
    void run(){

      three_valued_type _out = _"^monitor_name^"_compute(env,0);
      DEBUG_RTEMLD3(\"Veredict:%d\\n\", _out);
    }

  public:
    "^String.capitalize_ascii monitor_name^"(useconds_t p): RTML_monitor(p,SCHED_FIFO,50), env(std::make_pair (0, 0), &trace, __observation) {}

  };

  #endif //MONITOR_"^String.uppercase_ascii monitor_name^"_H" in
    Printf.fprintf stream "%s\n" code;
    close_out stream;

  (* monitor dependent functions ends here *)

  ) () (get_settings_monitor helper);


  (* the next functions will be used to manage the buffers assigned to each formula *)

  let stream = open_out (cluster_name^"/Rmtld3_reader.h") in
  let code = "
  #ifndef _RMTLD3_READER_H_
  #define _RMTLD3_READER_H_

  #include <functional>
  //#include <iterator>
  #include <numeric>

  #include \"RTML_reader.h\"

  #include \"rmtld3.h\"
  #include \"Rmtld3_reader_it.h\"

  template<typename T>
  class RMTLD3_reader : public RTML_reader<T>
  {
    // current buffer state (idx and cnt)
    int state_cnt;
    size_t state_idx;

    timespanw formula_t_upper_bound;

    public:
      RMTLD3_reader(CircularBuffer<T> * const buffer, timespan ub) :
        RTML_reader<T>(buffer),
        formula_t_upper_bound(ub)
      {};

      /** match timestamp and return the index using forward direction (iterator++) */
      std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> searchIndexForwardUntil(TraceIterator<T> &iterator, timespanw t) {

        timeabs current_time;
        timespanw aligned_time;
        size_t current_idx;

        std::pair<timeabs, size_t> pair_state =  iterator.getReader()->getCurrentBufferState();
        current_time = pair_state.first;
        current_idx = pair_state.second;

        aligned_time = iterator.getReader()->getTimeAlignment(current_time);

        DEBUGV_RTEMLD3(\"    ENTER_search_index: c_t:%llu c_idx:%u\\n\", current_time, current_idx);

        /* create an iterator where its end is the endpoint of the buffer
         * and process the iteration until the timespanw t is found. After
         * that return the index and other data where t holds.
         */
        TraceIterator<T> tmp_it = TraceIterator<T>(
          iterator.getReader(),
          iterator.getIt(),                  // lower bound for index
          current_idx,                       // upper bound for index
          iterator.getIt(),                  // [TODO] this idx should be changed;

          iterator.getCurrentAbsoluteTime(), // set lower abs time to the old current abs time
          aligned_time,                      // set the new upper abs time
          iterator.getCurrentAbsoluteTime()  // begin iterator at lower abs time
        );

        //iterator.debug();
        //tmp_it.debug();

        /* lets start the iteration to find the event at time t.
         * the tuple consists of the cumulative timestamp, the event, the
         * absolute index, and a boolean flag indicating tat the event was found.
         * type is std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool>
         */
        auto event_find_tuple = std::accumulate(
          tmp_it.begin(),
          tmp_it.end(),
          std::make_tuple ( tmp_it.getCurrentAbsoluteTime(), "^ get_event_fulltype(helper) ^"(), tmp_it.getIt(), false ),
            [t]( const std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> a, "^ get_event_fulltype(helper) ^" e )
            {
              timespanw time_lowerbound = std::get<0>(a);
              timespanw time_upperbound = time_lowerbound + e.getTime();

              bool found = std::get<3>(a);
              bool cdt = ( time_lowerbound <= t && t < time_upperbound );

              return std::make_tuple (
                (found || cdt)? time_lowerbound : time_upperbound, // time is always cumulating
                (found)? std::get<1>(a) : e, // old event or new event
                (found || cdt)? std::get<2>(a) : std::get<2>(a) + 1, // increment it or hold it
                (cdt)? true : found // the test for the existence of the event
              );
            }
        );

        //::printf(\"aligned_time:%llu found_time:%llu init_value(%llu)\\n\", aligned_time, std::get<0>(event_find_tuple), tmp_it.getCurrentAbsoluteTime());

        // assert if there is no event found then timestamps should be equal
        ASSERT_RMTLD3( (std::get<3>(event_find_tuple)) || (aligned_time == std::get<0>(event_find_tuple)) );

        DEBUGV_RTEMLD3(\"    EXIT_searchindex: find(%d) time(%llu) t=%llu idx(%d)\\n\", std::get<3>(event_find_tuple), std::get<0>(event_find_tuple), t, std::get<2>(event_find_tuple));

        

        return event_find_tuple;
      }; // [TODO]

      // pre-indexed search
      three_valued_type searchOForward(std::pair <size_t, timespanw> &state, proposition p, timespan t) {

        // construct iterator based on the state [TODO]
        // use env.state to speedup the calculation of the new bounds
        TraceIterator<T> it = TraceIterator<T> (this, state.first, 0, state.first, state.second, 0, state.second );

        DEBUGV_RTEMLD3(\"  searchOForward: \");
        //it.debug();

        /* use the function searchIndexForwardUntil to find the index where
         * t hold and check if proposition is satisfied at time t
         */
        // getting the index of current t
        auto event_tuple = searchIndexForwardUntil( it, t );

        if( !std::get<3>(event_tuple) ) // is it not a valid event?
        {
          timespanw current_time = std::get<0>(event_tuple);
          /* here we require to distinguish the Unknown value due to the unknown trace
           * if the bound does not hold than return false
           */
          if ( current_time > formula_t_upper_bound )
          {
            // the bound is violated here; raise error message
            DEBUGV_RTEMLD3(\"monitor overloads with %llu > %llu\\n\", current_time, formula_t_upper_bound);
            return T_FALSE;
          }
          else
          {
            //DEBUGV_RTEMLD3(\"    Unknown\\n\");
            return T_UNKNOWN;
          }
        }
        
        proposition new_p = std::get<1>(event_tuple).getData();

        DEBUGV_RTEMLD3(\"end searchOForward\\n\");

        if(new_p == p)
        {
          //DEBUGV_RTEMLD3(\"    True\\n\");
          return T_TRUE;
        }
        else
        {
          //DEBUGV_RTEMLD3(\"    False\\n\");
          return T_FALSE;
        }

      }; // [TODO]

      three_valued_type searchOBackward(std::pair <size_t, timespanw> &state, proposition p, timespan t) { return T_FALSE; }; // [TODO]

  };


  "^ synth_environment helper ^"

  #endif //_RMTLD3_READER_H_
  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


  let stream = open_out (cluster_name^"/Rmtld3_reader_it.h") in
  let code = "

  #include \"RTML_reader.h\"

  // defines an interator for a trace
  template<typename T>
  class TraceIterator :  public std::iterator< std::input_iterator_tag, T >
  {
    /*
     * Lets define the reader for communication with the rtemlib
     */
    RTML_reader<T> * __reader;

    size_t ibegin, iend, it;

    timespanw lower_abstime; // absolute time of the first iterator
    timespanw upper_abstime; // absolute time of the last iterator
    timespanw current_abstime; // absolute time of the current iterator

    bool enough; // a flag of enough elements for setbound function


    void setBegin(size_t b) { ibegin = b; }
    void setEnd(size_t e) { iend = e; }
    void setIt(size_t iter) { it = iter; }

    void setLowerAbsTime(timespanw lower_t) { lower_abstime = lower_t; }
    void setUpperAbsTime(timespanw upper_t) { upper_abstime = upper_t; }
    void setCurrentAbsTime(timespanw curr_t) { current_abstime = curr_t; }

    public:
      TraceIterator<T> (RTML_reader<T> * _l_reader,
        size_t i, size_t e, size_t iter,
        timespanw t_l, timespanw t_u, timespanw ct) :
        __reader(_l_reader),
        ibegin(i), iend(e), it(iter),
        lower_abstime(t_l), upper_abstime(t_u), current_abstime(ct)
      {};

      RTML_reader<T> * getReader() { return __reader; }

      size_t getBegin() { return ibegin; }
      size_t getIt() { return it; }
      bool getEnoughSize() { return enough; }

      timespanw getLowerAbsoluteTime() { return lower_abstime; }
      timespanw getUpperAbsoluteTime() { return upper_abstime; }
      timespanw getCurrentAbsoluteTime() { return current_abstime; }

      void setBound(std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> lb,
        std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> ub) {

        auto lb_absolute_idx = std::get<2>(lb);
        auto ub_absolute_idx = std::get<2>(ub) + 1;

        auto lower_abs_time = std::get<0>(lb);
        auto upper_abs_time = std::get<0>(ub);

        enough = std::get<3>(ub);

        setBegin( lb_absolute_idx );
        setIt( lb_absolute_idx );
        setEnd( ub_absolute_idx );

        // updates the timestamps
        setLowerAbsTime(lower_abs_time);
        setCurrentAbsTime(lower_abs_time);
        setUpperAbsTime(upper_abs_time);

        DEBUGV_RTEMLD3(\"setBound: \");
        debug();
      }

      TraceIterator<T> begin() {
        return TraceIterator<T> (
          __reader,
          ibegin, iend, ibegin, lower_abstime, lower_abstime, upper_abstime );
      }

      TraceIterator<T> end() {
        return TraceIterator<T> (
          __reader,
          ibegin, iend, iend, upper_abstime, lower_abstime, upper_abstime);
      }

      TraceIterator<T>& operator++() {
        ++ it;

        // update absolute current time of the event
        current_abstime += operator*().getTime(); // [TODO] [VERIFY]

        return *this;
      } // [CONFIRM]

      bool operator==(const TraceIterator<T>& rhs) { return it == rhs.it; } // [CONFIRM]
      bool operator!=(const TraceIterator<T>& rhs) { return !(operator==(rhs)); } // [CONFIRM]

      Event<T> operator*() {

        // here we could adopt a small buffer to avoid successive call of dequeues (for instance a local buffer of 10 elements)
        // dequeue the event of the it index

        auto ev_it = it % __reader->getHigherIdx();

        std::pair<state_rd_t,Event<T> > x = __reader->dequeue(ev_it);

        return x.second;
      } // [CONFIRM]

      void debug()
      {
        DEBUGV_RTEMLD3(\"It:%u %u %u %llu %llu %llu. pt:%p\\n\", ibegin, iend, it, lower_abstime, upper_abstime, current_abstime, __reader);
      }
  };
  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


  let stream = open_out (cluster_name^"/rmtld3.h") in
  let code = "
  #ifndef _RMTLD3_H_
  #define _RMTLD3_H_

  #include \"Event.h\"
  #include \"time_compat.h\"

  typedef unsigned int proposition;
  typedef float realnumber;
  typedef std::pair<realnumber, bool> duration;

  enum three_valued_type { T_TRUE, T_FALSE, T_UNKNOWN };
  enum four_valued_type { FV_TRUE, FV_FALSE, FV_UNKNOWN, FV_SYMBOL };

  #define make_duration(r,b) std::make_pair ((realnumber)r,b)

  // type conversion from three_valued_type to four_valued_type
  #define b3_to_b4(b3) (b3 == T_TRUE) ? FV_TRUE : ( ( b3 == T_FALSE ) ? FV_FALSE : FV_UNKNOWN )

  // convert four_valued_type into three_valued type
  #define b4_to_b3(b4) (b4 == FV_TRUE ) ? T_TRUE : ( ( b4 == FV_FALSE ) ? T_FALSE : T_UNKNOWN )


  /** OR */
  #define b3_or(b31, b32) \\
    ( b31 == T_TRUE || b32 == T_TRUE ) ? T_TRUE : \\
      (( b31 == T_FALSE && b32 == T_FALSE ) ? T_FALSE : T_UNKNOWN)

  /** NOT */
  #define b3_not(b3) ( (b3 == T_TRUE) ? T_FALSE : ( (b3 == T_FALSE) ? T_TRUE : T_UNKNOWN ) )

  /** Relation operator < */
  #define b3_lessthan(n1,n2) \\
    ( (std::get<1>(n1) || std::get<1>(n2))? T_UNKNOWN : ( ( std::get<0>(n1) < std::get<0>(n2) )? T_TRUE : T_FALSE ) )

  #define ASSERT_RMTLD3(l) \
    if(!(l)) ::printf(\"assert failed.\\n\")

  //#define DEBUG_VERBOSE

  #ifndef DEBUG_VERBOSE
    #define DEBUGV_RTEMLD3(...)
  #else
    #define DEBUGV_RTEMLD3(args ...) \
      ::printf(args)
  #endif

  #define DEBUG_RTEMLD3(args ...) ::printf(args)

  #define out_p(res) \
  (res == T_TRUE)? \"true\" : ((res == T_FALSE)? \"false\": \"unknown\")

  #define out_fv(fv) \
  (fv == FV_TRUE)? \"true\" : ((fv == FV_FALSE)? \"false\" : ((fv == FV_UNKNOWN)? \"unknown\" : \"symbol\" ) )

  extern int count_until_iterations;

  #endif //_RMTLD3_H_
  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


(* this standalone cpp file instantiates the monitors that have been synthesized *)
  let stream = open_out (cluster_name^"/"^ cluster_name ^".cpp") in
  let headers = List.fold_left (fun h (monitor_name,_,_) -> h^"#include \""^String.capitalize_ascii monitor_name^".h\"\n" ) "" (get_settings_monitor helper) in
  let functions = List.fold_left (fun f (monitor_name,monitor_period,_) -> f^String.capitalize_ascii monitor_name^" mon_"^monitor_name^"("^string_of_int monitor_period^");\n" ) "" (get_settings_monitor helper) in
  let code = headers ^"#include \"RTML_buffer.h\"

int count_until_iterations;

#ifdef __NUTTX__
__EXPORT RTML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^" __attribute__((used));
#else
RTML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^" __attribute__((used));
#endif
"
^ functions ^
"
void __start_periodic_monitors()
{
"^ List.fold_left (fun f (monitor_name,monitor_period,_) -> f^"  if (mon_"^monitor_name^".enable()) {::printf(\"ERROR\\n\");}\n" ) "" (get_settings_monitor helper) ^"
}
"
in
  Printf.fprintf stream "%s\n" code;
  close_out stream;

let stream = open_out (cluster_name^"/"^ cluster_name ^".h") in
let propositions = 
  let tbl = get_proposition_hashtbl helper in
  Hashtbl.fold (fun a_string b_int prop_list -> prop_list^ "#define P_"^a_string^" "^(string_of_int b_int)^"\n" ) tbl ""
in
let code = "#ifndef _"^String.uppercase_ascii cluster_name^"_H_
#define _"^String.uppercase_ascii cluster_name^"_H_

#include \"RTML_buffer.h\"

extern void __start_periodic_monitors();

extern RTML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^";

"^propositions^"

#endif //_"^String.uppercase_ascii cluster_name^"_H_
"
in
Printf.fprintf stream "%s\n" code;
close_out stream;


let stream = open_out (cluster_name^"/Makefile") in
let code =
"
.DEFAULT_GOAL := all

DIR = tests

arm-monitor:
\t arm-none-eabi-g++ -std=c++0x -march=armv7-m -g -fverbose-asm -O -IC:\\ardupilot_pixhawk_testcase\\ardupilot\\modules\\PX4NuttX\\nuttx\\include -Wframe-larger-than=1200 -DCONFIG_WCHAR_BUILTIN -I../../arch/arm/include -I../../ -DARM_CM4_FP -D__NUTTX__ --verbose -c monitor_set1.cpp

x86-monitor:
\t g++ -Wall -g -O0 -std=c++0x -I../../ -D__x86__ --verbose -c "^cluster_name^".cpp

.PHONY: tests
tests:
\t make -C $(DIR)

x86-mtest: x86-monitor tests
\t g++ "^cluster_name^".o tests/tests.o -L../../ -lrteml -pthread -o "^cluster_name^"

arm-mon: arm-monitor

x86-mon: x86-monitor

all:

" in
Printf.fprintf stream "%s\n" code;
close_out stream;

(* the buffers need to be managed according to its size and type; one buffer
   could be used for several monitors; a monitor is the synthesis of a formula
 *)

(* lets generate the tests *)
if (search_settings_string "gen_tests" helper) = "true" then
begin
  create_dir (cluster_name^"/tests");
  Rmtld3synth_unittest.test () cluster_name helper;

  Rmtld3synth_unittest.rmtld3_unit_test_generation () compute helper cluster_name helper;
end

let sat_gen formula =
begin
  let stream = open_out ("smt/smtlibv2_formula_example.smt2") in
  Printf.fprintf stream "%s\n" (rmtld3synthsmt formula);
  close_out stream;
end


open Version
open Rmdslparser

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
    
    ("--verbose", Arg.Set verbose, " Enables verbose mode");
    ("--version", Arg.Unit (fun () -> print_endline ("Git version "^(Version.git)); exit 0), " Version and SW information\n");
  ]
  in let usage_msg = "rmtld3synth flags [options] input [output]\n\n Flags: "
  in Arg.parse (Arg.align speclist) print_endline usage_msg;

  

  (* for sat case *)
  if !smtlibv2_formula <> false then
    begin
      (* rmtld_formula is undefined ? try rmtld_formula_ltxeq *)
      if !rmtld_formula <> "" then
        sat_gen (formula_of_sexp (Sexp.of_string !rmtld_formula))
      else if !rmtld_formula_ltxeq <> "" then
        begin
          print_endline "Latex Eq parsing enabled.";
          Texeqparser.texeqparser !rmtld_formula_ltxeq;
        end
      else
          print_endline "Rmdsl parsing enabled.";
          Rmdslparser.rmdslparser !expression_rmdsl
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
        Rmtld3synth_simplify.simplify (formula_of_sexp (Sexp.of_string !rmtld_formula));
      end
      else
        print_endline "No formula specified."
    end

  else
    print_endline "Nothing to do. Type --help"
  
