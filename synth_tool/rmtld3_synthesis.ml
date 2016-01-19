(*pp camlp4o -I C:/cygwin/home/anmap/.opam/system/lib/type_conv C:\cygwin\home\anmap\.opam\system\lib\type_conv\pa_type_conv.cma -I C:/cygwin/home/anmap/.opam/system/lib/sexplib C:\cygwin\home\anmap\.opam\system\lib\sexplib\pa_sexp_conv.cma -I +camlp4 *)

open Helper

(* function that pretty prints 'observation' function as a lambda functions in c++ *)
let synth_obs_function observation_funcname struct_name =
  (* *)
  let code = "
  // obs lambda function
  auto "^observation_funcname^" = []( struct "^struct_name^" env, proposition p, timespan t) -> three_valued_type
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
    "^circular_buffer_vartype^" *trace;
    three_valued_type (*evaluate)(struct Environment, proposition, timespan);

    "^struct_name^"(
      std::pair <size_t, timespanw> st,
      "^circular_buffer_vartype^" * t,
      three_valued_type (*ev)(struct Environment, proposition, timespan)
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

let compute_term_function_head = "[](struct Environment env, timespan t) -> duration";;

(* Synthesis of the compute function  *)
let rec compute_term term helper =
  match term with
    | Constant value       -> "std::make_pair("^string_of_float value^",false)"
    | Duration (di,phi)    -> compute_term_duration (compute_term di helper) (compute phi helper)
    | FPlus (tr1,tr2)      -> "std::make_pair("^compute_term tr1 helper ^" + "^ compute_term tr2 helper^",false)"
    | FTimes (tr1,tr2)     -> "std::make_pair("^compute_term tr1 helper ^" * "^ compute_term tr2 helper^",false)"
    | _ -> raise (Failure "compute_terms: missing term")
and compute formula helper =
  match formula with
    | Prop p                  -> let tbl = get_proposition_hashtbl helper in
                                 let counter = get_proposition_counter helper in 
                                 compute_function_head^" { return env.evaluate(env, "^
                                  string_of_int (
                                    try Hashtbl.find tbl p with Not_found -> Hashtbl.add tbl p counter; counter 
                                                )^
                                 ", t); }"
    | Not sf                  -> compute_function_head^" { return b3_not ("^ compute sf helper ^"(env,t)); }"
    | Or (sf1, sf2)           -> compute_function_head^" { return b3_or ("^ compute sf1 helper ^"(env,t), "^ compute sf2 helper ^"(env,t) ); }"
    | Until (gamma, sf1, sf2) -> if gamma > 0. then compute_uless gamma (compute sf1 helper) (compute sf2 helper) helper else raise  (Failure "Gamma of U operator is a non-negative value") 
    | LessThan (tr1,tr2)      -> compute_function_head^" { return "^compute_function_head^" { return b3_lessthan ("^ compute_term tr1 helper ^", "^ compute_term tr2 helper ^" ); }(env,t); }"
    | _ -> raise (Failure "compute: missing formula")
and compute_uless gamma sf1 sf2 helper =
  let trace_iterator = "TraceIterator< "^ get_event_type(helper) ^" >" in
  compute_function_head ^"
  {
    auto eval_fold = []( struct Environment env, timespan t, "^trace_iterator^" iter) -> four_valued_type
    {

      // eval_b lambda function
      auto eval_b = []( struct Environment env, timespan t, four_valued_type v ) -> four_valued_type
      {
        // eval_i lambda function
        auto eval_i = [](three_valued_type b1, three_valued_type b2) -> four_valued_type
        {
          return (b2 != T_FALSE) ? b3_to_b4(b2) : ( (b1 != T_TRUE && b2 == T_FALSE) ? b3_to_b4(b1) : FV_SYMBOL );
        };

        if ( v == FV_SYMBOL )
        {
          DEBUGV_RTEMLD3(\"  compute phi1\\n\");
          // compute phi1
          three_valued_type cmpphi1 = "^sf1^"(env, t);

          DEBUGV_RTEMLD3(\"  compute phi2\\n\");
          // compute phi2
          three_valued_type cmpphi2 = "^sf2^"(env, t);

          four_valued_type rs = eval_i(cmpphi1, cmpphi2);

          return rs;
        }
        else
        {
          return v;
        }
      };

      DEBUGV_RTEMLD3(\"until_op: \");
      iter.debug();

      four_valued_type s = std::accumulate(
        iter.begin(),
        iter.end(),
        std::pair<four_valued_type, timespan>(FV_SYMBOL, t),
          [env, eval_b]( const std::pair<four_valued_type, timespan> a, "^ get_event_fulltype(helper) ^" e ) {
            
            DEBUGV_RTEMLD3(\"  until++\\n\");

            return std::make_pair( eval_b( env, a.second, a.first ), a.second + e.getTime() );
          }
      ).first;

      return s;
    };


    // sub_k function defines a sub-trace
    auto sub_k = []( struct Environment env, timespan t) -> "^trace_iterator^"
    {

      // use env.state to speedup the calculation of the new bounds
      "^trace_iterator^" iter = "^trace_iterator^" (env.trace, env.state.first, 0, env.state.first, env.state.second, env.state.second, 0 );

      // to use the iterator for both searches we use one reference
      "^trace_iterator^" &it = iter;

      auto lower = env.trace->searchIndexForwardUntil( it, t);
      auto upper = env.trace->searchIndexForwardUntil( it, t + "^string_of_float (gamma)^" );

      // set TraceIterator for interval [t, t+"^string_of_float (gamma)^"[
      it.setBound(lower, upper);

      // return iterator ... interval length may be zero
      return it;
    };

    "^trace_iterator^" subk = sub_k(env, t);
    
    /* update the new state based on the sub_k calculate iterator.
     * optimization step: update current index to avoid re-read events several times
     */
    env.state = std::make_pair ( subk.getBegin(),  subk.getLowerAbsoluteTime());

    four_valued_type eval_c = eval_fold(env, t, subk );
    
    return ( eval_c == FV_SYMBOL ) ?
      ( ( !subk.getEnoughSize() ) ? T_UNKNOWN : T_FALSE )
    :
      b4_to_b3(eval_c);
  }

  "
  and compute_term_duration di tf =
    compute_term_function_head^" {
    
    auto indicator_function = [](struct Environment env, timespan t) -> duration {
      auto formula = "^tf^"(env, t);

      return (formula == T_TRUE)? std::make_pair (1,false) : ( (formula == T_FALSE)? std::make_pair (0,false) : std::make_pair (0,true)) ;

    };

    indicator_function(env,t);

    // lets calculate the duration
    return std::make_pair (0,false);

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
let mon_filename = ref "config"
let mon_formulas = ref ""

let set_config_file file = mon_filename := file
let set_formulas f = mon_formulas := f

open Unix
open Sexplib
open Sexplib.Conv

open Rmtld3_synth_test

(* global_int settings *)
type global_int = string * int with sexp
(* global_string settings *)
type global_string = string * string with sexp
(* monitor setting entry*)
type monitor = string * int * Rmtld3.formula with sexp

exception Settings_Not_Found of string;;

let rec search_settings lst word =
    if lst = [] then raise (Settings_Not_Found word);
    let (el_id, el_val) = List.hd lst in
    if el_id = word then el_val else search_settings (List.tl lst) word
;;

let _ =

  let speclist = [
    ("-f", Arg.String (set_formulas), "Formula(s) to be synthesized");
    ("-n", Arg.String (set_config_file), "File containing synthesis settings");
    ("-v", Arg.Set verbose, "Enables verbose mode");
  ]
  in let usage_msg = "rmtld3synthcpp [options]"
  in Arg.parse speclist print_endline usage_msg;

  print_endline ("Default synthesis filename: " ^ !mon_filename);

  let create_dir dir_name = try let state = Sys.is_directory dir_name in if state then () else  Unix.mkdir dir_name 0o666; with _ -> Unix.mkdir dir_name 0o666 in

  (* lets parsing configuration file into global_int and monitor type variables *)
  let remainig_elements = Sexp.load_sexps !mon_filename in
  let list_global_int_settings, remainig_elements = List.fold_left (
    fun (lst,lst2) sexp_el -> ( try (global_int_of_sexp sexp_el) :: lst, lst2 with _ -> (lst, sexp_el::lst2) ) ) ([],[]) remainig_elements in
  let list_global_string_settings, remainig_elements = List.fold_left (
    fun (lst,lst2) sexp_el -> ( try (global_string_of_sexp sexp_el) :: lst, lst2 with _ -> (lst, sexp_el::lst2) ) ) ([],[]) remainig_elements in
  let list_monitor_settings, remainig_elements = List.fold_left (
    fun (lst,lst2) sexp_el -> ( try (monitor_of_sexp sexp_el) :: lst, lst2 with _ -> (lst, sexp_el::lst2) ) ) ([],[]) remainig_elements in
  (* lets draw the settings that are not recognized *)
  List.fold_left (fun lst sexp_el -> print_endline ( ( Sexp.to_string_hum sexp_el ) ^ " setting is not recognized.");  ) () remainig_elements;

  (* c++ type templates *)
  let evt_subtype = "int" in
  let evt_type = "Event" in

  (* monitor synthesis settings *)

  (* buffer size *)
  let event_queue_size = (search_settings list_global_int_settings "maximum_inter_arrival_time") in
  (* monitor cluster name *)
  let cluster_name = search_settings list_global_string_settings "cluster_name" in (* search that in global_string parameters *)
  create_dir cluster_name;

  (* manage helpers *)
  let helper = (evt_type, evt_subtype, ref 0, Hashtbl.create 10, ref 0) in

  (* generate monitors *)
  List.fold_left (fun _ (monitor_name,monitor_period,formula) ->

    (*create_dir (cluster_name^"/"^monitor_name);*)

    (* Synthesize ocaml evaluation algorithm into c++ *)

    (* monitor dependent c++ functions begin here *)

    (* Synthesize ocaml formula evaluation algorithm into c++ *)
    let stream = open_out (cluster_name^"/"^monitor_name^"_compute.h") in
    let code = "
  #ifndef _"^ String.uppercase (monitor_name^"_compute") ^"_H_
  #define _"^ String.uppercase (monitor_name^"_compute") ^"_H_

  #include \"rmtld3.h\"
  
  auto _"^monitor_name^"_compute = "^compute (formula) helper^";

  #endif //_"^ String.uppercase (monitor_name^"_compute") ^"_H_
    " in
    Printf.fprintf stream "%s\n" code;
    close_out stream;


    let stream = open_out (cluster_name^"/"^String.capitalize monitor_name^".h") in
    let code = "
  #ifndef MONITOR_"^String.uppercase monitor_name^"_H
  #define MONITOR_"^String.uppercase monitor_name^"_H

  #include \"Rmtld3_reader.h\"
  #include \"RTEML_monitor.h\"

  #include \""^monitor_name^"_compute.h\"
  #include \""^ cluster_name ^".h\"

  class "^String.capitalize monitor_name^" : public RTEML_monitor {

  private:
    RMTLD3_reader< "^ get_event_type(helper) ^" > trace = RMTLD3_reader< "^ get_event_type(helper) ^" >( __buffer_"^ cluster_name ^".getBuffer(), "^ string_of_float (calculate_t_upper_bound formula) ^" );

    struct Environment env;

  protected:
    void run(){

      three_valued_type _out = _"^monitor_name^"_compute(env,0);
      DEBUG_RTEMLD3(\"Veredict:%d\\n\", _out);
    }

  public:
    "^String.capitalize monitor_name^"(useconds_t p): RTEML_monitor(p,SCHED_FIFO,5), env(std::make_pair (0, 0), &trace, __observation) {}

  };

  #endif //MONITOR_"^String.uppercase monitor_name^"_H" in
    Printf.fprintf stream "%s\n" code;
    close_out stream;

  (* monitor dependent functions ends here *)

  ) () list_monitor_settings;


  (* the next functions will be used to manage the buffers assigned to each formula *)

  let stream = open_out (cluster_name^"/Rmtld3_reader.h") in
  let code = "
  #ifndef _RMTLD3_READER_H_
  #define _RMTLD3_READER_H_

  #include <functional>
  //#include <iterator>
  #include <numeric>

  #include \"RTEML_reader.h\"

  #include \"rmtld3.h\"
  #include \"Rmtld3_reader_it.h\"

  template<typename T>
  class RMTLD3_reader : public RTEML_reader<T>
  {
    // current buffer state (idx and cnt)
    int state_cnt;
    size_t state_idx;

    timespanw formula_t_upper_bound;

    public:
      RMTLD3_reader(CircularBuffer<T> * const buffer, timespan ub) :
        RTEML_reader<T>(buffer),
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
          iterator.getCurrentAbsoluteTime() // begin iterator at lower abs time
        );

        iterator.debug();
        tmp_it.debug();

        /* lets start the iteration to find the event at time t.
         * the tuple consists of the cumulative timestamp, the event, the
         * absolute index, and a boolean flag indicating tat the event was found.
         * type is std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool>
         */
        auto event_find_tuple = std::accumulate(
          tmp_it.begin(),
          tmp_it.end(),
          std::make_tuple ( iterator.getCurrentAbsoluteTime(), "^ get_event_fulltype(helper) ^"(), 0, false ), // [TODO] change 0
           [t, &tmp_it]( const std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> a, "^ get_event_fulltype(helper) ^" e )
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

        DEBUGV_RTEMLD3(\"aligned_time:%llu found_time:%llu \\n\", aligned_time, std::get<0>(event_find_tuple));

        // assert if there is no event found then timestamps should be equal
        ASSERT( (std::get<3>(event_find_tuple)) || (aligned_time == std::get<0>(event_find_tuple)) );

        //if(std::get<2>(event_find_tuple) > 0)
        //  std::get<2>(event_find_tuple) = std::get<2>(event_find_tuple) - 1;

        //std::get<0>(event_find_tuple) = std::get<0>(event_find_tuple) - std::get<1>(event_find_tuple).getTime();

        DEBUGV_RTEMLD3(\"    EXIT_searchindex: find:%d %llu t=%llu cycle=%d\\n\", std::get<3>(event_find_tuple), std::get<0>(event_find_tuple), t, std::get<2>(event_find_tuple));

        

        return event_find_tuple;
      }; // [TODO]

      // pre-indexed search
      three_valued_type searchOForward(std::pair <size_t, timespanw> state, proposition p, timespan t) {

        // construct iterator based on the state [TODO]
        // use env.state to speedup the calculation of the new bounds
        TraceIterator<T> it = TraceIterator<T> (this, state.first, 0, state.first, state.second, 0, state.second );

        DEBUGV_RTEMLD3(\"  searchOForward: \");
        it.debug();

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
            DEBUGV_RTEMLD3(\"    Unknown\\n\");
            return T_UNKNOWN;
          }
        }
        
        proposition new_p = std::get<1>(event_tuple).getData();

        if(new_p == p)
        {
          DEBUGV_RTEMLD3(\"    True\\n\");
          return T_TRUE;
        }
        else
        {
          DEBUGV_RTEMLD3(\"    False\\n\");
          return T_FALSE;
        }

      }; // [TODO]

      three_valued_type searchOBackward(std::pair <size_t, timespanw> state, proposition p, timespan t) { return T_FALSE; }; // [TODO]

  };


  "^ synth_environment helper ^"

  #endif //_RMTLD3_READER_H_
  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


  let stream = open_out (cluster_name^"/Rmtld3_reader_it.h") in
  let code = "

  #include \"RTEML_reader.h\"

  // defines an interator for a trace
  template<typename T>
  class TraceIterator :  public std::iterator< std::input_iterator_tag, T >
  {
    /*
     * Lets define the reader for communication with the rtemlib
     */
    RTEML_reader<T> * __reader;

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
      TraceIterator<T> (RTEML_reader<T> * _l_reader,
        size_t i, size_t e, size_t iter,
        timespanw t_l, timespanw t_u, timespanw ct) :
        __reader(_l_reader),
        ibegin(i), iend(e), it(iter),
        lower_abstime(t_l), upper_abstime(t_u), current_abstime(ct)
      {};

      RTEML_reader<T> * getReader() { return __reader; }

      size_t getBegin() { return ibegin; }
      size_t getIt() { return it; }
      bool getEnoughSize() { return enough; }

      timespanw getLowerAbsoluteTime() { return lower_abstime; }
      timespanw getUpperAbsoluteTime() { return upper_abstime; }
      timespanw getCurrentAbsoluteTime() { return current_abstime; }

      void setBound(std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> lb,
        std::tuple<timespanw, "^ get_event_fulltype(helper) ^", size_t, bool> ub) {

        auto lb_absolute_idx = std::get<2>(lb);
        auto ub_absolute_idx = std::get<2>(ub);

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
        Event<T> event;

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

  #define ASSERT(l) \
    if(!(l)) ::printf(\"assert failed.\\n\")


  #ifndef DEBUG_VERBOSE
    #define DEBUGV_RTEMLD3(...)
  #else
    #define DEBUGV_RTEMLD3(args ...) \
      ::printf(args)
  #endif

  #define DEBUG_RTEMLD3(args ...) ::printf(args)


  #endif //_RMTLD3_H_
  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


(* this standalone cpp file instantiates the monitors that have been synthesized *)
  let stream = open_out (cluster_name^"/"^ cluster_name ^".cpp") in
  let headers = List.fold_left (fun h (monitor_name,_,_) -> h^"#include \""^String.capitalize monitor_name^".h\"\n" ) "" list_monitor_settings in
  let functions = List.fold_left (fun f (monitor_name,monitor_period,_) -> f^String.capitalize monitor_name^" mon_"^monitor_name^"("^string_of_int monitor_period^");\n" ) "" list_monitor_settings in
  let code = headers ^"#include \"RTEML_buffer.h\"

RTEML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^";

"
^ functions ^
"
void __start_periodic_monitors()
{
"^ List.fold_left (fun f (monitor_name,monitor_period,_) -> f^"  if (mon_"^monitor_name^".enable()) {::printf(\"ERROR\\n\");}\n" ) "" list_monitor_settings ^"
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
let code = "#ifndef _"^String.uppercase cluster_name^"_H_
#define _"^String.uppercase cluster_name^"_H_

#include \"RTEML_buffer.h\"

extern void __start_periodic_monitors();

extern RTEML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^";

"^propositions^"

#endif //_"^String.uppercase cluster_name^"_H_
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
if (search_settings list_global_string_settings "gen_tests") = "true" then
begin
  create_dir (cluster_name^"/tests");
  Rmtld3_synth_test.test () cluster_name
  (search_settings list_global_string_settings "gen_concurrency_tests")
  (search_settings list_global_string_settings "gen_unit_tests");

  Rmtld3_synth_test.rmtld3_unit_test_generation () compute helper cluster_name;
end
