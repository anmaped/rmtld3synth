(* function that prety prints 'observation' function as a lambda functions in c++ *)
let synth_obs_function observation_funcname struct_name =
  (* *)
  let code = "
    // obs lambda function
    auto "^observation_funcname^" = []( struct "^struct_name^" env, proposition p, timespan t) -> three_valued_type
    {
      return b3_or ( env.trace->searchOForward(env.state_pos, p, t), env.trace->searchOBackward(env.state_pos, p, t) );
    };
  " in
  code

  (* function that prety prints 'environment' function as a lambda function in c++ *)
  let synth_environment =
    let struct_name = "Environment" in
    let circular_buffer_varname = "_local_buffer" in
    let circular_buffer_vartype = "Trace<Event<int>>" in
    let observation_funcname = "__observation" in
    (* object to read traces containing current duration and element numbers (since is a circular buffer then size is constant)*)
    let code_init = "
      #include \"Trace.h\"

      struct "^struct_name^" {
        size_t state_pos;
        "^circular_buffer_vartype^" *trace;
        three_valued_type (*evaluate)(duration, "^circular_buffer_vartype^", proposition, timespan);

        "^struct_name^"(
          size_t s_pos,
          "^circular_buffer_vartype^" * t,
          three_valued_type (*ev)(duration, "^circular_buffer_vartype^", proposition, timespan)
        ) :
          state_pos(s_pos),
          trace(t),
          evaluate(ev) {};
      };
    " in

    (* observation function where duration_varname is the name of the current duration variable *)
    let code_obs = synth_obs_function observation_funcname (struct_name) in
    let code_end = "
      // instantiation of the formula buffer
      "^circular_buffer_vartype^" "^circular_buffer_varname^";

      struct "^struct_name^" env(0, &"^circular_buffer_varname^", "^observation_funcname^");
      extern struct "^struct_name^" env;
    " in
    (code_init^code_obs^code_end)


  open Rmtld3.RMTLD3

  (* Synthesization of the compute function  *)
  let rec compute_term term =
    match term with
      | Constant value       -> string_of_float value
      | Duration (di,phi)    -> "D" (*compute_term_duration m (t,t +. (compute_term m t  di)) phi*)
      | FPlus (tr1,tr2)      -> compute_term tr1 ^" +. "^ compute_term tr2
      | FTimes (tr1,tr2)     -> compute_term tr1 ^" *. "^ compute_term tr2
      | _ -> raise (Failure "compute_terms: missing term") 
  and compute formula =
    let compute_function_head = "[](struct Environment env, timespan t)" in
    match formula with
      | Proposition p           -> compute_function_head^" { return env.ev(env, "^string_of_int (int_of_char (p.[0]))^", t); }"
      | Not sf                  -> compute_function_head^" { return b3_not ("^ compute sf ^"); }"
      | Or (sf1, sf2)           -> compute_function_head^" { return "^compute_function_head^" { return b3_or ("^ compute sf1 ^"(env,t), "^ compute sf2 ^"(env,t) ); }; }"
      | Until (gamma, sf1, sf2) -> if gamma > 0. then compute_uless gamma (compute sf1) (compute sf2) else raise  (Failure "Gamma of U operator is a non-negative value") 
      | LessThan (tr1,tr2)      -> compute_function_head^" { return "^compute_function_head^" { return b3_lessthan ("^ compute_term tr1 ^"(env,t), "^ compute_term tr2 ^"(env,t) ); }; }"
      | _ -> raise (Failure "compute: missing formula")
  and compute_uless gamma sf1 sf2 =
    let trace_iterator = "TraceIterator<Event<int>>" in
    "
    []( struct Environment env, timespan t ) -> three_valued_type
    {
      auto eval_fold = []( struct Environment env, timespan t, "^trace_iterator^" x) -> four_valued_type
      {

        // eval_b lambda function
        auto eval_b = []( struct Environment env, timespan t, four_valued_type v ) -> four_valued_type
        {
          // eval_i lambda function
          auto eval_i = [](three_valued_type b1, three_valued_type b2) -> four_valued_type
          {
            return (b2 != FALSE) ? b3_to_b4(b2) : ( (b1 != TRUE && b2 == FALSE) ? b3_to_b4(b1) : FV_SYMBOL );
          };

          if ( v == FV_SYMBOL )
          {
            // compute phi1
            three_valued_type cmpphi1 = "^sf1^"(env, t);
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

        // let s,_ = fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') phi1 phi2 v, ii2)) (Symbol,t) x in
        four_valued_type s = std::accumulate(
          x.begin(),
          x.end(),
          std::pair<four_valued_type, timespan>(FV_SYMBOL, t),
            [env, eval_b]( const std::pair<four_valued_type, timespan> a, "^trace_iterator^" it ) {
              return std::make_pair<four_valued_type, timespan>( eval_b( env, a.second, a.first ), a.second + it.getEvent().getTime() );
            }
        ).first;

        return s;
      };


      // sub_k function defines a sub-trace
      auto sub_k = []( struct Environment env, timespan t) -> "^trace_iterator^"
      {
        TraceIterator<Event<int>> it = env.trace->getTraceIterator();

        // set TraceIterator for interval [t,"^string_of_float gamma^"[
        it.setBegin( env.trace->serachIndexForwardUntil(t) );
        it.setEnd( env.trace->serachIndexForwardUntil("^string_of_float gamma^") );

        // return iterator ... interval length may be zero
        return it;

       
       /*if length tb < 1 then
         []
       else
         let p1,p2 = partition (fun (_,(i1,i2)) -> if t <= i1 && i1 < (t+.gamma) then true else false) tb in
         p1*/

      };

      "^trace_iterator^" subk = sub_k(env, t);
      // optimization step: update current index to avoid re-read events several times
      env.state_pos = subk.getBegin();

      four_valued_type eval_c = eval_fold(env, t, subk );
      
      return ( eval_c = FV_SYMBOL ) ?
        (
        // we have two cases to consider
        // when the time bound is the last symbol return False
        // when the time bound is greater than trace return Unknown
        ( env.trace->getDuration() <= (t + "^string_of_float gamma^") ) ? UNKNOWN : FALSE
        )
      :
        b4_to_b3(eval_c)
      ;

    }

    "


let _ =
  let event_queue_size = 100 in

  (* Synthesize ocaml evaluation algorithm into c++ *)
  let stream = open_out "eval_environment.h" in
  Printf.fprintf stream "%s\n" "#ifndef _EVAL_ENVIRONMENT_H_
    #define _EVAL_ENVIRONMENT_H_";
  let r = synth_environment in
  Printf.fprintf stream "%s %s\n" r "#endif //_EVAL_ENVIRONMENT_H_";
  close_out stream;



  let stream = open_out "Trace.h" in
  let code = "
    #ifndef _TRACE_H_
    #define _TRACE_H_

    #include <functional>
    #include <iterator>
    #include <numeric>

    #include \"Event.h\"
    #include \"time_compat.h\"

    typedef unsigned int duration;
    typedef unsigned int proposition;
    typedef unsigned int timespan;

    enum three_valued_type {TRUE, FALSE, UNKNOWN};

    /** OR */
    #define b3_or(b31, b32) \\
      ( b31 == TRUE || b32 == TRUE ) ? TRUE : \\
        (( b31 == FALSE && b32 == FALSE ) ? FALSE : UNKNOWN)

    /** NOT */
    #define b3_not(b3) (b3 == TRUE) ? FALSE : ( (b3 == FALSE) ? TRUE : UNKNOWN )

    /** Relation operator < */
    #define b3_lessthan(n1,n2)  (n1 < n2) ? TRUE : ( (n1 >= n2) ? FLASE : UNKNOWN )

    // defines an interator for trace
    template<typename T>
    class TraceIterator :  public std::iterator< std::input_iterator_tag, T >
    {
      size_t idx_begin;
      size_t idx_end;

      T * buffer;

      public:
        TraceIterator<T> (T * b, size_t i, size_t e) : buffer(b), idx_begin(i), idx_end(e) {};

        size_t getBegin() { return idx_begin; };
        void setBegin(size_t b) { idx_begin = b; };
        void setEnd(size_t e) { idx_end = e; };

        T getEvent() { return buffer[0]; }; // [TODO]

        TraceIterator<T> begin() { return *this; }; // [TODO]
        TraceIterator<T> end() { return *this; }; // [TODO]
        TraceIterator<T>& operator++() { return *this; } // [TODO]
        bool operator!=(const TraceIterator<T>& rhs) { return true; } // [TODO]
        TraceIterator<T>& operator*() { return *this; } // [TODO]
    };

    template<typename T>
    class Trace 
    {

      TraceIterator<T> iterator;

      T buffer["^string_of_int event_queue_size^"]; // statically assigned

      public:
        Trace() : iterator(TraceIterator<T> (buffer, 0, "^string_of_int (event_queue_size-1)^")) {};

        // pre-indexed search
        three_valued_type searchOForward(size_t idx, proposition p, timespan t) { return UNKNOWN; }; // [TODO]
        three_valued_type searchOBackward(size_t idx, proposition p, timespan t) { return UNKNOWN; }; // [TODO]

        /** match index with timestamp using forward direction (left-right way) */
        size_t serachIndexForwardUntil(timespan) { return 0; }; // [TODO]

        /** get the current maximum duration of the trace */
        timespan getDuration() { return 0; }; // [TODO]

        TraceIterator<T> getTraceIterator() { return iterator; };
    };

    #endif //_TRACE_H_

  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


  (* Synthesize ocaml formula evaluation algorithm into c++ *)
  let stream = open_out "eval_formula.h" in
  Printf.fprintf stream "%s\n" "
    #ifndef _EVAL_FORMULA_H_
    #define _EVAL_FORMULA_H_

    #include \"eval_environment.h\"

    enum four_valued_type { FV_TRUE, FV_FALSE, FV_UNKNOWN, FV_SYMBOL };

    // type conversion from three_valued_type to four_valued_type
    #define b3_to_b4(b3) (b3 == TRUE) ? FV_TRUE : ( ( b3 == FALSE ) ? FV_FALSE : FV_UNKNOWN )

    // convert four_valued_type into three_valued type
    #define b4_to_b3(b4) (b4 == FV_TRUE ) ? TRUE : ( ( b4 == FV_FALSE ) ? FALSE : UNKNOWN )

  ";
  let r = "auto _compute = "^compute (Or(Until(2.,Proposition("A"),Proposition("C")),Proposition("B")))^";" in
  Printf.fprintf stream "%s %s\n" r "
  #endif //_EVAL_FORMULA_H_
  ";
  close_out stream;
