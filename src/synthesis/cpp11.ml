(*
   Synthesis from RMTLD3 to cpp11
*)

open Batteries
open Unix
open Sexplib
open Sexplib.Conv

open Rmtld3
open Helper

type body = string * string

(* function that pretty prints 'observation' function as a lambda functions in c++ *)
let synth_obs_function observation_funcname struct_name =
  (* *)
  let code = "
  // obs lambda function
  auto "^observation_funcname^" = []( struct "^struct_name^" &env, proposition p, timespan t) mutable -> three_valued_type
  {
    DEBUGV_RMTLD3(\"  eval: %lu prop:%d\\n\", t, p);
    return b3_or ( env.trace->searchOForward(env.state, p, t), env.trace->searchOBackward(env.state, p, t) );
  };
  " in
  code

(*
 * some macros for cpp11
 *)
let trace_iterator helper = "TraceIterator< "^ get_event_type(helper) ^", "^ get_event_fulltype(helper) ^" >";;
let compute_term_function_head = "[](environment env, timespan t) -> duration";;
let compute_function_head = "[](environment env, timespan t) -> three_valued_type";;
let compute_function_head_mutable = "[](environment &env, timespan t) mutable -> three_valued_type";;

(*
 * Compute terms
 *)
let synth_tm_constant value helper = ("make_duration("^string_of_float value^",false)","")

let synth_tm_duration (di,_) (tf,_) helper =
    (compute_term_function_head^" {
    
    auto eval_eta =  [](environment env, timespan t, timespan t_upper, "^trace_iterator helper^" iter) -> duration
    {
      auto indicator_function = [](environment env, timespan t) -> duration {
        auto formula = "^tf^"(env, t);

        return (formula == T_TRUE)? std::make_pair (1,false) : ( (formula == T_FALSE)? std::make_pair (0,false) : std::make_pair (0,true)) ;

      };


      // compare if t is equal to the lower bound
      auto lower = iter.getLowerAbsoluteTime();
      // compare if t is equal to the upper bound
      auto upper = iter.getUpperAbsoluteTime();

      timespan val1 = ( t == lower )? 0 : t - lower;
      timespan val2 = ( t_upper == upper )? 0 : t_upper - upper;

      DEBUGV_RMTLD3(\"dur lower(%ld) upper(%ld)\\n\", val1, val2);

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

          DEBUGV_RMTLD3(\"dur=%f bottom=%d\\n\", d.first + (x.first * ( e.getTime() - valx )), d.second || x.second);

          return std::make_pair (make_duration (d.first + (x.first * ( e.getTime() - valx )), d.second || x.second), p.second + e.getTime());
        }
      ).first;
      
    };

    // sub_k function defines a sub-trace
    auto sub_k = []( environment env, timespan t, timespan t_upper) -> "^trace_iterator helper^"
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

    }(env,t)","")

let synth_tm_plus (cmptr1,_) (cmptr2,_) helper = ("sum_dur("^ cmptr1 ^" , "^cmptr2^")","")

let synth_tm_times (cmptr1,_) (cmptr2,_) helper = ("mult_dur("^ cmptr1 ^" , "^ cmptr2 ^")","")


(*
 * compute formulas
*)
let synth_fm_true helper =
  (compute_function_head_mutable^" { return T_TRUE; }","")

let synth_fm_p p helper =
  (*let tbl = get_proposition_hashtbl helper in
  let counter = get_proposition_counter helper in *)
  (compute_function_head_mutable^" { return env.evaluate(env, "^
    string_of_int p  ^ ", t); }","")

let synth_fm_not (cmpfm,_) helper = (compute_function_head_mutable^" { auto sf = "^ cmpfm ^"(env,t); return b3_not (sf); }","")

let synth_fm_or (cmpfm1,_) (cmpfm2,_) helper = (compute_function_head_mutable^" { auto sf1 = "^ cmpfm1 ^"(env,t); auto sf2 = "^ cmpfm2 ^"(env,t); return b3_or (sf1, sf2); }","")

let synth_fm_less (cmptr1,_) (cmptr2,_) helper = (compute_function_head_mutable^" { return "^compute_function_head^" { auto tr1 = "^ cmptr1 ^"; auto tr2 = "^ cmptr2 ^"; return b3_lessthan (tr1, tr2); }(env,t); }","")

let eval_b sf1 sf2 = "
  // eval_b lambda function
  auto eval_b = []( environment env, timespan t, four_valued_type v ) -> four_valued_type
  {
    // eval_i lambda function
    auto eval_i = [](three_valued_type b1, three_valued_type b2) -> four_valued_type
    {
      return (b2 != T_FALSE) ? b3_to_b4(b2) : ( (b1 != T_TRUE && b2 == T_FALSE) ? b3_to_b4(b1) : FV_SYMBOL );
    };

    // change this (trying to get the maximum complexity)
    //if ( v == FV_SYMBOL )
    //{
      DEBUGV_RMTLD3(\"  compute phi1\\n\");
      // compute phi1
      three_valued_type cmpphi1 = "^sf1^"(env, t);

      DEBUGV_RMTLD3(\"  compute phi2\\n\");
      // compute phi2
      three_valued_type cmpphi2 = "^sf2^"(env, t);

      four_valued_type rs = eval_i(cmpphi1, cmpphi2);

      DEBUGV_RMTLD3(\" phi1=%s UNTIL phi2=%s\\n\", out_p(cmpphi1), out_p(cmpphi2) );

    if ( v == FV_SYMBOL )
    {
      return rs;
    }
    else
    {
      return v;
    }
  };
"

let eval_fold sf1 sf2 helper = "
  auto eval_fold = []( environment env, timespan t, "^trace_iterator helper^" iter) -> four_valued_type
  {
    "^ eval_b sf1 sf2 ^"

    iter.debug();

    ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

    auto cos = iter.getBegin();

    four_valued_type s = std::accumulate(
      iter.begin(),
      iter.end(),
      std::pair<four_valued_type, timespan>(FV_SYMBOL, t),
        [&env, &cos, eval_b]( const std::pair<four_valued_type, timespan> a, "^ get_event_fulltype helper ^" e ) {
          
          DEBUGV_RMTLD3(\"  until++ (%s)\\n\", out_fv(a.first));

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
"

let synth_fm_uless gamma (sf1,_) (sf2,_) helper =
  (compute_function_head ^"
  {
    
    "^ eval_fold sf1 sf2 helper ^"

    // sub_k function defines a sub-trace
    auto sub_k = []( environment env, timespan t) -> "^trace_iterator helper^"
    {

      // use env.state to speedup the calculation of the new bounds
      "^trace_iterator helper^" iter = "^trace_iterator helper^" (env.trace, env.state.first, 0, env.state.first, env.state.second, 0, env.state.second );

      // to use the iterator for both searches we use one reference
      "^trace_iterator helper^" &it = iter;

      ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

      auto lower = env.trace->searchIndexForwardUntil( it, t);
      auto upper = env.trace->searchIndexForwardUntil( it, (t + "^string_of_float (gamma)^") - 1 ); /* [TODO] check this minus */

      // set TraceIterator for interval [t, t+"^string_of_float (gamma)^"[
      it.setBound(lower, upper);

      // return iterator ... interval length may be zero
      return it;
    };

    "^trace_iterator helper^" subk = sub_k(env, t);

    DEBUGV_RMTLD3(\"BEGIN until_op_less.\\n\\n \");

    four_valued_type eval_c = eval_fold(env, t, subk );

    DEBUGV_RMTLD3(\"END until_op_less (%s) enough(%d) .\\n\\n \", out_fv(eval_c), subk.getEnoughSize() );
    
    return ( eval_c == FV_SYMBOL ) ?
      ( ( !subk.getEnoughSize() ) ? T_UNKNOWN : T_FALSE )
    :
      b4_to_b3(eval_c);
  }

  ","")

let synth_fm_ueq gamma (sf1,_) (sf2,_) helper =
  (compute_function_head ^"
  {
    
    "^ eval_fold sf1 sf2 helper ^"

    // sub_k function defines a sub-trace
    auto sub_k = []( environment env, timespan t) -> "^trace_iterator helper^"
    {

      // use env.state to speedup the calculation of the new bounds
      "^trace_iterator helper^" iter = "^trace_iterator helper^" (env.trace, env.state.first, 0, env.state.first, env.state.second, 0, env.state.second );

      // to use the iterator for both searches we use one reference
      "^trace_iterator helper^" &it = iter;

      ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

      auto lower = env.trace->searchIndexForwardUntil( it, t);
      auto upper = env.trace->searchIndexForwardUntil( it, (t + "^string_of_float (gamma)^") - 1 ); /* [TODO] check this minus */

      // set TraceIterator for interval [t, t+"^string_of_float (gamma)^"[
      it.setBound(upper, upper); /* [TODO] check this lower=upper and upper=upper to get only the last element */

      // return iterator ... interval length may be zero
      return it;
    };

    "^trace_iterator helper^" subk = sub_k(env, t);

    DEBUGV_RMTLD3(\"BEGIN until_op_leq.\\n\\n \");

    four_valued_type eval_c = eval_fold(env, t, subk );

    DEBUGV_RMTLD3(\"END until_op_leq (%s) enough(%d) .\\n\\n \", out_fv(eval_c), subk.getEnoughSize() );
    
    return ( eval_c == FV_SYMBOL ) ?
      ( ( !subk.getEnoughSize() ) ? T_UNKNOWN : T_FALSE )
    :
      b4_to_b3(eval_c);
  }

  ","")

let synth_fm_ulesseq gamma (sf1,a) (sf2,b) helper =
  synth_fm_or (synth_fm_ueq gamma (sf1,a) (sf2,b) helper) (synth_fm_uless gamma (sf1,a) (sf2,b) helper) helper


(* monitor dependent c++ functions begin here *)
let synth_cpp11 (out_file,out_dir) cluster_name monitor_name monitor_period formula compute helper =
    let cmp_str = compute (formula) helper in
    (* Synthesize ocaml formula evaluation algorithm into c++ *)
    let code1 = "
  #ifndef _"^ String.uppercase_ascii (monitor_name^"_compute") ^"_H_
  #define _"^ String.uppercase_ascii (monitor_name^"_compute") ^"_H_

  #include \"rmtld3/rmtld3.h\"

  typedef struct Environment< "^ get_event_type(helper) ^", "^ get_event_fulltype(helper) ^" > environment;
  
  auto _"^monitor_name^"_compute = "^ cmp_str ^";

  // SORTS
  "^ Hashtbl.fold (fun x y str -> str^(Printf.sprintf "#define SORT_%s %i\n  " x y)) (get_proposition_hashtbl helper) "" ^"

#ifdef USE_MAP_SORT
  #include <string>
  #include <unordered_map>

  // Create an unordered_map of sorts (that map to integers)
  std::unordered_map<std::string, int> _mapsorttostring = {
  "^ Hashtbl.fold (fun x y str -> str^(Printf.sprintf "{\"%s\",%i},\n  " x y)) (get_proposition_hashtbl helper) "" ^"};

  // Create an unordered_map of sorts (that map to strings)
  std::unordered_map<int, std::string> _mapsorttoint = {
  "^ Hashtbl.fold (fun x y str -> str^(Printf.sprintf "{%i,\"%s\"},\n  " y x)) (get_proposition_hashtbl helper) "" ^"};
#endif

  #endif //_"^ String.uppercase_ascii (monitor_name^"_compute") ^"_H_
    " in


    
    let code2 = "
  #ifndef MONITOR_"^String.uppercase_ascii monitor_name^"_H
  #define MONITOR_"^String.uppercase_ascii monitor_name^"_H

  #include \"rmtld3/reader.h\"
  #include \"RTML_monitor.h\"

  #include \""^monitor_name^"_compute.h\"
  #include \""^ cluster_name ^".h\"

  class "^String.capitalize_ascii monitor_name^" : public RTML_monitor {

  private:
    RMTLD3_reader< "^ get_event_type(helper) ^", "^ get_event_fulltype(helper) ^" > trace = RMTLD3_reader< "^ get_event_type(helper) ^", "^ get_event_fulltype(helper) ^" >( __buffer_"^ cluster_name ^".getBuffer(), "^ string_of_float (calculate_t_upper_bound formula) ^" );

    environment env;

  protected:
    void run(){

      three_valued_type _out = _"^monitor_name^"_compute(env,0);
      DEBUG_RTMLD3(\"Veredict:%d\\n\", _out);
    }

  public:
    "^String.capitalize_ascii monitor_name^"(useconds_t p): RTML_monitor(p,SCHED_FIFO,50), env(std::make_pair (0, 0), &trace, __observation< "^ get_event_type(helper) ^", "^ get_event_fulltype(helper) ^" >) {}
    "^String.capitalize_ascii monitor_name^"(useconds_t p, int sche, int prio): RTML_monitor(p,sche,prio), env(std::make_pair (0, 0), &trace, __observation< "^ get_event_type(helper) ^", "^ get_event_fulltype(helper) ^" >) {}

  };

  #endif //MONITOR_"^String.uppercase_ascii monitor_name^"_H" in


  if out_file <> "" || out_dir <> "" then
    begin
    let stream = open_out (out_dir^"/"^monitor_name^"_compute.h") in
      Printf.fprintf stream "%s\n" code1;
    close_out stream;

    let stream = open_out (out_dir^"/"^String.capitalize_ascii monitor_name^".h") in
      Printf.fprintf stream "%s\n" code2;
    close_out stream;
    end
  else
    (* print to console *)
    print_endline code1;
    print_endline code2

  (* monitor dependent functions ends here *)
;;

let synth_cpp11_env src_dir cluster_name evt_subtype event_queue_size helper =
(* this standalone cpp file instantiates the monitors that have been synthesized *)
  let stream = open_out (src_dir^"/"^ cluster_name ^".cpp") in
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

let stream = open_out (src_dir^"/"^ cluster_name ^".h") in
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


let stream = open_out (src_dir^"/Makefile") in
let code =
"
.DEFAULT_GOAL := all

ifndef RTMLIB_INCLUDE_DIR
  $(error RTMLIB_INCLUDE_DIR is undefined)
endif

DIR = tests
CXX = g++

ifeq ($(OS),Windows_NT)
  CXX_NAMES = x86_64-w64-mingw32-g++ i686-w64-mingw32-g++
  CXX := $(foreach exec,$(CXX_NAMES),$(if $(shell which $(exec)),$(exec),))
  ifeq ($(CXX),)
    $(error \"No $(exec) in PATH\")
  endif
endif

CXX := $(shell echo \"$(CXX)\" | cut -f 1 -d \" \")

arm-monitor:
\t if [ -z $NUTTX_OS_INCLUDE_DIR ]; then \\
\t   echo \"Error: NUTTX_OS_INCLUDE_DIR is missing\"; \\
\t else \\
\t   if [ -z $CMSIS_INCLUDE_DIR ]; then \\
\t     echo \"Error: CMSIS_INCLUDE_DIR is missing\"; \\
\t   else \\
\t     $(if $(shell which arm-none-eabi-g++),,$(error \"No arm-none-eabi-g++ in PATH\")) \\
\t     arm-none-eabi-g++ \
-I$(NUTTX_OS_INCLUDE_DIR) \
-I$(NUTTX_OS_INCLUDE_DIR)/cxx \
-I$(CMSIS_INCLUDE_DIR) \
-I$(RTMLIB_INCLUDE_DIR) \
-fno-builtin-printf \
-fno-exceptions \
-fno-rtti \
-std=gnu++11 \
-mthumb \
-march=armv7-m \
-g \
-DARM_CM4_FP \
-D__NUTTX__ \
-DCONFIG_C99_BOOL8 \
-DCONFIG_WCHAR_BUILTIN \
-D__FILE_defined \
-Wframe-larger-than=1200 \
-fverbose-asm \
--verbose \
-c mon1.cpp; \\
\t   fi \\
\t fi

x86-monitor:
\t$(CXX) -Wall -g -O0 -std=c++11 -I$(RTMLIB_INCLUDE_DIR) -D__x86__ --verbose -c "^cluster_name^".cpp

.PHONY: tests
tests:
\t make -C $(DIR)

x86-mtest: x86-monitor tests
\t ifndef RTMLIB_LIB_DIR \
$(error RTMLIB_LIB_DIR is undefined) \
endif
\t g++ "^cluster_name^".o tests/tests.o -L$(RTMLIB_LIB_DIR) -lrtml -pthread -o "^cluster_name^"

arm-mon: arm-monitor

x86-mon: x86-monitor

all:

" in
Printf.fprintf stream "%s\n" code;
close_out stream;

(* the buffers need to be managed according to its size and type; one buffer
   could be used for several monitors; a monitor is the synthesis of a formula
 *)

;;
