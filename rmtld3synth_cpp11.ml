(*
   Synthesis from RMTLD3 to cpp11
*)

open Batteries
open Unix
open Sexplib
open Sexplib.Conv

open Rmtld3
open Rmtld3synth_helper


(* monitor dependent c++ functions begin here *)
let synth_cpp11_compute cluster_name monitor_name monitor_period formula compute helper =
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
;;

let synth_cpp1_external_dep cluster_name synth_environment helper =

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
;;


let synth_cpp11_env cluster_name evt_subtype event_queue_size helper =
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

;;
