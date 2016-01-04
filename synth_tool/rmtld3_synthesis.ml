(*pp camlp4o -I C:/cygwin/home/anmap/.opam/system/lib/type_conv C:\cygwin\home\anmap\.opam\system\lib\type_conv\pa_type_conv.cma -I C:/cygwin/home/anmap/.opam/system/lib/sexplib C:\cygwin\home\anmap\.opam\system\lib\sexplib\pa_sexp_conv.cma -I +camlp4 *)

open Hashtbl

type helper = string * string * int ref * (string, int) t

let get_event_fulltype (t1,t2,_,_) = 
  t1 ^ "< " ^ t2 ^" >"

let get_event_type (t1,t2,_,_) =
  t2

let get_proposition_hashtbl (_,_,_,tbl) =
  tbl

let get_proposition_counter (_,_,count,_) =
  count := !count + 1;
  !count

(* function that pretty prints 'observation' function as a lambda functions in c++ *)
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
let synth_environment helper =
  let struct_name = "Environment" in
  let circular_buffer_varname = "_local_buffer" in
  let circular_buffer_vartype = "RMTLD3_reader< "^ get_event_type(helper) ^" >" in
  let observation_funcname = "__observation" in
  (* object to read traces containing current duration and element numbers (since is a circular buffer then size is constant)*)
  let code_init = "
  struct "^struct_name^" {
    size_t state_pos;
    "^circular_buffer_vartype^" *trace;
    three_valued_type (*evaluate)(struct Environment, proposition, timespan);

    "^struct_name^"(
      size_t s_pos,
      "^circular_buffer_vartype^" * t,
      three_valued_type (*ev)(struct Environment, proposition, timespan)
    ) :
      state_pos(s_pos),
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

(* Synthesis of the compute function  *)
let rec compute_term term helper =
  match term with
    | Constant value       -> string_of_float value
    | Duration (di,phi)    -> "D" (*compute_term_duration m (t,t +. (compute_term m t  di)) phi*) (* THIS PART IS NOT FINISHED *)
    | FPlus (tr1,tr2)      -> compute_term tr1 helper ^" +. "^ compute_term tr2 helper
    | FTimes (tr1,tr2)     -> compute_term tr1 helper ^" *. "^ compute_term tr2 helper
    | _ -> raise (Failure "compute_terms: missing term")
and compute formula helper =
  let compute_function_head = "[](struct Environment env, timespan t) -> three_valued_type" in
  match formula with
    | Prop p                  -> let tbl = get_proposition_hashtbl helper in
                                 let counter = get_proposition_counter helper in 
                                 compute_function_head^" { return env.evaluate(env, "^
                                  string_of_int (
                                    try Hashtbl.find tbl p with Not_found -> Hashtbl.add tbl p counter; counter 
                                                )^
                                 ", t); }"
    | Not sf                  -> compute_function_head^" { return b3_not ("^ compute sf helper ^"); }"
    | Or (sf1, sf2)           -> compute_function_head^" { return b3_or ("^ compute sf1 helper ^"(env,t), "^ compute sf2 helper ^"(env,t) ); }"
    | Until (gamma, sf1, sf2) -> if gamma > 0. then compute_uless gamma (compute sf1 helper) (compute sf2 helper) helper else raise  (Failure "Gamma of U operator is a non-negative value") 
    | LessThan (tr1,tr2)      -> compute_function_head^" { return "^compute_function_head^" { return b3_lessthan ("^ compute_term tr1 helper ^"(env,t), "^ compute_term tr2 helper ^"(env,t) ); }; }"
    | _ -> raise (Failure "compute: missing formula")
and compute_uless gamma sf1 sf2 helper =
  let compute_function_head = "[](struct Environment env, timespan t) -> three_valued_type" in
  let trace_iterator = "TraceIterator< "^ get_event_type(helper) ^" >" in
  compute_function_head ^"
  {
    auto eval_fold = []( struct Environment env, timespan t, "^trace_iterator^" x) -> four_valued_type
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
          [env, eval_b]( const std::pair<four_valued_type, timespan> a, "^ get_event_fulltype(helper) ^" e ) {
            return std::make_pair<four_valued_type, timespan>( eval_b( env, a.second, a.first ), a.second + e.getTime() );
          }
      ).first;

      return s;
    };


    // sub_k function defines a sub-trace
    auto sub_k = []( struct Environment env, timespan t) -> "^trace_iterator^"
    {
      "^trace_iterator^" it = env.trace->getTraceIterator();

      // set TraceIterator for interval [t,t+"^string_of_float (gamma)^"[
      it.setBound(env.trace->searchIndexForwardUntil(t), env.trace->searchIndexForwardUntil(t+"^string_of_float (gamma)^"));

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
    
    return ( eval_c == FV_SYMBOL ) ?
      (
      // we have two cases to consider
      // when the time bound is the last symbol return False
      // when the time bound is greater than trace return Unknown
      ( env.trace->getDuration() <= (t + "^string_of_float gamma^") ) ? T_UNKNOWN : T_FALSE
      )
    :
      b4_to_b3(eval_c)
    ;

  }

  "

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

  print_endline ("Default synthesis directory: " ^ !mon_filename);

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
  let helper = (evt_type, evt_subtype, ref 0, Hashtbl.create 10) in

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
    RTEML_reader<int> __reader = RTEML_reader<int>(__buffer_"^ cluster_name ^".getBuffer());
    RMTLD3_reader< "^ get_event_type(helper) ^" > trace = RMTLD3_reader< "^ get_event_type(helper) ^" >( &__reader, "^ string_of_float (calculate_t_upper_bound formula) ^" );

    struct Environment env;

  protected:
    void run(){

      three_valued_type _out = _"^monitor_name^"_compute(env,0);
      ::printf(\"Veredict:%d\\n\", _out);
    }

  public:
    "^String.capitalize monitor_name^"(IEventBuffer<int> &buffer, useconds_t p): RTEML_monitor(p,SCHED_FIFO,5), env(0, &trace, __observation) {
      //configReader<int>(__reader, buffer); [IS NOT REQUIRED... BUFFER IS STATICALLY ASSIGNED]
    }

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
  class RMTLD3_reader 
  {

    TraceIterator<T> iterator; // it allows us to run over the circular buffer constrained by an interval

    // instead of a new memory allocator we will use the RTEML_reader of the monitor class
    RTEML_reader<T> * __reader;

    // current buffer state (idx and cnt)
    int state_cnt;
    size_t state_idx;

    timespan formula_t_upper_bound;

    public:
      RMTLD3_reader(RTEML_reader<T> * rd, timespan ub) : iterator(TraceIterator<T> (rd, 0, 0, 0, 0, 0)), __reader(rd), formula_t_upper_bound(ub)  {};

      /** match timestamp and return the index using forward direction (iterator++) */
      std::pair<size_t,int> searchIndexForwardUntil(timespan t) {

        timeabs current_time;
        size_t current_idx;
        __reader->getCurrentBufferState(current_time, current_idx);

        /* while current buffer endpoint timestamp is not greater or equal than timespan t then
         * yield (save state and context-switch) else exit
         */
        /*while( current_time % formula_t_upper_bound < t )
        {
          // wait
          //yield(); // [TODO]
          continue;
        }*/

        if (current_time % formula_t_upper_bound < t)
        {
          return std::pair<timespan, int>(0, 0);
        }

        /* create an iterator where its end is the endpoint of the buffer
         * and process the iteration until the timespan t is found. After
         * that return the index and cnt where t holds.
         */
        TraceIterator<T> tmp_it = TraceIterator<T>(__reader, iterator.getIt(), current_idx, iterator.getIt(), iterator.getCnt(), iterator.getCurrentAbsoluteTime());

        // lets start the iteration to find the event at time t
        std::pair<timespan, int> tuple = std::accumulate(
          tmp_it.begin(),
          tmp_it.end(),
          std::pair<timespan, int>( iterator.getCurrentAbsoluteTime(), -1 ),
           [t, &tmp_it]( const std::pair<timespan, int> a, "^ get_event_fulltype(helper) ^" e ) {
              return std::make_pair<timespan, int>( a.first + e.getTime() , (int)(( a.first <= t && t < a.first + e.getTime() )? tmp_it.getIt() : a.second) );
           }
         );

        return tuple;
      }; // [TODO]

      // pre-indexed search
      three_valued_type searchOForward(size_t idx, proposition p, timespan t) {

        /* use the function searchIndexForwardUntil to find the index where
         * t hold and check if proposition is satisfied at time t
         */

        return T_UNKNOWN;
      }; // [TODO]
      three_valued_type searchOBackward(size_t idx, proposition p, timespan t) { return T_UNKNOWN; }; // [TODO]

      /** get the current maximum duration of the trace */
      timespan getDuration() { return 0; }; // [TODO]

      TraceIterator<T> getTraceIterator() { return iterator; };
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
    size_t b_lower_bound, b_upper_bound;
    size_t ibegin, iend, it;
    int cnt;

    timespan absolute_time;

    RTEML_reader<T> * __reader;

    public:
      TraceIterator<T> (RTEML_reader<T> * _l_reader,
          size_t i, size_t e, size_t iter, int c, timespan ct) :
        b_lower_bound(_l_reader->getLowerIdx()),
        b_upper_bound(_l_reader->getHigherIdx()),
        ibegin(i), iend(e), it(iter), cnt(c), absolute_time(ct),
        __reader(_l_reader) {};

      size_t getBegin() { return ibegin; }

      size_t getIt() { return it; }

      int getCnt() { return cnt; }

      timespan getCurrentAbsoluteTime() { return absolute_time; }

      void setBegin(size_t b) { ibegin = b; }
      void setEnd(size_t e) { iend = e; }
      void setCnt(int c) { cnt = c; }
      void setIt(size_t iter) { it = iter; }

      void setBound(std::pair<size_t,int> lb, std::pair<size_t,int> ub) {
        setBegin( lb.first );
        setEnd( ub.first );
        setIt( lb.first );
        setCnt( lb.second );
      }

      TraceIterator<T> begin() {
        return TraceIterator<T> (
          __reader,
          ibegin, iend, ibegin, (ibegin <= it ) ? cnt : cnt-1, 0); // [TODO] 0 problem
      } // cnt can be cnt or cnt-1 [CONFIRM]

      TraceIterator<T> end() {
        return TraceIterator<T> (
          __reader,
          ibegin, iend, iend, (it <= iend) ? cnt : cnt+1, 0); // [TODO] 0 problem
      } // cnt can be cnt or cnt+1  [CONFIRM]

      TraceIterator<T>& operator++() {
        ++ it;
        if ( it == b_upper_bound ) {
           ++ cnt;
           it = b_lower_bound;
        }

        // update absolute current time of the event
        absolute_time += operator*().getTime(); // [TODO] [VERIFY]

        return *this;
      } // [CONFIRM]

      bool operator==(const TraceIterator<T>& rhs) { return it == rhs.it && cnt == rhs.cnt; } // [CONFIRM]
      bool operator!=(const TraceIterator<T>& rhs) { return !(operator==(rhs)); } // [CONFIRM]

      Event<T> operator*() {
        Event<T> event;

        // here we could adopt a small buffer to avoid successive call of dequeues (for instance a local buffer of 10 elements)
        // dequeue the event of the it index
        std::pair<state_rd_t,Event<T> &> x = __reader->dequeue((int)it);

        return x.second;
      } // [CONFIRM]
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

  typedef unsigned int duration;
  typedef unsigned int proposition;
  //typedef unsigned int timespan;

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
  #define b3_not(b3) (b3 == T_TRUE) ? T_FALSE : ( (b3 == T_FALSE) ? T_TRUE : T_UNKNOWN )

  /** Relation operator < */
  #define b3_lessthan(n1,n2)  (n1 < n2) ? T_TRUE : ( (n1 >= n2) ? T_FALSE : T_UNKNOWN )

  #endif //_RMTLD3_H_
  " in
  Printf.fprintf stream "%s\n" code;
  close_out stream;


(* this standalone cpp file instantiates the monitors that have been synthesized *)
  let stream = open_out (cluster_name^"/"^ cluster_name ^".cpp") in
  let headers = List.fold_left (fun h (monitor_name,_,_) -> h^"#include \""^String.capitalize monitor_name^".h\"\n" ) "" list_monitor_settings in
  let functions = List.fold_left (fun f (monitor_name,monitor_period,_) -> f^String.capitalize monitor_name^" mon_"^monitor_name^"(__buffer_"^ cluster_name ^", "^string_of_int monitor_period^");\n" ) "" list_monitor_settings in
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

arm-monitor-lib:
\t arm-none-eabi-g++ -std=c++0x -march=armv7-a -g -fverbose-asm -O -IC:\\ardupilot_pixhawk_testcase\\ardupilot\\modules\\PX4NuttX\\nuttx\\include -Wframe-larger-than=1200 -DCONFIG_WCHAR_BUILTIN -I../../arch/arm/include -I../../ -DARM_CM4_FP -D__NUTTX__ --verbose -c monitor_set1.cpp

x86-monitor-lib:
\t g++ -Wall -g -O0 -std=c++0x -I../../ -D__x86__ --verbose -c "^cluster_name^".cpp

.PHONY: tests
tests:
\t make -C $(DIR)

x86-test: x86-monitor-lib tests
\t g++ "^cluster_name^".o tests/tests.o -L../../ -lrteml -pthread -o "^cluster_name^"

arm-mon: arm-monitor-lib

x86-mon: x86-monitor-lib

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
  Rmtld3_synth_test.test () cluster_name;
end
