open Rmtld3
open Helper
open Synthesis.Standard
open Synthesis.Smt
open Sexplib.Conv

let create_dir dir_name =
  try
    let state = Sys.is_directory dir_name in
    if state then () else Unix.mkdir dir_name 0o777
  with _ -> Unix.mkdir dir_name 0o777

let rmtld3_unit_test_case_generation trace formula computed_value computef
    helper filename cluster_name n =
  let id =
    if n > 1 then get_counter_test_cases helper
    else get_inc_counter_test_cases helper
  in
  let string_of_three_valued v =
    if v = Unknown then "T_UNKNOWN"
    else if v = True then "T_TRUE"
    else "T_FALSE"
  in
  (* lets generate formulas in c++ lambda functions *)
  let monitor_eval =
    "\tauto _local_compute = " ^ computef formula helper ^ ";"
  in
  (* do the map between event name and id numbers *)
  let hasht = get_proposition_hashtbl helper in
  let code, _ =
    List.fold_left
      (fun (a, count) (p, (t1, t2)) ->
        ( a ^ "\tbuf->writeEvent("
          ^ string_of_int
              ( try Hashtbl.find hasht p with Not_found ->
                let cnt = get_proposition_counter helper in
                set_proposition_two_way_map p cnt helper;
                cnt
                  (*Printf.printf "Proposition %s is not found.\n" d ;
                  raise (Failure "rmtld3_synth_test: proposition is missing.")*)
              )
          ^ ","
          ^ string_of_float (t2 -. t1)
          ^ "," ^ string_of_int count ^ ");\n"
        , count + 1 ) )
      ( "bool __attribute__ ((noinline)) __unit_test_" ^ cluster_name ^ "_c"
        ^ string_of_int id ^ "_" ^ string_of_int n
        ^ " () {\n\tauto buf = __buffer_" ^ cluster_name
        ^ ".getBuffer(); \n\tbuf->resetFrameCounter();\n"
      , 0 )
      trace
  in
  let code = code ^ monitor_eval in
  let code =
    code ^ "
\n
\n\t__buffer_" ^ cluster_name
    ^ ".debug();
\n
\n\tDEBUG_RTEMLD3(\"##__unit_test_" ^ cluster_name ^ "_c"
    ^ string_of_int id
    ^ "\\n\");
\n
\n\t// lets defining the reader
\n\tRMTLD3_reader<"
    ^ get_event_type helper ^ "> trace = RMTLD3_reader<"
    ^ get_event_type helper
    ^ ">( buf, 200000. );
\n\
       \tstruct Environment env = Environment(std::make_pair (0, 0), &trace, \
       __observation); // [TODO] CHECK state pair
\n\
       
\n\
       \tcount_until_iterations = 0;
\n\
       
\n\
       \t
\n\
       #ifdef __NUTTX__
\n\
       \t// reset stack coloring
\n\
       
\n\
       \t// BEGIN stack measurement
\n\
       \tunsigned stack_size = (uintptr_t)sched_self()->adj_stack_ptr - \
       (uintptr_t)sched_self()->stack_alloc_ptr;
\n\
       \tunsigned stack_free = 0;
\n\
       \tuint8_t *stack_sweeper = (uint8_t *)sched_self()->stack_alloc_ptr;
\n\
       
\n\
       \tuint32_t sp;
\n\
      \  \t__asm__ volatile
\n\
      \  \t(
\n\
      \    \t\"\\tmov %0, sp\\n\\t\"
\n\
      \    \t: \"=r\"(sp)
\n\
      \  \t);
\n\
       
\n\
       \tuint8_t *stack_begin = (uint8_t *)sched_self()->stack_alloc_ptr;
\n\
       
\n\
       \tuintptr_t d_p = ((uintptr_t)sp - \
       (uintptr_t)sched_self()->stack_alloc_ptr);
\n\
       
\n\
       \t// set 0xff until adj_stack_ptr
\n\
       \tint i=0;
\n\
       \twhile(i< d_p - 10)
\n\
       \t{
\n\
       \t\t*stack_begin++ = 0xff;
\n\
       \t\ti++;
\n\
       \t}
\n\
       #endif
\n\
       
\n\
       \t// let measure the execution time
\n\
       \tSTART_MEASURE();
\n\
       
\n\
       \tthree_valued_type comp = [_local_compute](struct Environment env) \
       __attribute__ ((noinline)) { return  _local_compute(env, 0.); }(env);
\n\
       
\n\
       \tSTOP_MEASURE();
\n\
       \tDEBUGV(\"TIME_MES: %llu:%d\\n\", stop-start, "
    ^ string_of_int (List.length trace)
    ^ ");
\n\
       
\n\
       #ifdef __NUTTX__
\n\
       \t// BEGIN stack measurement
\n\
       
\n\
       \twhile (stack_free < stack_size) {
\n\
       \t\tif (*stack_sweeper++ != 0xff)
\n\
       \t\t\tbreak;
\n\
       
\n\
       \t\tstack_free++;
\n\
       \t}
\n\
       
\n\
       \t::printf(\"sp_usage_begin:%u sp_usage_end:%u/%u \
       already_consumed:%u\\n\", d_p, stack_size - stack_free, stack_size, \
       stack_size-d_p);
\n\
       \t::printf(\"function %s used: %u\\n\", \"__unit_test_" ^ cluster_name
    ^ "_c" ^ string_of_int id
    ^ " ()\", (stack_size - stack_free)-(stack_size-d_p));
\n\
       \t// END stack measurement
\n\
       #endif
\n\
       
\n\
       \tDEBUG_RTEMLD3(\"count:%d\", count_until_iterations);
\n\
       
\n\
       \tif( comp == "
    ^ string_of_three_valued computed_value
    ^ " )
\n\t\tDEBUG_RTEMLD3(\"Checked: %s : __unit_test_" ^ cluster_name
    ^ "_c" ^ string_of_int id
    ^ "\\n\", out_p (comp));
\n\
       \telse
\n\
       \t\tDEBUG_RTEMLD3(\"Failed: %s : __unit_test_" ^ cluster_name ^ "_c"
    ^ string_of_int id ^ "\\n\", out_p (comp));
\n
\n\treturn comp == "
    ^ string_of_three_valued computed_value
    ^ ";
\n}
\n
\n"
  in
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 filename in
  output_string oc code ; close_out oc ; ()

exception TEST_FAIL of string

let rmtld3_unit_test_generation () computef helper cluster_name _ =
  let filename = cluster_name ^ "/tests/unit_test_cases.h" in
  if Sys.file_exists filename then Sys.remove filename else () ;
  let oc = open_out filename in
  output_string oc
    ( "
\n\
       #include \"Rmtld3_reader.h\"
\n\
       #include \"RTML_monitor.h\"
\n\
       #include \"" ^ cluster_name
    ^ ".h\"
\n
\n#ifdef __NUTTX__
\n#include <nuttx/sched.h>
\n#endif
\n
\n" ) ;
  close_out oc ;
  let t_u = logical_environment in
  (* a logic environment for all tests *)
  let call_list = ref "" in
  (* this function will generate the test case for a formula *)
  let pass_test_n expected_value lb trace formula n =
    let rec repeat model formula s =
      let ex = compute model formula in
      if s = 0 then ex else repeat model formula (s - 1)
    in
    Printf.printf "%s -> " lb ;
    let k = environment trace in
    (* generate the environment based on the input trace *)
    
    (* do several tries to aproximate the execution time *)
    count := 0 ;
    let d = 10 in
    let time_start = Sys.time () in
    (*for rep = 1 to d do
			compute (k, t_u, 0.) formula;
		done;*)
    let _t_value = repeat (k, t_u, 0.) formula d in
    let time_end = Sys.time () in
    let delta_t = (time_end -. time_start) /. float_of_int d in
    (* end of the measure part *)
    count := 0 ;
    let t_value = compute (k, t_u, 0.) formula in
    if t_value = expected_value then (
      let id = string_of_int (get_counter_test_cases helper) in
      Printf.printf "[PASSED]%s \n" (b3_to_string t_value) ;
      (* to generate C++ unit tests *)
      rmtld3_unit_test_case_generation trace formula t_value computef helper
        filename cluster_name n ;
      Printf.printf "count_until: %i, stack_deep:%i count_full:%d\n" !count
        (calculate_heap_cost formula)
        (calculate_cycle_cost formula trace) ;
      Printf.printf "__unit_test_%s_c%s_%d(): TIME_MES: %d:%d\n" cluster_name
        id n
        (int_of_float (delta_t *. 1000000000.))
        (List.length trace) ;
      call_list :=
        !call_list ^ "__unit_test_" ^ cluster_name ^ "_c"
        ^ string_of_int (get_counter_test_cases helper)
        ^ "_" ^ string_of_int n ^ "();" ;
      (* generate smt benchmark tests *)
      (* create directory *)
      let oc =
        open_out_gen
          [Open_creat; Open_text; Open_append]
          0o640
          (cluster_name ^ "/" ^ id ^ ".smt2")
      in
      (*let smtlibv2 = Sexp.to_string (sexp_of_formula formula) in*)
      let smtlibv2 = rmtld3synthsmt formula helper in
      output_string oc smtlibv2 ; close_out oc )
    else raise (TEST_FAIL (b3_to_string t_value))
  in
  let pass_test expected_value lb trace formula =
    pass_test_n expected_value lb trace formula 0
  in
  (* section to generate unit tests *)
  (* setting: gen_unit_tests true *)
  if search_settings_string "gen_unit_tests" helper = "true" then (
    (* basic tests for RMTLD3 *)
    let test1_trace =
      [ ("A", (0., 1.))
      ; ("B", (1., 2.))
      ; ("A", (2., 3.))
      ; ("B", (3., 4.))
      ; ("B", (4., 5.))
      ; ("A", (5., 6.))
      ; ("C", (6., 7.)) ]
    in
    let test2_trace =
      [ ("A", (0., 1.))
      ; ("C", (1., 2.))
      ; ("A", (2., 3.))
      ; ("B", (3., 4.))
      ; ("B", (4., 5.))
      ; ("A", (5., 6.))
      ; ("C", (6., 7.)) ]
    in
    let test3_trace =
      [ ("A", (0., 1.))
      ; ("A", (1., 2.))
      ; ("A", (2., 3.))
      ; ("A", (3., 4.))
      ; ("A", (4., 5.))
      ; ("A", (5., 6.))
      ; ("A", (6., 9.))
      ; ("B", (9., 20.)) ]
    in
    (* basic tests set *)
    pass_test True "true " test1_trace mtrue ;
    pass_test True "false" test1_trace (Not mfalse) ;
    pass_test True "A    " test1_trace (Prop "A") ;
    pass_test True "~C   " test1_trace (Not (Prop "C")) ;
    (* TODO: set propositions; they depend of the input formula of the configuration file *)
    
    (* duration tests set *)
    pass_test True "int 5 A < 3.0(0)1  " test1_trace
      (LessThan
         ( Duration (Constant 5., Prop "A")
         , Constant (3. +. (epsilon_float *. 3.)) )) ;
    pass_test True "~(int 5 A < 2)     " test1_trace
      (Not (LessThan (Duration (Constant 5., Prop "A"), Constant 2.))) ;
    (* until tests set *)
    pass_test True "B U A       " test1_trace (Until (3., Prop "B", Prop "A")) ;
    pass_test True "~(C U B)    " test1_trace
      (Not (Until (3., Prop "C", Prop "B"))) ;
    pass_test True "(A U B)     " test1_trace (Until (3., Prop "A", Prop "B")) ;
    pass_test True "~(F 6 C)    " test1_trace (Not (meventually 6. (Prop "C"))) ;
    pass_test True "~(F 5 C)  " test1_trace (Not (meventually 5. (Prop "C"))) ;
    pass_test True "F 7.0(0)1 C " test1_trace
      (meventually (7. +. (epsilon_float *. 3.)) (Prop "C")) ;
    pass_test True "F_2.0(0)1 ~A" test1_trace
      (meventually (2. +. epsilon_float) (Not (Prop "A"))) ;
    (* set of tests for temporal formulas *)
    pass_test True "~(A -> (F_1 C))   " test1_trace
      (Not (mimplies (Prop "A") (meventually 1. (Prop "C")))) ;
    pass_test True "A -> (F_2.0(0)1 B)" test1_trace
      (mimplies (Prop "A") (meventually (2. +. epsilon_float) (Prop "B"))) ;
    pass_test False "G_2 ~A" test2_trace (malways 2. (Not (Prop "A"))) ;
    pass_test True "G_4 (A -> (F_2 B))" test1_trace
      (malways 4. (mimplies (Prop "A") (meventually 2. (Prop "B")))) ;
    pass_test Unknown "G_9.1 (A -> (F_2 B))" test1_trace
      (malways 9.1 (mimplies (Prop "A") (meventually 2. (Prop "B")))) ;
    (* complexity *)
    (* (y-2)*(x*(2*x))+((y-3)*x)+x *)
    
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x+x
	 * is simplified to 17x-56 where x is the length of the trace
	 *)
    pass_test False "(A U_10 B) U_10 (A U_10 *)" test3_trace
      (Until
         (10., Until (10., Prop "A", Prop "B"), Until (10., Prop "A", Prop "*"))) ;
    (* 5*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x)+4*x
	 * is simplified to 84x-280 where x is the length of the trace
	 *)
    pass_test False
      "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)" test3_trace
      (Until
         ( 10.
         , Until
             ( 10.
             , Until (10., Prop "A", Prop "B")
             , Until (10., Prop "A", Prop "B") )
         , Until
             ( 10.
             , Until (10., Prop "A", Prop "B")
             , Until (10., Prop "A", Prop "*") ) )) ;
    pass_test False
      "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)" test3_trace
      (Until
         ( 10.
         , Until
             ( 10.
             , Until (10., Prop "A", Prop "B")
             , Until (10., Prop "A", Prop "B") )
         , Prop "*" ))
    (*
	* NEW test [TODO]
	(((3.528682  < int[int[0.643269 ] (E ) ] ((E  or E )) ) or ((E  U_3.204527 E ) o
	r (int[2.963296 ] (E )  < 3.456293 ))) U_4.239142 ((int[0.333695 ] (B )  < int[2
	.105323 ] (A ) ) U_2.887519 ((int[3.716714 ] (E )  < 3.871726 ) U_1.413040 (E  o
	r E ))))Duration 44.778000s
	Metrics:
	Cardinality: 101
	Measure(temporal operators): 4
	Measure(duration terms): 6
	*) ) ;
  (* section to generate paper results *)
  (* setting: gen_paper_results true *)
  if search_settings_string "gen_paper_results" helper = "true" then (
    let sample_trace =
      [ ("B", (0., 1.))
      ; ("B", (1., 2.))
      ; ("B", (2., 3.))
      ; ("A", (3., 4.))
      ; ("B", (4., 5.))
      ; ("B", (5., 6.))
      ; ("B", (6., 9.))
      ; ("B", (9., 20.)) ]
    in
    let lst = [10; 100; 1000] in
    (* check if buffer has 1000 length *)
    if search_settings_int "buffer_size" helper < 1000 then
      raise (Failure "rmtld3synth_unittest: buffer length is not enough.") ;
    for trace_size = 1 to 3 do
      (* let generate big traces *)
      let new_trace =
        List.rev
          (strategic_uniform_trace 0. (List.nth lst (trace_size - 1) - 1) 1. [])
      in
      (*print_trace new_trace;*)
      pass_test_n True
        ("eventually " ^ string_of_int (List.nth lst (trace_size - 1)) ^ ". B")
        new_trace
        (meventually (float_of_int (List.nth lst (trace_size - 1))) (Prop "B"))
        trace_size
    done ;
    for trace_size = 1 to 3 do
      let new_trace2 =
        repeat_trace (List.nth lst (trace_size - 1) / 8) sample_trace [] 0. 20.
      in
      pass_test_n True
        ("A->always " ^ string_of_int (List.nth lst (trace_size - 1)) ^ ". B")
        new_trace2
        (mimplies (Prop "A")
           (malways (float_of_int (List.nth lst (trace_size - 1))) (Prop "B")))
        trace_size
    done ;
    (* this results is equal to the previous one *)
    (*for trace_size = 1 to 3 do

		let new_trace3 = repeat_trace ( (List.nth lst (trace_size-1))/8)  sample_trace [] in
		pass_test_n True ("A->eventually "^(string_of_int (List.nth lst (trace_size-1)))^". B") new_trace3
		(
			mimplies (Prop("A")) (meventually (float_of_int (List.nth lst (trace_size-1))) (Prop("B")))
		) trace_size;

		done;*)
    for trace_size = 1 to 3 do
      let new_trace4 =
        repeat_trace (List.nth lst (trace_size - 1) / 8) sample_trace [] 0. 20.
      in
      (*print_trace new_trace4;*)
      pass_test_n True
        ( "always "
        ^ string_of_int (List.nth lst (trace_size - 1))
        ^ ". ((int 4. A) < 2.)" )
        new_trace4
        (malways
           (float_of_int (List.nth lst (trace_size - 1)))
           (LessThan (Duration (Constant 4., Prop "A"), Constant 2.)))
        trace_size
    done ;
    for trace_size = 1 to 3 do
      let new_trace5 =
        repeat_trace (List.nth lst (trace_size - 1) / 8) sample_trace [] 0. 20.
      in
      pass_test_n True
        ( "A -> int "
        ^ string_of_int (List.nth lst (trace_size - 1))
        ^ ". A < 2." )
        new_trace5
        (mimplies (Prop "A")
           (LessThan
              ( Duration
                  ( Constant (float_of_int (List.nth lst (trace_size - 1)))
                  , Prop "A" )
              , Constant 2. )))
        trace_size
    done ) ;
  (* use cases generation test -- TO CHANGE TO CONFIG FILE *)
  let m_or_fold list_formulas =
    List.fold_left (fun a b -> Or (b, a)) mfalse list_formulas
  in
  let pi_1 = 1000000. in
  let pi_2 = 1000000. in
  let psi_1 = Prop "N" in
  let psi_2 = Prop "B" in
  let theta = 5000000. in
  let usecase1_formula =
    m_or_fold
      (List.rev
         [ mand
             (m_duration_equal (Constant pi_1) psi_1 (Constant 0.))
             (mand
                (m_duration_lessorequal2 (Constant 0.) (Constant pi_2) psi_2)
                (m_duration_less (Constant pi_2) psi_2 (Constant theta)))
         ; Prop "A" ])
  in
  (*let oc = open_out "formula_out" in
	Sexp.output_hum_indent 2 oc (sexp_of_formula usecase1_formula);
	close_out oc;*)
  let trc =
    [ ("B", (0., 1.))
    ; ("A", (1., 2.))
    ; ("A", (2., 3.))
    ; ("A", (3., 4.))
    ; ("A", (4., 5.))
    ; ("A", (5., 6.))
    ; ("B", (6., 9.))
    ; ("A", (9., 20.)) ]
  in
  pass_test True "usecase1" trc usecase1_formula ;
  (* <--- TO CHANGE TO CONFIG FILE *)
  
  (* lets create a function to run all tests *)
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 filename in
  output_string oc ("
\nauto __run_unit_tests = []() {" ^ !call_list ^ "
\n};") ;
  close_out oc

let test () cluster_name helper =
  let concurrency_on = search_settings_string "gen_concurrency_tests" helper in
  let unit_on = search_settings_string "gen_unit_tests" helper in
  print_endline "Test generation for monitors is enabled!" ;
  (* lets define the makefile *)
  let stream = open_out (cluster_name ^ "/tests/Makefile") in
  let code =
    "
\n\
     x86-test:
\n\
     \t g++ -Wall -g -O0 -std=c++0x -I../../../ -I../ -D__x86__ --verbose -c \
     tests.cpp
\n\
     
\n"
  in
  Printf.fprintf stream "%s\n" code ;
  close_out stream ;
  (* lets define the main test file for multi-thread procucer/consumer *)
  (* each task consumes and produces certain amount of events; we use three dummy tasks *)
  let stream = open_out (cluster_name ^ "/tests/tests.cpp") in
  let consumer_lambda_function l =
    List.fold_left
      (fun a (b, _) ->
        "
\n\tauto consumer" ^ string_of_int b
        ^ " = [](void *) -> void*
\n\
           \t{
\n\
           \t\tstatic RTEML_reader<int> __reader = RTEML_reader<int>(__buffer_"
        ^ cluster_name
        ^ ".getBuffer());
\n\
           \t\tEvent<int> tmpEvent;
\n\
           
\n\
           \t\tstd::pair<state_rd_t,Event<int> > rd_tuple = \
           __reader.dequeue();
\n\
           \t\ttmpEvent = rd_tuple.second;
\n\
           \t\t::printf(\"Event_consumed: %lu, %d code: %d\\n\", \
           tmpEvent.getTime(), tmpEvent.getData(), rd_tuple.first);
\n\
           
\n\
           \t\treturn NULL;
\n\
           \t};
\n\
           \t" ^ a )
      "" l
  in
  let producer_lambda_function l =
    List.fold_left
      (fun a (b, _) ->
        "
\n\tauto producer" ^ string_of_int b
        ^ " = [](void *) -> void*
\n\
           \t{
\n\
           \t\tstatic RTML_writer<int> __writer = RTML_writer<int>(__buffer_"
        ^ cluster_name ^ ".getBuffer());
\n
\n\t\t__writer.enqueue("
        ^ string_of_int b ^ ");
\n
\n\t\t__buffer_" ^ cluster_name
        ^ ".debug();
\n\t\treturn NULL;
\n\t};
\n\t" ^ a )
      "" l
  in
  Random.self_init () ;
  let rec gen_task_ids l size =
    let id, _ = if List.length l = 0 then (0, 0) else List.hd l in
    if size = List.length l then l
    else gen_task_ids ((id + 1, (1 + Random.int 100) * 50000) :: l) size
  in
  let producers_ids = gen_task_ids [] 50 in
  let consumers_ids = gen_task_ids [] 40 in
  let code =
    "
\n\
     #include <stdio.h>
\n\
     #include <unistd.h>
\n\
     
\n\
     #include \"task_compat.h\"
\n\
     
\n\
     #include \"" ^ cluster_name
    ^ ".h\"
\n\
       
\n\
       #include \"unit_test_cases.h\"
\n\
       
\n\
       
\n\
       int main( int argc, const char* argv[] )
\n\
       {
\n\
       \tprintf( \"RMTLD3 test for " ^ cluster_name
    ^ "\\n\" );
\n
\n\t//__start_periodic_monitors();
\n
\n\t"
    (* begins the test for concurrency if enabled *)
    ^ ( if concurrency_on = "true" then
        "
\n\t// basic enqueue and dequeue test case
\n\t"
        ^ producer_lambda_function producers_ids
        ^ "
\n
\n\t"
        ^ consumer_lambda_function consumers_ids
        ^ "
\n
\n\t// lets create three producers
\n\t"
        ^ List.fold_left
            (fun a (id, p) ->
              "__attribute__ ((unused)) __task producer_" ^ string_of_int id
              ^ " = __task(\"producer" ^ string_of_int id ^ "\", producer"
              ^ string_of_int id
              ^ ", sched_get_priority_max(SCHED_FIFO), SCHED_FIFO, "
              ^ string_of_int p ^ ");
\n\t" ^ a )
            "" producers_ids
        ^ "
\n\
           
\n\
           \t// and two consumers
\n\
           \t/* we have two cases:
\n\
           \t * - consumer is faster than producer (it will cause no side \
           efects)
\n\
           \t * - producer is faster than consumer (it will cause overwritten \
           of buffer)
\n\
           \t */
\n\
           
\n\
           \t"
        ^ List.fold_left
            (fun a (id, p) ->
              "__attribute__ ((unused)) __task consumer_" ^ string_of_int id
              ^ " = __task(\"consumer" ^ string_of_int id ^ "\", consumer"
              ^ string_of_int id
              ^ ", sched_get_priority_max(SCHED_FIFO), SCHED_FIFO, "
              ^ string_of_int p ^ ");
\n\t" ^ a )
            "" consumers_ids
        ^ "
\n\t"
      else "" )
    ^ "
\n
\n\t"
    (* begins the test for units if enabled *)
    ^ ( if
        unit_on = "true"
        || search_settings_string "gen_paper_results" helper = "true"
      then "
\n\t// if unit tests on then do that
\n\t__run_unit_tests();
\n\t"
      else "" )
    ^ ( if concurrency_on = "true" then
        " while(true) {sleep(1);}; // do sleep (delay) "
      else "" )
    ^ "
\n}
\n
\n"
  in
  Printf.fprintf stream "%s\n" code ;
  close_out stream

let _ =
  (* ask test parameters *)
  let out_dir = ref "." in
  let cluster_name = "_cluster" in
  let settings_sexp =
    [ setting_from_string "(buffer_size 100)"
    ; setting_from_string "(maximum_inter_arrival_time 1)"
    ; setting_from_string "(maximum_period 2000000)"
    ; setting_from_string "(event_type Event)"
    ; setting_from_string "(event_subtype int)"
    ; setting_from_string "(cluster_name mon1)"
    ; setting_from_string "(gen_concurrency_tests false)"
    ; setting_from_string "(gen_unit_tests true)"
    ; setting_from_string "(gen_paper_results false)" ]
  in
  let helper = set_parameters (settings settings_sexp) mk_helper in
  let module Conv_cpp11 = Translate (Synthesis.Cpp11) in
  create_dir cluster_name ;
  create_dir (!out_dir ^ "/" ^ cluster_name ^ "/tests") ;
  test () cluster_name helper ;
  rmtld3_unit_test_generation ()
    (fun a b ->
      let x, _ = Conv_cpp11.synth a b in
      x )
    helper cluster_name helper
