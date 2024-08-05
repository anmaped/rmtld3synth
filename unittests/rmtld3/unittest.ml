open Rmtld3
open Helper
open Synthesis.Standard
open Sexplib.Conv

let create_dir dir_name =
  try
    let state = Sys.is_directory dir_name in
    if state then () else Unix.mkdir dir_name 0o777
  with _ -> Unix.mkdir dir_name 0o777

let rmtld3_unit_test_case_generation trace formula computed_value cpp11_compute
    helper cluster_name n =
  let id =
    if n > 1 then get_counter_test_cases helper
    else get_inc_counter_test_cases helper
  in
  let string_of_three_valued v =
    if v = Unknown then "T_UNKNOWN"
    else if v = True then "T_TRUE"
    else "T_FALSE"
  in
  (* generate monitor from one formula as one lambda function *)
  let monitor_eval id =
    let function_name, body = cpp11_compute formula helper in
    body ^ "\n\ntemplate <class T>\nthree_valued_type _local_compute"
    ^ string_of_int id ^ "(T &trace, timespan &t) {\nreturn " ^ function_name
    ^ "; };"
  in
  (* do the map between event name and id numbers *)
  let hasht = get_proposition_hashtbl helper in
  let code, _ =
    List.fold_left
      (fun (a, count) (p, (t1, t2)) ->
        ( a ^ "\ttmp_x = event_t("
          ^ string_of_int
              (try Hashtbl.find hasht p
               with Not_found ->
                 let cnt = get_proposition_counter helper in
                 set_proposition_two_way_map p cnt helper;
                 cnt
                 (*Printf.printf "Proposition %s is not found.\n" d ;
                   raise (Failure "rmtld3_synth_test: proposition is missing.")*))
          ^ ","
          ^ string_of_int (int_of_float t1)
          ^ ");\n\t__buffer_" ^ cluster_name ^ ".push(tmp_x);\n",
          count + 1 ))
      ( monitor_eval id ^ "\n\ninline static bool __unit_test_" ^ cluster_name
        ^ "_c" ^ string_of_int id ^ "_" ^ string_of_int n
        ^ " () {\nevent_t tmp_x;\nstatic buffer_t __buffer_" ^ cluster_name
        ^ ";\n",
        0 )
      trace
  in
  let code =
    code ^ "\n\n\t__buffer_" ^ cluster_name
    ^ ".debug();\n\n\tDEBUG_RMTLD3(\"##__unit_test_" ^ cluster_name ^ "_c"
    ^ string_of_int id
    ^ "\\n\");\n\n\tint tzero = 0; \n\ttrace_t trace = trace_t( __buffer_"
    ^ cluster_name
    ^ ", tzero );\n\
       \ttrace.synchronize();\n\
       \tcount_until_iterations = 0;\n\n\
       \t\n\
       #ifdef __NUTTX__\n\
       \t// reset stack coloring\n\n\
       \t// BEGIN stack measurement\n\
       \tunsigned stack_size = (uintptr_t)sched_self()->adj_stack_ptr - \
       (uintptr_t)sched_self()->stack_alloc_ptr;\n\
       \tunsigned stack_free = 0;\n\
       \tuint8_t *stack_sweeper = (uint8_t *)sched_self()->stack_alloc_ptr;\n\n\
       \tuint32_t sp;\n\
      \  \t__asm__ volatile\n\
      \  \t(\n\
      \    \t\"\\tmov %0, sp\\n\\t\"\n\
      \    \t: \"=r\"(sp)\n\
      \  \t);\n\n\
       \tuint8_t *stack_begin = (uint8_t *)sched_self()->stack_alloc_ptr;\n\n\
       \tuintptr_t d_p = ((uintptr_t)sp - \
       (uintptr_t)sched_self()->stack_alloc_ptr);\n\n\
       \t// set 0xff until adj_stack_ptr\n\
       \tint i=0;\n\
       \twhile(i< d_p - 10)\n\
       \t{\n\
       \t\t*stack_begin++ = 0xff;\n\
       \t\ti++;\n\
       \t}\n\
       #endif\n\n\
       \t// let measure the execution time\n\
       \tSTART_MEASURE();\n\n\
       \ttimespan t = 0;\n\
       \tthree_valued_type comp = _local_compute" ^ string_of_int id
    ^ "<trace_t>(trace, t);\n\n\
       \tSTOP_MEASURE();\n\
       \tDEBUGV(\"TIME_MES: %llu:%d\\n\", stop-start, "
    ^ string_of_int (List.length trace)
    ^ ");\n\n\
       #ifdef __NUTTX__\n\
       \t// BEGIN stack measurement\n\n\
       \twhile (stack_free < stack_size) {\n\
       \t\tif (*stack_sweeper++ != 0xff)\n\
       \t\t\tbreak;\n\n\
       \t\tstack_free++;\n\
       \t}\n\n\
       \t::printf(\"sp_usage_begin:%u sp_usage_end:%u/%u \
       already_consumed:%u\\n\", d_p, stack_size - stack_free, stack_size, \
       stack_size-d_p);\n\
       \t::printf(\"function %s used: %u\\n\", \"__unit_test_" ^ cluster_name
    ^ "_c" ^ string_of_int id
    ^ " ()\", (stack_size - stack_free)-(stack_size-d_p));\n\
       \t// END stack measurement\n\
       #endif\n\n\
       \tDEBUG_RMTLD3(\"count:%d \", count_until_iterations);\n\n\
       \tif( comp == "
    ^ string_of_three_valued computed_value
    ^ " )\n\t\tDEBUG_RMTLD3(\"\\033[0;32m**Checked: %s\\033[0m : __unit_test_"
    ^ cluster_name ^ "_c" ^ string_of_int id
    ^ "\\n\", out_p (comp));\n\
       \telse\n\
       \t\tDEBUG_RMTLD3(\"\\033[0;31mFailed: %s\\033[0m : __unit_test_"
    ^ cluster_name ^ "_c" ^ string_of_int id
    ^ "\\n\", out_p (comp));\n\n\treturn comp == "
    ^ string_of_three_valued computed_value
    ^ ";\n}\n\n"
  in
  code

exception TEST_FAIL of string

let rmtld3_unit_test_generation cluster_name cpp11_compute helper =
  (* a logic environment for all tests *)
  let t_u = logical_environment in
  let call_list = ref "" in
  let call_code = ref "" in
  (* this function will generate the test case for a formula *)
  let pass_test_n expected_value lb trace formula n =
    let rec repeat model formula s =
      let ex = compute model formula in
      if s = 0 then ex else repeat model formula (s - 1)
    in
    Printf.printf "%s -> " lb;
    let k = environment trace in

    (* generate the environment based on the input trace *)

    (* do several tries to aproximate the execution time *)
    count := 0;
    let d = 10 in
    let time_start = Sys.time () in
    (*for rep = 1 to d do
      	compute (k, t_u, 0.) formula;
      done;*)
    let _t_value = repeat (k, t_u, 0.) formula d in
    let time_end = Sys.time () in
    let delta_t = (time_end -. time_start) /. float_of_int d in
    (* end of the measure part *)
    count := 0;
    let t_value = compute (k, t_u, 0.) formula in
    if t_value = expected_value then (
      let id = string_of_int (get_counter_test_cases helper) in
      Printf.printf "\x1b[32m[Sucess]\x1b[0m (%s) \n" (b3_to_string t_value);
      (* to generate C++ unit tests *)
      call_code := 
        !call_code
        ^ "// " ^ lb ^ "\n"
        ^ (rmtld3_unit_test_case_generation trace formula t_value cpp11_compute helper
            cluster_name n);
      Printf.printf "count_until: %i, stack_deep:%i count_full:%d\n" !count
        (calculate_heap_cost formula)
        (calculate_cycle_cost formula trace);
      Printf.printf "__unit_test_%s_c%s_%d(): TIME_MES: %d:%d\n" cluster_name id
        n
        (int_of_float (delta_t *. 1000000000.))
        (List.length trace);
      call_list :=
        !call_list ^ "__unit_test_" ^ cluster_name ^ "_c"
        ^ string_of_int (get_counter_test_cases helper)
        ^ "_" ^ string_of_int n ^ "();")
    else raise (TEST_FAIL (b3_to_string t_value))
  in
  let pass_test expected_value lb trace formula =
    pass_test_n expected_value lb trace formula 0
  in
  (* section to generate unit tests *)
  (* setting: gen_unit_tests true *)
  if get_setting_string "gen_unit_tests" helper = "true" then (
    (* basic tests for RMTLD3 *)
    let test1_trace =
      [
        ("A", (0., 1.));
        ("B", (1., 2.));
        ("A", (2., 3.));
        ("B", (3., 4.));
        ("B", (4., 5.));
        ("A", (5., 6.));
        ("C", (6., 7.));
        ("E", (7., 9.));
      ]
    in
    let test2_trace =
      [
        ("A", (0., 1.));
        ("C", (1., 2.));
        ("A", (2., 3.));
        ("B", (3., 4.));
        ("B", (4., 5.));
        ("A", (5., 6.));
        ("C", (6., 7.));
      ]
    in
    let test3_trace =
      [
        ("A", (0., 1.));
        ("A", (1., 2.));
        ("A", (2., 3.));
        ("A", (3., 4.));
        ("A", (4., 5.));
        ("A", (5., 6.));
        ("A", (6., 9.));
        ("B", (9., 20.));
        ("E", (20., 21.));
      ]
    in
    (* basic tests set *)
    pass_test True "true " test1_trace mtrue;
    pass_test True "false" test1_trace (Not mfalse);
    pass_test True "A    " test1_trace (Prop "A");
    pass_test True "~C   " test1_trace (Not (Prop "C"));

    (* duration tests set *)
    pass_test True "int 5 A < 3.0(0)1  " test1_trace
      (LessThan
         ( Duration (Constant 5., Prop "A"),
           Constant (3. +. (epsilon_float *. 3.)) ));
    pass_test True "~(int 5 A < 2)     " test1_trace
      (Not (LessThan (Duration (Constant 5., Prop "A"), Constant 2.)));

    (* until tests set *)
    pass_test True "B U A       " test1_trace (Until (3., Prop "B", Prop "A"));
    pass_test True "~(C U B)    " test1_trace
      (Not (Until (3., Prop "C", Prop "B")));
    pass_test True "(A U B)     " test1_trace (Until (3., Prop "A", Prop "B"));
    pass_test True "~(F 6 C)    " test1_trace (Not (meventually 6. (Prop "C")));
    pass_test True "~(F 5 C)    " test1_trace (Not (meventually 5. (Prop "C")));
    pass_test True "F 7.0(0)1 C " test1_trace
      (meventually (7. +. (epsilon_float *. 3.)) (Prop "C"));
    pass_test True "F_2.0(0)1 ~A" test1_trace
      (meventually (2. +. epsilon_float) (Not (Prop "A")));

    (* set of tests for temporal formulas *)
    pass_test True "~(A -> (F_1 C))   " test1_trace
      (Not (mimplies (Prop "A") (meventually 1. (Prop "C"))));
    pass_test True "A -> (F_2.0(0)1 B)" test1_trace
      (mimplies (Prop "A") (meventually (2. +. epsilon_float) (Prop "B")));
    pass_test False "G_2 ~A" test2_trace (malways 2. (Not (Prop "A")));
    pass_test True "G_4 (A -> (F_2 B))" test1_trace
      (malways 4. (mimplies (Prop "A") (meventually 2. (Prop "B"))));
    pass_test False "G_9.1 (A -> (F_2 B))" test1_trace
      (malways 9.1 (mimplies (Prop "A") (meventually 2. (Prop "B"))));

    (* complexity *)
    (* (y-2)*(x*(2*x))+((y-3)*x)+x *)

    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x+x
       * is simplified to 17x-56 where x is the length of the trace
    *)
    pass_test False "(A U_10 B) U_10 (A U_10 *)" test3_trace
      (Until
         (10., Until (10., Prop "A", Prop "B"), Until (10., Prop "A", Prop "STAR")));
    (* 5*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x)+4*x
       * is simplified to 84x-280 where x is the length of the trace
    *)
    pass_test False
      "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)" test3_trace
      (Until
         ( 10.,
           Until
             ( 10.,
               Until (10., Prop "A", Prop "B"),
               Until (10., Prop "A", Prop "B") ),
           Until
             ( 10.,
               Until (10., Prop "A", Prop "B"),
               Until (10., Prop "A", Prop "STAR") ) ));
    pass_test False
      "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)" test3_trace
      (Until
         ( 10.,
           Until
             ( 10.,
               Until (10., Prop "A", Prop "B"),
               Until (10., Prop "A", Prop "B") ),
           Prop "STAR" ))
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
	*));
  (* section to generate paper results *)
  (* setting: gen_paper_results true *)
  if get_setting_string "gen_paper_results" helper = "true" then (
    let sample_trace =
      [
        ("B", (0., 1.));
        ("B", (1., 2.));
        ("B", (2., 3.));
        ("A", (3., 4.));
        ("B", (4., 5.));
        ("B", (5., 6.));
        ("B", (6., 9.));
        ("B", (9., 20.));
      ]
    in
    let lst = [ 10; 100; 1000 ] in
    (* check if buffer has 1000 length *)
    if get_setting_int "rtm_buffer_size" helper < 1000 then
      raise (Failure "rmtld3synth_unittest: buffer length is not enough.");
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
    done;
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
    done;
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
        ("always "
        ^ string_of_int (List.nth lst (trace_size - 1))
        ^ ". ((int 4. A) < 2.)")
        new_trace4
        (malways
           (float_of_int (List.nth lst (trace_size - 1)))
           (LessThan (Duration (Constant 4., Prop "A"), Constant 2.)))
        trace_size
    done;
    for trace_size = 1 to 3 do
      let new_trace5 =
        repeat_trace (List.nth lst (trace_size - 1) / 8) sample_trace [] 0. 20.
      in
      pass_test_n True
        ("A -> int "
        ^ string_of_int (List.nth lst (trace_size - 1))
        ^ ". A < 2.")
        new_trace5
        (mimplies (Prop "A")
           (LessThan
              ( Duration
                  ( Constant (float_of_int (List.nth lst (trace_size - 1))),
                    Prop "A" ),
                Constant 2. )))
        trace_size
    done);
  (* use cases generation test *)
  (* TODO: modify with usecase1 config file instead! *)
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
         [
           mand
             (m_duration_equal (Constant pi_1) psi_1 (Constant 0.))
             (mand
                (m_duration_lessorequal2 (Constant 0.) (Constant pi_2) psi_2)
                (m_duration_less (Constant pi_2) psi_2 (Constant theta)));
           Prop "A";
         ])
  in
  (*let oc = open_out "formula_out" in
    Sexp.output_hum_indent 2 oc (sexp_of_formula usecase1_formula);
    close_out oc;*)
  let trc =
    [
      ("B", (0., 1.));
      ("A", (1., 2.));
      ("A", (2., 3.));
      ("A", (3., 4.));
      ("A", (4., 5.));
      ("A", (5., 6.));
      ("B", (6., 9.));
      ("A", (9., 20.));
    ]
  in
  pass_test Unknown (string_of_rmtld_fm usecase1_formula) trc usecase1_formula;

  let filename = cluster_name ^ "/tests/unit_test_cases.h" in
  let oc = open_out filename in
  output_string oc
    (beautify_cpp_code
       ("\n\
         #include <reader.h>\n\
         #include <rmtld3/reader.h>\n\
         #include <rmtld3/formulas.h>\n\
         #include <periodicmonitor.h>\n\
         #include \"prop.h\"\n\n\
         #ifdef __NUTTX__\n\
         #include <nuttx/sched.h>\n\
         #endif\n\n\
         typedef " ^ get_event_fulltype helper
      ^ " event_t;\n\
         typedef RTML_buffer<event_t, 100> buffer_t;\n\
         typedef RMTLD3_reader<RTML_reader<buffer_t>, int> trace_t;\n" ^ !call_code
      ^ "\ninline static void __run_unit_tests (void) {" ^ !call_list ^ "\n};"));
  close_out oc;

  let filename = cluster_name ^ "/tests/prop.h" in
  let oc = open_out filename in
  output_string oc
    (beautify_cpp_code
       ("#include <rmtld3/rmtld3.h>\n// Propositions\n"
       ^ Hashtbl.fold
           (fun x y str ->
             str ^ Printf.sprintf "const proposition PROP_%s = %i;\n  " x y)
           (get_proposition_hashtbl helper)
           ""
       ^ "\n"));
  close_out oc

let generate_auxiliar_files cluster_name helper =
  (* makefile *)
  let code1 = (* -DRTMLIB_ENABLE_DEBUG_RMTLD3 -DRTMLIB_ENABLE_DEBUGV_RMTLD3 *)
    "\n\
     x86-test:\n\
     \t g++ -Wall -g -O0 -std=gnu++11 -I../../../rtmlib2/src \
     -DRTMLIB_ENABLE_DEBUG_RMTLD3 --verbose \
     tests.cpp -o tests -pthread -latomic\n"
  in
  let oc = open_out (cluster_name ^ "/tests/Makefile") in
  output_string oc code1;
  close_out oc;
  (* main file *)
  let code2 =
    "#include <stdio.h>\n\
     #include <unistd.h>\n\
     #include <task_compat.h>\n\
     #include \"unit_test_cases.h\"\n\n\
     int count_until_iterations;\n\
     int main( int argc, const char* argv[] )\n\
     {\n\
     \tprintf( \"RMTLD3 test for " ^ cluster_name ^ "\\n\" );\n"
    (* includes unittests *)
    ^ (if
         get_setting_string "gen_unit_tests" helper = "true"
         || get_setting_string "gen_paper_results" helper = "true"
       then "\t__run_unit_tests();\n"
       else "")
    ^ "\treturn 0;\n}"
  in
  let oc = open_out (cluster_name ^ "/tests/tests.cpp") in
  output_string oc (beautify_cpp_code code2);
  close_out oc

let _ =
  (* ask test parameters *)
  let cluster_name = "_cluster" in
  let helper = mk_helper in

  let default_settings =
    "(rtm_period 200000)\n\
     (rtm_buffer_size 100)\n\
     (rtm_min_inter_arrival_time 1)\n\
     (rtm_max_period 2000000)\n\
     (rtm_event_type Event)\n\
     (rtm_event_subtype proposition)\n\
     (rtm_monitor_name_prefix rtm_#_%)\n\
     (gen_unit_tests true)\n\
     (rtm_monitor_time_unit s)\n\
     (gen_paper_results false)"
  in
  (* convert settings *)
  let s_num, s_str, s_fm = settings (settings_from_string default_settings) in
  List.iter (fun (a, b) -> set_setting a (Num b) helper) s_num;
  List.iter (fun (a, b) -> set_setting a (Txt b) helper) s_str;

  let module Conv_cpp11 = Translate (Synthesis.Cpp11) in
  create_dir cluster_name;
  create_dir (cluster_name ^ "/tests");
  print_endline "Generating auxiliar files ...";
  generate_auxiliar_files cluster_name helper;
  print_endline "Generating cpp11 test files ...";
  rmtld3_unit_test_generation cluster_name Conv_cpp11.synth helper;
  print_endline "Generating cpp11 test files ... Finished!"
