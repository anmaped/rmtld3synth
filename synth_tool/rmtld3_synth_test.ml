
open Rmtld3

open Helper

let rmtld3_unit_test_case_generation trace formula computed_value computef helper filename cluster_name =
	let id = get_inc_counter_test_cases helper in
	let string_of_three_valued v = if v = Unknown then "T_UNKNOWN" else if v = True then "T_TRUE" else "T_FALSE" in
	(* lets generate formulas in c++ lambda functions *)
	let monitor_eval = "	auto _local_compute = "^computef (formula) helper ^";" in
	(* do the map between event name and id numbers *)
	let hasht = get_proposition_hashtbl helper in
	let code,_ = List.fold_left
		(fun (a,count) (d,(t1,t2)) -> (a^"	buf->writeEvent("^string_of_int (Hashtbl.find hasht d)^","^string_of_float (t2-.t1)^","^string_of_int count^");\n", count+1))
		("auto __unit_test_"^cluster_name^"_c"^string_of_int id^" = []() {\n	auto buf = __buffer_"^ cluster_name^".getBuffer(); \n	buf->resetFrameCounter();\n", 0) trace in
	let code = code ^ monitor_eval in
	let code = code ^ "

	__buffer_"^ cluster_name^".debug();

	DEBUG_RTEMLD3(\"##__unit_test_"^cluster_name^"_c"^string_of_int id^"\\n\");

	// lets defining the reader
	RMTLD3_reader<"^get_event_type(helper)^"> trace = RMTLD3_reader<"^get_event_type(helper)^">( buf, 200000. );
	struct Environment env = Environment(std::make_pair (0, 0), &trace, __observation); // [TODO] CHECK state pair

	three_valued_type comp = _local_compute(env, 0.);

	if( comp == "^string_of_three_valued computed_value^" )
		DEBUG_RTEMLD3(\"Checked: %s : __unit_test_"^cluster_name^"_c"^string_of_int id^"\\n\", out_p (comp));
	else
		DEBUG_RTEMLD3(\"Failed: %s : __unit_test_"^cluster_name^"_c"^string_of_int id^"\\n\", out_p (comp));

	return comp == "^string_of_three_valued computed_value^";
};

" in
	
	let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 filename in
    output_string oc code;
    close_out oc;

	()


exception TEST_FAIL of string;;

let rmtld3_unit_test_generation () computef helper cluster_name =

	let filename = cluster_name^"/tests/unit_test_cases.h" in
	if Sys.file_exists filename then Sys.remove filename else ();

	let oc = open_out filename in
    output_string oc ("
#include \"Rmtld3_reader.h\"
#include \"RTEML_monitor.h\"
#include \""^ cluster_name ^".h\"

#define out_p(res) \
	(res == T_TRUE)? \"true\" : ((res == T_FALSE)? \"false\": \"unknown\")

");
    close_out oc;

	(* basic tests for RMTLD3 *)
	let test1_trace = [("A",(0.,1.)); ("B",(1.,2.)); ("A",(2.,3.)); ("B",(3.,4.));
	  ("B",(4.,5.)); ("A",(5.,6.)); ("C",(6.,7.))] in
	let test2_trace = [("A",(0.,1.)); ("C",(1.,2.)); ("A",(2.,3.)); ("B",(3.,4.));
	  ("B",(4.,5.)); ("A",(5.,6.)); ("C",(6.,7.))] in
	let test3_trace = [("A",(0.,1.)); ("A",(1.,2.)); ("A",(2.,3.)); ("A",(3.,4.));
	("A",(4.,5.)); ("A",(5.,6.)); ("A",(6.,9.)); ("B",(9.,20.));] in

	let t_u = logical_environment in (* a logic environment for all tests *)

	(* this function will generate the test case for a formula *)
	let pass_test expected_value lb trace formula =
		Printf.printf "%s -> " lb ;
		let k = environment trace in (* generate the environment based on the input trace *)
		let t_value = compute (k, t_u, 0.) formula in
		if t_value = expected_value then
		begin
	  		Printf.printf "[PASSED]%s \n" (b3_to_string t_value);
	  		rmtld3_unit_test_case_generation trace formula t_value computef helper filename cluster_name ;
	  	end
	  	else raise (TEST_FAIL (b3_to_string t_value)); in

	(* basic tests set *)
	pass_test True "true " test1_trace ( mtrue ) ;
	pass_test True "false" test1_trace ( Not(mfalse) ) ;
	pass_test True "A    " test1_trace ( Prop("A") ) ;
	pass_test True "~C   " test1_trace ( Not(Prop("C")) ) ;

	(* duration tests set *)
	pass_test True "int 5 A < 2.0(0)1  " test1_trace
		(LessThan(Duration(Constant(5.),Prop("A")), Constant(2. +.
		(epsilon_float *. 3.)))
	) ;
	pass_test True "~(int 5 A < 2)     " test1_trace
		(Not(LessThan(Duration(Constant(5.),Prop("A")), Constant(2.)))
	) ;

	(* until tests set *)
	pass_test True "B U A       " test1_trace
		(Until (3., Prop("B"), Prop("A"))
	) ;
	pass_test True "~(C U B)    " test1_trace
		(Not(Until (3., Prop("C"), Prop("B")))
	) ;
	pass_test True "(A U B)     " test1_trace
		(Until (3., Prop("A"), Prop("B"))
	) ;
	pass_test True "~(F 6 C)    " test1_trace
		(Not(meventually 6. (Prop("C")))
	) ;
	pass_test True "~(F 5.9 C)  " test1_trace
		(Not(meventually 5.9 (Prop("C")))
	) ;
	pass_test True "F 7.0(0)1 C " test1_trace
		(meventually (7. +. (epsilon_float *. 3.)) (Prop("C"))
	) ;
	pass_test True "F_2.0(0)1 ~A" test1_trace
		(meventually (2. +. epsilon_float) (Not(Prop("A")))
	) ;

	(* set of tests for temporal formulas *)
	pass_test True "~(A -> (F_1 C))   " test1_trace
		(Not(mimplies (Prop("A"))  (meventually 1. (Prop("C"))))
	) ;
	pass_test True "A -> (F_2.0(0)1 B)" test1_trace
		(mimplies (Prop("A"))  (meventually (2. +. epsilon_float) (Prop("B")))
	) ;
	pass_test False "G_2 ~A" test2_trace
		(malways 2. (Not(Prop("A")))
	) ;
	pass_test True "G_4 (A -> (F_2 B))" test1_trace
		(malways 4. (mimplies (Prop("A")) (meventually 2. (Prop("B"))))
	) ;
	pass_test Unknown "G_9.1 (A -> (F_2 B))" test1_trace
		(malways 9.1 (mimplies (Prop("A")) (meventually 2. (Prop("B"))))
	) ;

	(* complexity *)
	(* (y-2)*(x*(2*x))+((y-3)*x)+x *)
	count := 0 ;
	(* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x+x *)
	pass_test False "(A U_10 B) U_10 (A U_10 *)" test3_trace
	  (Until(10.,Until(10.,Prop("A"),Prop("B")),Until(10.,Prop("A"),Prop("*")))
	) ;
	Printf.printf "count: %i\n" !count ;

	count := 0 ;
	(* 5*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x)+4*x *)
	(*pass_test False "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)" test3_trace
	  (Until(10.,Until(10.,Until(10.,Prop("A"),Prop("B")),
	  Until(10.,
	  Prop("A"),Prop("B"))),Until(10.,Until(10.,Prop("A"),Prop("B")),Until(10.,
	  Prop("A"),Prop("*"))))
	) ;*)
	Printf.printf "count: %i\n" !count ;

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
	*)

	let rec ones n l = (if n <> 0 then ones (n-1) ((if List.length l = 0 then 1 else (List.hd l)+1)::l) else l) in
	let list_id = ones (get_counter_test_cases helper) [] in
	(* lets create a function to run all tests *)
	let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 filename in
    output_string oc ("
auto __run_unit_tests = []() {"^
	List.fold_left (fun a b -> "\n__unit_test_"^cluster_name^"_c"^string_of_int b^"();"^a) "" list_id^"
};");
    close_out oc;
;;


let test () cluster_name concurrency_on unit_on =
	print_endline "Test generation for monitors is enabled!";

	(* lets define the makefile *)
	let stream = open_out (cluster_name^"/tests/Makefile") in
let code =
"
x86-test:
\t g++ -Wall -g -O0 -std=c++0x -I../../../ -I../ -D__x86__ --verbose -c tests.cpp

" in
	Printf.fprintf stream "%s\n" code;
	close_out stream;

	(* lets define the main test file for multi-thread procucer/consumer *)
	(* each task consumes and produces certain amount of events; we use three dummy tasks *)
	let stream = open_out (cluster_name^"/tests/tests.cpp") in

	let consumer_lambda_function l =
	List.fold_left (fun a (b,_) -> "
	auto consumer"^string_of_int b^" = [](void *) -> void*
	{
		static RTEML_reader<int> __reader = RTEML_reader<int>(__buffer_"^ cluster_name ^".getBuffer());
		Event<int> tmpEvent;

		std::pair<state_rd_t,Event<int> > rd_tuple = __reader.dequeue();
		tmpEvent = rd_tuple.second;
		::printf(\"Event_consumed: %lu, %d code: %d\\n\", tmpEvent.getTime(), tmpEvent.getData(), rd_tuple.first);

		return NULL;
	};
	"^a) "" l in

	let producer_lambda_function l =
	List.fold_left (fun a (b,_) -> "
	auto producer"^string_of_int b^" = [](void *) -> void*
	{
		static RTEML_writer<int> __writer = RTEML_writer<int>(__buffer_"^ cluster_name ^".getBuffer());

		__writer.enqueue("^string_of_int b^");

		__buffer_"^ cluster_name ^".debug();
		return NULL;
	};
	"^a) "" l in

	Random.self_init ();
	let rec gen_task_ids l size =
		let id,_ = if (List.length l) = 0 then (0,0) else (List.hd l) in
		if size = (List.length l) then l else gen_task_ids( (id+1, ((1+Random.int (100))*50000))::l) size in

	let producers_ids = (gen_task_ids [] 50) in
	let consumers_ids = (gen_task_ids [] 40) in

	let code =
"
#include <stdio.h>
#include <unistd.h>

#include \"task_compat.h\"

#include \""^cluster_name^".h\"

#include \"unit_test_cases.h\"


int main( int argc, const char* argv[] )
{
	printf( \"RMTLD3 test for "^cluster_name^"\\n\" );

	//__start_periodic_monitors();

	"^
	(* begins the test for concurrency if enabled *)
	(if concurrency_on = "true" then "
	// basic enqueue and dequeue test case
	"^producer_lambda_function producers_ids^"

	"^consumer_lambda_function consumers_ids^"

	// lets create three producers
	"^List.fold_left (fun a (id,p) -> "__attribute__ ((unused)) __task producer_"^string_of_int id^" = __task(\"producer"^string_of_int id^"\", producer"^string_of_int id^", sched_get_priority_max(SCHED_FIFO), SCHED_FIFO, "^string_of_int p^");
	"^a) "" producers_ids^"

	// and two consumers
	/* we have two cases:
	 * - consumer is faster than producer (it will cause no side efects)
	 * - producer is faster than consumer (it will cause overwritten of buffer)
	 */

	"^List.fold_left (fun a (id,p) -> "__attribute__ ((unused)) __task consumer_"^string_of_int id^" = __task(\"consumer"^string_of_int id^"\", consumer"^string_of_int id^", sched_get_priority_max(SCHED_FIFO), SCHED_FIFO, "^string_of_int p^");
	"^a) "" consumers_ids^"
	" else "" )^"

	"^
	(* begins the test for units if enabled *)
	(if unit_on = "true" then "
	// if unit tests on then do that
	__run_unit_tests();
	" else "") ^"
	while(true) {sleep(1);}; // do sleep (delay)
}

" in
	Printf.fprintf stream "%s\n" code;
	close_out stream;
