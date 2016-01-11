

let test () cluster_name =
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


int main( int argc, const char* argv[] )
{
	printf( \"RMTLD3 test for "^cluster_name^"\\n\" );

	//__start_periodic_monitors();


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

	while(true) {sleep(1);}; // do sleep (delay)
}

" in
	Printf.fprintf stream "%s\n" code;
	close_out stream;
