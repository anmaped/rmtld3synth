

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
	auto producer = [](void *) -> void*
	{
		static RTEML_writer<int> __writer = RTEML_writer<int>(__buffer_"^ cluster_name ^".getBuffer());

		__writer.enqueue(1);

		__buffer_"^ cluster_name ^".debug();
		return NULL;
	};

	auto producer2 = [](void *) -> void*
	{
		static RTEML_writer<int> __writer = RTEML_writer<int>(__buffer_"^ cluster_name ^".getBuffer());

		__writer.enqueue(2);

		__buffer_"^ cluster_name ^".debug();
		return NULL;
	};

	auto producer3 = [](void *) -> void*
	{
		static RTEML_writer<int> __writer = RTEML_writer<int>(__buffer_"^ cluster_name ^".getBuffer());

		__writer.enqueue(3);

		__buffer_"^ cluster_name ^".debug();

		return NULL;
	};

	auto consumer = [](void *) -> void*
	{
		static RTEML_reader<int> __reader = RTEML_reader<int>(__buffer_"^ cluster_name ^".getBuffer());
		Event<int> tmpEvent;

		std::pair<state_rd_t,Event<int> &> rd_tuple = __reader.dequeue();
		tmpEvent = rd_tuple.second;
		::printf(\"event_out: %lu, %d code: %d\\n\", tmpEvent.getTime(), tmpEvent.getData(), rd_tuple.first);

		return NULL;
	};

	// lets create three producers
	__task producer_A = __task(producer, 0, SCHED_FIFO, 100000);
	__task producer_B = __task(producer2, 0, SCHED_FIFO, 300000);
	__task producer_C = __task(producer3, 0, SCHED_FIFO, 500000);

	// and two consumers
	/* we have two cases:
	 * - consumer is faster than producer (it will cause no side efects)
	 * - producer is faster than consumer (it will cause overwritten of buffer)
	 */
	__task consumer_A = __task(consumer, 0, SCHED_FIFO, 50000);
	//__task consumer_B = __task([](void *)->void*{return NULL;}, 0, SCHED_FIFO, 2000000);

	while(true) {sleep(1);}; // do sleep (delay)
}

" in
	Printf.fprintf stream "%s\n" code;
	close_out stream;
