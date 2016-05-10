
#include <stdlib.h>
#include <systemlib/systemlib.h>
#include <nuttx/config.h>
#include <nuttx/rtc.h>
#include <nuttx/clock.h>
#include <unistd.h>
#include <stdio.h>
#include <pthread.h>
#include <poll.h>
#include <drivers/drv_hrt.h>

// sample monitor (MANUAL SETTINGS)
#include "monitor_set1.h"
#include "task_compat.h"
#include "unit_test_cases.h"


extern "C" __EXPORT int rtemlib_main(int argc, char * const argv[]);

static int daemon_task;

static int monitor_main_loop(int argc, char **argv)
{
	/*bool state;
	__buffer.configWriter(__writer);
	__buffer.configReader(__reader);
	
	for (int i=0; i < 15; i++)
	{
		__writer.enqueue(i);
	}

	__buffer.debug();

	::printf("%lu\n", __buffer.getLength());

	Event<int> event;
	bool gap;
	for (int i=0; i < 20; i++)
	{
		state = __reader.dequeue(event, gap);
		::printf("%lu, %d, %d\n", event.getTime(), event.getData(), state);
	}

	// begin the execution of the monitor
	mon_one.enable();
	*/

	auto producer1 = [](void *) -> void*
	{
		auto enqueue_test = [](const int &data)
		{

		    // lets do one test
		    irqstate_t flags;
		    flags = irqsave();
		    //__disable_irq();
		    //sched_lock();
		    static uint32_t local = 0;
		    uint32_t * dest = &local;

		    START_MEASURE();
		    ATOMIC_begin(!, dest);
		    COUNT_CYCLE();
		    /*for(int i=0; i<10000; i++)
		    {
		        NOP;
		    }*/
		    //ISB;
		    ATOMIC_end(dest);
		    STOP_MEASURE();
		    //sched_unlock();
		    
		    irqrestore(flags);
		    //__enable_irq();

		};

		static RTEML_writer<int> __writer = RTEML_writer<int>(__buffer_monitor_set1.getBuffer());

		enqueue_test(1);

		START_MEASURE();
		for(int i=0; i<10; i++)
			__writer.enqueue(i);
		STOP_MEASURE();

		__buffer_monitor_set1.debug();
		return NULL;
	};

	auto consumer1 = [](void *) -> void*
	{
		static RTEML_reader<int> __reader = RTEML_reader<int>(__buffer_monitor_set1.getBuffer());
		Event<int> tmpEvent;

		std::pair<state_rd_t,Event<int> > rd_tuple = __reader.dequeue();
		tmpEvent = rd_tuple.second;
		::printf("Event_consumed: %lu, %d code: %d\n", tmpEvent.getTime(), tmpEvent.getData(), rd_tuple.first);

		// lets get a set of values instead of only one
		auto rd_tuple_10 = __reader.dequeue_20();
		auto code = std::get<0>(rd_tuple_10);
		uint32_t to_consume = std::get<1>(rd_tuple_10);
		auto rd_tuple_array = std::get<2>(rd_tuple_10);

		if(code == AVAILABLE || code == AVAILABLE_PARTIALLY)
		{
			uint32_t consumed = 0;

			for (auto i=rd_tuple_array.begin(); i != rd_tuple_array.end(); i++)
			{
				if(consumed >= to_consume)
					break;
				
				consumed++;

				tmpEvent = *i;
				::printf("Event_consumed_n: %lu, %d\n", tmpEvent.getTime(), tmpEvent.getData());
			}
		}
		else if(code == OVERWRITTEN)
		{
			DEBUGV("SYNC:%d\n", __reader.synchronize());
			
		}

		return NULL;
	};

	//__attribute__ ((unused)) __task producer_1 = __task("producer1", producer1, 242, SCHED_FIFO, 1000000);

	//__attribute__ ((unused)) __task consumer_1 = __task("consumer1", consumer1, sched_get_priority_max(SCHED_FIFO), SCHED_FIFO, 2350000);

	auto __run = [](void*) -> void*
	{
		//__run_unit_tests();
		while(true){sleep(1);}
	};

	// lets run the unit tests
	//__attribute__ ((unused)) __task task_unit_tests = __task("task_unit_tests", __run, 50, SCHED_FIFO, 2000000);

	// enable native monitors
	__start_periodic_monitors();

	while(true) {sleep(1);}; // do sleep (delay)

	return OK;
}

int rtemlib_main(int argc, char * const argv[])
{
	::printf("Mon starting...\n");

	daemon_task = px4_task_spawn_cmd("rtemlib_main",
                                     SCHED_FIFO,
                                     241,
                                     2048,
                                     monitor_main_loop,
                                     NULL);
    

	::printf("Mon started.\n");

	exit(0);

	//return OK;
}

