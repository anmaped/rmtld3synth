
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

//#include "Monitor_One.h"
#include "Monitor_One.h"


extern "C" __EXPORT int Monitor_main(int argc, char * const argv[]);

static RTEML_buffer<int, 20> __buffer;
static RTEML_writer<int> __writer;
static RTEML_reader<int> __reader;

Monitor_One mon_one(__buffer, 1000000); // period in microseconds

static int daemon_task;

static int monitor_main_loop(int argc, char **argv)
{
	bool state;
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

	return OK;
}

int Monitor_main(int argc, char * const argv[])
{
	::printf("Mon starting...\n");

	daemon_task = px4_task_spawn_cmd("Monitor",
                                     SCHED_FIFO,
                                     40,
                                     2048,
                                     monitor_main_loop,
                                     NULL);
    

	::printf("Mon started.\n");

	return OK;
}

