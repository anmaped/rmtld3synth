
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

static EventBuffer<int, 20> evt_buffer;
static EventWriter<int> writer;
static EventReader<int> reader;

Monitor_One mon_one(evt_buffer, 1000000); // period in microseconds

static int daemon_task;

static int monitor_main_loop(int argc, char **argv)
{
	bool state;
	evt_buffer.configWriter(writer);
	evt_buffer.configReader(reader);
	
	for (int i=0; i < 15; i++)
	{
		writer.enqueue(i);
	}

	evt_buffer.debug();

	::printf("%lu\n", evt_buffer.getLength());

	Event<int> event;
	bool gap;
	for (int i=0; i < 20; i++)
	{
		state = reader.dequeue(event, gap);
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

