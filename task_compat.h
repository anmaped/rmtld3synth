
#include <pthread.h>
#include <errno.h>
#include "time_compat.h"

#define P_OK 0

#define pcheck(val) \
    if (val != P_OK) \
    { \
        ::printf("Error: %d\n", val); \
        return val; \
    } \

#define pcheck_print(val, checkval, extra_action) \
    if (val != checkval) \
    { \
        ::printf("Error: %d\n", val); \
        extra_action \
    }

#define pcheck_attr(val, attr) \
    if (val != P_OK) \
    { \
        ::printf("Destroying ERROR:%d\n", val); \
        pthread_attr_destroy(attr); \
        return val; \
    }

enum status {ACTIVATION, RUNNING, DELAY, ABORT, ABORTED, UNACTIVATE};

struct task {
	
	pthread_t thread;

	pthread_mutex_t fmtx;

	pthread_cond_t cond;

	const useconds_t period;

	const int sched_policy;

	const int priority;

	bool running;

	status st;

	void* (*run)(void *);

	int create_task(void* (*loop)(void *), const int priority, const int sched_policy, int stack_size = 1000000)
	{
		pthread_attr_t attribute = {0};
	    struct sched_param parameter;

	    ::printf( "Task started!\n" );

	    pcheck( pthread_attr_init( &attribute ) );

	    pcheck_attr( pthread_attr_setschedpolicy( &attribute, sched_policy ), &attribute );

	    pcheck_attr( pthread_attr_setstacksize(&attribute, stack_size),  &attribute );
	    
	    ::printf("Priority:%d\n", priority);
	    parameter.sched_priority = priority;

	    pcheck_attr( pthread_attr_setschedparam( &attribute, &parameter ), &attribute );

	    pcheck_attr( pthread_create( &thread, &attribute, loop, this ), &attribute );

	    pcheck( pthread_attr_destroy( &attribute ) );

	    running = true;

	    return 0;
	}

	task(void* (*loop)(void *), const int prio, const int sch_policy, const useconds_t p) : period(p), sched_policy(sch_policy), priority(prio), run(loop)
	{
		create_task([](void *tsk)-> void*
		{
			struct task * ttask = (struct task *) tsk;
			struct timespec now = {0}, next = {0}, tmp = {0};

			// Mutex and conditional variables for pthread_cond_timedwait
		    pcheck_print( pthread_mutex_init ( &ttask->fmtx, NULL ), P_OK, return NULL; );
		    pcheck_print( pthread_cond_init ( &ttask->cond, NULL ), P_OK, return NULL; );

		    clock_gettime(CLOCK_REALTIME, &next);

		    for (;;) {

		        ::printf("loop+...\n");

		        clock_gettime(CLOCK_REALTIME, &now);

		        // convert useconds_t to struct timespec
		        struct timespec p;

		        ::printf("useconds: %lu\n", ttask->period);

		        useconds_t2timespec( &ttask->period, &p );

		        ::printf("timespecp: %lu,%lu\n", p.tv_sec, p.tv_nsec);

		        ::printf("timespec: %lu,%lu\n", next.tv_sec, next.tv_nsec);

		        timespecadd( &next, &p, &next );

		        ::printf("timespec2: %lu,%lu\n", next.tv_sec, next.tv_nsec);

		        ::printf("timespecnow: %lu,%lu\n", now.tv_sec, now.tv_nsec);
		        
		        

		        if ( timespeccmp( &now, &next, > ) ) {

		            timespecsub(&next, &now, &tmp);
		            ::printf("Task is missing their deadline for %lu s.%lu ns\n", tmp.tv_sec, tmp.tv_nsec);

		        }

		        pthread_mutex_lock( &ttask->fmtx );
		        pcheck_print( pthread_cond_timedwait( &ttask->cond, &ttask->fmtx, &next ), ETIMEDOUT, break; );
		        pthread_mutex_unlock( &ttask->fmtx );

		        ttask->run(NULL);

		        if ( ttask->st == ABORT )
		        {
		            ttask->running = false;
		            return NULL;
		        }
		        
		    }

		    return NULL;

		},prio,sch_policy);
	}

};

typedef struct task __task;
