
#ifndef _TASK_COMPAT_H_
#define _TASK_COMPAT_H_

#include <pthread.h>
#include <errno.h>
#include "time_compat.h"
#include "debug_compat.h"

#ifdef __NUTTX__
	#define STACK_SIZE 6000
#elif defined __x86__
	#define STACK_SIZE 1000000
#endif


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

	char const * tid;

	const useconds_t period;

	const int sched_policy;

	const int priority;

	bool running;

	status st;

	void* (*run)(void *);

	

	int create_task(void* (*loop)(void *), const int pri, const int s_policy, int stack_size = STACK_SIZE)
	{
		pthread_attr_t attribute = {0};
	    struct sched_param parameter;

	    DEBUGV("Task %s started!\n", tid);

	    pcheck( pthread_attr_init( &attribute ) );

	    pcheck_attr( pthread_attr_setschedpolicy( &attribute, sched_policy ), &attribute );

	    pcheck_attr( pthread_attr_setstacksize(&attribute, stack_size),  &attribute );
	    DEBUGV("Stack:%d\n", stack_size);
	    
	    DEBUGV("Priority:%d\n", priority);
	    parameter.sched_priority = priority;

	    pcheck_attr( pthread_attr_setschedparam( &attribute, &parameter ), &attribute );

	    pcheck_attr( pthread_create( &thread, &attribute, loop, this ), &attribute );

	    pcheck( pthread_attr_destroy( &attribute ) );

	    running = true;

	    return 0;
	}

	task(char const * id, void* (*loop)(void *), const int prio, const int sch_policy, const useconds_t p) : tid(id), period(p), sched_policy(sch_policy), priority(prio), run(loop)
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

		        DEBUGV3("#loop+_%s\n", ttask->tid);

		        clock_gettime(CLOCK_REALTIME, &now);

		        // convert useconds_t to struct timespec
		        struct timespec spec_p;

		        DEBUGV3("useconds: %lu\n", ttask->period);

		        useconds_t2timespec( &ttask->period, &spec_p );

		        DEBUGV3("timespecp: %lu,%lu\n", spec_p.tv_sec, spec_p.tv_nsec);
		        DEBUGV3("timespec: %lu,%lu\n", next.tv_sec, next.tv_nsec);

		        timespecadd( &next, &spec_p, &next );

		        DEBUGV3("timespec2: %lu,%lu\n", next.tv_sec, next.tv_nsec);
		        DEBUGV3("timespecnow: %lu,%lu\n", now.tv_sec, now.tv_nsec);
		        
		        

		        if ( timespeccmp( &now, &next, > ) ) {

		            timespecsub(&next, &now, &tmp);
		            DEBUGV_ERROR("Task is missing their deadline for %lu s.%lu ns\n", tmp.tv_sec, tmp.tv_nsec);

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

#endif //_TASK_COMPAT_H_
