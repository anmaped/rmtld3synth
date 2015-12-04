
#ifndef _TIME_COMPAT_H_
#define _TIME_COMPAT_H_

//#include "atomic_compat.h"

#ifdef __NUTTX__
#include <nuttx/clock.h>

/* we have for timestamps uint8_t uint16_t uint24_t uint32_t and uint64_t
* uint8_t is not enough even for super fast monitors (huge overhead)
* uint16_t can be enough for faster monitors (Cortex-M is not the case)
* uint24_t is enough if the monitor has a period aproximatly of 100hz
* uint32_t is enough if the monitor has a period aproximatly of 1hz
* for other cases uint64_t is required
* the timestamps are always in nanoseconds
* @author Andre Pedro (anmap@isep.ipp.pt)
*/
typedef uint64_t timeabs;
typedef uint32_t timespan;

#define clockgettime()  ({ \
    timespan ms = g_system_timer; \
    timespan ns = SysTick->VAL; \
    (ms * 1000000) +  \
    ((1000000. / SysTick->LOAD) * \
    (SysTick->LOAD - ns));})

#elif defined __x86__

#include <time.h>
#include <pthread.h>

typedef long long timeabs;
typedef long timespan;

#define clockgettime() ({ \
    struct timespec __n; \
    clock_gettime(CLOCK_REALTIME, &__n); \
    1;})

#else

    #error "This monitoring library only supports NuttX ARM Cortex-M4 and x86 architecture!"

#endif


/* Operations on timespecs. */
#define timespecclear(tsp)      (tsp)->tv_sec = (tsp)->tv_nsec = 0

#define timespecisset(tsp)      ((tsp)->tv_sec || (tsp)->tv_nsec)

#define timespeccmp(tsp, usp, cmp)                  \
    (((tsp)->tv_sec == (usp)->tv_sec) ?             \
        ((tsp)->tv_nsec cmp (usp)->tv_nsec) :           \
        ((tsp)->tv_sec cmp (usp)->tv_sec))

#define timespecadd(tsp, usp, vsp)                  \
    do {                                \
        (vsp)->tv_sec = (tsp)->tv_sec + (usp)->tv_sec;      \
        (vsp)->tv_nsec = (tsp)->tv_nsec + (usp)->tv_nsec;   \
        if ((vsp)->tv_nsec >= 1000000000L) {            \
            (vsp)->tv_sec++;                \
            (vsp)->tv_nsec -= 1000000000L;          \
        }                           \
    } while (0)

#define timespecsub(tsp, usp, vsp)                  \
    do {                                \
        (vsp)->tv_sec = (tsp)->tv_sec - (usp)->tv_sec;      \
        (vsp)->tv_nsec = (tsp)->tv_nsec - (usp)->tv_nsec;   \
        if ((vsp)->tv_nsec < 0) {               \
            (vsp)->tv_sec--;                \
            (vsp)->tv_nsec += 1000000000L;          \
        }                           \
    } while (0)

#define useconds_t2timespec(tsp, vsp)                 \
    do {                                            \
    	(vsp)->tv_sec = *tsp / 1000000L;            \
    	(vsp)->tv_nsec = (*tsp % 1000000L) * 1000;  \
    } while (0)

#endif //_TIME_COMPAT_H_
