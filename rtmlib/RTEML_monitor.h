#ifndef MONITOR_H
#define MONITOR_H

#include <time.h>
#include <pthread.h>
#include <stdio.h>

#ifdef __NUTTX__
#include <sched.h>
#endif

#include <errno.h>

#include "time_compat.h"
#include "RTEML_buffer.h"

#include "task_compat.h"

/**
 * Represents a periodic RTEML_monitor.
 *
 * This class represents a periodic monitor, capable of monitoring several EventBuffers.  The monitor creates a periodic
 * pthread that executes the code on the pure virtual function run.
 *
 * \warning
 * The monitor must always exist in memory after enabling it, failure to do so will result in undefined behavior.
 *
 * Users can create SynchronizedEventReaders by calling the configSynchronizedEventReader function.
 *
 * @author André Pedro (anmap@isep.ipp.pt)
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
class RTEML_monitor {
private:

    /** The Monitors pthread. */
    pthread_t thread;

    /** Status for monitor. */
    enum mon_status {ACTIVATION, RUNNING, DELAY, ABORT, ABORTED, UNACTIVATE};

    struct Monitor_state
    {
        /** The schedule policy for the current RTEML_monitor as defined in pthread. */
        const int sched_policy;

        /** The priority for the schedule policy, please see the pthread documentation for more information. */
        const int priority;

        /** Status of the monitor */
        mon_status status;

        /** Mutex for monitor */
        pthread_mutex_t fmtx;

        /** Conditional variable for monitor */
        pthread_cond_t cond;

        /** The Monitors period. */
        useconds_t period;

        Monitor_state(const int sch, const int prio, const useconds_t p) :
            sched_policy(sch),
            priority(prio),
            status(UNACTIVATE),
            period(p) {};

    } m_state;

    /**
     * Receives a pointer to a monitor and executes its run function in a loop until it asynch_disable is called,
     * checking for dead line misses.
     *
     * The task will arrive with the correct period.
     *
     * @param ptr pointer to the monitor being ran.
     */
    static void* loop(void*);

protected:

    /**
     * The monitor execution code.
     *
     * Users should overwrite this function  with their monitoring code. It will be called periodically as defined by
     * the users period.
     *
     */
    virtual void run() = 0;

public:

    /**
     * Instantiates a new monitor with a period of timespec.
     *
     * The schedule policy will be SCHED_OTHER with priority 0.
     *
     * @param period the monitors period.
     */
    RTEML_monitor(const useconds_t period);

    /**
     * Instantiates a new monitor with a certain period, a schedule policy and one priority.
     *
     * @param period the RTEML_monitor period.
     * @param policy the posix schedule policy for this monitor.
     * @param priority the priority for this RTEML_monitor.
     */
    RTEML_monitor(const useconds_t period, unsigned int policy, unsigned int priority);

    /**
     * Creates a thread for this monitor which will run function f.
     *
     * @param f the function to start the thread with.
     */
    int enable();

    /**
     * Checks whether this RTEML_monitor is running.
     *
     * @returns true if the monitor is running.
     */
    bool isRunning() const;

    /**
     * Returns the Monitors period.
     * @return the monitor period.
     */
    const useconds_t & getPeriod() const;

    /** Sets new monitor period. */
    void setPeriod(const useconds_t & p);
};

#endif //MONITOR_H
