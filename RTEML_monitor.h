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
 * Represents a periodic Monitor.
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
class Monitor {
private:

    /** The Monitors pthread. */
    pthread_t thread;

    /** Status for monitor. */
    enum mon_status {ACTIVATION, RUNNING, DELAY, ABORT, ABORTED, UNACTIVATE};

    struct Monitor_state
    {
        /** The schedule policy of this Monitor. Can be: SCHED_OTHER, SCHED_FIFO, SCHED_RR, SCHED_BATCH, SCHED_IDLE as defined in pthread. */
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
     * Configures a RTEML_reader to an IEventBuffer buffer.
     *
     * @param _reader the RTEML_reader to be configured.
     * @param buffer a reference to an IEventBuffer.
     */
    template<typename T>
    void configReader(RTEML_reader<T> &_reader, IEventBuffer<T> &buffer);

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
    Monitor(const useconds_t period);

    /**
     * Instantiates a new monitor with a period of timespec, with schedule policy schedule_policy and priority priority. Can be: SCHED_OTHER, SCHED_FIFO, SCHED_RR, SCHED_BATCH, SCHED_IDLE as defined in pthread.
     *
     * @param period the monitors period.
     * @param schedule_policy the posix schedule policy for this monitor.
     * @param priority the priority for this monitor, should match the schedule policy.
     */
    Monitor(const useconds_t period, unsigned int schedule_policy, unsigned int priority);

    /**
     * Creates a thread for this monitor which will run function f.
     *
     * @param f the function to start the thread with.
     */
    int enable();

    /**
     * Checks whether this Monitor is running.
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

template<typename T>
void Monitor::configReader
    (
        RTEML_reader<T> &_reader,
        IEventBuffer<T> &buffer
    )
{
    buffer.configReader(_reader);
}

#endif //MONITOR_H
