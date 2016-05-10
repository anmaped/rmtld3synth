/*
 *  rtemlib is a Real-Time Embedded Monitoring Library.
 *  It has been developed in CISTER, a Research Centre in Real-Time
 *  and Embedded Computing Systems.
 *
 *    Copyright (C) 2015 André Pedro
 *
 *  This file is part of rtemlib.
 *
 *  rtemlib is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  rtemlib is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with rtemlib.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _CIRCULAR_BUFFER_H_
#define _CIRCULAR_BUFFER_H_

#include <stdio.h>
#include <time.h>

#include "Event.h"
#include "atomic_compat.h"
#include "debug_compat.h"

/**
 * Ring buffer for RTEML_reader and RTEML_writer.
 *
 * This buffer receives a pre-allocated memory region with a certain length and allow
 * atomic enqueues of events.
 *
 * @see Event.h
 *
 * @author André Pedro (anmap@isep.ipp.pt)
 * @date
 */
template<typename T>
class CircularBuffer {
private:

    // type identifying state enumeration for each node
    enum st {UPDATABLE=0, READY};

    /*
     * The definition of nodes for circular array. 
     */
    struct __node {
        std::atomic<st> state;
        Event<T> ev;
    };

    /* 
     * This secondary union is used to divide a 64bit wide integer.
     */
    union cnt {
        uint64_t ct64;
        struct {
            uint32_t idx;
            uint32_t t;
        };
    };

    /*
     * The enqueue operation requires a frame for CAS purposes. The frame is composed
     * by the absolute times. It can be real-time (days, years, months...), or
     * monotonic, i.e., the processor timer). Normally, it is of type uint64_t.
     */
    struct timming_page {

        uint64_t current_time;
        uint32_t counter;

        //timming_page() {}
        timming_page(uint64_t t) :  current_time(t) {}
    };

    // the absolute time when buffer is ready to accept nodes
    const timespanw initial_clock;

    // global page frame
    struct timming_page local_tm_page;
    
    /** Constant pointer to the start of the array */
    struct __node *const ca_accesspointer;

    /** Array Length */
    const size_t ca_length;

    /*
    *  There are two modes of Compare-and-swap (CAS). The first is the double CAS
    *  for x86, and the second is the simple CAS. Simple CAS suffers from ABA problem.
    *  However in our setting we strongly believe that is not affected since we swap
    *  distinct addresses that were previously allocated. [VERIFY]
    */
    FRAME_ADDRESS_type FRAME_ADDRESS;
    FRAME_ADDRESS_type & frame = FRAME_ADDRESS;


    timeabs getCounterCurrentTimestamp(uint64_t counter) const;
    timeabs getCounterCurrentTimestamp(uint32_t counter) const;

    size_t   getCounterValue(uint64_t counter) const;
    size_t   getCounterValue(uint32_t counter) const;

    uint32_t getCounterCurrentPage(uint64_t counter) const;
    uint32_t getCounterCurrentPage(uint32_t counter) const;

    void     setCounterCurrentPage(uint64_t &counter, uint32_t t) const;
    void     setCounterCurrentPage(uint32_t &counter, uint32_t t) const;

    void     setCounterValue(uint64_t &counter, size_t idx) const;
    void     setCounterValue(uint32_t &counter, size_t idx) const;

public:

    typedef struct __node node;
    typedef struct timming_page tm_page;

    /**
     * Instantiates a new buffer using external memory allocation.
     *
     * @param array a reference to a constant pointer that points to the array.
     * @param length the length of the array.
     */
    CircularBuffer(node * const &array, const size_t length);

    /**
     * Atomically enqueues data into the circular buffer.
     *
     * Index is always atomically updated after writing an element, and state
     * variables are also atomically swapped. A timestamp is attached when the
     * element is enqueued.
     *
     * @param data the data to be pushed.
     */
    void enqueue(const T &data, tm_page & new_tm_page);

    /**
     * Reads an index from the buffer.
     *
     * @param event A reference to an event object where the data will be stored.
     * @param index the index to read from.
     */
    void readEvent(Event<T> &event, const size_t index) const;

    /**
     * Gets the state of the buffer.
     *
     * @param time the current time.
     * @param idx  the current index.
     */
    void getState(timeabs &time, size_t &idx) const;

    /**
    * Maps the counter to the circular array index.
    *
    * @param lcounter the counter to be mapped.
    */
    size_t counterToIndex(uint32_t lcounter) const;

    /**
     * Gets the number of enqueued events.
     *
     * @return the number of elements since buffer was initialized.
     */
    size_t getCounterId() const;

    /**
     * Gets the current index, i.e., the index where the writer last wrote.
     *
     * @return the index.
     */
    size_t getHead() const;

    /**
     * Checks the availability of the node to be read.
     *
     * @param idx the index of the node
     *
     * @return true if the node is available, and false otherwise
     */
    bool nodeIsReady(const size_t idx) const;

    /**
     * Gets the difference of the current time with the initial absolute time
     * when buffer is created.
     *
     * @param unaligned_time the time to be aligned with the initial clock
     * @return the time input time aligned
     */
    timespanw getTimeAlignment(timespanw unaligned_time) const;

    /**
     * Gets the array length.
     *
     * @return the array length.
     */
    size_t getLength() const { return ca_length; };

    /**
     * Gets the frame by reference.
     *
     * @return the frame reference.
     */
    FRAME_ADDRESS_type & getFrameReference() const { return frame; };

    // functions for debug purposes only
    DEBUG_HELPER_BUFFER_FUNCTIONS();
};

template<typename T>
CircularBuffer<T>::CircularBuffer(node* const &array, const size_t length) :
    initial_clock(clockgettime()),
    local_tm_page(initial_clock),
    ca_accesspointer(array),
    ca_length(length)
{
    FRAME_ADDRESS_subtype x;

    setCounterCurrentPage(x, (uint32_t)&local_tm_page);
    setCounterValue(x, 0);

    getFrameReference().store(x);
}

template<typename T>
size_t CircularBuffer<T>::counterToIndex(uint32_t lcounter) const {
    return lcounter % getLength();
}

template<typename T>
timeabs CircularBuffer<T>::getCounterCurrentTimestamp(uint64_t lcounter) const
{
    union cnt x;
    x.ct64 = lcounter;
    tm_page * previous_tm_page = (tm_page *) x.t;

    return previous_tm_page->current_time;
}

template<typename T>
size_t CircularBuffer<T>::getCounterValue(uint64_t lcounter) const
{
    union cnt x;
    x.ct64 = lcounter;
    return x.idx;
}

template<typename T>
uint32_t CircularBuffer<T>::getCounterCurrentPage(uint64_t lcounter) const
{
    union cnt x;
    x.ct64 = lcounter;
    return x.t;
}

template<typename T>
void CircularBuffer<T>::setCounterCurrentPage(uint64_t &lcounter, uint32_t t) const
{
    union cnt x;
    x.ct64 = lcounter;
    x.t = t;
    lcounter = x.ct64;
}

template<typename T>
void CircularBuffer<T>::setCounterValue(uint64_t &lcounter, size_t idx) const
{
    union cnt x;
    x.ct64 = lcounter;
    x.idx = idx;
    lcounter = x.ct64;
}

template<typename T>
timeabs CircularBuffer<T>::getCounterCurrentTimestamp(uint32_t lcounter) const
{
    tm_page * previous_tm_page = (tm_page *) lcounter;

    return previous_tm_page->current_time;
}

template<typename T>
size_t CircularBuffer<T>::getCounterValue(uint32_t lcounter) const
{
    tm_page * previous_tm_page = (tm_page *) lcounter;

    return previous_tm_page->counter;
}

template<typename T>
uint32_t CircularBuffer<T>::getCounterCurrentPage(uint32_t lcounter) const
{
    return lcounter;
}

template<typename T>
void CircularBuffer<T>::setCounterCurrentPage(uint32_t &lcounter, uint32_t pg) const
{
    lcounter = pg;
}

template<typename T>
void CircularBuffer<T>::setCounterValue(uint32_t &lcounter, size_t idx) const
{
    tm_page * previous_tm_page = (tm_page *) lcounter;

    previous_tm_page->counter = idx;
}

template<typename T>
void CircularBuffer<T>::enqueue(const T &data, tm_page & new_tm_page) {

    uint32_t tempCounter;
    size_t tempIndex;
    timeabs tmptime; // verify type ... was uint64_t --> change to timeabs
    timespan nextTime;
    uint32_t newtempCounter;
    Event<T> *tempEvent;
    tm_page * ntp;

    // this part need to be processed atomically (set timestamp and increment the counter)
    // since several less significant bytes are available in one 64 bit word, we will use
    // it to store the counter. the counter should be larger as much as the buffer size.
    FRAME_ADDRESS_subtype new_value = 0; // this value may be used with incorrect initialization [DO NOT MAKE CHANGES BELOW]

    // instructions for measure time and number of tries
    //START_MEASURE();

    ATOMIC_begin_VALUE64(getFrameReference());

        //COUNT_CYCLE();

        // lets increment and get the counter atomically
        tempCounter = getCounterValue(OLD_FRAME_ADDRESS);
        // map counter to buffer index
        tempIndex = counterToIndex(tempCounter);

        // get timestamp
        tmptime = clockgettime();

        DEBUGV3("Time:%lld - %lld = %lld : Pointer: %p _> %p\n",
            tmptime,
            getCounterCurrentTimestamp(OLD_FRAME_ADDRESS),
            tmptime - getCounterCurrentTimestamp(OLD_FRAME_ADDRESS),
            new_tm_page,
            &local_tm_page
        );

        nextTime = tmptime - getCounterCurrentTimestamp(OLD_FRAME_ADDRESS) ;

        // case new_tm_page is currently in use then swap it with the local_tm_page
        if ( getCounterCurrentPage(OLD_FRAME_ADDRESS) == (uint32_t)&new_tm_page )
        {
            DEBUGV3("using main page\n");
            ntp = &local_tm_page;
        }
        else
        {
            ntp = &new_tm_page;
        }

        ntp->current_time = tmptime;

        // lets create the new value using the old value with incremented counter and new page
        newtempCounter = tempCounter + 1;
        
        setCounterCurrentPage(new_value, (uint32_t)ntp);
        setCounterValue(new_value, newtempCounter);

        // lets mark the event updatable
        ca_accesspointer[tempIndex].state.store(UPDATABLE);

        DEBUGV3("Time:%lld -- %ld : Pointer: %p\n",
            ntp->current_time,
            nextTime,
            ntp
        );

    ATOMIC_end_VALUE64(new_value, getFrameReference());

    // now we know the new index and timestamp
    // lets modify the new event and set it to ready state

    tempEvent = &ca_accesspointer[tempIndex].ev;
    tempEvent->setTime( (timespan) nextTime );
    tempEvent->setData( data );

    ca_accesspointer[tempIndex].state.store(READY);

    //STOP_MEASURE();

    DEBUGV3("Counter is lock free:%d\n", atomic_is_lock_free(&getFrameReference()));
}

template<typename T>
void CircularBuffer<T>::readEvent(Event<T> &event, const size_t index) const
{
    event = ca_accesspointer[index].ev;
}

template<typename T>
void CircularBuffer<T>::getState(timeabs &time, size_t &idx) const
{
    /* Frame can be swapped when time and idx are retrieved; a guarantee for
     * frame swapping is ensured by comparing the frame address before and
     * after the assignments.
     */
    ATOMIC_begin_VALUE64_NOEXCHANGE(getFrameReference());
        time = getCounterCurrentTimestamp(OLD_FRAME_ADDRESS);
        idx = getCounterValue(OLD_FRAME_ADDRESS);
    ATOMIC_end_VALUE64_NOEXCHANGE(getFrameReference());
}

template<typename T>
size_t CircularBuffer<T>::getCounterId() const
{
    size_t idx;

    /* Frame can be swapped when frame and idx are retrieved; a guarantee for
     * frame swapping is ensured by comparing the frame address before and
     * after the assignment.
     */
    ATOMIC_begin_VALUE64_NOEXCHANGE(getFrameReference());
        idx = getCounterValue(OLD_FRAME_ADDRESS);
    ATOMIC_end_VALUE64_NOEXCHANGE(getFrameReference());

    return idx;
}

template<typename T>
size_t CircularBuffer<T>::getHead() const
{
    return counterToIndex(getCounterId());
}

template<typename T>
bool CircularBuffer<T>::nodeIsReady(const size_t idx) const
{
    return ca_accesspointer[idx].state.load() == READY;
}

template<typename T>
timespanw CircularBuffer<T>::getTimeAlignment(timespanw unaligned_time) const
{
    return (unaligned_time >= initial_clock)? unaligned_time - initial_clock : 0;
};


#endif //_CIRCULAR_BUFFER_H_
