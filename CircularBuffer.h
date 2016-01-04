/*
 *  RTEML is a Real-Time Embedded Monitoring Library.
 *  It has been developed in CISTER, a Research Centre in Real-Time
 *  and Embedded Computing Systems.
 *
 *    Copyright (C) 2015 André Pedro
 *
 *  This file is part of RTEML.
 *
 *  RTEML is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  RTEML is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with RTEML.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _CIRCULAR_BUFFER_H_
#define _CIRCULAR_BUFFER_H_

#include <stdio.h>
#include <time.h>

#include "Event.h"
#include "atomic_compat.h"

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

    enum st {UPDATABLE=0, READY};

    struct nd {
        std::atomic<st> state;
        Event<T> ev;
    };

    union cnt {
        uint64_t ct64;
        struct {
            uint32_t idx;
            uint32_t t;
        };
    };

    struct timming_page {
        uint64_t current_time;

        timming_page() {}
        timming_page(uint64_t t) :  current_time(t) {}
    };
    
    /** Constant pointer to the start of the array */
    struct nd *const ca_accesspointer;

    /** Array Length */
    const size_t ca_length;


    uint64_t getCounterCurrentTimestamp(uint64_t counter) const;
    size_t getCounterValue(uint64_t counter) const;
    uint32_t getCounterCurrentPage(uint64_t counter) const;

    void setCounterCurrentPage(uint64_t &counter, uint32_t t) const;
    void setCounterValue(uint64_t &counter, size_t idx) const;

public:

    typedef struct nd node;
    typedef struct timming_page tm_page;

    std::atomic<uint64_t> counter; // counter for node assignment

    tm_page local_tm_page;

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
    void enqueue(const T &data, tm_page * new_tm_page);

    /**
     * Reads an index from the buffer.
     *
     * @param event A reference to an event object where the data will be stored.
     * @param index the index to read from.
     */
    void readEvent(Event<T> &event, const size_t index) const;

    void getState(timeabs &time, size_t &idx) const;

    size_t counterToIndex(uint32_t lcounter) const;


    /**
     * Gets the number of enqued events.
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

    bool nodeIsReady(const size_t idx) const;

    /**
     * Gets the array length.
     *
     * @return the array length.
     */
    size_t getLength() const;

};

template<typename T>
CircularBuffer<T>::CircularBuffer(node* const &array, const size_t length) :
    ca_accesspointer(array),
    ca_length(length),
    local_tm_page(tm_page(clockgettime()))
{
    union cnt x;
    x.t = (uint32_t)&local_tm_page;
    x.idx = 0;
    counter.store((uint64_t)x.ct64);
}

template<typename T>
size_t CircularBuffer<T>::counterToIndex(uint32_t lcounter) const {
    return lcounter % getLength();
}

template<typename T>
uint64_t CircularBuffer<T>::getCounterCurrentTimestamp(uint64_t counter) const
{
    union cnt x {.ct64 = counter}; // [TODO: we need to confirm...]
    tm_page * previous_tm_page = (tm_page *) x.t;

    return previous_tm_page->current_time;
}

template<typename T>
size_t CircularBuffer<T>::getCounterValue(uint64_t counter) const
{
    union cnt x {.ct64 = counter}; // [TODO: we need to confirm...]
    return x.idx;
}

template<typename T>
uint32_t CircularBuffer<T>::getCounterCurrentPage(uint64_t counter) const
{
    union cnt x {.ct64 = counter}; // [TODO: we need to confirm...]
    return x.t;
}

template<typename T>
void CircularBuffer<T>::setCounterCurrentPage(uint64_t &counter, uint32_t t) const
{
    union cnt x {.ct64 = counter}; // [TODO: we need to confirm...]
    x.t = t;
    counter = x.ct64;
}

template<typename T>
void CircularBuffer<T>::setCounterValue(uint64_t &counter, size_t idx) const
{
    union cnt x {.ct64 = counter}; // [TODO: we need to confirm...]
    x.idx = idx;
    counter = x.ct64;
}

template<typename T>
void CircularBuffer<T>::enqueue(const T &data, tm_page * new_tm_page) { // [TODO FRAME FOR EACH WRITER]

    uint32_t tempCounter;
    size_t tempIndex;
    Event<T> *tempEvent;
    uint64_t tmptime;
    timespan nextTime;

    // this part need to be processed atomically (set timestamp and increment the counter)
    // since several less significant bytes are available in one 64 bit word, we will use
    // it to store the counter. the counter should be larger as much as the buffer size.
    uint64_t new_value;

    ATOMIC_begin_VALUE64(counter);

        // lets increment and get the counter atomically
        tempCounter = getCounterValue(old_value64);
        // map counter to buffer index
        tempIndex = counterToIndex(tempCounter);

        // get timestamp
        tmptime = clockgettime();

        ::printf("Time:%lld - %lld = %lld : Pointer: %p _> %p\n",
            tmptime,
            getCounterCurrentTimestamp(old_value64),
            tmptime - getCounterCurrentTimestamp(old_value64),
            new_tm_page,
            &local_tm_page
        );

        nextTime = tmptime - getCounterCurrentTimestamp(old_value64) ;

        // case new_tm_page is currently in use then swap it with the local_tm_page
        if ( getCounterCurrentPage(old_value64) == (uint32_t)new_tm_page )
        {
            ::printf("using main page\n");
            new_tm_page = &local_tm_page;
        }

        new_tm_page->current_time = tmptime;

        // lets create the new value using the old value with incremented counter and new page
        uint32_t newtempCounter = tempCounter + 1;
        setCounterValue(new_value, newtempCounter);
        setCounterCurrentPage(new_value, (uint32_t)new_tm_page);

        // lets mark the event updatable
        ca_accesspointer[tempIndex].state.store(UPDATABLE);

        ::printf("Time:%lld -- %ld : Pointer: %p\n",
            new_tm_page->current_time,
            nextTime,
            new_tm_page
        );

    ATOMIC_end_VALUE64(new_value, counter);

    // now we know the new index and timestamp
    // lets modify the new event and set it to ready state

    tempEvent = &ca_accesspointer[tempIndex].ev;
    tempEvent->setTime( (timespan) nextTime );
    tempEvent->getData() = data;

    ca_accesspointer[tempIndex].state.store(READY);
}

template<typename T>
void CircularBuffer<T>::readEvent(Event<T> &event, const size_t index) const
{
    event = ca_accesspointer[index].ev;
}

template<typename T>
void CircularBuffer<T>::getState(timeabs &time, size_t &idx) const
{
    uint64_t tempCounter = counter.load();
    time = getCounterCurrentTimestamp(tempCounter);
    idx = getCounterValue(tempCounter);
}

template<typename T>
size_t CircularBuffer<T>::getCounterId() const
{
    //uint64_t tempCounter = (counter.load() & 0xFFFFFFFF00000000) >> 32;

    return getCounterValue(counter.load());
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
size_t CircularBuffer<T>::getLength() const
{
    return ca_length;
}

#endif //_CIRCULAR_BUFFER_H_
