/*
 *  RTEML is a library for monitoring of embedded NuttX OS applications.
 *  It has been developed in CISTER, a Research Centre in Real-Time
 *  and Embedded Computing Systems.
 *
 *    Copyright (C) 2015 André Pedro
 *
 *  This file is part of RunTime Embeeded Monitoring Library (RTEML).
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
 * Circular buffer for EventReaders and EventWriters.
 *
 * Circular buffer does not allocate any memory space due to static dependencies
 * of size atribute of the template. For that purpose this buffer receives a pre
 * allocated memory space pointer and its respective length.
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

    std::atomic<uint64_t> counter; // counter for node assignment

    typedef struct nd node;

    struct timming_page {
        uint64_t current_time;

        timming_page() {}
        timming_page(uint64_t t) :  current_time(t) {}
    };

    typedef struct timming_page tm_page;

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
     * variables are also atomically swaped. A timestamp is attached when the
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
    void readEventFromIndex(Event<T> &event, const size_t index) const;

    void getState(timeabs &time, size_t &idx) const;

    size_t counterToIndex(uint32_t lcounter) const;

    /**
     * Reads an absolute timestamp given an index.
     *
     * @warning the index must be valid otherwise an overflow will occur
     *
     * @param time A reference to a time span where the data will be stored.
     * @param index the index to read from.
     */
    //void readTimeabsFromIndex(timeabs &time, const size_t index) const;

    /**
     * Gets the number of enqued events.
     *
     * @return the number of elements since buffer was initialized.
     */
    size_t getElementsCount() const;

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

        ::printf("Time:%lld - %lld = %lld : Pointer: %p _> %p\n", tmptime, getCounterCurrentTimestamp(old_value64), tmptime - getCounterCurrentTimestamp(old_value64), new_tm_page, &local_tm_page );
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

        ::printf("Time:%lld -- %ld : Pointer: %p\n", new_tm_page->current_time, nextTime, new_tm_page);

    ATOMIC_end_VALUE64(new_value, counter);

    // now we know the new index and timestamp
    // lets modify the new event and set it to ready state

    tempEvent = &ca_accesspointer[tempIndex].ev;
    tempEvent->setTime( (timespan) nextTime );
    tempEvent->getData() = data;

    ca_accesspointer[tempIndex].state.store(READY);

    /*// now we have reserved the position for this push operation

    // atomic operations (interface with the buffer)
    ATOMIC_begin(atomic_ca_state_pointer);
        inftyBufferState * tmp_point = (inftyBufferState *)atomic_ca_state_pointer.load();

        //tempIndex = getNextIndex( tmp_point->writer_index );
        tempEvent = (Event<T> *) &ca_accesspointer[tempIndex];
        
        tempEvent->setTime( (timespan) nextTime );
        tempEvent->getData() = data;
        // for swapping
        ca_state_p->writer_index = tempIndex;
        ca_state_p->lastelementpushed_ts = tmptime;
        ca_state_p->el_count = tmp_point->el_count + 1;

        ca_state_p_old = tmp_point;

    ATOMIC_end(ca_state_p, atomic_ca_state_pointer);

    // swap state done.
    
    // end of atomic operations*/

}

template<typename T>
void CircularBuffer<T>::readEventFromIndex(Event<T> &event, const size_t index) const
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
   
/*template<typename T>
void CircularBuffer<T>::readTimeabsFromIndex(timeabs &time, const size_t index) const
{
    timeabs cumulate;
    size_t tmp_idx;

    inftyBufferState * tmp_point = (inftyBufferState *)atomic_ca_state_pointer.load();

    cumulate = tmp_point->lastelementpushed_ts;
    tmp_idx = tmp_point->writer_index;

    // requirement: reconstruct the absolute timestamp
    // reconstruction is backwards

    size_t idx = getPrevIndex(tmp_idx);
    while (idx != index)
    {
        cumulate -= ca_accesspointer[idx].getTime();
        idx = getPrevIndex(idx);
    }

    time = cumulate;
}*/

template<typename T>
size_t CircularBuffer<T>::getElementsCount() const
{
    //uint64_t tempCounter = (counter.load() & 0xFFFFFFFF00000000) >> 32;

    return getCounterValue(counter.load());
}

template<typename T>
size_t CircularBuffer<T>::getHead() const
{
    return counterToIndex(getElementsCount());
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
