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

    /** CircularBuffer write operations state */
    struct circular_array_state {
        /** The index the writer is writing at */
        size_t writer_index;
        
        /** element counting */
        size_t el_count;

        /** Store the absolute time of the last element pushed */
        timeabs lastelementpushed_ts;

    } ca_state[2];

    /** atomic variable */
    bool atomic_current_idx = false;
    
    /** Constant pointer to the start of the array */
    const Event<T> * ca_accesspointer;

    /** Array Length */
    const size_t ca_length;

    size_t getPrevIndex(size_t idx) const;

    size_t getNextIndex(size_t idx) const;

public:
    typedef struct circular_array_state inftyBufferState;

    /**
     * Instantiates a new buffer using external memory allocation.
     *
     * @param array a reference to a constant pointer that points to the array.
     * @param length the length of the array.
     */
    CircularBuffer(const Event<T>* const &array, const size_t length);

    /**
     * Atomically enqueues data into the circular buffer.
     *
     * Index is always atomically updated after writing an element, and state
     * variables are also atomically swaped. A timestamp is attached when the
     * element is enqueued.
     *
     * @param data the data to be pushed.
     */
    void enqueue(const T &data);

    /**
     * Reads an index from the buffer.
     *
     * @param event A reference to an event object where the data will be stored.
     * @param index the index to read from.
     */
    void readEventFromIndex(Event<T> &event, const size_t index) const;

    /**
     * Reads an absolute timestamp given an index.
     *
     * @warning the index must be valid otherwise an overflow will occur
     *
     * @param time A reference to a time span where the data will be stored.
     * @param index the index to read from.
     */
    void readTimeabsFromIndex(timeabs &time, const size_t index) const;

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

    /**
     * Gets the array length.
     *
     * @return the array length.
     */
    size_t getLength() const;

};

template<typename T>
CircularBuffer<T>::CircularBuffer(const Event<T>* const &array, const size_t length) :
    ca_accesspointer(array),
    ca_length(length)
{
    // initialize states for swaping
    ca_state[0].writer_index = 0;
    ca_state[0].lastelementpushed_ts = clockgettime();
    ca_state[0].el_count = 0;

    ca_state[1].writer_index = 0;
    ca_state[1].lastelementpushed_ts = ca_state[0].lastelementpushed_ts;
    ca_state[1].el_count = 0;
}

template<typename T>
size_t CircularBuffer<T>::getNextIndex(size_t idx) const {
    size_t tmpval = idx + 1;

    if (tmpval >= ca_length)
        return 0;

    return tmpval;
}

template<typename T>
size_t CircularBuffer<T>::getPrevIndex(size_t idx) const {

    if ((int)(idx) - 1 < 0)
        return ca_length;

    return (size_t)(idx - 1);
}

template<typename T>
void CircularBuffer<T>::enqueue(const T &data) {

    size_t tempIndex;
    Event<T> *tempEvent;

    // atomic operations (interface with the buffer)
    uint32_t new_value;
    static uint32_t * dest = (uint32_t *) &atomic_current_idx; // static is ensuring that anyone can modify it (stack could be optimized for others)
    
    ATOMIC_begin(new_value, !, dest);

        //::printf("%d\n", new_value);
        tempIndex = getNextIndex( ca_state[!new_value].writer_index );
        tempEvent = (Event<T> *) &ca_accesspointer[tempIndex];
        // get time
        timeabs tmptime = clockgettime();
        tempEvent->setTime( (timespan) tmptime - ca_state[!new_value].lastelementpushed_ts );
        tempEvent->getData() = data;
        // for swaping
        ca_state[new_value].writer_index = tempIndex;
        ca_state[new_value].lastelementpushed_ts = tmptime;
        ca_state[new_value].el_count = ca_state[!new_value].el_count + 1;

    ATOMIC_end(new_value, dest);

    // swap state done.
    
    // end of atomic operations
}

template<typename T>
void CircularBuffer<T>::readEventFromIndex(Event<T> &event, const size_t index) const
{
    event = ca_accesspointer[index];
}
   
template<typename T>
void CircularBuffer<T>::readTimeabsFromIndex(timeabs &time, const size_t index) const
{
    timeabs cumulate;
    size_t tmp_idx;

    cumulate = ca_state[atomic_current_idx].lastelementpushed_ts;
    tmp_idx = ca_state[atomic_current_idx].writer_index;

    // requirement: reconstruct the absolute timestamp
    // reconstruction is backwards

    size_t idx = getPrevIndex(tmp_idx);
    while (idx != index)
    {
        cumulate -= ca_accesspointer[idx].getTime();
        idx = getPrevIndex(idx);
    }

    time = cumulate;
}

template<typename T>
size_t CircularBuffer<T>::getElementsCount() const
{
    return ca_state[atomic_current_idx].el_count;
}

template<typename T>
size_t CircularBuffer<T>::getHead() const
{
    return ca_state[atomic_current_idx].writer_index;
}

template<typename T>
size_t CircularBuffer<T>::getLength() const
{
    return ca_length;
}

#endif //_CIRCULAR_BUFFER_H_
