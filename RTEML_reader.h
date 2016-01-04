#ifndef _RTEML_READER_H_
#define _RTEML_READER_H_

#include <time.h>

#include "CircularBuffer.h"
#include "IEventReader.h"


/**
 * Reads events from an RTEML_buffer.
 *
 * @author André Pedro (anmap@isep.ipp.pt)
 * @date
 */
template<typename T>
class RTEML_reader : public IEventReader<T> {
private:
    /**  Constant pointer to a constant circular Buffer this RTEML_reader performs atomic read operations from.
     * @see CircularBuffer
     */
    const CircularBuffer<T> *buffer;

    /** Number of readed elements */
    size_t n_elems_reader;

    /** The timestamp of the last event this buffer read.
     * @see time.h
     */
    timeabs lastread_ts;

public:

    /**
     * Instantiates a new RTEML_reader.
     *
     * Instantiates a new event reader that reads from buffer.
     *
     * @param buffer a constant pointer that points to a constant buffer.
     */
    RTEML_reader(const CircularBuffer<T> * buffer);

    /**
     * Dequeue the next event from the buffer.
     *
     * @param idx the optional index to dequeue
     *
     * @return a tuple
     *
     */
    std::pair<state_rd_t,Event<T> &> dequeue(int idx = -1);

    bool dequeueArray(Event<T> * event, bool &isConsitent);

    /**
     * Synchronize the RTEML_reader index according to a timestamp
     *
     * @param time defines the timestamp to syncronize.
     *
     * @return true if the RTEML_reader was synchronized, false otherwise.
     */
    bool synchronize(timeabs time);

    /**
     * Compares the current RTEML_reader absolute timestamp with the
     * current absolute timestamp of the buffer.
     *
     * @return true if matched, false otherwise.
     */
    bool consistencyCheck() const;


    size_t getLowerIdx() { return 0; }

    size_t getHigherIdx() { return buffer->getLength(); }

    void getCurrentBufferState(timeabs &time, size_t &idx) { buffer->getState(time, idx); }
};

template<typename T>
RTEML_reader<T>::RTEML_reader(const CircularBuffer<T> * bbuffer) :
    buffer(bbuffer),
    n_elems_reader(0),
    lastread_ts(0)
{

}

template<typename T>
std::pair<state_rd_t,Event<T> &> RTEML_reader<T>::dequeue(int idx) {

    Event<T> tempEvent;
    size_t n_elems_writer;

    size_t index_for_event = (idx == -1) ? buffer->counterToIndex(n_elems_reader) : idx;

    // atomic operation block       >####
    
    ATOMIC_begin_VALUE64_NOEXCHANGE(buffer->counter);

        buffer->readEvent(tempEvent, index_for_event);  // unsafe in terms of empty buffer
        n_elems_writer = buffer->getCounterId();

        if (!buffer->nodeIsReady(index_for_event))
        {
            continue;
        }

    ATOMIC_end_VALUE64_NOEXCHANGE(buffer->counter);

    // end of atomic operation block >####
    
    // continue processing ...

    if (n_elems_reader < n_elems_writer)
    {
        // measure the distance between the indexes of the reader and writer
        // is greater than buffer length (this indicate an event overwrite)
        if(n_elems_writer-n_elems_reader > buffer->getLength())
        {
            // when there is an overwrite of the events
            return std::pair<state_rd_t,Event<T> &>(OVERWRITEN, tempEvent);
        }

        if(idx == -1)
        {
            n_elems_reader ++; // locally increment the number of read elements

            // update absolute time state of the monitor
            lastread_ts += tempEvent.getTime();
        }

        // when successful
        return std::pair<state_rd_t,Event<T> &>(AVAILABLE, tempEvent);
    }
    else
    {
        // when there is no event available
        return std::pair<state_rd_t,Event<T> &>(UNAVAILABLE, tempEvent);
    }
    
}

template<typename T>
bool RTEML_reader<T>::dequeueArray(Event<T> * event, bool &isConsitent) {
    // [todo]
    return false;
}

template<typename T>
bool RTEML_reader<T>::synchronize(timeabs time)
{
    // [todo]
    return false;
}

template<typename T>
bool RTEML_reader<T>::consistencyCheck() const
{
    // [todo]
    return false;
}

#endif //_RTEML_READER_H_
