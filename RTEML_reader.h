#ifndef _RTEML_READER_H_
#define _RTEML_READER_H_

#include <time.h>

#include "CircularBuffer.h"
#include "IEventReader.h"


/**
 * Reads events from an EventBuffer.
 *
 * Event Reader reads Events from an EventBuffer FIFO fashion. RTEML_reader does what it can when elements
 * are written faster than it can read. When overflows occurs RTEML_reader tries to find the oldest element it has yet to
 * read and notifies the user of the overflow.
 *
 * RTEML_readers are instantiated or configured by EventBuffers.
 *
 * @see EventBuffer
 *
 * @author André Pedro (anmap@isep.ipp.pt)
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<typename T>
class RTEML_reader : public IEventReader<T> {
private:
    /**  Constant pointer to a constant circular Buffer this RTEML_reader performs atomic read operations from.
     * @see Buffer
     */
    const CircularBuffer<T> *buffer;

    /** Pointer to a constant event which is the current index on this RTEML_reader. */
    size_t index;

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
     * The event reader is blank and should only be configured by calling getEventReader on an EventBuffer.
     */
    RTEML_reader();

    /**
     * Instantiates a new RTEML_reader.
     *
     * Instantiates a new event reader that reads from buffer.
     *
     * @param buffer a constant pointer that points to a constant buffer.
     */
    RTEML_reader(const CircularBuffer<T> *const &buffer);

    /**
     * Dequeue the next event from the buffer.
     *
     * @param event a reference to a Element object which will be updated with the new data.
     * @param gap if there was a gap in the buffer: if one or more elements were overwritten that we didn't read, it is
     * always updated.
     *
     * @return true if an element was read, false if the RoundBuffer is empty.
     *
     * @see popGap .
     */
    bool dequeue(Event<T> &event, bool &gap, int idx = -1);

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

    /**
     * Sets this RTEML_reader Buffer.
     *
     * Called during RTEML_reader configuration by the EventBuffer. lastread_ts is reset to zero, thus the same RTEML_reader can be reconfigured
     * to read from different buffers
     *
     * @param buffer a constant pointer to a constant Buffer to configure this RTEML_reader to.
     */
    void setBuffer(const CircularBuffer<T> *const buffer);


    size_t getLowerIdx() { return 0; }

    size_t getHigherIdx() { return buffer->getLength(); }

    typedef CircularBuffer<T> circular_buffer;
    typename circular_buffer::inftyBufferState getCurrentBufferState() {}
};

template<typename T>
RTEML_reader<T>::RTEML_reader() :
    index(1),
    lastread_ts(0)
{

}

template<typename T>
RTEML_reader<T>::RTEML_reader(const CircularBuffer<T> *const &bbuffer) :
    buffer(bbuffer),
    index(1),
    lastread_ts(0)
{

}

template<typename T>
bool RTEML_reader<T>::dequeue(Event<T> &event, bool &gap, int idx) {

    Event<T> tempEvent;
    size_t length;
    size_t n_elems_writer;

    // atomic operation block       >####
    uint32_t new_value;
    static uint32_t * dest = &n_elems_reader; // static is ensuring that anyone can modify it (stack could be optimized for others)
    
    ATOMIC_begin(new_value, 1+, dest);

        buffer->readEventFromIndex(tempEvent, (idx == -1) ? index : idx );  // unsafe in terms of empty buffer
        n_elems_writer = buffer->getElementsCount();
        length = buffer->getLength();

    ATOMIC_end(new_value, dest);
    // end of atomic operation block >####

    // continue processing ...

    // compare if is possible to consume elements
    if (n_elems_reader < n_elems_writer) 
    {
        // measure if the distance between reader and writer is greater than length (a gap)
        if (n_elems_writer-n_elems_reader >= (length-1))
        {
            gap = true;
            return false;
        }

        // perform dequeue of event

        if(idx == -1)
        {
            //increment the index for the next read
            if (++index >= length) index = 0;

            // update absolute time state of the monitor
            lastread_ts = lastread_ts + tempEvent.getTime();
        }


        
        event = tempEvent;

        gap = false;
        return true;
    }
    else
    {
        // abort reading
        n_elems_writer--;
        gap = false;
        return false;
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

template<typename T>
void RTEML_reader<T>::setBuffer(const CircularBuffer<T> *const bbuffer) {

    this->buffer = bbuffer;

    index = 1; // --- verify... why not 0 ?
    lastread_ts = 0;
}

#endif //_RTEML_READER_H_
