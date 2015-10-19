#ifndef _MONITOR_EVENTREADER_H_
#define _MONITOR_EVENTREADER_H_

#include <time.h>

#include "CircularBuffer.h"
#include "IEventReader.h"


/**
 * Reads events from an EventBuffer.
 *
 * Event Reader reads Events from an EventBuffer FIFO fashion. EventReader does what it can when elements
 * are written faster than it can read. When overflows occurs EventReader tries to find the oldest element it has yet to
 * read and notifies the user of the overflow.
 *
 * EventReaders are instantiated or configured by EventBuffers.
 *
 * @see EventBuffer
 *
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<typename T>
class EventReader : public IEventReader<T> {
private:
    /**  Constant pointer to a constant circular Buffer this EventReader performs atomic read operations from.
     * @see Buffer
     */
    const CircularBuffer<T> *buffer;

    /** Pointer to a constant event which is the current index on this EventReader. */
    size_t index;

    /** Number of readed elements */
    size_t n_elems_reader;

    /** The timestamp of the last event this buffer read.
     * @see time.h
     */
    timeabs lastread_ts;

public:
    /**
     * Instantiates a new EventReader.
     *
     * The event reader is blank and should only be configured by calling getEventReader on an EventBuffer.
     */
    EventReader();

    /**
     * Instantiates a new EventReader.
     *
     * Instantiates a new event reader that reads from buffer.
     *
     * @param buffer a constant pointer that points to a constant buffer.
     */
    EventReader(const CircularBuffer<T> *const &buffer);

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
    bool dequeue(Event<T> &event, bool &gap);

    bool dequeueArray(Event<T> * event, bool &isConsitent);

    /**
     * Synchronize the eventReader index according to a timestamp
     *
     * @param time defines the timestamp to syncronize.
     *
     * @return true if the eventReader was synchronized, false otherwise.
     */
    bool synchronize(timeabs time);

    /**
     * Compares the current eventReader absolute timestamp with the
     * current absolute timestamp of the buffer.
     *
     * @return true if matched, false otherwise.
     */
    bool consitencyCheck() const;

    /**
     * Sets this EventReader Buffer.
     *
     * Called during EventReader configuration by the EventBuffer. lastread_ts is reset to zero, thus the same EventReader can be reconfigured
     * to read from different buffers
     *
     * @param buffer a constant pointer to a constant Buffer to configure this EventReader to.
     */
    void setBuffer(const CircularBuffer<T> *const buffer);
};

template<typename T>
EventReader<T>::EventReader() :
    index(1),
    lastread_ts(0)
{

}

template<typename T>
EventReader<T>::EventReader(const CircularBuffer<T> *const &bbuffer) :
    buffer(bbuffer),
    index(1),
    lastread_ts(0)
{

}

template<typename T>
bool EventReader<T>::dequeue(Event<T> &event, bool &gap) {

    Event<T> tempEvent;
    size_t length;
    size_t n_elems_writer;

    // atomic operation block       >####
    uint32_t new_value;
    static uint32_t * dest = &n_elems_reader; // static is ensuring that anyone can modify it (stack could be optimized for others)
    
    ATOMIC_begin(new_value, 1+, dest);

        buffer->readEventFromIndex(tempEvent, index);  // unsafe in terms of empty buffer
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

        //increment the index for the next read
        if (++index >= length) index = 0;

        // update absolute time state of the monitor
        lastread_ts = lastread_ts + tempEvent.getTime();
        
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
bool EventReader<T>::dequeueArray(Event<T> * event, bool &isConsitent) {
    // [todo]
    return false;
}

template<typename T>
bool EventReader<T>::synchronize(timeabs time)
{
    // [todo]
    return false;
}

template<typename T>
bool EventReader<T>::consitencyCheck() const
{
    // [todo]
    return false;
}

template<typename T>
void EventReader<T>::setBuffer(const CircularBuffer<T> *const bbuffer) {

    this->buffer = bbuffer;

    index = 1; // --- verify... why not 0 ?
    lastread_ts = 0;
}

#endif //_MONITOR_EVENTREADER_H_
