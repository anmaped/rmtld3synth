#ifndef _MONITOR_EVENTWRITER_H_
#define _MONITOR_EVENTWRITER_H_

#include "Event.h"
#include "CircularBuffer.h"

/**
 * Writes events to an EventBuffer.
 *
 * EventWriters are instantiated by EventBuffers.
 *
 * @see EventBuffer
 *
 * @author Humberto Carvalho (1120409@isep.ipp.pt)
 * @date
 */
template<typename T>
class EventWriter {
private:
    /**  Pointer to a circular Buffer this EventWriter writes to.
     * @see Buffer
     */
    CircularBuffer<T> *buffer;

public:
    /**
     * Instantiates a new EventWriter.
     *
     * The event writer is blank and should only be configured by calling configWriter on an EventBuffer.
     */
    EventWriter();

    /**
     * Instantiates a new EventWriter.
     *
     * @param buffer the Buffer to write to.
     *
     * Instantiates a new EventWriter to write to Buffer buffer.
     */
    EventWriter(CircularBuffer<T> *buffer);

    /**
    * enqueues an event to the Buffer.
    *
    * Pushes events to the buffer in O(1) time.
    *
    * @param data a constant reference to the data to be pushed.
    */
    void enqueue(const T &data);

    /**
    * Sets this EventWriter Buffer.
    *
    * Called during EventWriter configuration by the EventBuffer.
    *
    * @param buffer the Buffer to configure this EventReader to.
    */
    void setBuffer(CircularBuffer<T> *buffer);
};

template<typename T>
EventWriter<T>::EventWriter() : buffer(NULL)
{

}

template<typename T>
EventWriter<T>::EventWriter(CircularBuffer<T> *bbuffer) : buffer(bbuffer)
{

}

template<typename T>
void EventWriter<T>::enqueue(const T &data) {
    buffer->enqueue(data);
}

template<typename T>
void EventWriter<T>::setBuffer(CircularBuffer<T> *bbuffer) {
    this->buffer = bbuffer;
}

#endif //_MONITOR_EVENTWRITER_H_
