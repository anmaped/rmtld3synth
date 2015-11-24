#ifndef _RTEML_WRITER_H_
#define _RTEML_WRITER_H_

#include "Event.h"
#include "CircularBuffer.h"

/**
 * Writes events to a RTEML_buffer.
 *
 * @see RTEML_buffer
 *
 * @author André Pedro (anmap@isep.ipp.pt)
 * @author Humberto Carvalho (1120409@isep.ipp.pt)
 * @date
 */
template<typename T>
class RTEML_writer {
private:
    /**
     * Pointer to a circular buffer this RTEML_writer writes to.
     * @see RTEML_buffer
     */
    CircularBuffer<T> *buffer;

public:
    /**
     * Instantiates a new RTEML_writer.
     *
     * The event writer is blank and should only be configured by calling configWriter on an RTEML_buffer.
     */
    RTEML_writer();

    /**
     * Instantiates a new RTEML_writer.
     *
     * @param buffer the Buffer to write to.
     */
    RTEML_writer(CircularBuffer<T> *buffer);

    /**
    * enqueues an event to the Buffer.
    *
    * @param data a constant reference to the data to be pushed.
    */
    void enqueue(const T &data);

    /**
    * Sets this RTEML_writer Buffer.
    *
    * Called during RTEML_writer configuration by the RTEML_buffer.
    *
    * @param buffer the buffer to configure this EventReader to.
    */
    void setBuffer(CircularBuffer<T> *buffer);
};

template<typename T>
RTEML_writer<T>::RTEML_writer() : buffer(NULL)
{

}

template<typename T>
RTEML_writer<T>::RTEML_writer(CircularBuffer<T> *bbuffer) : buffer(bbuffer)
{

}

template<typename T>
void RTEML_writer<T>::enqueue(const T &data) {
    buffer->enqueue(data);
}

template<typename T>
void RTEML_writer<T>::setBuffer(CircularBuffer<T> *bbuffer) {
    this->buffer = bbuffer;
}

#endif //_RTEML_WRITER_H_
