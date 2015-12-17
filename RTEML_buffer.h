#ifndef _RTML_BUFFER_H_
#define _RTML_BUFFER_H_

#include <stdio.h>
#include <time.h>

#include "CircularBuffer.h"
#include "IEventBuffer.h"
#include "RTEML_writer.h"
#include "RTEML_reader.h"

/**
 * RTEML_buffer allows instrumented applications and monitors to share the
 * the same buffer by splinting the read and write operations. Monitor
 * uses RTEML_reader, and RTEML_writer is used for software modules under
 * observation.
 *
 * @see Event
 * @see IEventBuffer
 * @see IEventReader
 * @see RTEML_reader
 * @see RTEML_monitor
 *
 * @author André Pedro (anmap@isep.ipp.pt)
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<typename T, size_t N>
class RTEML_buffer : public IEventBuffer<T> {
private:
    /**
     * The Event array where events are kept. Size is defined via template
     * parameter N, avoiding dynamic memory usage.
     * @see Event
     */
    typedef CircularBuffer<T> cb;
    typename cb::node array[N];

    /**
     * The infinite buffer that is used for readers and writers of the RTEML.
     * @see CircularBuffer
     */
    CircularBuffer<T> buffer;

    /**
     * The writer flag that indicates if a writer has been attached.
     */
    bool writer;

public:
    /**
     * Instantiates a new RTEML_buffer.
     */
    RTEML_buffer();

    /**
     * Gets the static length of the buffer.
     *
     * @return the template parameter N.
     */
    size_t getLength() const;

    CircularBuffer<T> *const getBuffer() const;

    /**
     * Debugs the infinite buffer into the stdout
     */
    void debug() const;
};

template<typename T, size_t N>
RTEML_buffer<T, N>::RTEML_buffer() :
    buffer(array, N),
    writer(false)
{

}

template<typename T, size_t N>
size_t RTEML_buffer<T, N>::getLength() const {
    return N;
}

template<typename T, size_t N>
CircularBuffer<T> *const RTEML_buffer<T, N>::getBuffer() const {
    return (CircularBuffer<T> *const)&buffer;
}

template<typename T, size_t N>
void RTEML_buffer<T, N>::debug() const
{
    for (unsigned int idx=0; idx < N; idx++)
        ::printf("%lu,%d; ", array[idx].ev.getTime(), array[idx].ev.getData());
}

#endif //_RTML_BUFFER_H_
