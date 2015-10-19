#ifndef _MONITOR_EVENTBUFFER_H_
#define _MONITOR_EVENTBUFFER_H_

#include <stdio.h>
#include <time.h>

#include "CircularBuffer.h"

#include "EventWriter.h"
#include "EventReader.h"
//#include "SynchronizedEventReader.h"
#include "IEventBuffer.h"

/**
 * EventBuffers store Events of a template type T, these events can be written in O(1) time and read asynchronously without
 * using dynamic memory.
 *
 * The main data structure behind the EventBuffer is a circular array. Events are read by EventReaders but they are never
 * deleted from the buffer, only overwritten as the buffer fills up. All readers subscribed to the buffers will read every
 * event written to it, assuming they are preempted enough to read them all. Thus readers must be preempted sufficient times such
 * that events are read at an equal or faster pace than they are written.
 *
 * To ensure data integrity, writing and reading operations are encapsulated in two different objects. Thus it becomes
 * possible to guarantee that readers cannot accidentally write into a buffer.
 *
 * Reading operations are accomplished by two classes, both are child of the IEventReader Interface.The readers are meant
 * to be used in a monitor, which periodically wakes up to check the monitored application code is functioning correctly.
 *  - EventReader: reads events in a FIFO order.
 *  - SynchronizedEventReader: reads events events that are larger than a synchronization timestamp variable, shared
 *  between different buffers in FIFO order; this event reader is meant to be used inside a Monitor.
 *
 * Writing operations are done by a single EventWriter class, where all operations take constant O(1) time. Thus writing
 * will always take the same time. This makes the timing analysis of the application deterministic and not dependent on
 * how many readers are present in the buffer, full timing isolation for the writer is achieved.
 *
 * The class also uses a template parameter to define its size, thus avoiding the use of dynamic memory with a clean
 * design. However this template parameter also has the disadvantage of burdening the user with the exact same size
 * declaration whenever he wants to use it. To avoid this a superclass name IEventBuffer was created.
 *
 * To avoid cyclic dependency where the EventBuffer needs to know readers and writers, and those need to know the EventBuffer,
 * a Buffer class was created. The buffer class contains the data readers and writers need and also defines some common
 * operations such as atomic writing elements to the buffer, or atomically reading an index.
 *
 * @see Event
 * @see IEventBuffer
 * @see IEventReader
 * @see EventReader
 * @see SynchronizedEventReader
 * @see Monitor
 *
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<typename T, size_t N>
class EventBuffer : public IEventBuffer<T> {
private:
    /** The Event array where events are kept. Size is defined via template parameter N, voiding the use of dynamic memory.
     * @see Event
     */
    Event<T> array[N];

    /** The buffer data holder object which will be used by eventReaders, synchronizedEvent readers and writers
     * to perform operations on the buffer.
     * @see Buffer
     */
    CircularBuffer<T> buffer;

    /**
     * The writer flag that indicates if a writer has been created for this EventBuffer.
     */
    bool writer;

    //EventBuffer<T, N> &operator=(const EventBuffer<T, N> &eventBuffer);

public:
    /**
     * Instantiates a new EventBuffer.
     *
     * The array timestamp values are zeroed out.
     */
    EventBuffer();

    /**
     * Configures an EventWriter for this EventBuffer.
     *
     * Configures an EventWriter to write to this buffer, only one EventWriter for each Buffer may exist, so a check
     * that a writer has not been configured before is done. A writer is only created if no writer has been created
     * before.
     *
     * @param eventWriter an EventWriter to configure.
     * @return true if the writer was configured.
     *
     * @see EventWriter
     */
    bool configWriter(EventWriter<T> &eventWriter);

    /**
     * Configures a new EventReader for this buffer.
     *
     * @param eventReader the EventReader to configure.
     *
     * @see EventReader
     */
    void configReader(EventReader<T> &eventReader) const;

    /**
     * Configures a new SynchronizedEventReader with afterTime as a synchronization variable.
     *
     * @param afterTime the synchronization variable to use with this SynchronizedEventReader. This variable is used to
     * synchronize several SynchronizedEventReaders so that each one pops elements that are newer than afterTime.
     * @param eventReader the SynchronizedEventReader to configure.
     *
     * @see SynchronizedEventReader
     * @see Monitor
     */
    //void configSynchronizedEventReader(nclock_t *const &afterTime,
    //                                   SynchronizedEventReader<T> &eventReader) const;

    /**
     * Gets the EventBuffers length.
     *
     * Get length returns template parameter N, thus it is very efficient as the compiler should inline N.
     *
     * @return the EventBuffer length.
     */
    size_t getLength() const;

    void debug() const;
    /**
     * Checks if the buffer is empty.
     *
     * @return true if the buffer is empty.
     */
    //bool isEmpty() const;
};

template<typename T, size_t N>
EventBuffer<T, N>::EventBuffer() :
    buffer(array, N),
    writer(false)
{

}

template<typename T, size_t N>
bool EventBuffer<T, N>::configWriter(EventWriter<T> &eventWriter)
{
    if (!writer) {
        writer = true;
        eventWriter.setBuffer(&buffer);
        return true;
    } else
        return false;
}

template<typename T, size_t N>
void EventBuffer<T, N>::configReader(EventReader<T> &eventReader) const {
    eventReader.setBuffer(&buffer);
}

/*template<typename T, size_t N>
void EventBuffer<T, N>::configSynchronizedEventReader(nclock_t *const &afterTime,
                                                      SynchronizedEventReader<T> &synchronizedEventReader) const {
    synchronizedEventReader.setAfterTime(afterTime);
    synchronizedEventReader.setBuffer(&buffer);
}*/

template<typename T, size_t N>
size_t EventBuffer<T, N>::getLength() const {
    return N;
}

template<typename T, size_t N>
void EventBuffer<T, N>::debug() const
{
    for (unsigned int idx=0; idx < N; idx++)
        ::printf("%lu,%d; ", array[idx].getTime(), array[idx].getData());
}

/*template<typename T, size_t N>
bool EventBuffer<T, N>::isEmpty() const {
    return buffer.isEmpty();
}*/

#endif //_MONITOR_EVENTBUFFER_H_
