#ifndef MONITOR_IEVENTREADER_H
#define MONITOR_IEVENTREADER_H

#include "Event.h"

/**
 * IEventReader declares an interface to read Events from EventBuffers. This allows clients to decouple from a specific
 * implementation of a reader.
 *
 * This class defines the specific operations EventReaders must implement.
 *
 * The current implementations of this interface are the EventReader and the SynchronizedEventReader.
 *
 * @see EventReader
 * @see SynchronizedEventReader
 * @see EventBuffer
 *
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<class T>
class IEventReader {
public:
    /**
     * Dequeue the next Event from the EventBuffer.
     *
     * @param event a reference to an object where the next event is copied to, the event is marked as read.
     * @param gap true if there was a gap in the trace, that is if elements have been overwritten on the buffer without
     * this IEventReader reading them.
     *
     * @return if an element was popped, false if the RoundBuffer is empty.
     */
    virtual bool dequeue(Event<T> &event, bool &gap) = 0;
    
};

#endif //MONITOR_IEVENTREADER_H
