#ifndef MONITOR_IEVENTBUFFER_H
#define MONITOR_IEVENTBUFFER_H

#include "RTEML_writer.h"
#include "RTEML_reader.h"

/**
 * IEventBuffer declares an interface for the EventBuffer class.
 *
 * The main reason this class exists is to decouple readers and writers from having to use the exact template parameter
 * N when passing the EventBuffer, for example if one declared the following:
 *
 * \code
 *  EventBuffer<int, 5>
 * \endcode
 *
 * He would have to use the exact same template size_n parameter 5 everywhere in the code. If the author wanted to
 * change the EventBuffer size he would have to change every declaration in the code.
 *
 * Thus authors using the class should always use the IEventBuffer interface where possible.
 *
 * @see EventBuffer
 *
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<typename T>
class IEventBuffer {
public:

    /**
     * Gets the EventBuffers length.
     *
     * Get length returns template parameter N, thus it is very efficient as the compiler should inline N.
     *
     * @return the EventBuffer length.
     */
    virtual size_t getLength() const = 0;

};


#endif //MONITOR_IEVENTBUFFER_H
