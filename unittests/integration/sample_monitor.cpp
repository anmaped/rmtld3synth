
// SAMPLE (filename sample_monitor.cpp)
// compile sample_monitor.cpp and sample_instrumentation.cpp
// gcc -I../rtmlib2/src/ sample_monitor.cpp sample_instrumentation.cpp -o test
// -latomic -fno-rtti -fno-exceptions -DRTMLIB_ENABLE_DEBUG_RMTLD3

#include "Rtm_monitor_3a50.h"

RTML_BUFFER0_SETUP(); // defines the buffer here

int sample() {
  RTML_BUFFER0_TRIGGER_PERIODIC_MONITORS(); // triggers the thread to run the
                                            // monitor

  while (true) {
    // do something to wait for threads
  }
}
