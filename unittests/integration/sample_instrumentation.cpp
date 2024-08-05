
// SAMPLE (filename sample_instrumentation.cpp)
// gcc -I../rtmlib2/src/ sample_instrumentation.cpp -o test -latomic
// will ask for __buffer_rtm_monitor_3a50 at linkage step

#include "Rtm_instrument_3a50.h"

extern int sample(); // the external function to construct the sample monitor

int main() {
  using Writer = Writer_rtm__3a50;

  // using the new name
  Writer w;

  // push symbol 'a' with timestamp 0 seconds
  // Note that all propositions are defined inside the writer.
  // There is no way to send other symbols that are not defined there.
  w.push(Writer::a, Writer::time_of_s(0));

  sample(); // run the sample monitor
}
