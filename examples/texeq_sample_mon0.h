/*
 * automatically generated code from rmtld3synth Git version v0.2-alpha-46-g38ff318 (38ff318181783ef464b346f42a37ef72d08340a8)
 * cmd: ./rmtld3synth --synth-cpp11 --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} (c \lor d) < 4"
 */

#ifndef MONITOR_MON0_H
#define MONITOR_MON0_H

#include "Rmtld3_reader.h"
#include "RTML_monitor.h"

#include "mon0_compute.h"
#include "mon1.h"

class Mon0 : public RTML_monitor {

private:
RMTLD3_reader< int > trace = RMTLD3_reader< int >( __buffer_mon1.getBuffer(), 10. );

struct Environment env;

protected:
void run(){

  three_valued_type _out = _mon0_compute(env,0);
  DEBUG_RTEMLD3("Veredict:%d\n", _out);
}

public:
Mon0(useconds_t p): RTML_monitor(p,SCHED_FIFO,50), env(std::make_pair (0, 0), &trace, __observation) {}

};

#endif //MONITOR_MON0_H
