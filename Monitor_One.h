#ifndef MONITOR_MONITOR_ONE_H
#define MONITOR_MONITOR_ONE_H

#include "Monitor.h"

class Monitor_One : public Monitor {

private:
	RTEML_reader<int> _reader;

protected:
	void run(){
		

	}

public:
	Monitor_One(IEventBuffer<int> &buffer, useconds_t p): Monitor(p) {
		configReader<int>(_reader, buffer);
	}

};

#endif //MONITOR_MONITOR_ONE_H
