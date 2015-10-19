#ifndef MONITOR_MONITOR_ONE_H
#define MONITOR_MONITOR_ONE_H

#include "Monitor.h"

class Monitor_One : public Monitor {

private:
	EventReader<int> eventReader;

protected:
	void run(){
		

	}

public:
	Monitor_One(IEventBuffer<int> &buffer, useconds_t p): Monitor(p) {
		configReader<int>(eventReader, buffer);
	}

};

#endif //MONITOR_MONITOR_ONE_H
