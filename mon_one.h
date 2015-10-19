
// monitor one

class Monitor_One : public Monitor {

private:
	SynchronizedEventReader<int> eventReader;

protected:
	void run(){
		Event<int> data;
		bool gap;
		if(eventReader.popHead(data, gap)) {
			printf("%d",data.getData());
			if(gap) {
				printf("oh no! a gap!");
			}
		}
	}

public:
	MonitorTestClass(IEventBuffer<int> &buffer): Monitor(1,0) {
		configSynchronizedEventReader<int>(eventReader, buffer);
	}

};