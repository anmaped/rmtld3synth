#!/bin/sh

cppgen="
#include <unistd.h>
#include <list>
#include <string>
#include <unordered_map>

#include <circularbuffer.h>
#include <reader.h>
#include <rmtld3/reader.h>
#include <rmtld3/rmtld3.h>


int count_until_iterations;

int wait_time=1000000;

typedef Event<int> event_t;
typedef RTML_buffer<event_t, 1000> buffer_t;
typedef RMTLD3_reader<RTML_reader<buffer_t>, int> trace_t;

"
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	available_files=$(ls "$TEST_DIR/cpp/mon$i")

	namespace="namespace Test$i {\n"

	for j in $available_files
	do
		namespace+="#include \"cpp/mon$i/$j\"\n"
		break # just includes the first file
	done

	namespace+="}\n"
	
	cppgen+="$(echo -e "$namespace")
void test$i() {

	std::list<std::pair<std::string,int>> trc =  { $( echo "$(dos2unix $TEST_DIR/cpp/res$i.trace; cat $TEST_DIR/cpp/res$i.trace)" | sed -e "s/)/}/g" -e "s/(/{/g" -e "s/;/,/g" ) };
	
	buffer_t buf;

	timespan delay_time = 0;
	for (auto it = trc.begin(); it != trc.end(); ++it) {
		DEBUGV_RMTLD3(\"%d %s\n\",
			Test$i::_mapsorttostring[(*it).first.c_str()],
			(*it).first.c_str()
		);
		buf.push ( event_t(Test$i::_mapsorttostring[(*it).first], delay_time ) );
		delay_time += (timespan) (*it).second;
	}

	int tzero = 0.;
	trace_t trace = trace_t(buf, tzero);

	trace.synchronize();

    buf.debug();

	timespan t = 0;
	three_valued_type _out = Test$i::_rtm_compute_$(echo ${j} | cut -c 13- | rev | cut -c 3- | rev)_0<trace_t>(trace,t);
	auto _out_readable = (_out == T_TRUE) ? \"\x1b[32m[true]\x1b[0m\" : ((_out == T_FALSE) ? \"\x1b[31m[false]\x1b[0m\" : \"\x1b[33m[unknown]\x1b[0m\" );
	DEBUG_RMTLD3(\"$i) %s\n\", _out_readable );
}
"
done

cppgen+="
int main()
{
"

for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
cppgen+="
	test$i();"
done

cppgen+="
	return 0;
}
"

echo "$cppgen" > $TEST_DIR/cpptest.cpp

# int main()
# {
# 	::printf(\"Test\n\");
# 	//__buffer_mon1.debug();

# 	Mon0 mon_mon0(wait_time,SCHED_OTHER,50);

# 	// add trace

# 	if (mon_mon0.enable()) {::printf(\"ERROR\n\");}

# 	mon_mon0.disable();

# 	sleep(1);

# 	return 0;
# }