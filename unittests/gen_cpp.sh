#!/bin/sh

cppgen="
#include <unistd.h>
#include <list>
#include <string>
#include <unordered_map>

#include \"RTML_buffer.h\"
#include \"RTML_monitor.h\"
#include \"rmtld3/reader.h\"
#include \"rmtld3/rmtld3.h\"


int count_until_iterations;

int wait_time=1000000;

RTML_buffer<int, 300> __buffer_mon1 __attribute__((used));

"
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	cppgen+="
namespace Test$i {
#include \"cpp/mon$i/mon0_compute.h\"
}

void test$i() {
	std::list<std::pair<std::string,int>> trc =  { $( echo "$(dos2unix $TEST_DIR/cpp/res$i.trace; cat $TEST_DIR/cpp/res$i.trace)" | sed -e "s/)/}/g" -e "s/(/{/g" -e "s/;/,/g" ) };
	std::list<std::pair<int,timespan>> enc_trc;

	timespan delay_time = 0;
	for (auto it = trc.begin(); it != trc.end(); ++it) {
		delay_time = (timespan) (*it).second;
		DEBUGV_RMTLD3(\"%d %s\n\",Test$i::_mapsorttostring[(*it).first.c_str()],(*it).first.c_str());
		enc_trc.push_back ( std::make_pair (Test$i::_mapsorttostring[(*it).first], delay_time ) );
	}

	RTML_writer< int > __writer = RTML_writer< int >( __buffer_mon1.getBuffer() );
    __writer.unsafe_enqueue_n(enc_trc);

    __buffer_mon1.debug();

    RMTLD3_reader< int, Event < int > > __reader = RMTLD3_reader< int, Event < int > >( __buffer_mon1.getBuffer(), 10. );
	Test$i::environment env = Environment< int, Event < int > >(std::make_pair (0, 0), &__reader, __observation< int, Event < int > >);
	three_valued_type _out = Test$i::_mon0_compute(env,0);
	auto _out_readable = (_out == T_TRUE) ? \"\x1b[32m[true]\x1b[0m\" : ((_out == T_FALSE) ? \"\x1b[31m[false]\x1b[0m\" : \"\x1b[33m[unknown]\x1b[0m\" );
	DEBUG_RTMLD3(\"$i) %s\n\", _out_readable );
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