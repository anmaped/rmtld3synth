#!/bin/sh

cppgen=""
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	cppgen+="
#include \"gtests/cpp/mon$i/Mon0.h\"

/*module T$i : Trace = struct let trc = [$(dos2unix gtests/res$i.trace; cat gtests/res$i.trace)] end;;
module M$i = Res$i.Mon0(T$i);;
if M$i.mon = Rmtld3.True then print_endline (\"\x1b[32m[true]\x1b[0m\") else (if M$i.mon = Rmtld3.False then print_endline (\"\x1b[31m[false]\x1b[0m\") else print_endline (\"\x1b[33m[unknown]\x1b[0m\"))
*/
"
done

cppgen+="
#include <unistd.h>
#include \"RTML_buffer.h\"

int count_until_iterations;

int wait_time=1000000;

RTML_buffer<int, 100> __buffer_mon1 __attribute__((used));

int main()
{
	::printf(\"Test\n\");
	//__buffer_mon1.debug();

	Mon0 mon_mon0(wait_time,SCHED_OTHER,50);

	// add trace

	if (mon_mon0.enable()) {::printf(\"ERROR\n\");}

	mon_mon0.disable();

	sleep(1);

	return 0;
}
"

echo "$cppgen" > cpptest.cpp
