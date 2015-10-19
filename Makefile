


monitor-lib:
	arm-none-eabi-g++ -nostdinc -IC:\ardupilot_pixhawk_testcase\ardupilot\modules\PX4NuttX\nuttx\include -DUNICORE_PROCESSOR -DCONFIG_WCHAR_BUILTIN -c ArraySeqLock.cpp Monitor.cpp