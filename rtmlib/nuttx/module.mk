#
# rtemlib module for NuttX OS
#

# editable settings...
MONITOR_SOURCE_FILES = ../synth_tool/monitor_set1/monitor_set1.cpp
MONITOR_FOLDER = $(SKETCHBOOK)/modules/rtemlib/synth_tool/monitor_set1
MONITOR_TEST_CASES_FOLDER = $(SKETCHBOOK)/modules/rtemlib/synth_tool/monitor_set1/tests

MODULE_COMMAND = rtemlib
SRCS = libatomic.c main.cpp ../RTEML_monitor.cpp $(MONITOR_SOURCE_FILES)
MODULE_STACKSIZE = 4096
EXTRACXXFLAGS = -Wframe-larger-than=1200 -DCONFIG_WCHAR_BUILTIN -I$(SKETCHBOOK)/modules/rtemlib -I$(SKETCHBOOK)/modules/rtemlib/arch/arm/include -I$(MONITOR_FOLDER) -I$(MONITOR_TEST_CASES_FOLDER) -DARM_CM4_FP -D__NUTTX__ -std=c++0x --verbose -w
