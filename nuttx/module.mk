#
# rtemlib module for NuttX OS
# 

MODULE_COMMAND = rtemlib
SRCS = main.cpp
MODULE_STACKSIZE = 4096
EXTRACXXFLAGS = -Wframe-larger-than=1200 -DCONFIG_WCHAR_BUILTIN -I$(SKETCHBOOK)/../rteml/arch/arm/include -DARM_CM4_FP -D__NUTTX__ -std=c++0x --verbose
