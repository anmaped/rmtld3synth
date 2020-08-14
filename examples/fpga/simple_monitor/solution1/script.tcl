############################################################
## This file is generated automatically by Vivado HLS.
## Please DO NOT edit it.
## Copyright (C) 1986-2020 Xilinx, Inc. All Rights Reserved.
############################################################
open_project simple_monitor
set_top demo
add_files simple_monitor/mon1.cpp -cflags "-I../../rtmlib_0.2 -std=c++0x -D__HW__"
add_files -tb simple_monitor/mon1.cpp -cflags "-I../../rtmlib_0.2 -DUSE_DEBUG_RMTLD3 -DUSE_DEBUGV_RMTLD3 -DDEBUG=3 -Dx86 -D__HW__ -std=c++0x -Wno-unknown-pragmas" -csimflags "-Wno-unknown-pragmas"
open_solution "solution1"
set_part {xc7a200t-fbg484-3}
create_clock -period 5 -name default
config_sdx -target none
config_export -vivado_optimization_level 2 -vivado_phys_opt place -vivado_report_level 0
set_clock_uncertainty 12.5%
#source "./simple_monitor/solution1/directives.tcl"
csim_design
csynth_design
cosim_design
export_design -format ip_catalog
