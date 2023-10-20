<project xmlns="com.autoesl.autopilot.project" name="simple_monitor" top="demo">
    <includePaths/>
    <libraryPaths/>
    <Simulation>
        <SimFlow name="csim" csimMode="0" lastCsimMode="0"/>
    </Simulation>
    <files xmlns="">
        <file name="../mon1.cpp" sc="0" tb="1" cflags=" -I../../../../rtmlib_0.2/src  -DUSE_DEBUG_RMTLD3 -DUSE_DEBUGV_RMTLD3 -DDEBUG=3 -Dx86 -D__HW__ -std=c++0x -Wno-unknown-pragmas" csimflags=" -Wno-unknown-pragmas" blackbox="false"/>
        <file name="simple_monitor/mon1.cpp" sc="0" tb="false" cflags="-I../../rtmlib_0.2/src -std=c++0x -D__HW__" csimflags="" blackbox="false"/>
    </files>
    <solutions xmlns="">
        <solution name="solution1" status="active"/>
    </solutions>
</project>

