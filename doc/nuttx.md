## NuttX Sample Monitor

First, retrieve the last source files from Ardupilot using the command

```
git clone --recurse-submodules https://github.com/ArduPilot/ardupilot.git
```
It will take a while. Then, apply the patches to include rtmlib in NuttX using the commands, as follows:
```
cd ardupilot
mkdir modules/rtmlib
git clone https://github.com/anmaped/rtmlib.git modules/rtmlib/.
patch -p1 < modules/rtmlib/nuttx/add_rtmlib_module.patch
```

Replace the variables inside `module.mk` according to the correct paths
```
MONITOR_SOURCE_FILES /* name of the source files to include  */
MONITOR_FOLDER /* folder where the monitor sources are available */
MONITOR_TEST_CASES_FOLDER /* subfolder where the unit tests are available */
```

Type `make` for starting building ardupilot and use `px4-v2` flag to compile ardupilot for pixhawk.
```
cd ArduCopter
make px4-v2
```

For ArduPlane it should be similar. Use directory `ArduPlane` instead.
