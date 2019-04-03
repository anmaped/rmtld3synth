
rmtld3synth toolchain: Synthesis from a fragment of MTL with durations
===========================================

The rmtld3synth toolchain is capable of automatically generating cpp11/ocaml monitors and specifications in SMT-LIB v2 standard language from a fragment of metric temporal logic with durations. This logic is a three-valued extension over a restricted fragment of metric temporal logic with durations that allows us to reason about explicit time and temporal order of durations.

Supported by this formalism are polynomial inequalities (using the less relation <) and the common modal operators of temporal logics U (until) and S (since). 
The existential quantification over these formulas is also possible through the adoption of the cylindrical algebraic decomposition (CAD) method. This method is suitable for converting quantified formulas under various decomposed conditions without these quantifiers.

To check formula satisfiability, the tool is ready to synthesize the logic fragment in the input language accepted by the z3 SMT solver or any other prover supporting AUFNIRA logic and SMT-LIB v2. It can be used to discard various constraints involving duration and temporal order of the propositions.
For example, schedulability analysis of hard real-time systems is possible by specifying the complete problem in RMTLD3. First using rmtld3synth to synthesize the problem in SMT-LIB and then using Z3 to solve it. The idea is to know if there exists a trace for which the RMTLD3 problem is satisfiable, or whether the SMT gives us an unsatisfiable answer meaning that is impossible to schedule such a configuration. The latter reinforces the refinement by drawing a counterexample.

# Contents

- Usage options:
  - [Online demonstrator using js_of_ocaml](#online-demonstrator-using-js_of_ocaml)
  - [Tarball binaries for Windows](#tarball-binaries-for-windows)
  - [Building from Git](#building-from-git)
- [Documentation](#documentation)
- [License](#license)

### Get started with the online demonstrator
:camel: [Try it](https://anmaped.github.io/rmtld3synth).

### Tarball binaries for Windows

The current release version is [0.3-alpha2](../../releases/download/v0.3-alpha2/release-v0.3-alpha2-windows64.zip?raw=true).

Monitor generation can be done using rmtld3synth through command line arguments. To ilustrate how, we consider the use case one [available here](http://rawgit.com/cistergit/rmtld3synth/master/doc/usecase1.html) and the configuration file [`usecaseone`](/config/usecaseone?raw=true) that contains the input formula and some detailed settings to perform the synthesis process. To generate the monitor(s), we have to use the following shell command
```
./rmtld3synth.exe --config-file usecaseone
```
The `--config-file` argument instructs the rmtld3synth to use the configuration file `usecaseone`.

After executing the command, the `monitor_set1` folder is available and contains the generated source files of the monitor(s) specified for the `usecaseone` configuration.
At this point, we have the monitor ready to be supplied for GCC, LLVM or other "compatible" C/C++ compiler and deployed in the chosen target architecture. In the current version, C++ and Ocaml synthesis are fully supported but Spark2014 is not yet available.

More details on instrumenting the monitors can be found in the [Documentation](#documentation) section, including the description for integrating the monitors in bare metal platforms using NuttX and FreeRTOS real-time operating systems.

### Building from Git
[![Build Status](https://travis-ci.org/anmaped/rmtld3synth.svg?branch=master)](https://travis-ci.org/anmaped/rmtld3synth)

#### To compile rmtld3synth for Linux and OS X using Opam and Ocaml >= 4.03.0
Just pin the rmtld3synth as an opam package and switch the opam compiler to a version `>= 4.03.0`. All dependencies should be installed automatically.

```
opam pin add rmtld3synth https://github.com/anmaped/rmtld3synth.git
```

Use the commands below to compile z3 solver and respective ML bindings.
```
git clone https://github.com/Z3Prover/z3.git z3
cd z3
python scripts/mk_make.py --ml
cd build; make
sudo make install
# or just 
opam install z3 -v
```
Make sure that you have at least python 2.2.7 and g++-5 installed.


#### To compile rmtld3synth for Windows using ocaml >= 4.03.0
Get the [Andreas Hauptmann's installer](https://fdopen.github.io/opam-repository-mingw/installation/) and switch the opam compiler to the version `4.03.0`.
```
opam switch 4.03.0+mingw64
eval `opam config env`
```
In case you have not properlly installed the flexdll, download the new flexdll [here](http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip), and decompress the archive in the current directory (PWD) with folder name `flexdll-bin-0.35`.
```
export PATH=$(PWD)/flexdll-bin-0.35:$PATH
```
Then, you have to pin the rmtld3synth package as described on `compile rmtld3synth for Linux and OS X ` section to conclude the compilation.

:grey_exclamation:HINTS!!

To compile z3 using cygwin you should use the mingw32 or mingw64 tools, otherwise it will not work properlly.
If the correct version of GCC is not found when executing `mk_make.py`, modify the flags `CXX` and `AR` as needed. For instance,
```
CXX=x86_64-w64-mingw32-g++ CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar python scripts/mk_make.py --ml
```

Ensure also that libz3 is properly installed at the current environment.
If the libz3 is not found then use the copy command to copy the `z3/build/libz3.dll.a` library to the `/lib` or `/home/current-user/.opam/compiler-version/lib` directory.


#### Instructions to compile rtmlib

Current version is [0.1-alpha](../../releases/download/v0.3-alpha/rtmlib-0.1.zip?raw=true).

The rtmlib is a support library for embedding runtime monitors written in CPP11. We can skip this compilation step if we do not need to integrate the monitors into the target system or we just want to synthesize RMTLD3 specifications in SMT-Libv2.
Use the `make` command to perform the compilation of the library as usual. The outcome shall be the library file `librtml.a`. Please ensure that you have the gcc 4.7.0 or greater with c++0x standard flag enabled. Proper files to support atomics are provided in the GIT repository and do not need to be added afterward (only for gcc 4.7.0 version).

More details are available in the [rtmlib documentation repository](https://anmaped.github.io/rtmlib/doc/).



## Documentation

- [rmtld3synth's CLI](doc/general.md#overview-of-the-command-line-interface-of-rmtld3synth)
  - [The Configuration File](doc/general.md#overview-of-the-configuration-file)
- [Write Formulas in RMTLD3](doc/general.md#write-formulas-in-rmtld3)
- [Import Formulas from Latex Equations](doc/general.md#write-formulas-in-latex-and-know-how-to-use-them)
- [Compile generated Monitors](doc/general.md#compile-the-generated-monitors)
- [Map Event Symbols](doc/general.md#map-event-symbols)
- [Integrate Monitors using rtmlib in Bare Metal Platforms](doc/general.md#integrate-monitors-using-rtmlib-in-a-bare-metal-platform)
  - [NuttX](doc/general.md#nuttx-os)
  - [FreeRTOS](doc/general.md#freertos)
- [Unit Test Generation](doc/general.md#unit-test-generation)


## License

rmtld3synth toolchain files are licensed under LGPL 3.0.