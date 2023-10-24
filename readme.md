
rmtld3synth toolchain: Synthesis from a fragment of MTL with durations
======================================================================

The rmtld3synth toolchain is capable of automatically generating cpp11/ocaml monitors and specifications in SMT-LIB v2 standard language from a fragment of metric temporal logic with durations. This logic is a three-valued extension over a restricted fragment of metric temporal logic with durations that allow us to reason about explicit time and temporal order of durations.

Supported by this formalism are polynomial inequalities (using the less relation <) and the common modal operators of temporal logics U (until) and S (since). 
The existential quantification over these formulas is also possible through the adoption of the cylindrical algebraic decomposition (CAD) method. This method is suitable for converting quantified formulas under various decomposed conditions without these quantifiers.

To check formula satisfiability, the tool is ready to synthesize the logic fragment in the input language accepted by the z3 SMT solver or any other prover supporting AUFNIRA logic and SMT-LIB v2. It can be used to discard various constraints involving the duration and temporal order of the propositions.

For example, schedulability analysis of hard real-time systems is possible by specifying the complete problem in RMTLD3. First using rmtld3synth to synthesize the problem in SMT-LIB and then using Z3 to solve it. The idea is to know if there exists a trace for which the RMTLD3 problem is satisfiable, or whether the SMT gives us an unsatisfiable answer meaning that is impossible to schedule such a configuration. The latter reinforces the refinement by drawing a counterexample.

# Contents

- Usage options:
  - [Online Web Demonstrator](#get-started-with-the-online-demonstrator-camel)
  - [Tarball Binaries for Windows](#get-started-with-tarball-binaries)
  - [Container Images](#get-started-with-docker-whale)
  - [Building from Git](#building-from-git)
- [Documentation](#documentation)
- [License](#license)

### Get started with the online demonstrator :camel:

The stable version is available for testing in the browser [Try it](https://anmaped.github.io/rmtld3synth).

### Get started with Tarball binaries

The latest release version is [0.4-alpha](../../releases/download/v0.4-alpha/).

### Get started with docker :whale:

![build workflow](https://github.com/anmaped/rmtld3synth/actions/workflows/build-and-send-images.yml/badge.svg)


The latest docker image is available in [dockerhub](https://hub.docker.com/r/anmaped/rmtld3synth). To use it pull the image and execute it.
```shell
docker pull anmaped/rmtld3synth:latest # this will download the pre-built image from dockerhub
docker run -it anmaped/rmtld3synth  # this will create and run the container
```

Now you can use the rmtld3synth command line interface.

### Get started with rmtld3synth

To initiate monitor generation, you can utilize the `rmtld3synth` tool via the command line interface (CLI). Next, let's introduce an example of how to achieve this generation by specifying a straightforward input formula and specific settings.

 To create a sample monitor, execute the following shell command (assuming the tool is in the current shell environment):

```shell
rmtld3synth --synth-cpp11 --input-latexeq "a \until_{<10} b" --out-src out
```

The `--synth-cpp11` flag sets `rmtld3synth` to build a C++11 monitor using the formula `a \until_{<10} b` and save the output in the `out` directory.

At this point, you have a working monitor that can be provided to GCC, LLVM, or other C/C++ compiler. Both C++ and Ocaml synthesis flags are available, although SPARK support is planned for future versions.

For further insights on instrumenting the monitors, please refer to the illustrative examples in the [rtmlib2](https://github.com/anmaped/rtmlib/tree/master/examples) repository.
Alternatively, you may explore the [Documentation](#documentation) section of rmtld3synth. This resource provides valuable insights into creating monitors for various targets such as bare metal or operating systems like NuttX and FreeRTOS.


### Building from Git

![run tests workflow](https://github.com/anmaped/rmtld3synth/actions/workflows/run-tests.yml/badge.svg)

<!--[![Build Status](https://app.travis-ci.com/anmaped/rmtld3synth.svg?branch=master)](https://app.travis-ci.com/anmaped/rmtld3synth)-->

#### To compile rmtld3synth for Linux and macOS using Opam and Ocaml version >= 4.04.0

To build rmtld3synth for Linux and macOS using Opam and a minimum Ocaml version of 4.04.0, follow these steps:

1. Pin rmtld3synth as an `opam` package and set the Ocaml compiler to version `>= 4.04.0`. All necessary dependencies will be installed automatically.

```shell
opam pin add rmtld3synth https://github.com/anmaped/rmtld3synth.git
```

2. Compile the Z3 solver and its corresponding ML bindings with the following commands:

```shell
opam install z3 -v
```

Alternatively, you can compile and install Z3 from sources with the following command:

```shell
git clone https://github.com/Z3Prover/z3.git
cd z3
python scripts/mk_make.py --ml
cd build
make
sudo make install
```

Please ensure that you have at least Python version 2.7 and `g++-5` installed on your system.


#### To compile rmtld3synth for Windows using ocaml >= 4.04.0

Get [Andreas Hauptmann's installer](https://fdopen.github.io/opam-repository-mingw/installation/) and switch the Ocaml compiler to version `>= 4.04.0``.

```shell
opam switch 4.04.0+mingw64
eval `opam config env`
```

In case you have not properly installed the flexdll, download the new flexdll [here](http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip), and decompress the archive in the current directory (PWD) with the folder name `flexdll-bin-0.35``.

```shell
export PATH=$(PWD)/flexdll-bin-0.35:$PATH
```

Then, you have to pin the rmtld3synth package as described on `compile rmtld3synth for Linux and OS X ` section to conclude the compilation.

:grey_exclamation:HINTS!!

To compile z3 using Cygwin you should use the mingw32 or mingw64 tools, otherwise, it will not work properly.
If the correct version of GCC is not found when executing `mk_make.py`, modify the flags `CXX` and `AR` as needed. For instance:

```shell
CXX=x86_64-w64-mingw32-g++ CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar python scripts/mk_make.py --ml
```

Ensure also that libz3 is properly installed in the current environment.
If the libz3 is not found then use the copy command to copy the `z3/build/libz3.dll.a` library to the `/lib` or `/home/current-user/.opam/compiler-version/lib` directory.

#### Instructions to use rtmlib2

The current version is [v2.0.1](../../releases/download/v2.0.1/).

The rtmlib2 is a support library for embedding runtime monitors written in CPP11.

Note that we can skip this compilation step if we do not need to integrate the monitors into the target system or if we just want to synthesize RMTLD3 specifications in SMT-Libv2.

Please ensure that you have the gcc 4.7.0 or greater with the c++0x standard flag enabled. Proper files to support atomics are provided in the GIT repository and do not need to be added afterward (only for GCC 4.7.0 version).

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