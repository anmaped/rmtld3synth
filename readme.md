
rmtld3synth toolchain: Synthesis from a fragment of MTL with durations
======================================================================

The rmtld3synth toolchain is capable of automatically generating cpp11/ocaml monitors and specifications in the SMT-LIB v2 standard language from a fragment of metric temporal logic with durations. This logic is a three-valued extension of a restricted fragment of metric temporal logic with durations, which allows us to reason about explicit time and temporal order of durations.

Supported by this formalism are polynomial inequalities (using the less relation <) and the common modal operators of temporal logic U (until) and S (since). 
Existential quantification via these formulae is also possible by adopting the cylindrical algebraic decomposition (CAD) method. This method is suitable for converting quantified formulas under various decomposition conditions without these quantifiers.

To check the satisfiability of formulae, the tool is ready to synthesize the logic fragment in the input language accepted by the z3 SMT solver or any other prover supporting AUFNIRA logic and SMT-LIB v2. It can be used to discard various constraints concerning the duration and temporal order of the propositions.

For example, schedulability analysis of hard real-time systems is possible by specifying the complete problem in RMTLD3. First, rmtld3synth is used to synthesize the problem in SMT-LIB, and then Z3 is used to solve it. The idea is to know whether there is a trace for which the RMTLD3 problem is satisfiable, or whether the SMT gives us an unsatisfiable answer, meaning that it is impossible to plan such a configuration. The latter reinforces the refinement by giving a counterexample.

# Contents

- Usage options:
  - [Online Web Demonstrator](#get-started-with-the-online-demonstrator-camel)
  - [Tarball Binaries for Windows](#get-started-with-tarball-binaries)
  - [Container Images](#get-started-with-docker-whale)
  - [Building from Git](#building-from-git)
- [Documentation](#documentation)
- [License](#license)

## Get started with the online demonstrator :camel:

The stable version is available for testing in the browser [Try it](https://anmaped.github.io/rmtld3synth).

## Get started with Tarball binaries

The latest release version is [0.6](../../releases).

## Get started with docker :whale:

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


## Building from Git

![Linux build](https://github.com/anmaped/rmtld3synth/actions/workflows/linux-build.yml/badge.svg)
![macOS build](https://github.com/anmaped/rmtld3synth/actions/workflows/macos-build.yml/badge.svg)
![Windows build](https://github.com/anmaped/rmtld3synth/actions/workflows/windows-build.yml/badge.svg)

### To compile rmtld3synth for Linux and macOS using Opam and Ocaml version >= 4.04.0

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


### To compile rmtld3synth for Windows using ocaml >= 4.04.0

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

## Notes on rtmlib2

The current version is [v2.1.0](https://github.com/anmaped/rtmlib/tags).

![build workflow](https://github.com/anmaped/rtmlib/actions/workflows/build-and-send-images.yml/badge.svg)

The rtmlib2 is a support library for embedding runtime monitors written in CPP11.

Note that we can skip this compilation step if we do not need to integrate the monitors into the target system, or if we just want to synthesize the RMTLD3 specifications into SMT-Libv2.

Please make sure you have gcc 4.7.0 or later with the c++0x standard flag enabled. The appropriate files to support atomics are provided in the GIT repository and do not need to be added afterward (only for GCC 4.7.0 version).

See below for more details. See also the [rtmlib documentation repository](https://anmaped.github.io/rtmlib/doc/).


## Documentation

- [Overview of the Command Line Interface of rmtld3synth](doc/general.md#overview-of-the-command-line-interface-of-rmtld3synth)
- [Input Languages](doc/general.md#input-languages)
  - [With DSL (prefered)](doc/input-languages.md#with-dsl-prefered)
  - [With symbolic expressions (old style)](doc/input-languages.md#with-symbolic-expressions-old-style)
  - [LaTex expressions for paper writing purposes](doc/input-languages.md#latex-expressions-for-paper-writing-purposes)
- [Include Monitors with rtmlib2](doc/general.md#include-monitors-with-rtmlib2)
  - [Sample](doc/general.md#sample-runs-on-all-architectures-supported-by-rtmlib2)
  - [Bare Metal (without OS)](doc/general.md#bare-metal-without-os)
  - Real-time OS
    - [NuttX](doc/general.md#nuttx-os)
    - [FreeRTOS](doc/general.md#freertos)
    - [RTEMS](doc/general.md#rtems)
  - [Hardware (FPGA)](doc/general.md#hardware-fpga)
  - [Mapping between propositions and symbols](doc/general.md#to-show-the-mapping-between-the-propositions-and-the-symbols)
- [Translate rmtld3 to smtlibv2](doc/general.md#translate-rmtld3-to-smtlibv2)
- [The Configuration File (optional)](doc/general.md#configuration-file-optional)
  - [Unit Tests flags](doc/general.md#unit-tests-flags)


## License

rmtld3synth toolchain files are licensed under LGPL 3.0.