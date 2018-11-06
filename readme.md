
rmtld3synth toolchain: Synthesis from a fragment of MTL with durations
===========================================

The rmtld3synth toolchain is able to automatically generate cpp11/ocaml monitors and specifications in SMT-LIB v2 standard language from a fragment of metric temporal logic with durations. This logic is a three-valued extension over a restricted fragment of metric temporal logic with durations that allows us to reason about explicit time and temporal order of durations.

Supported by this formalism are polynomial inequalities (using the less relation <) and the common modal operators of temporal logics U (until) and S (since). 
The existential quantification over these formulas is also possible by adopting the cylindrical algebraic decomposition (CAD) method. This method is suitable to convert quantified formulas into several decomposed conditions without these quantifiers.

For formula satisfiability checking, the tool is ready to synthesize the logic fragment into the input language accepted by the z3 SMT solver. It can be used to discard offline and before execution several constraints involving duration and temporal order of propositions.
For instance, schedulability analysis of hard real-time systems is possible by specifying the complete problem in RMTLD3. First using rmtld3synth to synthesize the problem in SMT-LIB and then using Z3 to solve it. The idea is to know if there exists a trace for which the RMTLD3 problem is satisfiable, or whether the SMT gives us an unsatisfiable answer meaning that is impossible to schedule such configuration. The latter enforces the refinement by drawing a counter-example.

# Contents

- Usage options:
  - [Online demonstrator using js_of_ocaml](#online-demonstrator-using-js_of_ocaml)
  - [Tarball binaries for Windows](#tarball-binaries-for-windows)
  - [Building from Git](#building-from-git)
- [Documentation](#documentation)
- [License](#license)

### Online demonstrator using js_of_ocaml
:camel: [Try it](https://anmaped.github.io/rmtld3synth).

### Tarball binaries for Windows

Current version is [0.3-alpha2](../../releases/download/v0.3-alpha2/release-v0.3-alpha2-windows64.zip?raw=true).
Old version is [0.2-alpha](../../releases/download/v0.2-alpha/release-0.2.zip?raw=true).

Let us begin by overviewing a monitor generation using the rmtld3synth tool. For this example, we adopt the use case one [available here](http://rawgit.com/cistergit/rmtld3synth/master/doc/usecase1.html) and the configuration file [`usecaseone`](/config/usecaseone?raw=true) containing the input formula and some intructions for the rmtld3synth to process. To generate the monitor(s), we just need to type the following command in the command shell.
```
./rmtld3synth.exe -n usecaseone
```
Note that `-n` is deprecated and requires to set the config file in the same directory of the executable. For versions >= 0.3-alpha use `--config-file` argument instead of `-n` in order to set the correct path of the one shot configuration file.

After executing the command, the `monitor_set1` folder is available and contains the generated source files of the monitor(s) specified for the `usecaseone` setup.
At this point, we have the monitor ready to be supplied for GCC, LLVM or other "compatible" C/C++ compiler and deployed in the chosen target architecture. In the current version, C++ and Ocaml synthesis are fully supported and Spark2014 is not yet available.

### Building from Git
[![Build Status](https://travis-ci.org/anmaped/rmtld3synth.svg?branch=master)](https://travis-ci.org/anmaped/rmtld3synth)

#### To compile rmtld3synth for Linux and Mac OS using Opam and Ocaml 4.03.0
Just use the following commands and switch the opam compiler to version `4.03.0` if needed. All dependencies should be installed automatically.

```
opam pin add rmtld3synth https://github.com/anmaped/rmtld3synth.git
```

Use the following commands to switch opam to the OCaml compiler version 4.03.0
```
opam switch 4.03.0
eval `opam config env`
```

Use the commands below to compile z3 solver and respective ML bindings.
```
git clone https://github.com/Z3Prover/z3.git z3
cd z3
python scripts/mk_make.py --ml
cd build; make
sudo make install
```
Make sure that you have at least the python 2.2.7 installed, the gcc-5, and the g++-5.

#### To compile rmtld3synth for Windows using ocaml 4.03.0
Get the [Andreas Hauptmann's installer](https://fdopen.github.io/opam-repository-mingw/installation/). Using the available ocaml console switch the opam compiler to version `4.03.0`.
```
opam switch 4.03.0+mingw64
eval `opam config env`
```
In case you have not properlly installed the flexdll, download the new flexdll [here](http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip), and decompress the archive in the current directory (PWD) with folder name `flexdll-bin-0.35`.
```
export PATH=$(PWD)/flexdll-bin-0.35:$PATH
```
Then, install the required packages by using the following commands
```
opam install ocamlbuild ocamlfind batteries pa_sexp_conv sexplib.113.33.00+4.03 type_conv
```
:heavy_exclamation_mark:WARNING!! If opam does not found a valid version for `pa_sexp_conv` package then we need to compile this package manually.
Get the version 113.00.02 from [https://github.com/janestreet/pa_sexp_conv](https://github.com/janestreet/pa_sexp_conv) and uncompress it in the folder `pa_sexp_conv`. Then, use opam to compile this version, compile the `oasis` dependency and install them.
```
opam install oasis
opam pin add pa_sexp_conv https://github.com/janestreet/pa_sexp_conv.git
```

Use the same commands, as described above for the case of compiling rmtld3synth using Linux and Mac OS, to conclude the compilation.

:grey_exclamation:HINTS!!

If the correct version of GCC is not found when executing `mk_make.py` then use the environment variables below.
```
CXX=x86_64-w64-mingw32-g++ CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar python scripts/mk_make.py --ml
```

If the libz3 is not found then use the command below for manually copy the libraries.
```
cp z3/build/libz3.dll.a /lib/
```


#### Instructions to compile rtmlib

Current version is [0.1-alpha](../../releases/download/v0.3-alpha/rtmlib-0.1.zip?raw=true).

The rtmlib is a support library for embedding runtime monitors written in CPP11. We can skip this compilation step if we do not need to integrate the monitors into the target system or we just want to synthesize RMTLD3 specifications in SMT-Libv2.
Use the `make` command to perform the compilation of the library as usual. The outcome shall be the library file `librtml.a`. Please ensure that you have the gcc 4.7.0 or greater with c++0x standard flag enabled. Proper files to support atomics are provided in the GIT repository and do not need to be added afterward (only for gcc 4.7.0 version).

More details are available in the [rtmlib documentation repository](https://anmaped.github.io/rtmlib/doc/).



### Documentation


#### Overview of the command line arguments of the rmtld3synth

The available options for the current version are as follows:

Arg                   | Description
----------------------|-----------------------------------------------------
 Flags for synthesis: |
  --synth-smtlibv2    |Enables synthesis for SMT-LIBv2 language
  --synth-ocaml       |Enables synthesis for Ocaml language
  --synth-cpp11       |Enables synthesis for C++11 language
  --synth-spark2014   |Enables synthesis for Spark2014 language (Unavailable)
 Flags for solving:   |
  --simpl-cad         |Simplify quantified RMTLD formulas using CAD (Experimental)
  --solver-z3         |Enables solving smtlibv2 problems using Z3 SMT solver
  --solver-statistics |Enables printing the solve statistics
  --get-trace         |Returns the schedule
  --trace-style       |Sets the trace style
 Input:               |
  --input-sexp        |Inputs sexp expression (RMTLD3 formula)
  --input-latexeq     |Inputs latex equation expressions (RMTLD3 formula) (Experimental)
  --input-rmdsl       |Inputs rmdsl expressions for schedulability analysis (Experimental)
  --config-file       |File containing synthesis settings
 Output:              |
  --out-file          |Set the output file for synthesis
  --out-src           |Set the output directory for synthesis
 Options:             |
  --verbose           |Enables verbose mode
  --version           |Version and SW information
  --help              |Display this list of options



Consider that we want to solve the formula `(LessThan (Constant 0) (Duration (Constant 10) (Prop A)))`. Then, we use `rmtld3synth --synth-smtlibv2 --input-sexp <this-formula> --out-file <output-file-name>` to generate the Z3 input file. Run Z3 solver with the generated file to get `sat` or `unsat` result. A direct call from our tool to Z3 is yet implemented and available by using the flag `--solver-z3`. `--get-trace` flag can be used to retrieve a satisfiable trace.

#### Overview of the configuration file

Settings for RMTLD3 synthesis tool are defined using the syntax `(<setting_id> <bool_type | integer_type | string_type>)`, where '|' indicates the different types of arguments such as Boolean, integer or string, and `setting_id` the setting identifier of the type string.

See [the overall parameters](doc/configparameters.md) for more details.

~~~~~~~~~~~~~~~~~~~~~{.lisp}
(gen_tests true)

(minimum_inter_arrival_time 102)
(maximum_period 2000000)
(event_subtype uint_8)
(cluster_name monitor_set1)

(m_simple 1000000 (Or (Until 200000 (Prop A) (Prop C)) (Prop B)))
(m_morecomplex 500000 (Or (Until 200000 (Prop set_off) (Or (Until 200 (Prop A) (Prop C)) (Prop B))) (Prop B)))

~~~~~~~~~~~~~~~~~~~~~

`gen_tests` sets the automatic generations of test cases (to be used as a demo in the described illustration below).

`minimum_inter_arrival_time` establishes the minimum inter-arrival time that the events can have. It is a very pessimistic setting but provides some information for static checking.

`maximum_period` sets the maximum interval between two consecutive releases of tasks' jobs. It has a correlation between the periodic monitor and the minimum inter-arrival time. It provides static checks according to the size of time-stamps of events.

`event_subtype` provides the type for the event data. In that case, it is an identifier that can distinct 255 events. However, if more events are required, the type should be modified to *uint32_t* or greater. The number of different events versus the available size for the identifier is also statically checked.

`cluster_name` identifies the set of monitors. It acts as a label for grouping monitor specifications.



#### Write formulas in RMTLD3

RMTLD3 is composed of atomic propositions and logic variables that can be syntactically connected using some temporal operators and the relation '<' for terms. The syntax of RMTLD3 terms `t` and formulas `f` is defined inductively as follows:
```
t ::= (Constant `<constant>`) | (Variable `<lvariable>`) | (Plus t t) | (Times t t) | (Duration t formula)
f ::= (Prop `<proposition>`) | (LessThan t t) | (Or f f) | (Not f) | (Until `<number>` f f)
```
where `<constant>` is a real-number (interpreted as float/double), `<lvariable>` is a logic variable (identified by unique strings), `<proposition>` is a proposition (identified by unique strings), and `<number>` is a non-negative real-number (interpreted as float/double). For more details you can read the syntax defined in the paper [[3]](http://link.springer.com/chapter/10.1007%2F978-3-319-23820-3_11).

Let us interpret the sentence `"the duration of task A with an arbitrary period greater than 10 and small than 20 is less than 5 units."`
as a formula in RMTLD3. It can be described as
```
(And
  (Not (And (Lessthan (Variable x) (Constant 20)) (Lessthan (Constant 10) (Variable x))))  
  (LessThan (Duration x (Or task_a_release (Or task_a_start ...))) 5)
)

```
The formula contains `...` meaning the remaining events triggered by the task A, and `x` is a logic variable meaning that the duration can be measured such as the inequality `10 < x < 20`. Note that `(And a b)` is a shortcut for `(Not (Or (Not a) (Not b)))`.

Note that in this example the notion of event is as used in Discrete Event Systems. Events are directly encoded using propositions.



#### Write formulas in latex and know how to use them

Latex equation formulas are much less verbose than symbolic expressions. Due to that, we made available the synthesis of RMTLD3 formulas written as latex equations.
Consider the formula
```
(a \rightarrow ((a \lor b) \until{<10} c)) \land \int^{10} c < 4
```
that intuitively describes that given an event `a`, `b` occurs until `c` and, at the same time, the duration of `b` shall be less than four time units over the next `10` time units.
For instance, a trace that satisfies this formula is
```
(a,2),(b,2),(a,1),(c,3),(a,3),(c,10)
```
From rmtld3synth tool, we have synthesized the formula's example into the Ocaml language code described in the ![texeq_sample.ml](/examples/texeq_sample.ml?raw=true). For that, we have used the command
```
./rmtld3synth --synth-ocaml --input-latexeq
"(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > mon_sample.ml
```
We can also generate cpp11 monitors by replacing the argument `--synth-ocaml` with `--synth-cpp11`.
The outcome is the monitor illustrated in the ![texeq_sample_mon0.h](/examples/texeq_sample_mon0.h?raw=true) and ![texeq_sample_mon0_compute.h](/examples/texeq_sample_mon0_compute.h?raw=true) files.
Both monitors can now be coupled to the system under observation using rtmlib.

To use those monitors in Ocaml, we need to define a trace for Ocaml reference as follows:
```
open Mon_sample
module OneTrace : Trace = struct let trc = [("a",(0.,2.));("b",(2.,4.));("a",(4.,5.));("c",(5.,8.));
    ("a",(8.,11.));("c",(11.,21.))] end;;

module MonA = Mon_sample.Mon0(OneTrace);;
```


#### Unit test generation

rmtld3synth also supports the automatic generation of unit tests for C++ based on the oracle deployed in Ocaml. Based on it, we test the expected evaluation of a set of formulas against the evaluation in the embedded platform.

There are two flags. The `gen_unit_tests` enables the automatic generation of monitors including the makefiles, and the  `gen_concurrency_tests` that instructs the construction of a set of tests for the performance evaluation of the rmtlib.


#### Compile the generated monitors

To compile the generated monitors please use the generated `Makefile`. Please be aware that you need the `rtmlib.a` library.

Use `make x86-mon` to compile the monitors with the x86 target or use `x86-mtest` argument to compile both monitors and the unit tests at the same time.

Use `make arm-mon` to compile the monitors for ARM architecture with the support of the NuttX OS. After this step, we shall link the monitors as a standalone app or module as provided in the `rtmlib/nuttx` directory.
For that try to install the module files `main.cpp` and `module.mk` in the NuttX modules directory.

For the monitors to be used with Ardupilot replace the external Px4 makefile `px4_common.mk` in `modules/Px4` directory of the Ardupilot.


#### Integrate monitors using rtmlib in a bare metal platform

##### NuttX OS

Let us use as example the property 
```
(a \rightarrow ((a \lor b) \until{<10} c)) \land \int^{10} c < 4
```
and use the command 

```
./rmtld3synth --synth-cpp11 --input-latexeq
"(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src="mon1"
```
to create the monitor and some auxiliary functions in the folder `mon1`. The output tree should be something like
```
Makefile           (* The makefile with rules: x86-mon, arm-mon *)
Mon0.h             (* The monitor to be included in the system under observation. *)
mon0_compute.h     (* The body of the monitor procedure to be included. *)
mon1.cpp           (* The aggregation point (the monitor mon0 is included here) *)
mon1.h             (* The header file for aggregation of monitors *)
rmtld3.h           (* The header containing all the rmtld3 types and macros. *)
Rmtld3_reader.h    (* The binding for rtmlib *)
Rmtld3_reader_it.h (* The binding for rtmlib *)
```

In this case, we only have to compile `mon1.cpp` in order to include it in the SUO.
From now on, we are able to acess the symbols `__start_periodic_monitors` and `__buffer_mon1` in the object file `mon1.o`. The `mon1.h` header file
```
extern void __start_periodic_monitors();
extern RTML_buffer<int, 100> __buffer_mon1;
```
defines it using the extern keyword.

Note also that the buffer has the type `RTML_buffer<int, 100>` and can be instantiated elsewhere.
Inside the mon1.cpp we have
```
#ifdef __NUTTX__

__EXPORT RTML_buffer<int, 100> __buffer_mon1 __attribute__((used));

#else

RTML_buffer<int, 100> __buffer_mon1 __attribute__((used));

#endif
```
indicating that `__buffer_mon1` is reused or locally instantiated. The function `__start_periodic_monitors` will enable the task using the POSIX Threads interface defined in rtmlib (see task_compat.h for more details).


##### FreeRTOS

The approach should be similiar to NuttX OS since both OSs are POSIX compliant.


#### Match event types

Let us use the previous example containing three propositions `a, b, c` that will be mapped to the SUO in a different way.
The usage of ASCII characters or strings is not the most efficient way to proceed since they will quickly increase the memory footprint. We will see now how we can avoid it.

The content of the file `mon0_compute.h` describes where the symbols can appear as simple numbers. Indeed `a`,`b` and `c` were mapped using a hash table to some sort of binary enumeration (i.e., `0x04` for a, `0x03` for `b`, and `0x01` for `c`). To be able to match this encoding when generating events describing `a,b,c`, we have to use the macro definitions `SORT_a,SORT_b,SORT_c`. If we have a symbol `sym` then the respective macro should be `SORT_sym`. Althought a check is performed by the tool, we have to be carefull about the macro variable's names accepted in C++11 when describing the name of the propositions in RMTLD3 specifications.

The map function `_mapsorttostring` is also available if we do not want to work with sorts and instead use strings.

```
  #define SORT_a 4
  #define SORT_b 3
  #define SORT_c 1
  
  #include <string>
  #include <unordered_map>
  // Create an unordered_map of sorts (that map to integers)
  std::unordered_map<std::string, int> _mapsorttostring = {
  {"a",4},
  {"b",3},
  {"c",1},
  };
```

### License

rmtld3synth toolchain files are licensed under LGPL 3.0.