
rmtld3synth toolchain: Synthesis from a fragment of MTL with durations
===========================================

RMTLD3 synthesis toolchain allows us to automatically generate monitors and STMLibv2 specifications based on the formal language RMTLD3. RMTLD3 is a three-valued restricted metric temporal logic with durations that is able to reason about explicit time and temporal order of durations.

Polynomial inequalities are supported by this formalism as well as the common operators of temporal logics. Existential quantification over formulas is also supported by the cylindrical algebraic decomposition (CAD) abstraction of such formulas into several conditions without these quantifiers.

Synthesis for Z3 SMT solver is another feature. It can be used to discard before execution several constraints involving duration and temporal order of propositions.
For instance, schedulability analysis of hard real-time systems can be done by specifying the complete problem in RMTLD3. First using rmtld3synth to synthesize the problem in SMTLibv2 and then using Z3 to solve it. The idea is to know if there exists a trace for what the RMTLD3 problem is satisfiable, otherwise, if the SMT gives unsatisfiable then it means that is impossible to schedule such system, which enforces the refinement by drawing a counter-example.

# Contents

- Usage options:
  - [Tarball binaries for Windows](#tarball-binaries-for-windows)
  - [Building from Git](#building-from-git)
- [Documentation](#documentation)
- [License](#license)

### Tarball binaries version 0.2-alpha for Windows

New version is now [0.3-alpha](../../releases/download/v0.3-alpha/release-0.3.zip?raw=true).
Old version [0.2-alpha](../../releases/download/v0.2-alpha/release-0.2.zip?raw=true).

Let us begin with an overview of a simple monitoring case generation by the `rmtld3synth` tool, using as the basis the [use case one](http://rawgit.com/cistergit/rmtld3synth/master/doc/usecase1.html). The config file named [`usecaseone`](/config/usecaseone?raw=true) contains the output formula ready to be supplied to `rmtld3synth`. It can be executed by typing the following command in the windows shell and accordingly supplying the config file in the same path used for the invocation of the tool.
```
./rmtld3synth.exe -n usecaseone
```
Note that `-n` is deprecated. For versions >= 0.3-alpha use `--config-file` instead of `-n`.

After executing this step, the `monitor_set1` folder contains the generated source files of the monitor for the `usecaseone` file.
Note that the `rmtld3synth` can execute without any argument only guided by the configuration file. 
We have the monitor ready to be compiled with gcc or other "compatible" C/C++ compiler and deployed in the chosen target system. For version 0.3-alpha, C++ and Ocaml synthesis are fully supported and Spark2014 is experimental only.

### Building from Git
[![Build Status](https://travis-ci.org/anmaped/rmtld3synth.svg?branch=master)](https://travis-ci.org/anmaped/rmtld3synth)

#### To compile rmtld3synth for Linux and Mac OS using Opam and Ocaml 4.03
Just use the following commands. The dependencies will be installed automatically.
```
git clone https://github.com/anmaped/rmtld3synth.git rmtld3synth
cd rmtld3synth/
git submodule update --init --recursive
opam pin add rmtld3synth . -n
opam install rmtld3synth
```

#### To compile windows version using ocaml 4.03
```
opam switch 4.03
eval `opam config env`
export PATH=/flexdll-bin-0.35:$PATH
```
Use this [link](http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip) to download the new flexdll.
Decompress the archive in the root directory with folder name `flexdll-bin-0.35`.
Next, install the packages typing
```
opam install ocamlbuild ocamlfind batteries pa_sexp_conv sexplib type_conv
```
If pa_sexp_conv does not found a valid version we need to compile it manually.
Get version 113.00.02 from [https://github.com/janestreet/pa_sexp_conv](https://github.com/janestreet/pa_sexp_conv) and uncompress it in the folder `pa_sexp_conv`. Use opam to compile this version and install them.
```
opam install oasis

git clone https://github.com/janestreet/pa_sexp_conv.git pa_sexp_conv
cd pa_sexp_conv
opam pin add pa_sexp_conv . -n
opam install pa_sexp_conv
```

Compilation of the rmtld3synth may be done manually using the make command.


#### To compile RTMLIB
rtmlib is a support library for monitors synthesis. We can skip this step if we only need the synthesis of RMTLD3 in SMT-Libv2.
Use `make` to perform the compilation of the library. The outcome shall be the library file `librtml.a`. Please ensure that you have the gcc 4.7.0 or greater with c++0x standard flag enabled. Proper files to support atomics are provided in the GIT repository and do not need to be added afterward (only for gcc 4.7.0 version).

More details are available in the [rtmlib repository](https://github.com/anmaped/rtmlib/tree/ea11f011861e0a27253f531df043ca8ef41944e3).

### Documentation


#### Overview of the command line arguments of the rmtld3synth

The available options at the present time are as follows:

Arg                   | Description
----------------------|-----------------------------------------------------
 Flags:               |
  --synth-smtlibv2    |Enables synthesis for SMT-LIBv2 language
  --synth-ocaml       |Enables synthesis for Ocaml language
  --synth-cpp11       |Enables synthesis for C++11 language
  --synth-spark2014   |Enables synthesis for Spark2014 language (Experimental)
  --simpl-cad         |Simplify quantified RMTLD formulas using CAD (Experimental)
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



Consider that we want to solve the formula `(LessThan (Constant 0) (Duration (Constant 10) (Prop A)))`. Then, we use `rmtld3synth --synth-smtlibv2 --input-sexp <this-formula> --out-file <output-file-name>` to generate the Z3 input files. Run Z3 solver with the generated file to get `sat` or `unsat` result. A direct call from our tool to Z3 is not yet implemented.

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
"(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4"
```
We can also generate cpp11 monitors by replacing the argument `--synth-ocaml` with `--synth-cpp11`.
The outcome is the monitor illustrated in the ![texeq_sample_mon0.h](/examples/texeq_sample_mon0.h?raw=true) and ![texeq_sample_mon0_compute.h](/examples/texeq_sample_mon0_compute.h?raw=true) files.
Both monitors can now be coupled to the system under observation using rtmlib.

To use those monitors in Ocaml, we need to define a trace for Ocaml reference as follows:
```
module OneTrace : Trace = struct let trc = [("a",(0.,2.));("b",(2.,4.));("a",(4.,5.));("c",(5.,8.));
    ("a",(8.,11.));("c",(11.,21.))] end;;

module MonA = Mon0(OneTrace);;
```


#### Unit test generation

rmtld3synth also supports the automatic generation of unit tests for C++ based on the oracle deployed in Ocaml. Based on it, we test the expected evaluation of a set of formulas against the evaluation in the embedded platform.

There are two flags. The `gen_unit_tests` enables the automatic generation of monitors including the makefiles, and the  `gen_concurrency_tests` that instructs the construction of a set of tests for the performance evaluation of the rmtlib.

#### Compiling the generated monitors

To compile the generated monitors please use the generated `Makefile`. Please be aware that you need the `rtmlib.a` library.

Use `make x86-mon` to compile the monitors with the x86 target or use `x86-mtest` argument to compile both monitors and the unit tests at the same time.

Use `make arm-mon` to compile the monitors for ARM architecture with the support of the NuttX OS. After this step, we shall link the monitors as a standalone app or module as provided in the `rtmlib/nuttx` directory.
For that try to install the module files `main.cpp` and `module.mk` in the NuttX modules directory.

For the monitors to be used with Ardupilot replace the external Px4 makefile `px4_common.mk` in `modules/Px4` directory of the Ardupilot.

### License

rmtld3synth toolchain files are licensed under LGPL 3.0.
