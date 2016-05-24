

rmtld3synth toolchain: Generating monitors from a fragment of MTL with durations
===========================================

RMTLD3 synthesis toolchain allows us to automatically generate monitors based on the formal specification language RMTLD3. RMTLD3 is a three-valued restricted metric temporal logic with durations that is able to reason about explicit time.

Polynomial inequalities are supported by this formalism as well as the common operators of temporal logics. Quantification over formulas is also available as a manner to decompose quantified formulas into several monitoring conditions.

# Contents

- Usage options:
  - [Tarball binaries](#tarball-binaries)
  - [Building with make](#building-with-make)
- [Documentation](#documentation)
- [License](#license)

### Tarball binaries

Let's overview a simple monitoring case generation of the `rmtld3synthcpp` tool based on the use case [one](http://rawgit.com/cistergit/rmtld3synth/master/doc/usecase1.html) in a nutshell.

To execute the generation of monitors based on the [`config`](/bin/config?raw=true) file, please type [`rmtld3synthcpp.exe`](/bin/rmtld3synthcpp.exe?raw=true) in Windows command prompt, and verify that the `config` file is in the same directory of the executable.

The binary shall executes without any argument only typing `./rmtld3synthcpp.exe`, and shall generates a folder with name `monitor_set1` containing the source files of the monitor of the use case one.

Note that we only provide binaries for Windows, and use it in cygwin or native shell.

### Building with make

#### Compiling rmtld3synth2cpp tool
Type `make` and the compilation will proceed usign the `OCamlMakefile` developed by Markus Mottl. More references about ocaml installation steps can be found [here](https://ocaml.org/docs/install.html).

### Compiling rtmlib
Type `make` and the compilation will proceed creating the library file `librteml.a`. Please ensure that you have the gcc 4.7.0 or greater with c++0x standard enabled. Proper files to support atomics are provided in the GIT repository and do not need to be added afterwards.

### Documentation

#### Overview of the configuration settings

Settings for RMTLD3 synthesis tool are described using the syntax `(<setting_id> <bool_type | integer_type | string_type>)`, where '|' indicates the different types of arguments such as boolean, integer or string, and `setting_id` the setting identifier of type string.

See [the overall parameters](rmtld3_parameters.md) for more details.

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

`maximum_period` sets the minimum inter-arrival time. It has a correlation between the periodic monitor and the minimum inter-arrival time. It provides static checks according to the size of time-stamps of events.

`event_subtype` provides the type for the event data. In that case, it is an identifier that can distinct 255 events. However, if more events are required, the type should be modified to *uint32_t* or greater. The number of different events versus the available size for the identifier is also statically checked.

`cluster_name` identifies the set of monitors. It acts as a label for grouping monitor specifications.

#### Write formulas in RMTLD3

The formulas `m_simple` and `m_morecomplex` follow the same syntax defined in the paper [[3]](http://link.springer.com/chapter/10.1007%2F978-3-319-23820-3_11).

RMTLD3 is an extension of a well known temporal logic. It is composed of atomic propositions and logic variables that can be syntatically connected using some temporal operators and the less relation of terms. The syntax of RMTLD3 terms `t` and formulas `f` is defined inductively as follows:
```
t ::= `<constant>` | `<lvariable>` | Plus(t,t) | Times(t,t) | Duration(t, formula)
f ::= `<proposition>` | LessThan(t, t) | Or(f, f) | Not(f) | Until(`<number>`, f, f)
```
where `<constant>` is a real-number (interepreted as float/doubles), `<lvariable>` is a logic variable (identified by unique strings), `<proposition>` is a proposition (identified by unique strings), and `<number>` is a non-negative real-number (interpreted as float/doubles).


Let's write the duration formula meaning "the duration of the task A with an arbitrary period greater than 10 and small than 20 is less than 5 units".
```
And(Not(And(Lessthan(x,20),Not(Lessthan(x,10)))), Duration(x,Or(task_a_release,Or(task_a_start,...))) < 5)
```
The formula contains `...` meaning the remaining events triggered by the task A, and `x` is a logic variable meaning that the duration can be measured such as the inequality 10 < x < 20.

Note that in this case the notion of event is as used in Discrete Event Systems.


#### Unit test generation

[TODO]

#### Compiling the generated monitors

To compile the generated monitors please use the generated `Makefile`.

Type `make x86-mon` to compile the monitors with x86 target or using `x86-mtest` argument to compile the unit tests.

Type `make arm-mon` to compile the monitors for ARM architecture with support of the NuttX OS. After this step we shall link the monitors as a standalone app or module as provided in the `rtmlib/nuttx` directory.
For that install the module files `main.cpp` and `module.mk` in the NuttX modules directory. Repleace also the external Px4 makefile `px4_common.mk` in `modules/Px4` directory of the Ardupilot.

### License

rmtld3synth toolchain files are licensed under LGPL.
