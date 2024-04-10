
# General Documentation

## Table of Contents
- [rmtld3synth's CLI Overview](#overview-of-the-command-line-interface-of-rmtld3synth)
- [Input Languages](#input-languages)
  - [Write Formulas in RMTLD3](#write-formulas-in-rmtld3)
  - [Import Formulas from Latex Equations](#write-formulas-in-latex-and-know-how-to-use-them)
- [Compile generated Monitors](#compile-the-generated-monitors)
- [Map Event Symbols](#map-event-symbols)
- [Integrate Monitors using rtmlib in Bare Metal Platforms](#integrate-monitors-using-rtmlib-in-a-bare-metal-platform)
  - [NuttX](#nuttx-os)
  - [FreeRTOS](#freertos)
- [Unit Test Generation](#unit-test-generation)
- [Translate rmtld3 to smtlibv2](#translate-rmtld3-to-smtlibv2)
- [The Configuration File (optional)](#configuration-file-optional)


### Overview of the Command Line Interface of rmtld3synth

Here are the options available:

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


### Input Languages

DSL syntax available [here](dsl.md).


#### Write old style RMTLD3 formulas

RMTLD3 is composed of atomic propositions and logic variables that can be syntactically connected using some temporal operators and the relation '<' for terms. The syntax of RMTLD3 terms `t` and formulas `f` is defined inductively as follows:
```
t ::= (Constant `<constant>`) | (Variable `<lvariable>`) | (Plus t t) | (Times t t) | (Duration t formula)
f ::= (Prop `<proposition>`) | (LessThan t t) | (Or f f) | (Not f) | (Until `<number>` f f)
```
where `<constant>` is a real-number (interpreted as float/double), `<lvariable>` is a logic variable (identified by unique strings), `<proposition>` is a proposition (identified by unique strings), and `<number>` is a non-negative real-number (interpreted as float/double). For more details you can read the syntax defined in the paper [[3]](http://link.springer.com/chapter/10.1007%2F978-3-319-23820-3_11).

Let us now formalize the sentence `"the duration of task A with an arbitrary period greater than 10 and small than 20 is less than 5 units."`
as a formula in RMTLD3. It can be described as
```
(And
  (Not (And (Lessthan (Variable x) (Constant 20)) (Lessthan (Constant 10) (Variable x))))  
  (LessThan (Duration x (Or task_a_release (Or task_a_start ...))) 5)
)

```
The formula contains `...` meaning the remaining events triggered by the task A, and `x` is a logic variable meaning that the duration can be measured such as the inequality `10 < x < 20`. Note that `(And a b)` is a shortcut for `(Not (Or (Not a) (Not b)))`.

Note that in this example the notion of event is as used in Discrete Event Systems. Events are directly encoded using propositions and each event means an atomic proposition.



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


#### Map Event Symbols

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


### Include Monitors with rtmlib2

Let us assume that we would like to construct a monitor to observe the temporal behaviour and the duration. We define an example property as follows:
```
(a \rightarrow ((a \lor b) \until{<10} c)) \land \int^{10} c < 4
```

To build the monitor and some auxiliary functions in the folder `mon1`, we utilize the command
```
./rmtld3synth --synth-cpp11 --input-latexeq
"(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src="mon1"
```

The output tree should contain the files:
```
Rtm_compute_XXXX.h     (* The compute file where monitor is defined *)
Rtm_instrument_XXXX.h  (* The instrumentation to be included in the software under observation. *)
Rtm_monitor_XXXX.h     (* The monitor bindings to deploy the monitor as a thread or task. *)
```
Note that `XXXX` is the hexadecimal indetifier of the monitor. If the monitor changes, the identifier also changes.



#### Bare Metal (without OS)

To include the monitor as baremetal application we have to deploy it without thread support.

Please refer to [rtmlib2](https://github.com/anmaped/rtmlib) documentation for more details.

#### NuttX OS

In this case, we only have to include the header file `Rtm_monitor_XXXX.h` to include the monitor and the `Rtm_instrument_XXXX.h` to instrument the software under observation.
Now, we are able to access some symbols such as `__start_periodic_monitors` and `__buffer_mon1`. The `Rtm_monitor_XXXX.h` defines them as follows:
```
extern void __start_periodic_monitors();
extern RTML_buffer<int, 100> __buffer_mon1;
```
defines it using the extern keyword.

Note also that the buffer has the type `RTML_buffer<int, 100>` and can be instantiated elsewhere.
Inside the `Rtm_monitor_XXXX.h` we have
```
#ifdef __NUTTX__

__EXPORT RTML_buffer<int, 100> __buffer_mon1 __attribute__((used));

#else

RTML_buffer<int, 100> __buffer_mon1 __attribute__((used));

#endif
```
indicating that `__buffer_mon1` is reused or locally instantiated. The function `__start_periodic_monitors` will enable the task using the POSIX Threads interface defined in rtmlib (see task_compat.h for more details).

##### Compile existing demos

After this step, we shall link the monitors as a standalone app or module as provided in [rtmlib2 examples](https://github.com/anmaped/rtmlib) directory.

The monitors to be used with Ardupilot should be included in the external Px4 makefile `px4_common.mk` in the Ardupilot's directory `modules/Px4`. Check [these instructions](nuttx-ardupilot.md) for more details.


#### FreeRTOS

Please refer to [rtmlib2](https://github.com/anmaped/rtmlib) documentation for more details.


#### Hardware (FPGA)

[TBC]


#### Unit Test Generation

rmtld3synth also supports the automatic generation of unit tests for C++ based on the oracle deployed in Ocaml. Based on it, we test the expected evaluation of a set of formulas against the evaluation in the embedded platform.

There are two flags. The `gen_unit_tests` enables the automatic generation of monitors including the makefiles, and the  `gen_concurrency_tests` that instructs the construction of a set of tests for the performance evaluation of the rmtlib.


#### Translate rmtld3 to smtlibv2

To solve the formula `(LessThan (Constant 0) (Duration (Constant 10) (Prop A)))`, we can follow these steps:

1. Generate the Z3 input file by using the command `rmtld3synth --synth-smtlibv2 --input-sexp <this-formula> --out-file <output-file-name>`.

2. Run the Z3 solver with the generated file to obtain either a sat or unsat result.

3. If you want to directly call Z3 from our tool, you can use the flag `--solver-z3`.

4. To retrieve a satisfiable trace, use the flag `--get-trace`.

Please note that step 2 is optional.


#### Configuration File (optional)

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
