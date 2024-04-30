
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


## Overview of the Command Line Interface of rmtld3synth

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


## Input Languages

The primary language used is the DSL, whose syntax is available [here](dsl.md).

Other input languages are available [here](input-languages.md).


## Include Monitors with rtmlib2

Let us assume that we would like to construct a monitor to observe the temporal behaviour and the duration. We define an example property as follows:
```
(a -> (a || b) until c within 10s) && 
(duration of c in 0 .. 10) < 4
```

Plain LaTeX serves as an alternative encoding for expressions, and the aforementioned property can be encoded using it.
```
(a \rightarrow ((a \lor b) \until{<10} c)) \land \int^{10} c < 4
```

To construct the monitor and its accompanying functions within the `mon1` folder, we utilize the command 
```
./rmtld3synth --synth-cpp11
--input-dsl "(a -> (a || b) until c within 10s) && (duration of c in 0 .. 10) < 4" --out-src="mon1" 
```
Note that instead of using `--input-dsl`, you can utilize a latex expression by replacing it with `--input-latexeq`.
The files that should be included in the `mon1` folder are:
```
Rtm_compute_0746.h     (* The compute file where monitor is defined *)
Rtm_instrument_0746.h  (* The instrumentation to be included in the software under observation. *)
Rtm_monitor_0746.h     (* The monitor bindings to deploy the monitor as a thread or task. *)
```
Note that `0746` is the hexadecimal indetifier of the monitor. If the monitor structure and behaviour changes, the identifier also changes.

### Sample (runs on all architectures supported by rtmlib2)

Consider the previous generated monitor, their files in folder `mon1`, and the instrumentation file `sample_instrumentation.cpp` as defined below.

```C++
// filename sample_instrumentation.cpp

#include "Rtm_instrument_0746.h"

extern int sample(); // the external function to construct the sample monitor

int main() {
  using Writer = Writer_rtm__0746;

  // using the new name
  Writer w;

  // push symbol 'a' with timestamp 0
  // Note that all propositions are defined inside the writer.
  // There is no way to send other symbols that are not defined there.
  w.push(Writer::a, 0);

  sample(); // run the sample monitor
}
```

Now, we are able to compile the instrumentation sample
```
gcc -I../rtmlib2/src/ sample_instrumentation.cpp -o test -latomic
```
It will ask for `__buffer_rtm_monitor_3a50` at linkage step and the `sample` function.

We will now establish a sample monitor that incorporates the necessary symbols in the following manner:

```C++
// filename sample_monitor.cpp

#include "Rtm_monitor_3a50.h"

RTML_BUFFER0_SETUP(); // defines the buffer here

int sample() {
  RTML_BUFFER0_TRIGGER_PERIODIC_MONITORS(); // triggers the thread to run the
                                            // monitor

  while (true) {
    // do something to wait for threads
  }
}
```

We perform the compilation of `sample_monitor.cpp` and `sample_instrumentation.cpp` with

```
gcc -I../rtmlib2/src/ sample_monitor.cpp sample_instrumentation.cpp -o example -latomic -fno-rtti -fno-exceptions -DRTMLIB_ENABLE_DEBUG_RMTLD3
```
After creating the initial instrumentation and monitoring samples, you can execute them by running `./example`.

Great, you have successfully created and run your first C++ monitor!


### Bare Metal (without OS)

To include the monitor as baremetal application we have to deploy it without thread support.

Please refer to [rtmlib2](https://github.com/anmaped/rtmlib) documentation for more details.

### NuttX OS

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

#### Compile existing demos

After this step, we shall link the monitors as a standalone app or module as provided in [rtmlib2 examples](https://github.com/anmaped/rtmlib) directory.

The monitors to be used with Ardupilot should be included in the external Px4 makefile `px4_common.mk` in the Ardupilot's directory `modules/Px4`. Check [these instructions](nuttx-ardupilot.md) for more details.


### FreeRTOS

Please refer to [rtmlib2](https://github.com/anmaped/rtmlib) documentation for more details.


### Hardware (FPGA)

[TBD]



### To show the mapping between the propositions and the symbols.

Let us take the previous example with three propositions `a, b, c`, which are mapped to instrumentation and monitor side in a different way.
Using ASCII characters or strings is not the most efficient way to do this, as they will quickly increase the memory footprint. We will now see how we can avoid this.

The content of the file `Rtm_compute_0746.h` describes where the symbols can appear as simple numbers.

```C++
// Propositions
enum _auto_gen_prop
{
  PROP_a = 3,
  PROP_b = 2,
  PROP_c = 1,
};
```

In fact, `a`,`b` and `c` have been mapped to a kind of binary enumeration using a hash table (i.e. `0x03` for a, `0x02` for `b`, and `0x01` for `c`). To match this encoding when generating events describing `a,b,c` we need to use the macro definitions `PROP_a,PROP_b,PROP_c`. If we have a symbol `sym` then the corresponding macro should be `PROP_sym`.

Although the rtmtld3synth tool does a check and all variable names accepted by the DSL are safe, we need to be aware of the macro variable names accepted in C++11 when describing the names of propositions in other RMTLD3 specifications.

```C++
#ifdef RTMLIB_ENABLE_MAP_SORT
#include <string>
#include <unordered_map>

// Create an unordered_map of sorts (that map to integers)
std::unordered_map<std::string, int> _mapsorttostring = {
  { "a", PROP_a },
  { "b", PROP_b },
  { "c", PROP_c },
};

// Create an unordered_map of sorts (that map to strings)
std::unordered_map<int, std::string> _mapsorttoint = {
  { PROP_a, "a" },
  { PROP_b, "b" },
  { PROP_c, "c" },
};
#endif
```
The map function `_mapsorttostring' is also available if we do not want to work with sorts and want to use strings instead.

### Intrumentation side

Since version `0.4` all propositions are defined inside the writer. There is no way to send other symbols that are not defined there.
```C++
using Writer = Writer_rtm__0746;

Writer::a // means PROP_a
...
Writer::c // means PROP_c
```


## Translate rmtld3 to smtlibv2

To solve the formula `(LessThan (Constant 0) (Duration (Constant 10) (Prop A)))`, we can follow these steps:

1. Generate the Z3 input file by using the command `rmtld3synth --synth-smtlibv2 --input-sexp <this-formula> --out-file <output-file-name>`.

2. Run the Z3 solver with the generated file to obtain either a sat or unsat result.

3. If you want to directly call Z3 from our tool, you can use the flag `--solver-z3`.

4. To retrieve a satisfiable trace, use the flag `--get-trace`.

Please note that step 2 is optional.


## Configuration File (optional)

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

`gen_tests` sets the automatic generations of test cases (see other options below).

`minimum_inter_arrival_time` establishes the minimum inter-arrival time that the events can have. It is a very pessimistic setting but provides some information for static checking.

`maximum_period` sets the maximum interval between two consecutive releases of tasks' jobs. It has a correlation between the periodic monitor and the minimum inter-arrival time. It provides static checks according to the size of time-stamps of events.

`event_subtype` provides the type for the event data. In that case, it is an identifier that can distinct 255 events. However, if more events are required, the type should be modified to *uint32_t* or greater. The number of different events versus the available size for the identifier is also statically checked.

`cluster_name` identifies the set of monitors. It acts as a label for grouping monitor specifications.

### unit tests flags

rmtld3synth also supports the automatic generation of unit tests for C++ based on the oracle deployed in Ocaml. Based on it, we test the expected evaluation of a set of formulas against the evaluation in the embedded platform.

There are two flags. The `gen_unit_tests` enables the automatic generation of monitors including the makefiles, and the  `gen_concurrency_tests` that instructs the construction of a set of tests for the performance evaluation of the rmtlib.