# Input Languages

rtmtld3synth has three major input languages.

## With DSL (prefered)

Documentation available [here](dsl.md).

## With symbolic expressions (old style)

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



## LaTex expressions for paper writing purposes

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
