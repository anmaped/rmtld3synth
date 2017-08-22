
# Examples using latex equations as input language

Here, we will exemplify all the ways to synthesize the available input languages using rmtld3synth tool.

# For Ocaml synthesis

```
cd examples
../rmtld3synth.exe --synth-ocaml --input-latexeq
    "(a \rightarrow ((a \lor b) \until{<10} c)) \land \int^{10} c < 4"
```
The file that was generated is `texeq_sample.ml`.


# For cpp11 synthesis

```
cd examples
../rmtld3synth.exe --synth-cpp11 --input-latexeq
    "(a \rightarrow ((a \lor b) \until{<10} c)) \land \int^{10} c < 4"
```
The files that were genereated are `texeq_sample_mon0.h` and `texeq_sample_mon0_compute.h` .


## For SMTLibv2 synthesis

```
cd examples
../rmtld3synth.exe --synth-smtlibv2 --input-latexeq "`cat usecaseone.texeq`"
```

[To be completed.]


# For rmdsl synthesis

```
cd examples
../rmtld3synth.exe --synth-smtlibv2 --input-rmdsl
    "\rm{core_0}{ \tk{ts1}{10,8} \succ \tk{ts2}{5,20} \bowtie \tk{ts3}{7,27} }{\pi,\theta}"
```

[To be completed.]
