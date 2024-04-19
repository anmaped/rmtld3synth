
# Domain Specification Language

The BNF representation of the domain specification language is provided below:

```ocaml
<F> ::==                             (* Formulas *)
| <proposition>                      (* atomic proposition *)
| "~" <F>                            (* negation *)
| <F> "->" <F>                       (* implies *)
| <F> "&&" <F>                       (* and *)
| <F> "||" <F>                       (* or *)
| <T> "<" <T>                        (* less than *)
| <T> ">" <T>                        (* greater than *)
| <T> "<=" <T>                       (* less or equal than *)
| <T> ">=" <T>                       (* greater or equal than *)
| <F> "until" <F> "within" <C>       (* until operator *)
| <F> "since" <F> "within" <C>       (* since operator *)
| "always" <F> "within" <C>          (* always shorthand *)
| "historically" <F> "within" <C>    (* historically shorthand *)
| "eventually" <F> "within" <C>      (* eventually shorthand *)
| "past eventually" <F> "within" <C> (* past eventually shorthand *)
| "next" <F> "within" <C>            (* next shorthand: false U<T F *)
| "previous" <F> "within" <C>        (* previous shorthand: false S<T F *)
| "rise" <F> "within" <C>            (* rise shorthand *)
| "fall" <F> "within" <C>            (* fall shorthand *)

<T> ::==                             (* Terms *)
| <alpha>                            (* alpha *)
| "duration of" <F> "in" <I>         (* duration *)
| <T> "+" <T>                        (* sum *)
| <T> "*" <T>                        (* multiplication *)

<I> ::==                             (* Interval *)
| "[" <I> "," <I> "]"                (* Closed Interval *)
| <I> ".." <I>                       (* [ I, I [ *)
| <T>

<U> ::==                             (* Units *)
| "s"                                (* seconds *)
| "ms"                               (* milisenconds *)
| "us"                               (* microseconds *)
| "ns"                               (* nanoseconds *)

<C> ::==                             (* Time Constraint *)
| "=" <integer> <U>
| "<=" <integer> <U>
| <integer> <U>
```

## Examples

1)
![Wave 1](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave1.json)

2)
![Wave 2](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave2.json)

3)
![Wave 3](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave3.json)

4)
![Wave 4](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave4.json)