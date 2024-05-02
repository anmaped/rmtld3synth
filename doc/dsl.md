
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
    ```
    always (a until b within 3s) within 10s
    ```

    "Always, within a span of 3 seconds, condition 'a' holds true until condition 'b' is met, and this must hold true throughout the entire 10 seconds span."

    This implies that 'a' must always be true until 'b' becomes true, and the transition from 'a' to 'b' must happen within 3 seconds. Furthermore, this entire sequence must also occur within a 10-time-unit window.

2) 
    ```
    always ((rise a) -> (eventually b within 3s)) within 10s
    ```

    "Always, within a span of 10 seconds, if 'a' rises, then 'b' will eventually occur within 3 seconds."

    This means that whenever 'a' rises, it is always followed by 'b' within 3 units of time, and this pattern must hold true throughout the entire 10-time-unit window.

3)
    ```
    always ((rise a) -> (b on 3s)) within 10s
    ```

    or equivalent expression

    ```
    always ((rise a) -> (eventually b within =3s)) within 10s
    ```

    "Always, within a span of 10 units of time, if 'a' rises, then 'b' will occur exactly on the 3rd-second."

    This means that whenever 'a' rises, it is always followed by 'b' exactly on the 3rd-time-unit, and this pattern must hold true throughout the entire 10-time-unit window.

    - 3.a
        ![Wave 1](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave1.json)

    - 3.b
        ![Wave 2](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave2.json)

    - 3.c
        ![Wave 3](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave3.json)

    - 3.d
        ![Wave 4](https://svg.wavedrom.com/github/anmaped/rmtld3synth/v0.4/doc/waves/wave4.json)