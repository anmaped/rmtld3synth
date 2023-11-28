
# Domain Specification Language


```ocaml
F                     (* Formulas *)
| p                   (* atomic proposition *)
| F -> F              (* implies *)
| F && F              (* and *)
| F || F              (* or *)
| T > T               (* greater than *)
| F until F within C  (* until operator *)
| F since F within C  (* since operator *)
| always F within C          (* always shorthand *)
| historically F within C    (* historically shorthand *)
| eventually F within C      (* eventually shorthand *)
| past eventually F within C (* past eventually shorthand *)
| next F within C     (* next shorthand: false U<T F *)
| previous F within C (* previous shorthand: false S<T F *)
| rise F within C     (* rise shorthand *)
| fall F within C     (* fall shorthand *)

T                     (* Terms *)
| a                   (* alpha *)
| duration of F in T  (* duration *)
| T + T               (* sum *)
| T * T               (* multiplication *)

U                     (* Units *)
| s
| ms
| us
| ns

C                     (* Time Constraint *)
| = i U
| <= i U
| i U
```
