#!/bin/sh

ocamlgen=""
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	ocamlgen+="
open Res${i}
module T$i : Trace = struct let trc = [$(dos2unix $TEST_DIR/res$i.trace; cat $TEST_DIR/res$i.trace)] end;;
module M$i = Res${i}.Res${i}_0(T$i);;
print_string \"$i) \";;
if M$i.mon = Rmtld3_eval.True then print_endline (\"\x1b[32m[true]\x1b[0m\") 
else (if M$i.mon = Rmtld3_eval.False then print_endline (\"\x1b[31m[false]\x1b[0m\")
      else print_endline (\"\x1b[33m[unknown]\x1b[0m\"))
"
done

echo "
open Mon1
open Mon2
open Mon3

$ocamlgen" > $TEST_DIR/unittests.ml

modules=""
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	modules+="Res${i} "
done

echo "
(executable
  (name            unittests)
  (public_name     unittests)
  (libraries       bigarray-compat)
  (modules         Rmtld3_eval Unittests Mon1 Mon2 Mon3 $modules)
)
" > $TEST_DIR/dune

echo "(lang dune 1.5)
" > $TEST_DIR/dune-project

echo "
" > $TEST_DIR/unittests.opam
