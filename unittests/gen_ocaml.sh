#!/bin/sh

ocamlgen=""
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	ocamlgen+="
open Res$i
module T$i : Trace = struct let trc = [$(dos2unix gtests/res$i.trace; cat gtests/res$i.trace)] end;;
module M$i = Res$i.Mon0(T$i);;
print_string \"$i) \";;
if M$i.mon = Rmtld3.True then print_endline (\"\x1b[32m[true]\x1b[0m\") else (if M$i.mon = Rmtld3.False then print_endline (\"\x1b[31m[false]\x1b[0m\") else print_endline (\"\x1b[33m[unknown]\x1b[0m\"))
"
done

echo "
open Mon1
open Mon2
open Mon3

$ocamlgen" > unittests.ml
