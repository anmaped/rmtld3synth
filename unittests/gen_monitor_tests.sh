#!/bin/sh

set -x
set -e

echo "Generating Test Units for Monitor Generation using Ocaml"

CMDGENOCAML="../rmtld3synth.native --config-file "../config/default" --synth-ocaml"

$CMDGENOCAML --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > mon1.ml

$CMDGENOCAML --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > mon2.ml

$CMDGENOCAML --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" > mon3.ml

$CMDGENOCAML --input-latexeq "\always_{<6} a \land (\eventually_{<7} b )" > mon_sat1.ml

$CMDGENOCAML --input-latexeq "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))" > mon_sat2.ml

$CMDGENOCAML --input-latexeq "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )" > mon_sat3.ml

echo "Generating Test Units for Cpp11"

CMDGENCPP="../rmtld3synth.native --config-file "../config/default" --synth-cpp11"

$CMDGENCPP --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" --out-src=mon1 --verbose 2

$CMDGENCPP --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src=mon2 --verbose 2

$CMDGENCPP --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" --out-src=mon3 --verbose 2

echo "Generating Unit tests for smtlibv2"

# "(\eventually_{<2} a) \land (\eventually_{<2} b) \land (\eventually_{<6} c)" SAT
# "(\eventually_{<1} a) \land (\eventually_{<1} b)"                            UNSAT
# "(\eventually_{<1} a) \land (\eventually_{<2} b)"                            SAT
# "\eventually_{=0} a \land (\eventually_{=1} b) \land (\eventually_{=2} c)"   SAT
# "\eventually_{=0} a \land (\eventually_{=1} b) \land (\eventually_{=1} c)"   UNSAT

# more interesting formulas
CMDSAT="../rmtld3synth.native --synth-smtlibv2 --solve-z3 --get-trace --trace-style "tinterval" --input-latexeq"
# SAT:
$CMDSAT "\always_{<6} a \land (\eventually_{<7} b )" > mon_sat1.trace
# "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))"
$CMDSAT "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))" > mon_sat2.trace
# "\always_{<6} a \land (\eventually_{<6} ((\neg b) \until_{=6} b ) )" 
# "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )"
$CMDSAT "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )" > mon_sat3.trace

# UNSAT:
# "\always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) )"
$CMDSAT "\always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) )" > mon_sat4.trace

echo "Generating the Makefile...."
echo "

all:
	ocamlbuild -use-ocamlfind unittests.byte unittests.native
	cd mon1; make x86-monitor; cd ..
	cd mon2; make x86-monitor; cd ..

clean:
	ocamlbuild -clean
	rm -f -- unittests.ml *.byte *.native

" > Makefile

echo "
open Mon1
open Mon2
open Mon3


(* (a a a a a a idle idle a idle idle b b a a a a a a a a) *)
(*module OneTrace : Trace = struct let trc = [(\"a\",(0.,6.));(\"idle\",(6.,8.));(\"a\",(8.,9.));(\"idle\",(9.,11.));(\"b\",(11.,13.));(\"a\",(13.,20.));] end;;
*)
(* (a a a a a a b a a a a a a a a a a a a a a) *)
(*module OneTrace : Trace = struct let trc = [(\"a\",(0.,6.));(\"b\",(6.,7.));(\"a\",(7.,20.));] end;;
*)

open Mon_sat1
module OneTrace : Trace = struct let trc = [$(dos2unix mon_sat1.trace; cat mon_sat1.trace)] end;;
module Mon = Mon_sat1.Mon0(OneTrace);;
print_endline (Rmtld3.b3_to_string Mon.mon);

open Mon_sat2
module OneTrace2 : Trace = struct let trc = [$(dos2unix mon_sat2.trace; cat mon_sat2.trace)] end;;
module Mon2 = Mon_sat2.Mon0(OneTrace2);;
print_endline (Rmtld3.b3_to_string Mon2.mon);

open Mon_sat3
module OneTrace3 : Trace = struct let trc = [$(dos2unix mon_sat3.trace; cat mon_sat3.trace)] end;;
module Mon3 = Mon_sat3.Mon0(OneTrace3);;
print_endline (Rmtld3.b3_to_string Mon3.mon);

" > unittests.ml

cp ../src/rmtld3.ml rmtld3.ml

make

# execute and get result
./unittests.native

# read -p "Press enter to continue or wait 90s" -t 90
if read -r -s -n 1 -t 90 -p "Press enter to abort" key #key in a sense has no use at all
then
    echo "aborted"
else
    echo "continued"
	
	make clean

	# # remove files
	rm Makefile
	rm rmtld3.ml
	rm mon1.ml
	rm mon2.ml
	rm mon3.ml
	rm mon_sat1.ml
	rm mon_sat2.ml
	rm mon_sat3.ml
	rm mon_sat1.trace
	rm mon_sat2.trace
	rm mon_sat3.trace
	rm mon_sat4.trace

	# #remove cpp11 files
	rm -r -f mon1/
	rm -r -f mon2/
	rm -r -f mon3/

fi
