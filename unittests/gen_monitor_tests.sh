#!/bin/sh

set -x
set -e

mkdir gtests

# "(\eventually_{<2} a) \land (\eventually_{<2} b) \land (\eventually_{<6} c)" SAT
# "(\eventually_{<1} a) \land (\eventually_{<1} b)"                            UNSAT
# "(\eventually_{<1} a) \land (\eventually_{<2} b)"                            SAT
# "\eventually_{=0} a \land (\eventually_{=1} b) \land (\eventually_{=2} c)"   SAT
# "\eventually_{=0} a \land (\eventually_{=1} b) \land (\eventually_{=1} c)"   UNSAT

# SAT:
# "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))"
# "\always_{<6} a \land (\eventually_{<6} ((\neg b) \until_{=6} b ) )" 
# "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )"

# UNSAT:
# "\always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) )"

declare -a arrayrmtld=(
  "\always_{<6} a \land (\eventually_{<7} b )"
  "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))"
  "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )"
  "\eventaully_{=2} a \land \eventually_{=3} b \land \eventually_{=4} c"
  "\eventually_{=4} a \land (\eventually_{=5} b ) \land \eventually_{=2} c"
  "\always_{=4} a \land (\eventually_{=4} b )"
  "\neg ( \always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) ) )" #VALID FORMULA
#
  "a \land \always_{< b1 } a \rightarrow \eventually_{=2} a"
  "(p \lor q) \ \until_{<b1} r "
  "\int^{b1} p < 3"
  "\left( (p \lor q) \ \until_{<b1} r \right) \land \int^{9} r < 2"
  "\neg (\left( (p \lor q) \ \until_{<b1} r \right) \land 10 < \int^{9} r)" #VALID FORMULA
  "\neg ( \eventually_{<b1}  p \land \always_{<b2} \neg p )" #VALID FORMULA
  "\always_{<b2} (a \lor b) \ \until_{<b1} r"
)
arrayrmtldlength=${#arrayrmtld[@]}


echo "Generating Test Units for Monitor Generation using Ocaml"

CMDGENOCAML="../rmtld3synth.native --config-file "../config/default" --synth-ocaml"

$CMDGENOCAML --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > gtests/mon1.ml

$CMDGENOCAML --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > gtests/mon2.ml

$CMDGENOCAML --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" > gtests/mon3.ml


echo "Generating Test Units for Cpp11"

CMDGENCPP="../rmtld3synth.native --config-file "../config/default" --synth-cpp11"

$CMDGENCPP --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" --out-src="gtests/mon1" --verbose 2

$CMDGENCPP --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src="gtests/mon2" --verbose 2

$CMDGENCPP --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" --out-src="gtests/mon3" --verbose 2


echo "Generating Unit tests for smtlibv2"

# more interesting formulas
CMDSAT="../rmtld3synth.native --synth-smtlibv2 --solver-z3 --recurvive-unrolling --get-trace --trace-style "tinterval" --input-latexeq"

sample=10 # sample can be changed
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	REP=${arrayrmtld[$i-1]//b1/$sample}
    REPP=${REP//b2/$sample}
	$CMDSAT "$REPP" > gtests/res$i.trace
	$CMDGENOCAML --input-latexeq "$REPP" > gtests/res$i.ml
done


echo "Generating the Makefile...."
echo "

all:
	ocamlbuild -use-ocamlfind unittests.byte unittests.native
	cd gtests/mon1; make x86-monitor; cd ../..
	cd gtests/mon2; make x86-monitor; cd ../..

clean:
	ocamlbuild -clean
	rm -f -- unittests.ml *.byte *.native

" > Makefile

ocamlgen=""
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
	ocamlgen+="
open Res$i
module T$i : Trace = struct let trc = [$(dos2unix gtests/res$i.trace; cat gtests/res$i.trace)] end;;
module M$i = Res$i.Mon0(T$i);;
print_endline (Rmtld3.b3_to_string M$i.mon);
"
done

echo "
open Mon1
open Mon2
open Mon3

$ocamlgen" > unittests.ml

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
	
	rm -r -f gtests

fi
