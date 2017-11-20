#!/bin/sh

set -x


echo "Generating Test Units for Monitor Generation using Ocaml"

../rmtld3synth.native --config-file "../config/default" --synth-ocaml --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > mon1.ml

../rmtld3synth.native --config-file "../config/default" --synth-ocaml --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > mon2.ml

../rmtld3synth.native --config-file "../config/default" --synth-ocaml --input-latexeq "\always_{< 4} \ a \rightarrow \eventually_{= 2} \ b" > mon3.ml


echo "Generating Test Units for Cpp11"

../rmtld3synth.native --config-file "../config/default" --synth-cpp11 --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" --out-src=./mon1/ --verbose 2

../rmtld3synth.native --config-file "../config/default" --synth-cpp11 --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src=./mon2/ --verbose 2

../rmtld3synth.native --config-file "../config/default" --synth-cpp11 --input-latexeq "\always_{< 4} \ a \rightarrow \eventually_{= 2} \ b" --out-src=./mon3/ --verbose 2


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

" > unittests.ml

cp ../rmtld3.ml rmtld3.ml

make

# execute and get result

make clean

# remove files
rm Makefile
rm rmtld3.ml
rm mon1.ml
rm mon2.ml
rm mon3.ml

#remove cpp11 files
rm -r -f mon1/
rm -r -f mon2/
rm -r -f mon3/

