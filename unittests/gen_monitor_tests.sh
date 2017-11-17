#!/bin/sh

set -x

PWD=$(pwd)

echo "Generating Test Units for Monitor Generation"

../rmtld3synth.native --config-file "../config/default" --synth-ocaml --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > mon1.ml

../rmtld3synth.native --config-file "../config/default" --synth-ocaml --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > mon2.ml

../rmtld3synth.native --config-file "../config/default" --synth-ocaml --input-latexeq "\always_{< 4} \ a \rightarrow \eventually_{= 2} \ b" > mon3.ml

echo "Generating the Makefile...."
echo "

all:
	ocamlbuild -use-ocamlfind unittests.byte unittests.native

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
