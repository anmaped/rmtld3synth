#!/bin/sh

PWD=$(pwd)

echo "Generating Test Units for Monitor Generation"
echo $PWD/mon1.ml

../rmtld3synth --config-file "../config/default" --synth-ocaml --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > mon1.ml

../rmtld3synth --config-file "../config/default" --synth-ocaml --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > mon2.ml

echo "Generating the Makefile...."
echo "RESULT := unittests
SOURCES := \
  rmtld3.ml \
  mon1.ml \
  mon2.ml
  


ANNOTATE := yes
USE_CAMLP4 := yes

PACKS := unix type_conv sexplib batteries


all: native-code

OCAMLMAKEFILE := ../OCamlMakefile
include \$(OCAMLMAKEFILE)" > Makefile

cp ../rmtld3.ml rmtld3.ml

make

# execute and get result

make clean

# remove files
rm Makefile
rm rmtld3.ml
rm mon1.ml
rm mon2.ml