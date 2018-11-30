#!/bin/sh

set -x
set -e

TEST_DIR=_gtest

mkdir $TEST_DIR
mkdir $TEST_DIR/cpp

BINDIR=_build/install/default/bin
CMDGENOCAML="../$BINDIR/rmtld3synth --config-file "../config/default" --synth-ocaml"
CMDGENCPP="../$BINDIR/rmtld3synth --config-file "../config/default" --synth-cpp11"
CMDSAT="../$BINDIR/rmtld3synth --synth-smtlibv2 --solver-z3 --recursive-unrolling --get-trace"

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
  "\neg ( \always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) ) )" #Negation
#
  "a \land \always_{< b1 } a \rightarrow \eventually_{=2} a"
  "(p \lor q) \ \until_{<b1} r "
  "\int^{b1} p < 3"
  "\left( (p \lor q) \ \until_{<b1} r \right) \land \int^{9} r < 2"
  "\neg (\left( (p \lor q) \ \until_{<b1} r \right) \land 10 < \int^{9} r)" #Negation
  "\neg ( \eventually_{<b1}  p \land \always_{<b2} \neg p )" #Negation
  "\always_{<b2} (a \lor b) \ \until_{<b1} r"
)
arrayrmtldlength=${#arrayrmtld[@]}


echo "Generating Test Units for Monitor Generation using Ocaml"

$CMDGENOCAML --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > $TEST_DIR/mon1.ml

$CMDGENOCAML --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > $TEST_DIR/mon2.ml

$CMDGENOCAML --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" > $TEST_DIR/mon3.ml


echo "Generating Test Units for Cpp11"

$CMDGENCPP --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" --out-src="$TEST_DIR/mon1" --verbose 2

$CMDGENCPP --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src="$TEST_DIR/mon2" --verbose 2

$CMDGENCPP --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" --out-src="$TEST_DIR/mon3" --verbose 2

# Add these specific makefile rules
CPP_TO_BUILD="\tmake -C mon1 RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n"
CPP_TO_BUILD+="\tmake -C mon2 RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n"
CPP_TO_BUILD+="\tmake -C mon3 RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n"

# Automatic generation of monitors from a set of formulas
sample=10 # this sample can be changed
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
  REP=${arrayrmtld[$i-1]//b1/$sample}
  REPP=${REP//b2/$sample}
  $CMDSAT --trace-style "tcum" --input-latexeq "$REPP" > $TEST_DIR/cpp/res$i.trace
  $CMDGENCPP --input-latexeq "$REPP" --out-src="$TEST_DIR/cpp/mon$i"
done

# Add auto-generated makefile rules
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
    CPP_TO_BUILD+="	make -C cpp/mon$i RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n"
done


echo "Generating Unit tests for smtlibv2"

sample=10 # this sample can be changed
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
  REP=${arrayrmtld[$i-1]//b1/$sample}
  REPP=${REP//b2/$sample}
  $CMDSAT --trace-style "tinterval" --input-latexeq "$REPP" > $TEST_DIR/res$i.trace
  $CMDGENOCAML --input-latexeq "$REPP" > $TEST_DIR/res$i.ml
done


echo "Generating the Makefile...."

CHECK_GCC='CXX = g++
ifeq ($(OS),Windows_NT)
  CXX_NAMES = x86_64-w64-mingw32-g++ i686-w64-mingw32-g++
  CXX := $(foreach exec,$(CXX_NAMES),$(if $(shell which $(exec)),$(exec),))
  ifeq ($(CXX),)
    $(error \"No $(exec) in PATH\")
  endif
endif

CXX := $(shell echo "$(CXX)" | cut -f 1 -d " ")
'
CXX_INC='$(CXX)'

echo -e "

$CHECK_GCC

all:
	dune build -p unittests @install
	dune install -p unittests --prefix=./
$CPP_TO_BUILD
	$CXX_INC -Wall -Wextra -std=gnu++11 -D__x86__ -DUSE_UNSAFE_METHODS -DUSE_MAP_SORT -DUSE_DEBUGV_RMTLD3_ -DDEBUG=3 -I$(pwd)/../rtmlib -pthread -lm $(pwd)/../rtmlib/RTML_monitor.cpp cpptest.cpp -o cpptest

clean:
	ocamlbuild -clean
	rm -f -- unittests.ml *.byte *.native
	rm cpptest

" > $TEST_DIR/Makefile

. ./gen_ocaml.sh
. ./gen_cpp.sh

# copy auxiliar files for ocaml synthesis
cp ../src/rmtld3.ml $TEST_DIR/rmtld3.ml

make -C $TEST_DIR

# show results from ocaml synthesis
echo -e "\e[1m### result from ocaml synthesis\e[0m"
./$TEST_DIR/bin/unittests 2>&1

#show results from cpp synthesis
echo -e "\e[1m### result from cpp synthesis\e[0m"
./$TEST_DIR/cpptest 2>&1

../$BINDIR/unittest

# read -p "Press enter to continue or wait 90s" -t 90
if read -r -s -n 1 -t 90 -p "Press enter to abort" key #key in a sense has no use at all
then
  echo "aborted"
else
  echo "continued"
  rm -r -f $TEST_DIR
fi
