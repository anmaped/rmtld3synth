#!/bin/sh

#set -x
set -e

err_report() {
    echo "Error on line $1"
}

trap 'err_report $LINENO' ERR

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
WHITE='\033[1;37m'

[ -f "`ocamlfind query z3`/libz3.dylib" ] && export DYLD_LIBRARY_PATH=`ocamlfind query z3`

TEST_DIR=_gtest

mkdir -p $TEST_DIR
mkdir -p $TEST_DIR/cpp
mkdir -p $TEST_DIR/sat

BINDIR=_build/install/default/bin
export PATH=$PATH:$(pwd)/../_build/install/default/bin
CMDGENOCAML="rmtld3synth --config-file "../config/default" --synth-ocaml"
CMDGENCPP="rmtld3synth --config-file "../config/default" --synth-cpp11"

[ "$2" = "" ] || [ "$2" = "discrete" ] && {
echo "UNROLLING_SETTINGS=\"--assume-unary-seq --rec-unrolling=auto\""
UNROLLING_SETTINGS="--assume-unary-seq --rec-unrolling=auto"
}

[ "$2" = "interval" ] && {
echo "UNROLLING_SETTINGS=\"--rec-unrolling=auto\""
UNROLLING_SETTINGS="--rec-unrolling=auto"
}

CMDSAT_DEBUG="rmtld3synth --synth-smtlibv2 $UNROLLING_SETTINGS"
CMDSAT_NO_TRACE="rmtld3synth --synth-smtlibv2 --solver-z3 $UNROLLING_SETTINGS"
CMDSAT="$CMDSAT_NO_TRACE --get-trace"

# "(\eventually_{<2} a) \land (\eventually_{<2} b) \land (\eventually_{<6} c)" SAT (1-assumption)
# "(\eventually_{<1} a) \land (\eventually_{<1} b)"                            UNSAT (1-assumption)
# "(\eventually_{<1} a) \land (\eventually_{<2} b)"                            SAT (1-assumption)
# "\eventually_{=0} a \land (\eventually_{=1} b) \land (\eventually_{=2} c)"   SAT (1-assumption)
# "\eventually_{=0} a \land (\eventually_{=1} b) \land (\eventually_{=1} c)"   UNSAT (1-assumption)

# SAT (1-assumption):
# "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))"
# "\always_{<6} a \land (\eventually_{<6} ((\neg b) \until_{=6} b ) )"
# "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )"

# UNSAT (1-assumption):
# "\always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) )"

declare -a arrayrmtld=(
  "p"
  "P \lor \not p"
  "p \until_{<5} q"
  "p \until_{=5} q"
#  "p \until_{\leq 5} q"
  "\always_{<6} a \land (\eventually_{<7} b )"
  "\eventually_{<6} a \land (\eventually_{<2} (\always_{<3} b ))"
  "\always_{<6} a \land (\eventually_{<5} ((\neg b) \until_{=4} b ) )"
  "\eventually_{=2} a \land \eventually_{=3} b \land \eventually_{=4} c"
  "\eventually_{=4} a \land (\eventually_{=5} b ) \land \eventually_{=2} c"
  "\always_{=4} a \land (\eventually_{=4} b )"
  "\always_{<4} a \land ( (\neg c) \until_{=4} b )" # *NEW*
  "\neg ( \always_{<6} a \land ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) )" # *NEW*
  "\neg ( \always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) ) )" #valid (1-assumption)
  "a \land \always_{< b1 } a \rightarrow \eventually_{=2} a"
  "(p \lor q) \ \until_{<b1} r "
  "\int^{b1} p < 3"
  "\left( (p \lor q) \ \until_{<b1} r \right) \land \int^{9} r < 2"
  "\neg (\left( (p \lor q) \ \until_{<b1} r \right) \land 10 < \int^{9} r)" #valid (1-assumption)
  "\neg ( \eventually_{<b1}  p \land \always_{<b2} \neg p )" #valid (1-assumption)
  "\always_{<b2} (a \lor b) \ \until_{<b1} r"
  "\int^{10} a > 1 \land \int^{10} b > 2 \land \int^{10} c > 3 \land \int^{10} a \lor b \lor c < 10"
  "\int^{10} a + 1 > 1 \land \int^{10} b > 2 \land \int^{10} c > 3 \land \int^{10} a \lor b \lor c < 9"
  "\neg ( \int^{10} a + 0 > 1 \land \int^{10} b > 2 \land \int^{10} c > 3 \land \int^{10} a \lor b \lor c < 9)" #valid (1-assumption)
  "\int^{10} a * 2 > 1 \land \int^{10} b > 2 \land \int^{10} c > 3 \land \int^{10} a \lor b \lor c < 9"
  "\int^{10} a \lor b + \int^{10} a \land b = \int^{10} a + \int^{10} b"
)

arrayrmtldlength=${#arrayrmtld[@]}

declare -a arrayrmtld_sat=(
  "p \land q"
  "p \until_{<5} q"
  "10 < \int^{9} r"
  "3 < \int^{9} r"
  "\always_{<6} a \land (\eventually_{<6} ( ( (\neg a) \land (\neg b) ) \until_{=6} b ) )"
  "\left( (p \lor q) \ \until_{<10} r \right) \land 10 < \int^{9} r"
  "\eventually_{<10}  p \land \always_{<10} \neg p"
  "\int^{10} a + 0 > 1 \land \int^{10} b > 2 \land \int^{10} c > 3 \land \int^{10} a \lor b \lor c < 9"
# "\neg ( ((\int^{x} a \lor b + \int^{x} ( a \land b ) ) = \int^{x} a + \int^{x} b ) )" #unsat (but we need induction to prove it)
  "((\int^{x} a \lor b + \int^{x + y} ( a \land b ) ) = \int^{x + y} a + \int^{x} b ) \land x > 0 \land y > 10" #sat but not monitorable without decomposition
  "\int^{10} a + x > 1 \land \int^{10} b > 2 \land \int^{10} c > 3 \land \int^{10} a \lor b \lor c < 9" #sat but not monitorable without decomposition
)

arrayrmtld_satlength=${#arrayrmtld_sat[@]}

declare -a arrayrmtld_sat_expected_result=(
  "unsatisfiable"
  "satisfiable"
  "unsatisfiable"
  "satisfiable"
  "unsatisfiable"
  "unsatisfiable"
  "unsatisfiable"
  "unsatisfiable"
  # "unsatisfiable"
  "satisfiable"
  "satisfiable"
)

[ "$1" = "" ] && {

echo -e "unknown command\nUsage: test.sh [quickcpp/sat/crosscheck/allchecks]"
exit 1;
}

[ "$1" = "sat" ] || [ "$1" = "allchecks" ] && {

echo "Satisfiability check of rmtld3 formulas" ;

for (( i=1; i<${arrayrmtld_satlength}+1; i++ ));
do
  FORMULA=${arrayrmtld_sat[$i-1]}
  EXPECT_RES=${arrayrmtld_sat_expected_result[$i-1]}
  $CMDSAT_DEBUG --input-latexeq "$FORMULA" > $TEST_DIR/sat/fm_$i.smt2
  $CMDSAT_NO_TRACE --trace-style "tcum" --input-latexeq "$FORMULA" > $TEST_DIR/sat/fm_$i.result
  $CMDSAT --trace-style "tcum" --input-latexeq "$FORMULA" > $TEST_DIR/sat/fm_$i.result2
  # check result against expected output
  RES=$(head -n 1 "$TEST_DIR/sat/fm_$i.result" | tr -d '\n' | tr -d '\r')
  if [ "$EXPECT_RES" == "$RES" ];
  then
    printf "$i: \"expected: $EXPECT_RES/result: $RES\" ${GREEN}Successful${NC}\n"
  else
    printf "$i: \"expected: $EXPECT_RES/result: $RES\" ${RED}Fail${NC}\n"
  fi
done ;
}

[ "$1" = "quickcpp" ] || [ "$1" = "allchecks" ] && {

echo "Executing quick cpp test..." ;

rmtld3synth-unittest ;

make -C $TEST_DIR/../_cluster/tests ;

./$TEST_DIR/../_cluster/tests/tests ;

}

[ "$1" = "crosscheck" ] || [ "$1" = "allchecks" ] && {

echo "Generating Test Units for Monitor Generation using Ocaml" ;

$CMDGENOCAML --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" > $TEST_DIR/mon1.ml ;

$CMDGENOCAML --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" > $TEST_DIR/mon2.ml ;

$CMDGENOCAML --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" > $TEST_DIR/mon3.ml ;



echo "Generating Test Units for Cpp11" ;

$CMDGENCPP --input-sexp "(Or (Until 10 (Prop D) (Or (Prop A) (Not (Prop B)))) (LessThan (Duration (Constant 2) (Prop S) ) (FPlus (Constant 3) (Constant 4)) ))" --out-src="$TEST_DIR/mon1" --verbose 2 > /dev/null 2>&1 ;

$CMDGENCPP --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} c < 4" --out-src="$TEST_DIR/mon2" --verbose 2 > /dev/null 2>&1 ;

$CMDGENCPP --input-latexeq "\always_{< 4} a \rightarrow \eventually_{= 2} b" --out-src="$TEST_DIR/mon3" --verbose 2 > /dev/null 2>&1 ;

# Add these specific makefile rules
CPP_TO_BUILD="\tmake -C mon1 RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n" ;
CPP_TO_BUILD+="\tmake -C mon2 RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n" ;
CPP_TO_BUILD+="\tmake -C mon3 RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n" ;

# Automatic generation of monitors from a set of formulas
sample=10 # this sample can be changed
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
  REP=${arrayrmtld[$i-1]//b1/$sample}
  REPP=${REP//b2/$sample}
  $CMDSAT --trace-style "tcum" --input-latexeq "$REPP" > $TEST_DIR/cpp/res$i.trace
  $CMDGENCPP --input-latexeq "$REPP" --out-src="$TEST_DIR/cpp/mon$i" > /dev/null 2>&1
done ;

# Add auto-generated makefile rules
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
    CPP_TO_BUILD+="	make -C cpp/mon$i RTMLIB_INCLUDE_DIR=$(pwd)/../rtmlib x86-monitor\n"
done ;


echo "Generating Unit tests for smtlibv2" ;

sample=10 ; # this sample can be changed
for (( i=1; i<${arrayrmtldlength}+1; i++ ));
do
  REP=${arrayrmtld[$i-1]//b1/$sample}
  REPP=${REP//b2/$sample}
  $CMDSAT_DEBUG --input-latexeq "$REPP" > $TEST_DIR/res$i.smt2
  $CMDSAT --trace-style "tinterval" --input-latexeq "$REPP" > $TEST_DIR/res$i.trace
  $CMDGENOCAML --input-latexeq "$REPP" > $TEST_DIR/res$i.ml
done ;


echo "Generating the Makefile...." ;

CHECK_GCC='CXX = g++
ifeq ($(OS),Windows_NT)
  CXX_NAMES = x86_64-w64-mingw32-g++ i686-w64-mingw32-g++
  CXX := $(foreach exec,$(CXX_NAMES),$(if $(shell which $(exec)),$(exec),))
  ifeq ($(CXX),)
    $(error \"No $(exec) in PATH\")
  endif
endif

CXX := $(shell echo "$(CXX)" | cut -f 1 -d " ")
' ;
CXX_INC='$(CXX)' ;

echo -e "

$CHECK_GCC

all:
	dune build -p unittests @install
	dune install -p unittests --prefix=./
$CPP_TO_BUILD
	$CXX_INC -Wall -Wextra -std=gnu++11 -D__x86__ -DUSE_UNSAFE_METHODS -DUSE_MAP_SORT -DUSE_DEBUG_RMTLD3 -DUSE_DEBUGV_RMTLD3XX -DDEBUG=0 -I$(pwd)/../rtmlib -pthread -lm $(pwd)/../rtmlib/RTML_monitor.cpp cpptest.cpp -o cpptest

clean:
	ocamlbuild -clean
	rm -f -- unittests.ml *.byte *.native
	rm cpptest

" > $TEST_DIR/Makefile ;

. ./gen_ocaml.sh ;
. ./gen_cpp.sh ;

# copy auxiliar files for ocaml synthesis
cp ../src/rmtld3.ml $TEST_DIR/rmtld3.ml ;

printf "${WHITE}Compiling Ocaml and Cpp11 monitors...${NC}\n" ;

make -C $TEST_DIR > /dev/null 2>&1 ;

# show results from ocaml synthesis
echo -e "\e[1m### result from ocaml synthesis\e[0m" ;
./$TEST_DIR/bin/unittests 2>&1 ;

#show results from cpp synthesis
echo -e "\e[1m### result from cpp synthesis\e[0m" ;
./$TEST_DIR/cpptest 2>&1 ;

echo "" ;
printf "${GREEN}Tests ended Successfully.${NC}\n\n" ;

}

# read -p "Press enter to continue or wait 90s" -t 90
if read -r -s -n 1 -t 15 -p "Press enter to abort" key #key in a sense has no use at all
then
  echo " aborted"
else
  echo " continued"
  rm -r -f $TEST_DIR
fi
