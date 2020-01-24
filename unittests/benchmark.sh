#!/bin/bash

set -e

# bechmark file
echo "Initiating the benchmark process"

declare -a arrayrdsl=(
  "\rm{core0}{ \tk{ts1}{5,2} }{10,9}" # sat
  "\rm{core0}{ \tk{ts1}{5,4} }{10,9}" # unsat (overhead of one unit for the task releases)
  "\rm{core0}{ \tk{ts1}{15,2} }{30,9}" # sat
  #
  "\rm{core0}{ \tk{ts1}{12,2} \succ \tk{ts2}{10,1} }{1,1}" # sat
  "\rm{core0}{ \tk{ts1}{12,2} \succ \tk{ts2}{9,1} }{1,1}" # sat
  "\rm{core0}{ \tk{ts1}{12,2} \succ \tk{ts2}{8,1} }{1,1}" # sat
  "\rm{core0}{ \tk{ts1}{12,2} \succ \tk{ts2}{7,1} }{1,1}" # unsat (* to confirm using unsat cores *)
  "\rm{core0}{ \tk{ts1}{12,2} \succ \tk{ts2}{6,1} }{1,1}" # sat
  #
  "\rm{core0}{ \tk{ts1}{9,2} \succ \tk{ts2}{9,1} }{1,1}" #
	"\rm{core0}{ \tk{ts1}{9,2} \succ \tk{ts2}{9,1} }{20,8}" # unsat (without --recursive-unrolling; lcm=180)
  "\rm{core0}{ \tk{ts1}{9,2} \succ \tk{ts2}{9,7} }{20,8}" # unsat (without --recursive-unrolling; lcm=180)
  #"\rm{core0}{ \tk{ts1}{5,3} \succ \tk{ts2}{10,8} }{20,20}"
	"\rm{core0}{ \tk{ts1}{5,3} \succ \tk{ts2}{10,8} \succ \tk{ts3}{5,3} }{20,20}"
  #
  "\rm{server0}{ \tk{ts1}{15,2} }{1,1} \parallel \rm{server1}{ \tk{ts2}{20,3} }{1,1}"
  "\rm{server0}{ \tk{ts1}{15,2} }{1,1} \mid \rm{server1}{ \tk{ts2}{20,3} }{1,1}"
  #
	#"\rm{res0}{ \tk{ts1}{10,8} \succ \tk{ts2}{10,4} \bowtie \tk{ts3}{27,7} }{0,0} \parallel \rm{res1}{ \tk{ts4}{33,4} }{10,5}"
	#"\rm{core0}{ \tk{ts1}{20,9} \succ \tk{ts2}{15,8} }{60,50}"
	#"\rm{core0}{ \tk{ts1}{20,9} \succ \tk{ts2}{15,8} \succ \tk{ts3}{10,3} }{60,50}"
  "\rm{server0}{ \left( \tk{ts1}{10,8} \succ \tk{ts2}{20,5} \right) \bowtie \tk{ts3}{27,7} }{1,1} \mapsto core0  \parallel \rm{server1}{ \tk{ts4}{33,4} \succ \tk{ts5}{6,2} }{1,1} \mapsto core1" # unsat
  "\rm{server0}{ \tk{ts1}{9,8} \succ \tk{ts2}{3,1} }{20,12} \triangleleft \left( \evtskrunning_{(server_0,ts1)} \rightarrowtriangle \evtskrunning_{(server0,ts2)} \right)"
)


declare -a arrayrmtld=(
#  "a \land \always_{< b1 } a \rightarrow \eventually_{=2} a"
#  "(p \lor q) \ \until_{<b1} r "
  "\int^{b1} p < 3"
#  "\left( (p \lor q) \ \until_{<b1} r \right) \land \int^{9} r < 2"
#  "\left( (p \lor q) \ \until_{<b1} r \right) \land 10 < \int^{9} r"
#  "\eventually_{<b1}  p \land \always_{<b2} \neg p"
#  "\always_{<b2} (a \lor b) \ \until_{<b1} r"
)


# Parsing parameters and executing benchmarks

SOLVEFLAG="--solver-z3 --assume-unary-seq --rec-unrolling=auto"
#SOLVEFLAG="--solver-cvc4 --recursive-unrolling"
CVC4DIR="./cvc4-1.6-win64-opt.exe"
DIRNAME="_benchmark_suite"

if [ $# -eq 0 ] ; then
    echo "No arguments supplied"
elif [ $# -eq 1 ] && [ $1 == "rmtld" ] ; then

  #################################
  # benchamark for rmtld formulas #
  #################################

  echo "Chosen option: $1"

  # get length of an array
  arrayrmtldlength=${#arrayrmtld[@]}

  echo "Number of formulas: $arrayrmtldlength"
  
  declare -A arr

  mkdir $DIRNAME -p

  for (( sample=10, j=1; sample<60; sample+=10, j++ ));
  do
    echo $sample

    # use for loop to read all values and indexes
    for (( i=1; i<${arrayrmtldlength}+1; i++ ));
    do
      REP=${arrayrmtld[$i-1]//b1/$sample}
      REPP=${REP//b2/$sample}

      echo $i " / " ${arrayrmtldlength} " : " $REPP
      START=$(date +%s.%N)
      OCAMLRUNPARAM=b ../_build/install/default/bin/rmtld3synth --synth-smtlibv2 $SOLVEFLAG --input-latexeq "$REPP" > "$DIRNAME/testrmtld$i.$sample.result"
      END=$(date +%s.%N)

      OUT=$?
      if [ $OUT -ne 0 ]; then
        echo "WARNING: THE BENCHMARK HAS NOT BEEN COMPLETED!!!"
        break
      fi

      if [[ $SOLVEFLAG == *"--solver-cvc4"* ]]; then
        echo "$CVC4DIR $DIRNAME/testrmtld$i.$sample.smt2"
        $CVC4DIR $DIRNAME/testrmtld$i.$sample.smt2 2>&1
      fi

      DIFF=$(echo "$END - $START" | bc)
      echo $DIFF
      arr[$j,$i]=$DIFF

      #z3 "test$i.smt2" > "results$i.txt"
    done
  done

  line=""
  for (( i=1; i<${arrayrmtldlength}+1; i++ ));
  do
    line=""
    for (( sample=5, j=1; sample<50; sample+=5, j++ ));
    do
        line+="${arr[$j,$i]}, "
    done
    echo $line
  done

elif [ $# -eq 1 ] && [ $1 == "rmdsl" ] ; then
  echo "Chosen option: $1"

  ###################################
  # benchamark for rdsl expressions #
  ###################################

  # get length of an array
  arrayrdsllength=${#arrayrdsl[@]}

  echo "Number of expressions: $arrayrdsllength"

  mkdir trmdsl

  VERBOSE="--verbose 2"

  # use for loop to read all values and indexes
  # for (( i=1; i<${arrayrdsllength}+1; i++ ));
  # do
  #   echo $i " / " ${arrayrdsllength} " : " ${arrayrdsl[$i-1]}
  #   TIMEFORMAT="$i: %3R"; time OCAMLRUNPARAM=b ../rmtld3synth.native --synth-smtlibv2 --solver-z3 --get-trace --recursive-unrolling --input-rmdsl "${arrayrdsl[$i-1]}" $VERBOSE > "trmdsl/testrmdsl$i.log" &
  #   OUT=$?
  #   if [ $OUT -ne 0 ]; then
  #     echo "WARNING: THE BENCHMARK HAS NOT BEEN COMPLETED!!!"
  #     break
  #   fi
  #   #z3 "test$i.smt2" > "results$i.txt"
  #   if ! (($i % 2)); then
  #     wait
  #   fi
  # done

  for (( i=1; i<${arrayrdsllength}+1; i++ ));
  do
    echo "echo '$i " / " ${arrayrdsllength} " : " ${arrayrdsl[$i-1]}'; TIMEFORMAT='$i: %3R'; time OCAMLRUNPARAM=b ../rmtld3synth.native --synth-smtlibv2 --solver-z3 --get-trace --recursive-unrolling --input-rmdsl '${arrayrdsl[$i-1]}' $VERBOSE > 'trmdsl/testrmdsl$i.log'; grep -rnw 'trmdsl/testrmdsl$i.log' -e 'Result:'"
  done | xargs -d'\n' -I{} --max-procs 2 bash -c '
    {
      #echo "{}"
      {}
    }'


  # "\rm{core0}{ \left( \tk{ts1}{20,9} \succ \tk{ts2}{15,8} \right) \ \bowtie \ \tk{ts3}{10,3} }{60,50}"
  # "\rm{res0}{ \left( \tk{ts1}{10,8} \succ \tk{ts2}{20,5} \right) \ \bowtie \ \tk{ts3}{27,7} }{0,0} \ \parallel \ \rm{res1}{ \tk{ts4}{4,33} }{0,0}"



else

  echo ""Unsupported arguments. Use rmtld \(formulas\) or rmdsl \(expressions\)""

fi
