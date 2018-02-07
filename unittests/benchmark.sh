#!/bin/bash

set -e

# bechmark file
echo "Initiating the benchmark process"

declare -a arrayrdsl=(
	"\rm{core0}{ \tk{ts1}{5,2} }{10,10}"
	"\rm{core0}{ \tk{ts1}{9,2} \succ \tk{ts2}{9,1} }{20,8}"
  #"\rm{core0}{ \tk{ts1}{5,3} \succ \tk{ts2}{10,8} }{20,20}"
	"\rm{core0}{ \tk{ts1}{5,3} \succ \tk{ts2}{10,8} \succ \tk{ts3}{5,3} }{20,20}"
	#"\rm{res0}{ \tk{ts1}{10,8} \succ \tk{ts2}{10,4} \bowtie \tk{ts3}{27,7} }{0,0} \parallel \rm{res1}{ \tk{ts4}{33,4} }{10,5}"
	#"\rm{core0}{ \tk{ts1}{20,9} \succ \tk{ts2}{15,8} }{60,50}"
	#"\rm{core0}{ \tk{ts1}{20,9} \succ \tk{ts2}{15,8} \succ \tk{ts3}{10,3} }{60,50}"
)


declare -a arrayrmtld=(
  "a \land \always_{< b1 } a \rightarrow \eventually_{=2} a"
  "(p \lor q) \ \until_{<b1} r "
  "\int^{b1} p < 3"
  "\left( (p \lor q) \ \until_{<b1} r \right) \land \int^{9} r < 2"
  "\left( (p \lor q) \ \until_{<b1} r \right) \land 10 < \int^{9} r"
  "\eventually_{<b1}  p \land \always_{<b2} \neg p"
  "\always_{<b2} (a \lor b) \ \until_{<b1} r"
)


# Parsing parameters and executing benchmarks

SOLVEFLAG="--solver-z3 --recurvive-unrolling"

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

  mkdir t

  for (( sample=5, j=1; sample<50; sample+=5, j++ ));
  do
    echo $sample

    # use for loop to read all values and indexes
    for (( i=1; i<${arrayrmtldlength}+1; i++ ));
    do
      REP=${arrayrmtld[$i-1]//b1/$sample}
      REPP=${REP//b2/$sample}

      echo $i " / " ${arrayrmtldlength} " : " ${arrayrmtld[$i-1]}
      START=$(date +%s.%N)
      OCAMLRUNPARAM=b ../rmtld3synth.native --synth-smtlibv2 $SOLVEFLAG --input-latexeq "$REPP" > "t/testrmtld$i.$sample.smt2"
      END=$(date +%s.%N)

      OUT=$?
      if [ $OUT -ne 0 ]; then
        echo "WARNING: THE BENCHMARK HAS NOT BEEN COMPLETED!!!"
        break
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

  # use for loop to read all values and indexes
  for (( i=1; i<${arrayrdsllength}+1; i++ ));
  do
    echo $i " / " ${arrayrdsllength} " : " ${arrayrdsl[$i-1]}
    OCAMLRUNPARAM=b ../rmtld3synth.native --synth-smtlibv2 --solve-z3 --get-schedule --input-rmdsl "${arrayrdsl[$i-1]}" > "trmdsl/testrmdsl$i.smt2" &
    OUT=$?
    if [ $OUT -ne 0 ]; then
      echo "WARNING: THE BENCHMARK HAS NOT BEEN COMPLETED!!!"
      break
    fi
    #z3 "test$i.smt2" > "results$i.txt"
  done

  wait

  # "\rm{core0}{ \left( \tk{ts1}{20,9} \succ \tk{ts2}{15,8} \right) \ \bowtie \ \tk{ts3}{10,3} }{60,50}"

  # "\rm{res0}{ \left( \tk{ts1}{10,8} \succ \tk{ts2}{20,5} \right) \ \bowtie \ \tk{ts3}{27,7} }{0,0} \ \parallel \ \rm{res1}{ \tk{ts4}{4,33} }{0,0}"



else

  echo ""Unsupported arguments. Use rmtld \(formulas\) or rmdsl \(expressions\)""

fi
