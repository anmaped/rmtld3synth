#!/bin/bash

# Auxiliar function to compare package versions
vercmp() {
    version1=$1
    version2=$2
    condition=$3

    IFS=.
    v1_array=($version1)
    v2_array=($version2)
    v1=$((v1_array[0] * 100 + v1_array[1] * 10 + v1_array[2]))
    v2=$((v2_array[0] * 100 + v2_array[1] * 10 + v2_array[2]))
    diff=$((v2 - v1))
    [[ $condition = '='  ]] && ((diff == 0)) && return 0
    [[ $condition = '!=' ]] && ((diff != 0)) && return 0
    [[ $condition = '<'  ]] && ((diff >  0)) && return 0
    [[ $condition = '<=' ]] && ((diff >= 0)) && return 0
    [[ $condition = '>'  ]] && ((diff <  0)) && return 0
    [[ $condition = '>=' ]] && ((diff <= 0)) && return 0
    return 1
}

QUERY="ocamlfind query -qe"
QUERYVERSION="$QUERY -format %v"
# checks if z3 solver is available
# query which package name is installed (z3 or Z3) using `ocamlfind query`
#Z3_VERSION=`z3 --version | sed -ne 's/Z3 [^0-9]*\(\([0-9]\.\)\{0,4\}[0-9][^.]\).*/\1/p'`;

$QUERY z3 && { Z3_VERSION="$($QUERYVERSION z3)"; }
$QUERY Z3 && ! $QUERY z3 && { echo "Replace z3 by Z3."; sed -i 's/z3/Z3/' src/interface/dune; Z3_VERSION="$($QUERYVERSION Z3)"; }
! $QUERY Z3 && ! $QUERY z3 && { echo "No z3 package found. Just compile rmtld3synth without z3 solver support."; sed -i 's/z3//' src/interface/dune; Z3_VERSION=""; }
