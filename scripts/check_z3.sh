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

# checks if z3 solver is available
Z3_VERSION=`z3 --version | sed -ne 's/Z3 [^0-9]*\(\([0-9]\.\)\{0,4\}[0-9][^.]\).*/\1/p'`

echo "Z3 Version Found: $Z3_VERSION"
