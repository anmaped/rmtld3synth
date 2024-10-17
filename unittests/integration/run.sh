#!/bin/bash

set -e

err_report() {
    echo "Error on line $1"
}

trap 'err_report $LINENO' ERR

function path(){
if [[ $(uname -s) == CYGWIN* ]];then
  cygpath -w $1
else 
  echo $1
fi
}

SCRIPT_PATH="$(dirname -- "${BASH_SOURCE[0]}")"
SCRIPT_PATH="$(realpath -e -- "$SCRIPT_PATH")"
echo $SCRIPT_PATH

OUTPUT_DIR=$SCRIPT_PATH/../../_build_tests/_integration
mkdir -p $OUTPUT_DIR
OUTPUT_DIR="$(realpath -e -- "$OUTPUT_DIR")"

rmtld3synth --synth-cpp11 --input-dsl "a" --out-src="$(path "$OUTPUT_DIR")"

cp "$SCRIPT_PATH/sample_monitor.cpp" "$OUTPUT_DIR"
cp "$SCRIPT_PATH/sample_instrumentation.cpp" "$OUTPUT_DIR"

pushd $OUTPUT_DIR
gcc -I"$(path "$SCRIPT_PATH/../../rtmlib2/src/")" "sample_monitor.cpp" "sample_instrumentation.cpp" -o test -latomic -fno-rtti -fno-exceptions -DRTMLIB_ENABLE_DEBUG_RMTLD3
popd
