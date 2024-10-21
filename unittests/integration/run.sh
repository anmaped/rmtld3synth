#!/bin/bash

set -e

err_report() {
    echo "Error on line $1"
}

trap 'err_report $LINENO' ERR

function path() {
    if [[ $(uname -s) == CYGWIN* ]]; then
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

echo "Generating the Makefile..."

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

DEBUG='-DRTMLIB_ENABLE_DEBUG_RMTLD3 -DRTMLIB_DEBUG=0'

echo -e "

$CHECK_GCC

all:
	$CXX_INC -Wall -Wextra -std=gnu++11 $DEBUG -I\"$SCRIPT_PATH/../../rtmlib2/src/\" \"sample_monitor.cpp\" \"sample_instrumentation.cpp\" -o test -latomic

clean:
	rm test

" >$OUTPUT_DIR/Makefile

echo "Running the Makefile..."

pushd $OUTPUT_DIR

make all

popd
