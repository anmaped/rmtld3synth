#!/bin/bash
set -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

for entry in "$SCRIPT_DIR"/*.exp
do
  echo "check: $entry"
  gcc -c -gnats -x ada $entry
done
