#!/usr/bin/env bash

set -eu

if ! [ $# -eq 3 ]; then
  echo "usage: $0 <example> <arg1> <arg2>"
  exit 1
fi


cd ../../src
dune build
dune exec _build/default/bin/main.exe -- -example $1 -pp -out ../testing/examples/example-original.wat
dune exec _build/default/bin/main.exe -- -example $1 -dynchecks -out ../testing/examples/example-with-checks.wat
cd -

wat2wasm --debug-names example-original.wat
wat2wasm --debug-names example-with-checks.wat

echo "Timing original:"
perf stat -r 1 -d wasmtime --invoke foo example-original.wasm -- $2 $3
# sudo perf record -r 10 -k mono wasmtime --profile=jitdump --invoke foo example-original.wasm -- $2 $3
# sudo perf inject --jit --input perf.data --output original-perf.jit.data

echo "Timing code with dynamic checks:"
perf stat -r 1 -d wasmtime --invoke foo example-with-checks.wasm -- $2 $3
# sudo perf record -r 10 -k mono wasmtime --profile=perfmap --invoke foo example-with-checks.wasm -- $2 $3
# sudo perf inject --jit --input perf.data --output checks-perf.jit.data
