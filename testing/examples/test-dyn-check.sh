#!/usr/bin/env bash

set -eu

if ! [ $# -eq 2 ]; then
  echo "usage: $0 <example> <args>"
  exit 1
fi

cd ../../src
dune build
dune exec _build/default/bin/main.exe -- -example $1 -dynchecks -out ../testing/examples/dyn-check-example.wat
cd -
./run-unary.sh dyn-check-example $2
