#!/usr/bin/env bash

wat2wasm $1.wat && node unary-func.js $1.wasm $2
