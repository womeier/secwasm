#!/usr/bin/env bash

wat2wasm $1.wat && node binary-func.js $1.wasm $2 $3
