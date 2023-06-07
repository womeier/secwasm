help:
	@echo "This is the help page from the binary, see this Makefile for some shortcuts"
	@echo "********************************************************************************"
	dune build
	dune exec _build/default/bin/main.exe -- -help

demo-1:
	dune build
	dune exec _build/default/bin/main.exe -- -example demo1 -typecheck

demo-2:
	dune build
	dune exec _build/default/bin/main.exe -- -example demo2 -typecheck

demo-3:
	dune build
	dune exec _build/default/bin/main.exe -- -example demo3 -typecheck

demo-3-run:
	dune build
	rm -rf _benchmark/demo3
	mkdir -p _benchmark/demo3
	dune exec _build/default/bin/main.exe -- -example demo3 -typecheck
	dune exec _build/default/bin/main.exe -- -example demo3 -pp -out _benchmark/demo3/orig.wat
	dune exec _build/default/bin/main.exe -- -example demo3 -dynchecks -out _benchmark/demo3/dynchecks.wat

	cp ../testing/benchmarking/demo3-run.js _benchmark/demo3/orig.js
	sed -i 's/{{file}}/\"\/orig.wasm\"/g' _benchmark/demo3/orig.js

	cp ../testing/benchmarking/demo3-run.js _benchmark/demo3/dynchecks.js
	sed -i 's/{{file}}/\"\/dynchecks.wasm\"/g' _benchmark/demo3/dynchecks.js

	cd _benchmark/demo3 &&	wat2wasm dynchecks.wat
	cd _benchmark/demo3 &&	wat2wasm orig.wat

	@echo "***********************************************************"
	@echo "RUNNING INSECURE (STRIPPED LABELS)"
	cd _benchmark/demo3 &&	node orig.js

	@echo
	@echo "***********************************************************"
	@echo "RUNNING WITH INSTRUMENTED DYNAMIC CHECKS"
	cd _benchmark/demo3 &&	node dynchecks.js


run:
	dune build
	dune exec _build/default/bin/main.exe -- -example 1 -typecheck

bubblesort-benchmark:
	dune build
	rm -rf _benchmark/bubblesort
	mkdir -p _benchmark/bubblesort
	dune exec _build/default/bin/main.exe -- -example bubblesort -typecheck
	dune exec _build/default/bin/main.exe -- -example bubblesort -pp -out _benchmark/bubblesort/orig.wat
	dune exec _build/default/bin/main.exe -- -example bubblesort -dynchecks -out _benchmark/bubblesort/dynchecks.wat

	cp ../testing/benchmarking/generic-run.js _benchmark/bubblesort/orig.js
	sed -i 's/{{file}}/\"\/orig.wasm\"/g' _benchmark/bubblesort/orig.js
	cp ../testing/benchmarking/generic-run.js _benchmark/bubblesort/dynchecks.js
	sed -i 's/{{file}}/\"\/dynchecks.wasm\"/g' _benchmark/bubblesort/dynchecks.js

	cd _benchmark/bubblesort &&	wat2wasm dynchecks.wat
	cd _benchmark/bubblesort &&	wat2wasm orig.wat

	@echo "********************************************************************************"
	@cd _benchmark/bubblesort && node --stack-size=10000000 orig.js

	@echo "********************************************************************************"
	@cd _benchmark/bubblesort && node --stack-size=10000000 dynchecks.js

arg1 := $(shell python3 -c "print(2**31-5)")
arg2 := $(shell python3 -c "print(2**2)")

seqmem-benchmark:
	dune build
	rm -rf _benchmark/seqmem
	mkdir -p _benchmark/seqmem
	dune exec _build/default/bin/main.exe -- -example seq_mem_store -pp -out _benchmark/seqmem/orig.wat
	dune exec _build/default/bin/main.exe -- -example seq_mem_store -dynchecks -out _benchmark/seqmem/dynchecks.wat

	cd _benchmark/seqmem &&	wat2wasm dynchecks.wat
	cd _benchmark/seqmem &&	wat2wasm orig.wat

	echo "Timing original:"
	cd _benchmark/seqmem && perf stat -r 10 -d wasmtime --invoke foo orig.wasm -- $(arg1) $(arg2)

	# sudo perf record -r 10 -k mono wasmtime --profile=jitdump --invoke foo example-original.wasm -- $2 $3
	# sudo perf inject --jit --input perf.data --output original-perf.jit.data

	echo "Timing code with dynamic checks:"
	cd _benchmark/seqmem && perf stat -r 10 -d wasmtime --invoke foo dynchecks.wasm -- $(arg1) $(arg2)

	# sudo perf record -r 10 -k mono wasmtime --profile=perfmap --invoke foo example-with-checks.wasm -- $2 $3
	# sudo perf inject --jit --input perf.data --output checks-perf.jit.data

clean:
	dune clean
	rm -rf _coverage/ _benchmark
	rm -f *.wat *.wasm

test:
	dune runtest -f

coverage:
	find . -name '*.coverage' | xargs rm -f
	-dune runtest --instrument-with bisect_ppx --force
	@echo "======================"
	bisect-ppx-report html
	bisect-ppx-report summary
	@echo "======================"
	@echo "For summary see _coverage/index.html"
