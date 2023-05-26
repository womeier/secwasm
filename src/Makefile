help:
	@echo "This is the help page from the binary, see this Makefile for some shortcuts"
	@echo "********************************************************************************"
	dune build
	dune exec _build/default/bin/main.exe -- -help

run:
	dune build
	dune exec _build/default/bin/main.exe -- -example 1 -typecheck

bubblesort-benchmark:
	dune build
	rm -rf _benchmark/bubblesort
	mkdir _benchmark/bubblesort
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
	@echo "Running bubblesort benchmark original"
	@cd _benchmark/bubblesort && node orig.js

	@echo "********************************************************************************"
	@echo "Running bubblesort benchmark dynchecks"
	@cd _benchmark/bubblesort && node dynchecks.js

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
