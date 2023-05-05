all:
	@echo "EXAMPLE PROGRAMS:"
	@echo "  make run-add"
	@echo "  make run-mem"
	@echo "  make run-bintree"

%.wasm: %.wat
	wat2wasm $<

run-bintree: bintree.wasm
	@echo "Running bintree.{js, wasm} with node..."
	@node bintree.js

run-add: add.wasm
	@echo "Running add.{js, wasm} with node..."
	@node add.js

run-mem: mem.wasm
	@echo "Running mem.{js, wasm} with node..."
	@node mem.js

clean:
	rm -r *.wasm
