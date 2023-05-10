
run:
	dune build
	dune exec _build/default/ast.exe
	@echo "--------"
	@cat out.wat
	@echo "--------"
	wat2wasm out.wat

clean:
	dune clean
	rm -f *.wat *.wasm
