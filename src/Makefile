
run:
	dune build
	dune exec _build/default/ast.exe
	wat2wasm out.wat

clean:
	rm -f *.wat *.wasm
