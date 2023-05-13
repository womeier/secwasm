
run:
	dune build
	dune exec _build/default/bin/main.exe -- -example 1 -typecheck

clean:
	dune clean
	rm -f *.wat *.wasm

test:
	dune runtest -f
