
run:
	dune build
	dune exec _build/default/bin/main.exe -- -example 1 -typecheck

clean:
	dune clean
	rm -rf _coverage/
	rm -f *.wat *.wasm

test:
	dune runtest -f

coverage:
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	@echo "======================"
	bisect-ppx-report html
	bisect-ppx-report summary
	@echo "======================"
	@echo "For summary see _coverage/index.html"
