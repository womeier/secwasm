# secwasm Project - May 2023

# Setup

### General Setup
- Install opam dependencies: in `src/`: `opam pin add -y .`, `opam install --deps-only secwasm`
- Run the project with the Makefile in `src`

### Development Setup
- Ocaml formatting tool: `opam pin add ocamlformat 0.21.0`, `opam install ocamlformat`
- Pre-commit hook to ensure files are properly formatted: `pip install pre-commit`, in the root repo: `pre-commit install`
- Webassembly binary toolkit: `wabt`, provides `wat2wasm` and `wasm2wat` among others
- Nodejs
