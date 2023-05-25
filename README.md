# secwasm Project - May 2023

- Implementation of a security-typesystem following [this](https://plas2022.github.io/files/pdf/SecWasm.pdf) paper
- Security guarantee: termination-insensitive non-interference

## Setup

### General
- Install opam dependencies: `cd src/` `opam pin add -y .` `opam install --deps-only secwasm`
- Run the project with the Makefile in `src/`

### Development Setup
- Ocaml formatting tool: `opam pin add ocamlformat 0.21.0` `opam install ocamlformat`
- Pre-commit hook to ensure formatting: `pip install pre-commit` then in the root repo: `pre-commit install`
- Webassembly binary toolkit: `wabt`, provides `wat2wasm` and `wasm2wat` among others
- Nodejs:
  - known to work with version 18, 19
  - known to not work with version 12
