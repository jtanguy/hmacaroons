.PHONY: setup-cachix
setup-cachix:
	cachix use iohk
	cachix use mpickering

setup-nix-build:
	mkdir -p .nix-result

.PHONY: build
build: setup-nix-build setup-cachix
	nix build -f ci.nix --out-link .nix-result/lib hmacaroons.components.library

.PHONY: test
test:
	nix build -f ci.nix --out-link .nix-result/tests hmacaroons.components.tests
	.nix-result/tests/bin/test

.PHONY: doc
doc:
	nix build -f ci.nix --out-link .nix-result/hmacaroons hmacaroons.components.library.doc

.PHONY: all
all: build test doc
	ls -l .nix-result

.PHONY: ghcid
ghcid:
	nix-shell --run "ghcid -c 'cabal new-repl'"

.PHONY: repl
repl:
	nix-shell --run "cabal new-repl"
