update:
	cabal2nix . > model.nix

shell: update
	nix-shell

repl: update
	nix-shell --run "cabal configure; cabal repl exe:model"

run: update
	nix-shell --run "cabal configure; cabal run model"

com: update
	nix-shell --run "cabal configure; cabal run compiler"

build: update
	nix-build release.nix

clean:
	rm -rf cabal.*
	rm -rf dist*
	rm -rf result
