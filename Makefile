all: build

dev:
	ghcid --lint --command "ghci app/Main -fobject-code -i.:src:app" --test main

build:
	nix-build release.nix

format:
	stylish-haskell -irv .

specs:
	runhaskell -isrc:app:test test/Spec.hs

cabal2nix:
	cabal2nix . > default.nix

init-cabal-nix:
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'
 
db-access:
	PGPASSWORD=pass psql -d dev -U dev -h localhost -p 5444

db-start: db-up db-migrations

db-migrations:
	cd migrations && sqitch deploy

db-up:
	arion up -d

docker-image:
	nix-build docker.nix -o image
