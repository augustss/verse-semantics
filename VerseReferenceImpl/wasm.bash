#!/bin/bash
set -eux

nix shell \
  https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_8 \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  --command \
wasm32-wasi-cabal update

nix shell \
  https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_8 \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  --command \
wasm32-wasi-cabal install \
  --flag wasm \
  --installdir=www/static \
  --overwrite-policy=always \
  -O2

nix shell \
  https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_8 \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  --command \
wizer \
  --allow-wasi \
  --wasm-bulk-memory true \
  www/static/versewasm.wasm \
  -o www/static/versewasm.wizer.wasm

cp www/static/versewasm.wasm www/static/versewasm.wizer.wasm

nix shell \
  https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_8 \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  --command \
wasm-opt www/static/versewasm.wizer.wasm -o www/static/versewasm.wizer.wasm -Oz

cd www
npm install
npx webpack serve
