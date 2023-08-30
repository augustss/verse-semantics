#!/bin/bash
set -eux
wasm32-wasi-cabal install \
  --allow-newer \
  --with-alex=/home/andy/.cabal/bin/alex \
  --with-happy=/home/andy/.cabal/bin/happy \
  --installdir=www/static \
  --overwrite-policy=always
wizer \
  --allow-wasi \
  --wasm-bulk-memory true \
  www/static/versewasm.wasm \
  -o www/static/versewasm.wizer.wasm
cd www
npm install
npx webpack serve
