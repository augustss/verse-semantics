wasm32-wasi-cabal install \
  --installdir=www/static \
  --allow-newer \
  --overwrite-policy=always
wizer \
  --allow-wasi \
  --wasm-bulk-memory true \
  www/static/versewasm.wasm \
  -o www/static/versewasm.wizer.wasm
