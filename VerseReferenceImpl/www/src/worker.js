import { WASI } from "@bjorn3/browser_wasi_shim";

const wasi = new WASI([], [], []);

const imports = { wasi_snapshot_preview1: wasi.wasiImport };

const wasm = await WebAssembly.instantiateStreaming(fetch('./versewasm.wizer.wasm'), imports);

const instance = wasm.instance;

wasi.inst = instance;

const exports = instance.exports;

const memory = exports.memory;

const encoder = new TextEncoder();

const decoder = new TextDecoder();

const outPtrPtr = exports.calloc_ptr();

self.onmessage = ({ data: { stdinString } }) => {
    const stdinLength = Buffer.byteLength(stdinString);
    const stdinPtr = exports.malloc(stdinLength);
    let outLength;
    try {
        const stdinArray = new Uint8Array(memory.buffer, stdinPtr, stdinLength);
        encoder.encodeInto(stdinString, stdinArray);
        outLength = exports.verse_eval(stdinPtr, stdinLength, outPtrPtr);
    } finally {
        exports.free(stdinPtr);
    }
    const outPtrArray = new Uint32Array(memory.buffer, outPtrPtr, 1);
    const outPtr = outPtrArray[0];
    const outArray = new Uint8Array(memory.buffer, outPtr, outLength);
    const outString = decoder.decode(outArray);
    self.postMessage({ outString });
};
