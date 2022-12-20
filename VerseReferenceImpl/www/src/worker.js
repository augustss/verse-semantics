import { init, WASI } from '@wasmer/wasi';

await init();

const compileStreaming = () => WebAssembly.compileStreaming(fetch('./verse.wasm'));

const moveFile = (fs, path) => {
    let file;
    try {
        file = fs.open(path, {
            read: true
        });
    } catch (e) {
        return '';
    }
    const result = file.readString();
    fs.removeFile(path);
    return result;
};

const module = await compileStreaming();

let wasi = new WASI({
    env: {},
    args: []
});

let instance = wasi.instantiate(module, {});

let memory = instance.exports.memory;

const initialBuffer = new ArrayBuffer(memory.buffer.byteLength);
new Uint8Array(initialBuffer).set(new Uint8Array(memory.buffer));

const start = (stdinString) => {
    const stdin = wasi.fs.open('/in', {
        create: true,
        write: true,
        truncate: true
    });
    stdin.writeString(stdinString);
    let exitCode;
    try {
        exitCode = wasi.start(instance);
    } catch (e) {
        wasi = new WASI({
            env: {},
            args: []
        });
        instance = wasi.instantiate(module, {});
        memory = instance.exports.memory;
        return start(stdinString);
    }
};

self.onmessage = ({ data: { stdinString } }) => {
    try {
        const exitCode = start(stdinString);
        const stdoutString = moveFile(wasi.fs, '/out');
        const stderrString = moveFile(wasi.fs, '/err');
        self.postMessage({
            exitCode,
            stdoutString,
            stderrString
        });
    } finally {
        new Uint8Array(memory.buffer).set(new Uint8Array(initialBuffer));
    }
};
