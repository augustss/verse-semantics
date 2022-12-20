import { init, WASI } from '@wasmer/wasi';

await init();

const compileStreaming = () => WebAssembly.compileStreaming(fetch('./verse.wasm'));

const instantiate = (module, wasi) => wasi.instantiate(module, {});

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

const wasi = new WASI({
    env: {},
    args: []
});

const instance = instantiate(module, wasi);

const memory = instance.exports.memory;

const initialBuffer = new ArrayBuffer(memory.buffer.byteLength);
new Uint8Array(initialBuffer).set(new Uint8Array(memory.buffer));

const withMemory = f => {
    try {
        f();
    } finally {
        new Uint8Array(memory.buffer).set(new Uint8Array(initialBuffer));
    }
}

self.onmessage = ({ data: { stdinString } }) => {
    withMemory(() => {
        const stdin = wasi.fs.open('/in', {
            create: true,
            write: true,
            truncate: true
        });
        stdin.writeString(stdinString);
        const exitCode = wasi.start(instance);
        const stdoutString = moveFile(wasi.fs, '/out');
        const stderrString = moveFile(wasi.fs, '/err');
        self.postMessage({
            exitCode,
            stdoutString,
            stderrString
        });
    });
};
