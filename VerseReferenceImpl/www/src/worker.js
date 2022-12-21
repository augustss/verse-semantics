import { init, WASI } from '@wasmer/wasi';

await init();

const compileStreaming = () => WebAssembly.compileStreaming(fetch('./verse.wasm'));

const moveFileToString = (fs, path) => {
    let file;
    try {
        file = fs.open(path, {
            read: true
        });
    } catch (e) {
        return '';
    }
    try {
        return file.readString();
    } finally {
        fs.removeFile(path);
    }
};

const writeFile = (fs, path, string) => {
    const file = fs.open(path, {
        create: true,
        write: true,
        truncate: true
    });
    file.writeString(string);
};

const module = await compileStreaming();

let wasi;
let instance;
let memory;

const instantiate = () => {
    wasi  = new WASI({
        env: {},
        args: []
    });
    instance = wasi.instantiate(module, {});
    memory = instance.exports.memory;
};

instantiate();

const initialArray = new Uint8Array(new ArrayBuffer(memory.buffer.byteLength));
initialArray.set(new Uint8Array(memory.buffer));

const start = (stdinString) => {
    writeFile(wasi.fs, '/in', stdinString);
    try {
        return wasi.start(instance);
    } catch (e) {
        instantiate();
        writeFile(wasi.fs, '/in', stdinString);
        return wasi.start(instance);
    } finally {
        const array = new Uint8Array(memory.buffer);
        array.set(initialArray);
    }
};

self.onmessage = ({ data: { stdinString } }) => {
    const exitCode = start(stdinString);
    const stdoutString = moveFileToString(wasi.fs, '/out');
    const stderrString = moveFileToString(wasi.fs, '/err');
    self.postMessage({
        exitCode,
        stdoutString,
        stderrString
    });
};
