import { init, WASI } from '@wasmer/wasi';

await init();

const compile = () => WebAssembly.compileStreaming(fetch('./verse.wasm'));

const readFile = (fs, path) => {
    let file;
    try {
        file = fs.open(path, {
            read: true
        });
    } catch (e) {
        return '';
    }
    return file.readString();
};

let module = await compile();

self.onmessage = async ({ data: { stdinString } }) => {
    const wasi = new WASI({
        env: {},
        args: []
    });
    let instance;
    try {
        wasi.instantiate(module, {});
    } catch (e) {
        module = await compile();
        wasi.instantiate(module, {});
    }
    const stdin = wasi.fs.open('in', {
        create: true,
        write: true,
        truncate: true
    });
    stdin.writeString(stdinString);
    const exitCode = wasi.start();
    const stdoutString = readFile(wasi.fs, 'out');
    const stderrString = readFile(wasi.fs, 'err');
    self.postMessage({
        exitCode,
        stdoutString,
        stderrString
    });
};
