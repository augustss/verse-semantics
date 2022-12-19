import { init, WASI } from '@wasmer/wasi';

await init();

const compileStreaming = () => WebAssembly.compileStreaming(fetch('./verse.wasm'));

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

let module = await compileStreaming();

const instantiate = (module, wasi) => wasi.instantiate(module, {});

self.onmessage = async ({ data: { stdinString } }) => {
    const wasi = new WASI({
        env: {},
        args: []
    });
    const instance = instantiate(module, wasi);
    const stdin = wasi.fs.open('in', {
        create: true,
        write: true,
        truncate: true
    });
    stdin.writeString(stdinString);
    const exitCode = wasi.start(instance);
    const stdoutString = readFile(wasi.fs, 'out');
    const stderrString = readFile(wasi.fs, 'err');
    self.postMessage({
        exitCode,
        stdoutString,
        stderrString
    });
};
