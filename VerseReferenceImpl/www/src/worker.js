import { init, WASI } from '@wasmer/wasi';

await init();

const compile = () => WebAssembly.compileStreaming(fetch('./verse.wasm'))

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
    let stdoutString;
    try {
        const stdout = wasi.fs.open('out', {
            read: true
        });
        stdoutString = stdout.readString();
    } catch (e) {
        stdoutString = '';
    }
    let stderrString;
    try {
        const stderr = wasi.fs.open('err', {
            read: true
        });
        stderrString = stderr.readString();
    } catch (e) {
        stderrString = '';
    }
    self.postMessage({
        exitCode,
        stdoutString,
        stderrString
    });
};
