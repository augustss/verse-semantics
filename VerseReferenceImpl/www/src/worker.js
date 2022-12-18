import { init, WASI } from '@wasmer/wasi';

await init();

self.onmessage = async ({ data: { module, stdinString } }) => {
    const wasi = new WASI({
        env: {},
        args: []
    });
    const stdin = wasi.fs.open('in', {
        create: true,
        write: true,
        truncate: true
    });
    stdin.writeString(stdinString);
    const instance = wasi.instantiate(module, {});
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
