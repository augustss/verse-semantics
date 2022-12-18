const module = await WebAssembly.compileStreaming(fetch('./verse.wasm'));

const worker = new Worker(new URL('./worker.js', import.meta.url));
const stdinString = '1 + 2';

const out = document.querySelector('#out');

worker.onmessage = ({ data: { exitCode, stdoutString, stderrString } }) => {
    out.textContent = stdoutString;
};

document.querySelector('#in').addEventListener('input', event => {
    const stdinString = event.target.value;
    worker.postMessage({ module, stdinString });
});

export {};
