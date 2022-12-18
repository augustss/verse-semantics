const worker = new Worker(new URL('./worker.js', import.meta.url));

const out = document.querySelector('#out');

worker.onmessage = ({ data: { exitCode, stdoutString, stderrString } }) => {
    out.textContent = stdoutString;
};

document.querySelector('#in').addEventListener('input', event => {
    const stdinString = event.target.value;
    worker.postMessage({ stdinString });
});

export {};
