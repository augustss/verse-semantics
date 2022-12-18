const worker = new Worker(new URL('./worker.js', import.meta.url));

const out = document.querySelector('#out');

const err = document.querySelector('#err');

worker.onmessage = ({ data: { exitCode, stdoutString, stderrString } }) => {
    out.textContent = stdoutString;
    err.textContent = stderrString;
};

document.querySelector('#in').addEventListener('input', event => {
    const stdinString = event.target.value;
    worker.postMessage({ stdinString });
});

export {};
