const worker = new Worker(new URL('./worker.js', import.meta.url));

const stdin = document.querySelector('#in');

const out = document.querySelector('#out');

window.addEventListener('DOMContentLoaded', event => {
    const stdinString = localStorage.getItem('stdin');
    if (stdinString) {
        stdin.textContent = stdinString;
    }
    const outString = localStorage.getItem('out');
    if (outString) {
        out.textContent = outString;
    }
});

worker.onmessage = ({ data: { exitCode, stdoutString, stderrString } }) => {
    localStorage.setItem('stdout', stdoutString);
    localStorage.setItem('stderr', stderrString);
    const outString = stderrString? stderrString : stdoutString;
    localStorage.setItem('out', outString);
    out.textContent = outString;
};

stdin.addEventListener('input', event => {
    const stdinString = event.target.value;
    localStorage.setItem('stdin', stdinString);
    worker.postMessage({ stdinString });
});

export {};
