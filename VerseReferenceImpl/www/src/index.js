const worker = new Worker(new URL('./worker.js', import.meta.url));

const stdin = document.querySelector('#in');

const out = document.querySelector('#out');

window.addEventListener('DOMContentLoaded', event => {
    const stdinString = localStorage.getItem('stdin');
    if (stdinString) {
        stdin.textContent = stdinString;
    }
    const stderrString = localStorage.getItem('stderr');
    if (stderrString) {
        out.textContent = stderrString;
    } else {
        const stdoutString = localStorage.getItem('stdout');
        if (stdoutString) {
            out.textContent = stdoutString;
        }
    }
});

worker.onmessage = ({ data: { exitCode, stdoutString, stderrString } }) => {
    localStorage.setItem('stdout', stdoutString);
    localStorage.setItem('stderr', stderrString);
    if (stderrString) {
        out.textContent = stderrString;
    } else if (stdoutString) {
        out.textContent = stdoutString;
    }
};

stdin.addEventListener('input', event => {
    const stdinString = event.target.value;
    localStorage.setItem('stdin', stdinString);
    worker.postMessage({ stdinString });
});

export {};
