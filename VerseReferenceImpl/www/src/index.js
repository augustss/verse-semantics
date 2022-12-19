const worker = new Worker(new URL('./worker.js', import.meta.url));

const stdin = document.querySelector('#in');

const stdout = document.querySelector('#out');

const stderr = document.querySelector('#err');

window.addEventListener('DOMContentLoaded', event => {
    const stdinString = localStorage.getItem('stdin');
    if (stdinString) {
        stdin.textContent = stdinString;
    }
    const stdoutString = localStorage.getItem('stdout');
    if (stdoutString) {
        stdout.textContent = stdoutString;
    }
    const stderrString = localStorage.getItem('stderr');
    if (stderrString) {
        stderr.textContent = stderrString;
    }
});

worker.onmessage = ({ data: { exitCode, stdoutString, stderrString } }) => {
    localStorage.setItem('stdout', stdoutString);
    localStorage.setItem('stderr', stderrString);
    stdout.textContent = stdoutString;
    stderr.textContent = stderrString;
};

stdin.addEventListener('input', event => {
    const stdinString = event.target.value;
    localStorage.setItem('stdin', stdinString);
    worker.postMessage({ stdinString });
});

export {};
