#!/usr/bin/env node
const { execSync } = require('child_process');
const fs = require('fs');

const failures = [22, 25, 29, 31, 37, 38, 39, 41, 42, 48, 51, 59];

console.log('# Detailed Failure Analysis\n');

for (const num of failures) {
    const file = `test_verse/01_expressions/${num.toString().padStart(2, '0')}.verse`;

    console.log(`## Snippet ${num}.verse\n`);

    // Read file content
    try {
        const content = fs.readFileSync(file, 'utf8');
        console.log('**Code:**');
        console.log('```verse');
        console.log(content.split('\n').slice(0, 12).join('\n'));
        console.log('```\n');
    } catch (err) {
        console.log('*File not found*\n');
        continue;
    }

    // Get error
    try {
        const output = execSync(`node bin/vc "${file}"`, {
            encoding: 'utf8',
            timeout: 5000,
            stdio: ['pipe', 'pipe', 'pipe']
        });
    } catch (err) {
        const errors = err.stderr || err.stdout || '';
        const errorLines = errors.split('\n')
            .filter(line => line.includes('error'))
            .slice(0, 3);

        console.log('**Errors:**');
        errorLines.forEach(line => {
            const match = line.match(/(\d+): Verse compiler error (V\d+): (.+)/);
            if (match) {
                console.log(`- Line ${match[1]}: **${match[2]}** - ${match[3]}`);
            } else if (line.trim()) {
                console.log(`- ${line.trim()}`);
            }
        });
        console.log();
    }

    console.log('---\n');
}
