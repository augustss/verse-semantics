#!/usr/bin/env node
const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const dir = 'test_verse/01_expressions';
const files = fs.readdirSync(dir).filter(f => f.endsWith('.verse')).sort();

let passed = 0;
let failed = 0;
const failures = [];

for (const file of files) {
    const filePath = path.join(dir, file);
    try {
        const output = execSync(`node bin/vc "${filePath}"`, { encoding: 'utf8', timeout: 5000 });
        if (output.includes('Success')) {
            passed++;
        } else {
            failed++;
            failures.push(file);
        }
    } catch (err) {
        failed++;
        failures.push(file);
    }
}

console.log(`\nResults: ${passed} passed, ${failed} failed out of ${files.length} total`);
if (failures.length > 0) {
    console.log('\nFailed tests:');
    failures.forEach(f => console.log(`  - ${f}`));
}
