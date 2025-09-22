#!/usr/bin/env node

const { parse } = require('./dist/index');

console.log('Testing field parsing...\n');

// Test what a field declaration like "value: string = ""\" gets parsed as
const fieldTest = 'value: string = ""';

console.log(`Testing: ${fieldTest}`);
const result = parse(fieldTest, false);

if (result) {
    console.log(`✅ Parsed as: ${result.type}`);
    console.log(JSON.stringify(result, (key, value) => {
        if (key === 'trivia') return '<trivia>';
        if (key === 'span') return '<span>';
        return value;
    }, 2));
} else {
    console.log('❌ Failed to parse');
}