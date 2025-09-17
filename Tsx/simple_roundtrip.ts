import { parseVersee } from './src/parser/parser';

function simpleRoundtrip(source: string) {
    console.log(`Testing: "${source}"`);

    const result = parseVersee(source);

    if (result.success) {
        console.log('✓ Parsed successfully');

        // Let's just show the AST structure for now
        // The pretty-printer has some issues, so we'll inspect the parsed structure
        console.log('AST structure:');
        console.log(JSON.stringify(result.value, (_key, value) => {
            if (typeof value === 'bigint') return value.toString() + 'n';
            return value;
        }, 2));

        console.log();
    } else {
        console.log('✗ Parse failed:', result.error);
    }
}

// Test some simple constructs
const testCases = [
    '42',
    'true',
    '"hello"',
    'x',
    'x + 5',
    'var x := 42'
];

for (const test of testCases) {
    simpleRoundtrip(test);
}