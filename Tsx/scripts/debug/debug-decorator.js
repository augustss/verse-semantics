const { parseTopLevel, prettyPrint } = require('./dist/index.js');

const testCode = `@editable
class {
  x := 1
}`;

console.log('Original:');
console.log(testCode);
console.log('\n' + '='.repeat(50));

const result = parseTopLevel(testCode, true);
if (result) {
    console.log('✅ Parsing successful');
    console.log('AST Type:', result.type);

    const reconstructed = prettyPrint(result);
    console.log('\nReconstructed:');
    console.log(reconstructed);

    console.log('\nMatch:', testCode === reconstructed ? '✅' : '❌');

    if (testCode !== reconstructed) {
        console.log('\nDifferences:');
        console.log('Original length:', testCode.length);
        console.log('Reconstructed length:', reconstructed.length);

        // Character by character comparison
        for (let i = 0; i < Math.max(testCode.length, reconstructed.length); i++) {
            const orig = testCode[i] || '∅';
            const recon = reconstructed[i] || '∅';
            if (orig !== recon) {
                console.log(`Diff at position ${i}: "${orig}" vs "${recon}"`);
                break;
            }
        }
    }
} else {
    console.log('❌ Parsing failed');
}