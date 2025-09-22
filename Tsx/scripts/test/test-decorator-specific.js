const { parseTopLevel, parse, prettyPrint } = require('./dist/index.js');

const tests = [
    '@editable\nclass { x := 1 }',
    '@editable\nclass {\n  x := 1\n}',
    '@editable\nMyClass := class { x := 1 }',
    'CaptureControl := class:\n    @editable\n    Device:enabled_device = enabled_device{}'
];

tests.forEach((testCode, i) => {
    console.log(`\n=== Test ${i + 1} ===`);
    console.log('Original:');
    console.log(testCode);

    let result = parseTopLevel(testCode, true);
    if (!result) {
        result = parse(testCode, true);
    }

    if (result) {
        console.log('✅ Parsing successful');

        const reconstructed = prettyPrint(result);
        const match = testCode === reconstructed;
        console.log('Match:', match ? '✅' : '❌');

        if (!match) {
            console.log('\nReconstructed:');
            console.log(reconstructed);
            console.log('\nOriginal   length:', testCode.length);
            console.log('Reconstructed length:', reconstructed.length);
        }
    } else {
        console.log('❌ Parsing failed');
    }
});