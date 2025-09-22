const { parseTopLevel, parse, prettyPrint } = require('./dist/index.js');

const tests = [
    'class { x := 1 }',
    '@editable\nclass { x := 1 }',
    'MyClass := class { x := 1 }'
];

tests.forEach((testCode, i) => {
    console.log(`\n=== Test ${i + 1}: ${testCode.split('\n')[0]}... ===`);

    let result = parseTopLevel(testCode, true);
    if (!result) {
        result = parse(testCode, true);
    }

    if (result) {
        console.log('✅ Parsing successful');
        console.log('AST Type:', result.type);

        const reconstructed = prettyPrint(result);
        console.log('Match:', testCode === reconstructed ? '✅' : '❌');

        if (testCode !== reconstructed) {
            console.log('Original  :', JSON.stringify(testCode));
            console.log('Reconstructed:', JSON.stringify(reconstructed));
        }
    } else {
        console.log('❌ Parsing failed');
    }
});