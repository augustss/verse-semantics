const { parse } = require('./dist/index.js');

const tests = [
    '@editable',
    'Device:enabled_device',
    'Device:enabled_device = enabled_device{}',
    'enabled_device{}',
    'x = 5',
    'x:int = 5'
];

tests.forEach((test, i) => {
    console.log(`\n=== Test ${i + 1}: ${test} ===`);
    const result = parse(test, true);
    console.log(result ? '✅ Parsed' : '❌ Failed');
    if (result) {
        console.log('Type:', result.type);
    }
});