const { parseTopLevel, prettyPrint } = require('./dist/index.js');

const testCases = [
    // Interface cases
    'ISimple := interface()',
    'IPlayer := interface:\n    GetHealth():int\n    SetHealth(value:int):void',

    // Struct cases
    'Point := struct {}',
    'Vector3 := struct:\n    X: float\n    Y: float\n    Z: float',

    // Enum cases
    'Direction := enum {}',
    'CardSuit := enum:\n    Hearts\n    Diamonds\n    Clubs\n    Spades'
];

testCases.forEach((test, i) => {
    console.log(`\n=== Test ${i + 1}: ${test.split('\n')[0]}... ===`);

    const result = parseTopLevel(test, true);
    if (result) {
        console.log('✅ Parsing successful');
        const reconstructed = prettyPrint(result);
        const match = test === reconstructed;
        console.log('Match:', match ? '✅' : '❌');

        if (!match) {
            console.log('Original  :', JSON.stringify(test));
            console.log('Reconstructed:', JSON.stringify(reconstructed));
        }
    } else {
        console.log('❌ Parsing failed');
    }
});