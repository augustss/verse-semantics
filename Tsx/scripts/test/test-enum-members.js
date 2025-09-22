const { parseTopLevel, prettyPrint } = require('./dist/index.js');

const enumTest = `CardSuit := enum:
    Hearts
    Diamonds
    Clubs
    Spades`;

console.log('=== Testing Enum Member Parsing ===');
console.log('Original:');
console.log(enumTest);

const result = parseTopLevel(enumTest, true);
if (result) {
    console.log('\n✅ Parsing successful');
    console.log('AST:');
    console.log(JSON.stringify(result, null, 2));

    const reconstructed = prettyPrint(result);
    console.log('\nReconstructed:');
    console.log(reconstructed);

    const match = enumTest === reconstructed;
    console.log('\nMatch:', match ? '✅' : '❌');

    if (!match) {
        console.log('Expected length:', enumTest.length);
        console.log('Actual length:', reconstructed.length);
        console.log('Difference at character:', enumTest.split('').findIndex((c, i) => c !== reconstructed[i]));
    }
} else {
    console.log('❌ Parsing failed');
}