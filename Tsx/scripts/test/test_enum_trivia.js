const { parseTopLevel, prettyPrint } = require('./dist/index.js');

// Test enum with indentation
const code = `CardSuit := enum:
    Hearts
    Diamonds
    Clubs
    Spades`;

console.log('=== Original Code ===');
console.log(code);

console.log('\n=== Parsing ===');
const result = parseTopLevel(code, true);

if (result) {
    console.log('\n=== Reconstructed Code ===');
    const reconstructed = prettyPrint(result);
    console.log(reconstructed);

    console.log('\n=== Lossless Check ===');
    console.log('Original === Reconstructed:', code === reconstructed);

    console.log('\n=== Debug: Result Structure ===');
    console.log('Result type:', result.type);
    console.log('Result keys:', Object.keys(result));
    if (result.body) {
        console.log('Body length:', result.body.length);
        console.log('Body types:', result.body.map(m => m.type));
    }

    console.log('\n=== Enum Members Trivia ===');
    if (result.type === 'TopLevelDeclaration' && result.body) {
        result.body.forEach((member, index) => {
            console.log(`Member ${index} type: ${member.type}`);
            if (member.type === 'EnumMember') {
                console.log(`  Name: ${member.name.value}`);
                console.log(`  Leading trivia: ${JSON.stringify(member.name.trivia.leading)}`);
                console.log(`  Trailing trivia: ${JSON.stringify(member.name.trivia.trailing)}`);
                console.log(`  Token text: ${JSON.stringify(member.name.text)}`);
            }
        });
    }
} else {
    console.log('Failed to parse');
}