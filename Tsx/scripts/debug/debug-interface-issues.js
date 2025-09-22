const { parseTopLevel, prettyPrint } = require('./dist/index.js');

// Test failing cases
const testCases = [
  'IPlayer := interface:\n    GetHealth():int',
  'struct ModernStruct {\n    Field1: int\n}',
  'interface Empty4 {}',
  'Vector3 := struct:\n    X: float\n    Y: float'
];

for (let i = 0; i < testCases.length; i++) {
  const code = testCases[i];
  console.log(`\n=== Test ${i + 1}: ${code.replace(/\n/g, '\\n')} ===`);

  try {
    const ast = parseTopLevel(code);
    if (ast) {
      console.log('✅ Parsing succeeded');
      console.log('AST:', JSON.stringify(ast, null, 2));

      try {
        const reconstructed = prettyPrint(ast);
        console.log('Reconstructed:', JSON.stringify(reconstructed));

        if (reconstructed === code) {
          console.log('✅ Perfect reconstruction');
        } else {
          console.log('❌ Reconstruction mismatch');
          console.log('Expected:', JSON.stringify(code));
          console.log('Got:     ', JSON.stringify(reconstructed));
        }
      } catch (printErr) {
        console.log('❌ prettyPrint error:', printErr.message);
      }
    } else {
      console.log('❌ Parsing failed');
    }
  } catch (err) {
    console.log('❌ Error:', err.message);
  }
}