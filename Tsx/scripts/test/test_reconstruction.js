const { parse, prettyPrint } = require('./dist/index.js');

console.log('Testing reconstruction of the failing case...\n');

const failingCode = `result := if(true and not false) then {
  for(i : 0..10) {
    if(i > 5) then { break } else { continue }
  }
} else {
  case(status):
    0 => false
    1 => true
    _ => not true
}`;

console.log('Original code:');
console.log(failingCode);
console.log('\n' + '='.repeat(50) + '\n');

try {
  const result = parse(failingCode, false);
  if (result) {
    console.log('✅ Parse successful!');
    console.log('\nTesting reconstruction...');

    try {
      const reconstructed = prettyPrint(result);
      console.log('\nReconstructed code:');
      console.log(reconstructed);

      if (reconstructed === failingCode) {
        console.log('\n✅ RECONSTRUCTION MATCHES EXACTLY');
      } else {
        console.log('\n❌ RECONSTRUCTION DIFFERS');
        console.log('\nDifferences:');
        console.log('Original length:', failingCode.length);
        console.log('Reconstructed length:', reconstructed.length);

        // Show character-by-character diff for first few differences
        for (let i = 0; i < Math.max(failingCode.length, reconstructed.length); i++) {
          if (failingCode[i] !== reconstructed[i]) {
            console.log(`Diff at position ${i}:`);
            console.log(`  Original: ${JSON.stringify(failingCode[i] || '<END>')}`);
            console.log(`  Reconstructed: ${JSON.stringify(reconstructed[i] || '<END>')}`);
            break;
          }
        }
      }
    } catch (reconstructError) {
      console.log(`❌ RECONSTRUCTION ERROR: ${reconstructError.message}`);
    }
  } else {
    console.log('❌ Parse failed - returned null');
  }
} catch (error) {
  console.log(`❌ PARSE ERROR: ${error.message}`);
}