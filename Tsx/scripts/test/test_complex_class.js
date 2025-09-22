const { parseTopLevel, prettyPrint } = require('./dist/index.js');

console.log('Testing complex class reconstruction...\n');

const classCode = `game_device := class(creative_device):
    @editable
    EnabledDevice:enabled_device = enabled_device{}

    @editable
    Score:int = 0

    OnEnabled(Agent:agent):void = Print("Device enabled")`;

console.log('Original code:');
console.log(classCode);
console.log('\n' + '='.repeat(50) + '\n');

try {
  const result = parseTopLevel(classCode, false);
  if (result) {
    console.log('✅ Parse successful!');
    console.log('\nTesting reconstruction...');

    try {
      const reconstructed = prettyPrint(result);
      console.log('\nReconstructed code:');
      console.log(reconstructed);

      if (reconstructed === classCode) {
        console.log('\n✅ RECONSTRUCTION MATCHES EXACTLY');
      } else {
        console.log('\n❌ RECONSTRUCTION DIFFERS');
        console.log('\nLength comparison:');
        console.log('  Original:', classCode.length);
        console.log('  Reconstructed:', reconstructed.length);

        // Show first difference
        for (let i = 0; i < Math.max(classCode.length, reconstructed.length); i++) {
          if (classCode[i] !== reconstructed[i]) {
            console.log(`\nFirst difference at position ${i}:`);
            console.log(`  Original: ${JSON.stringify(classCode[i] || '<END>')}`);
            console.log(`  Reconstructed: ${JSON.stringify(reconstructed[i] || '<END>')}`);
            console.log(`\nContext around position ${i}:`);
            console.log(`  Original: ...${classCode.substring(Math.max(0, i-10), i+10)}...`);
            console.log(`  Reconstructed: ...${reconstructed.substring(Math.max(0, i-10), i+10)}...`);
            break;
          }
        }
      }
    } catch (reconstructError) {
      console.log(`❌ RECONSTRUCTION ERROR: ${reconstructError.message}`);
      console.log(reconstructError.stack);
    }
  } else {
    console.log('❌ Parse failed - returned null');
  }
} catch (error) {
  console.log(`❌ PARSE ERROR: ${error.message}`);
}