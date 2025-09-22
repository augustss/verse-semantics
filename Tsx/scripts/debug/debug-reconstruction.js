const { parse, prettyPrint } = require('./dist/index.js');

const code = 'ui_device := class(creative_device):\n    value := 1';
const result = parse(code, true);

console.log('Original:', JSON.stringify(code));
console.log('Parsed type:', result?.type);

if (result) {
  const reconstructed = prettyPrint(result);
  console.log('Reconstructed:', JSON.stringify(reconstructed));
  console.log('Match:', code === reconstructed);
}

// Let's traverse and find the variable 'value'
function findVariables(node, path = '') {
  if (!node || typeof node !== 'object') return;

  if (node.type === 'Variable') {
    console.log('Found Variable at', path + ':');
    console.log('  text:', JSON.stringify(node.token.text));
    console.log('  value:', JSON.stringify(node.token.value));
    console.log('  span:', node.span);
    return;
  }

  for (const [key, value] of Object.entries(node)) {
    if (Array.isArray(value)) {
      value.forEach((item, i) => findVariables(item, path + '.' + key + '[' + i + ']'));
    } else {
      findVariables(value, path + '.' + key);
    }
  }
}

console.log('\nSearching for variables:');
findVariables(result);