const { testReconstruction } = require('./dist/pretty-printer/grammar-audit');

// Test specific cases that were failing
const tests = [
  // Types
  ['Array type', 'x : []int'],
  ['Multi-dim array', 'x : [][]string'],
  ['Optional array', 'x : []?int'],
  
  // Comments
  ['Multiple comments', 'x # comment1\n# comment2'],
  
  // Whitespace
  ['Empty lines', 'x\n\ny'],
  
  // Operators (might be precedence related)
  ['Complex precedence', 'a + b * c - d / e'],
  
  // Variables
  ['Var declaration', 'var x : int = 5']
];

let passed = 0;
let total = tests.length;

for (const [name, source] of tests) {
  const result = testReconstruction(name, source);
  if (result) passed++;
}

console.log(`\nResults: ${passed}/${total} passed`);
