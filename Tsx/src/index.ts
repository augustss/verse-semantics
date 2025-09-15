import { parseVersee } from './parser/parser';

// Helper to convert BigInt to JSON-safe format
function astToJSON(obj: any): any {
  if (obj === null || obj === undefined) return obj;
  if (typeof obj === 'bigint') return obj.toString() + 'n';
  if (Array.isArray(obj)) return obj.map(astToJSON);
  if (typeof obj === 'object') {
    const result: any = {};
    for (const key in obj) {
      result[key] = astToJSON(obj[key]);
    }
    return result;
  }
  return obj;
}

export function testParser() {
  console.log('Testing Versee parser...\n');

  const testCases = [
    '42',
    'true',
    'false',
    '"hello"',
    'x',
    'x + 5',
    'x * y + z',
    '(x + y) * z',
    'x = 5',
    '<decides>',
    '<succeeds>',
    '<fails>',
    '<transacts>',
    '<computes>',
    '<ambiguates>',
    '<reads>',
    '<writes>',
    '<allocates>',
    '<suspends>',
    '<decides>, <succeeds>, <fails>'
  ];

  for (const test of testCases) {
    console.log(`Parsing: ${test}`);
    const result = parseVersee(test);

    if (result.success) {
      console.log('  ✓ Success');
      const jsonSafe = astToJSON(result.value);
      console.log('  AST:', JSON.stringify(jsonSafe, null, 2).split('\n').slice(0, 5).join('\n'));
    } else {
      console.log('  ✗ Failed:', result.error.message);
    }
    console.log();
  }
}

// Run tests or parse file if this is the main module
if (require.main === module) {
  const args = process.argv.slice(2);
  if (args.length > 0) {
    // Parse file mode
    const fs = require('fs');

    const filename = args[0];
    try {
      const fileContent = fs.readFileSync(filename, 'utf8');
      console.log(`Parsing file: ${filename}`);
      console.log('Content:');
      console.log(fileContent);
      console.log('\n' + '='.repeat(50));

      const result = parseVersee(fileContent);

      if (result.success) {
        console.log('✓ PARSING SUCCESSFUL');
        const jsonSafe = astToJSON(result.value);
        console.log('AST:', JSON.stringify(jsonSafe, null, 2));
      } else {
        console.log('✗ PARSING FAILED');
        console.log('Error:', result.error.message);
        console.log('Position:', result.error.position);
      }
    } catch (error: any) {
      console.log('Error reading file:', error.message);
    }
  } else {
    // Run default tests
    testParser();
  }
}

// Export main parsing function
export { parseVersee } from './parser/parser';
export * from './ast/expression';
export * from './ast/pattern';
export * from './ast/identifier';
export * from './ast/location';