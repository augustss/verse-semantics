const { parse } = require('./dist/index.js');

console.log('Testing indented syntax failures...\n');

const tests = [
  // Simple indented if (should work)
  `if(condition) then:
  result`,

  // Indented if with empty line (failing case)
  `if(condition) then:

  result`,

  // Empty then block
  `if(condition) then:


else:
  default`,

  // Complex nested with empty lines
  `for(item : items):
  result := process(item)
  if(result > threshold) then:
    cache[item.id] := transform(result)
    log("Processed: " + item.name)
  else:
    skip(item)`,
];

tests.forEach((test, i) => {
  console.log(`${i + 1}. Testing:`);
  console.log('```');
  console.log(test);
  console.log('```');
  console.log('');

  try {
    const result = parse(test, false);
    if (result) {
      console.log('✅ Parse successful');
    } else {
      console.log('❌ Parse failed - returned null');
    }
  } catch (error) {
    console.log(`❌ Parse error: ${error.message}`);
  }
  console.log('='.repeat(60) + '\n');
});