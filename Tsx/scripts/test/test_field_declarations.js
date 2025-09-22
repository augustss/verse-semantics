const { parseTopLevel, prettyPrint } = require('./dist/index.js');

console.log('Testing field declaration variations...\n');

const tests = [
  // Simple field with int value
  `MyClass := class:
    Score:int = 0`,

  // Field with string value
  `MyClass := class:
    Name:string = "test"`,

  // Field with constructor call - simple
  `MyClass := class:
    Point:Point = Point{}`,

  // Field with constructor call - with parameters
  `MyClass := class:
    Point:Point = Point{x:=1, y:=2}`,

  // Field with typed constructor (like the failing case)
  `MyClass := class:
    Device:enabled_device = enabled_device{}`,

  // Multiple fields including the problematic one
  `MyClass := class:
    Score:int = 0
    Device:enabled_device = enabled_device{}`,

  // With @editable decorator
  `MyClass := class:
    @editable
    Device:enabled_device = enabled_device{}`,
];

tests.forEach((test, i) => {
  console.log(`${i + 1}. Testing:`);
  console.log(test);
  console.log('');

  try {
    const result = parseTopLevel(test, false);
    if (result) {
      console.log('✅ Parse successful');

      try {
        const reconstructed = prettyPrint(result);
        if (reconstructed === test) {
          console.log('✅ Reconstruction exact match');
        } else {
          console.log('❌ Reconstruction differs');
          console.log('Expected length:', test.length);
          console.log('Actual length:', reconstructed.length);
          console.log('Reconstructed:');
          console.log(reconstructed);
        }
      } catch (e) {
        console.log('❌ Reconstruction error:', e.message);
      }
    } else {
      console.log('❌ Parse failed');
    }
  } catch (error) {
    console.log(`❌ Error: ${error.message}`);
  }
  console.log('='.repeat(60) + '\n');
});