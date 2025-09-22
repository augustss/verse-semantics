const { parse, prettyPrint } = require('./dist/index.js');

// Test cases that should now work
const tests = [
  {
    name: "Simple nested if-else in for loop",
    code: `for(i : 1..10):
  if(i % 2 == 0):
    process(i)
  else:
    skip()`
  },
  {
    name: "Multiple nested if-else",
    code: `for(i : 1..10):
  if(i % 2 == 0):
    if(i > 5):
      bigEven(i)
    else:
      smallEven(i)
  else:
    odd(i)`
  },
  {
    name: "If-else with complex expressions",
    code: `for(item : items):
  if(item.Type == ItemType.Weapon):
    player.AddWeapon(item)
  else:
    player.AddItem(item)`
  }
];

console.log('=== Final Test Results ===');
tests.forEach((test, i) => {
  console.log('\n' + (i + 1) + '. ' + test.name);
  const result = parse(test.code, true); // quiet mode
  if (result) {
    console.log('   ✅ Success');
    // Test reconstruction
    const reconstructed = prettyPrint(result);
    if (reconstructed === test.code) {
      console.log('   ✅ Perfect reconstruction');
    } else {
      console.log('   ⚠️  Reconstruction differs');
    }
  } else {
    console.log('   ❌ Failed to parse');
  }
});
