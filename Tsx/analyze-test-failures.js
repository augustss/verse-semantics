// Analyze test failures from the parseset output
const failureData = `
❌ FAIL [all-error-tests.parseset:Test #14] Expected Error Expression but parsed successfully: x => ()
❌ FAIL [all-error-tests.parseset:Test #54] Expected Error Expression but parsed successfully: ()
❌ FAIL [all-error-tests.parseset:Test #64] Expected Error Expression but parsed successfully: x ** y
❌ FAIL [error-type-expression.parseset:Test #2] Expected Error Expression but parsed successfully: emptyType : type{} = getValue()
❌ FAIL [error-type-expression.parseset:Test #4] Expected Error Expression but parsed successfully: result : type{+++} = getValue()
❌ FAIL [error-type-expression.parseset:Test #5] Expected Error Expression but parsed successfully: result : type{getValue(} = getValue()
❌ FAIL [error-type-expression.parseset:Test #6] Expected Error Expression but parsed successfully: result : type{getValue() + +} = getValue()
❌ FAIL [error-type-expression.parseset:Test #7] Expected Error Expression but parsed successfully: result : type{} = getValue()
❌ FAIL [error-type-expression.parseset:Test #9] Expected Error Expression but parsed successfully: result : type{x := 5} = getValue()
❌ FAIL [error-type-expression.parseset:Test #10] Expected Error Expression but parsed successfully: result : type{set x = 5} = getValue()
❌ FAIL [error-type-expression.parseset:Test #11] Expected Error Expression but parsed successfully: result : type{return 5} = getValue()
❌ FAIL [error-type-expression.parseset:Test #12] Expected Error Expression but parsed successfully: result : type{break} = getValue()
❌ FAIL [error-type-expression.parseset:Test #14] Expected Error Expression but parsed successfully: result : type{{getValue()}} = getValue()
❌ FAIL [now-passing.parseset:Test #96] Expected Valid Expression but got error: if:\n    result := calculate(x + y * z)\n    object := Entity
❌ FAIL [valid-data-structures.parseset:Test #86] Expected Valid Expression but got error: Config := class { host := "localhost"; port := 8080; url := "http://localhost:8080" }
❌ FAIL [valid-declarations.parseset:Test #6] Expected Valid Expression but got error: point := Point{x:=10, y:=20}
❌ FAIL [valid-declarations.parseset:Test #54] Expected Valid Expression but got error: config := Player:\n  name := playerName\n  x := 10\n  y := 20
❌ FAIL [valid-declarations.parseset:Test #57] Expected Valid Expression but got error: increment := x => x + 1
❌ FAIL [valid-expression.parseset:Test #61] Expected Valid Expression but got error: Player:\n  pos := Point{x:=0, y:=0}\n  velocity := Vector{x:=0, y:=0}
❌ FAIL [valid-expression.parseset:Test #62] Expected Valid Expression but got error: GameState:\n  player := Player{name:=hero, level:=10}
❌ FAIL [valid-expression.parseset:Test #258] Expected Valid Expression but got error: result := not false or true and false or (true and not false)
❌ FAIL [valid-expression.parseset:Test #262] Expected Valid Expression but got error: config := Config{\n  maxPlayers := 100,\n  enablePvP := true\n}
❌ FAIL [valid-expression.parseset:Test #286] Expected Valid Expression but got error: increment := x => x + 1
❌ FAIL [valid-type-expression.parseset:Test #11] Expected Valid Expression but got error: processValue(callback : type{_() : int}) : int = callback()
❌ FAIL [valid-type-expression.parseset:Test #12] Expected Valid Expression but got error: getProcessor() : type{_() : int} = getValue
❌ FAIL [valid-type-expression.parseset:Test #15] Expected Valid Expression but got error: outer(inner() : int) : int = inner()
`;

console.log('='.repeat(80));
console.log('ANALYSIS OF PRE-EXISTING PARSER FAILURES');
console.log('='.repeat(80));

// Parse the failure data
const lines = failureData.trim().split('\n').filter(line => line.includes('❌ FAIL'));

const categories = {
  'Expected Errors Now Passing': [],
  'Lambda Expression Issues': [],
  'Object Constructor Issues': [],
  'Class Definition Issues': [],
  'Indented Block Issues': [],
  'Boolean Expression Issues': [],
  'Function Type Parameter Issues': [],
  'Type Expression Validation Issues': [],
  'Other Issues': []
};

for (const line of lines) {
  const match = line.match(/❌ FAIL \[([^\]]+)\] (.+?):(.*)/);
  if (!match) continue;

  const [, testFile, errorType, codeSnippet] = match;
  const code = codeSnippet.trim();

  const failure = {
    testFile,
    errorType: errorType.trim(),
    code: code.length > 100 ? code.substring(0, 100) + '...' : code
  };

  // Categorize failures
  if (errorType.includes('Expected Error Expression but parsed successfully')) {
    categories['Expected Errors Now Passing'].push(failure);
  } else if (code.includes('=>')) {
    categories['Lambda Expression Issues'].push(failure);
  } else if (code.includes('Point{') || code.includes('Player{') || code.includes('Config{')) {
    categories['Object Constructor Issues'].push(failure);
  } else if (code.includes('class {')) {
    categories['Class Definition Issues'].push(failure);
  } else if (code.includes('\\n') && code.includes(':=')) {
    categories['Indented Block Issues'].push(failure);
  } else if (code.includes('not ') && code.includes(' or ') && code.includes(' and ')) {
    categories['Boolean Expression Issues'].push(failure);
  } else if (code.includes('type{_()')) {
    categories['Function Type Parameter Issues'].push(failure);
  } else if (code.includes('type{') && errorType.includes('Expected Error Expression but parsed successfully')) {
    categories['Type Expression Validation Issues'].push(failure);
  } else {
    categories['Other Issues'].push(failure);
  }
}

// Display analysis
for (const [category, failures] of Object.entries(categories)) {
  if (failures.length === 0) continue;

  console.log(`\n${category} (${failures.length} issues):`);
  console.log('-'.repeat(category.length + ` (${failures.length} issues):`.length));

  for (const failure of failures) {
    console.log(`  • ${failure.errorType}`);
    console.log(`    Code: ${failure.code}`);
    console.log(`    File: ${failure.testFile}`);
    console.log();
  }
}

console.log('='.repeat(80));
console.log('SUMMARY OF MAJOR PARSER GAPS');
console.log('='.repeat(80));

const majorIssues = [];

if (categories['Lambda Expression Issues'].length > 0) {
  majorIssues.push(`🔸 Lambda Expressions: ${categories['Lambda Expression Issues'].length} failures`);
  majorIssues.push('   Missing support for x => y syntax');
}

if (categories['Object Constructor Issues'].length > 0) {
  majorIssues.push(`🔸 Object Constructors: ${categories['Object Constructor Issues'].length} failures`);
  majorIssues.push('   Issues with Object{field:=value} syntax');
}

if (categories['Indented Block Issues'].length > 0) {
  majorIssues.push(`🔸 Indented Blocks: ${categories['Indented Block Issues'].length} failures`);
  majorIssues.push('   Problems with multi-line := assignments');
}

if (categories['Function Type Parameter Issues'].length > 0) {
  majorIssues.push(`🔸 Function Type Parameters: ${categories['Function Type Parameter Issues'].length} failures`);
  majorIssues.push('   type{_() : int} in function parameters');
}

if (categories['Expected Errors Now Passing'].length > 0) {
  majorIssues.push(`🔸 Error Test Validation: ${categories['Expected Errors Now Passing'].length} cases`);
  majorIssues.push('   Tests expect errors but parser now accepts them');
}

if (categories['Type Expression Validation Issues'].length > 0) {
  majorIssues.push(`🔸 Type Expression Validation: ${categories['Type Expression Validation Issues'].length} failures`);
  majorIssues.push('   Invalid type{} expressions should be rejected');
}

console.log(majorIssues.join('\n'));

console.log('\n🎯 RECOMMENDATION:');
console.log('The most impactful areas to work on would be:');
console.log('1. Lambda expressions (x => y) - commonly used feature');
console.log('2. Object constructors (Object{field:=value}) - core language feature');
console.log('3. Complex multi-line expressions - readability feature');
console.log('4. Function type parameters - advanced type system feature');

console.log('\nNote: The "Expected Errors Now Passing" category may actually indicate');
console.log('parser improvements rather than regressions.');