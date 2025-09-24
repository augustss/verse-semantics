/**
 * Grammar Reconstruction Audit
 *
 * This script systematically tests that every construct defined in GRAMMAR.md
 * can be perfectly reconstructed from its AST, preserving all tokens.
 */

import { TokenStream } from '../lexer';
import { createParser, createParserState, parseProgram } from '../parser';
import { reconstructFromAST } from './ast-reconstructor';
import * as fs from 'fs';

// Color codes
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const BLUE = '\x1b[34m';
const CYAN = '\x1b[36m';
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';

interface TestCase {
  category: string;
  name: string;
  source: string;
  description?: string;
}

/**
 * Test reconstruction of a source string
 */
function testReconstruction(test: TestCase): boolean {
  try {
    const tokenStream = TokenStream.fromString(test.source);
    const parser = createParser();
    const state = createParserState(tokenStream);

    // Try parsing as expression
    let ast;
    try {
      const result = parser.parseExpression(state);
      if (result && result.state.isAtEnd()) {
        ast = result.node;
      } else {
        // Try as declaration
        const program = parseProgram(test.source);
        if (!program || program.declarations.length === 0) throw new Error('Parse failed');
        ast = program.declarations[0];
      }
    } catch {
      const program = parseProgram(test.source);
      if (!program || program.declarations.length === 0) throw new Error('Parse failed');
      ast = program.declarations[0];
    }

    // Reconstruct
    const reconstructed = reconstructFromAST(test.source, ast, {
      includeTrailingTrivia: true,
      tokenStream
    });

    if (reconstructed === test.source) {
      console.log(`  ${GREEN}✓${RESET} ${test.name}`);
      return true;
    } else {
      console.log(`  ${RED}✗${RESET} ${test.name}`);
      console.log(`    Original:      "${test.source}"`);
      console.log(`    Reconstructed: "${reconstructed}"`);

      // Show difference
      const minLen = Math.min(test.source.length, reconstructed.length);
      for (let i = 0; i < minLen; i++) {
        if (test.source[i] !== reconstructed[i]) {
          console.log(`    First diff at position ${i}: '${test.source[i]}' vs '${reconstructed[i]}'`);
          break;
        }
      }
      if (test.source.length !== reconstructed.length) {
        console.log(`    Length diff: ${test.source.length} vs ${reconstructed.length}`);
      }
      return false;
    }
  } catch (error: any) {
    console.log(`  ${YELLOW}⚠${RESET} ${test.name}: ${error.message}`);
    return false;
  }
}

/**
 * Grammar test cases organized by section
 */
const grammarTests: TestCase[] = [
  // ============= LITERALS =============
  { category: 'Literals', name: 'Integer', source: '42' },
  { category: 'Literals', name: 'Negative integer', source: '-42' },
  { category: 'Literals', name: 'Float', source: '3.14' },
  { category: 'Literals', name: 'Float no leading', source: '.5' },
  { category: 'Literals', name: 'Float no trailing', source: '5.' },
  { category: 'Literals', name: 'Negative float', source: '-3.14' },
  { category: 'Literals', name: 'String double quotes', source: '"hello"' },
  { category: 'Literals', name: 'String with escapes', source: '"hello\\nworld"' },
  { category: 'Literals', name: 'Empty string', source: '""' },

  // ============= IDENTIFIERS =============
  { category: 'Identifiers', name: 'Simple identifier', source: 'myVariable' },
  { category: 'Identifiers', name: 'Underscore identifier', source: '_private' },
  { category: 'Identifiers', name: 'At-identifier', source: '@special' },
  { category: 'Identifiers', name: 'Mixed case', source: 'MyClass_123' },

  // ============= OPERATORS =============
  { category: 'Operators', name: 'Assignment :=', source: 'x := 5' },
  { category: 'Operators', name: 'Assignment =', source: 'x = 5' },
  { category: 'Operators', name: 'Compound +=', source: 'x += 5' },
  { category: 'Operators', name: 'Compound -=', source: 'x -= 5' },
  { category: 'Operators', name: 'Compound *=', source: 'x *= 5' },
  { category: 'Operators', name: 'Compound /=', source: 'x /= 5' },
  { category: 'Operators', name: 'Equality ==', source: 'x == y' },
  { category: 'Operators', name: 'Inequality !=', source: 'x != y' },
  { category: 'Operators', name: 'Less than <', source: 'x < y' },
  { category: 'Operators', name: 'Less equal <=', source: 'x <= y' },
  { category: 'Operators', name: 'Greater than >', source: 'x > y' },
  { category: 'Operators', name: 'Greater equal >=', source: 'x >= y' },
  { category: 'Operators', name: 'Addition +', source: 'x + y' },
  { category: 'Operators', name: 'Subtraction -', source: 'x - y' },
  { category: 'Operators', name: 'Multiplication *', source: 'x * y' },
  { category: 'Operators', name: 'Division /', source: 'x / y' },
  { category: 'Operators', name: 'Modulo %', source: 'x % y' },
  { category: 'Operators', name: 'Logical and', source: 'x and y' },
  { category: 'Operators', name: 'Logical or', source: 'x or y' },
  { category: 'Operators', name: 'Logical not', source: 'not x' },
  { category: 'Operators', name: 'Range ..', source: '1..10' },
  { category: 'Operators', name: 'Lambda =>', source: 'x => x + 1' },
  { category: 'Operators', name: 'Member .', source: 'obj.field' },

  // ============= DELIMITERS & SEPARATORS =============
  { category: 'Delimiters', name: 'Parentheses', source: '(x + y)' },
  { category: 'Delimiters', name: 'Braces', source: '{ x; y }' },
  { category: 'Delimiters', name: 'Brackets', source: 'arr[0]' },
  { category: 'Delimiters', name: 'Comma separator', source: 'f(x, y, z)' },
  { category: 'Delimiters', name: 'Semicolon separator', source: '{ x; y; z }' },
  { category: 'Delimiters', name: 'Colon in type', source: 'var x : int = 5' },

  // ============= COMPOUND EXPRESSIONS =============
  { category: 'Compound', name: 'Empty compound', source: '{}' },
  { category: 'Compound', name: 'Single element', source: '{ x }' },
  { category: 'Compound', name: 'Multiple elements', source: '{ x; y; z }' },
  { category: 'Compound', name: 'With newlines', source: '{\n  x\n  y\n}' },
  { category: 'Compound', name: 'With semicolons', source: '{ x; y; z; }' },

  // ============= ARRAY EXPRESSIONS =============
  { category: 'Arrays', name: 'Empty array', source: 'array{}' },
  { category: 'Arrays', name: 'Array with elements', source: 'array{1, 2, 3}' },
  { category: 'Arrays', name: 'Array with spaces', source: 'array{ 1 , 2 , 3 }' },
  { category: 'Arrays', name: 'Array trailing comma', source: 'array{1, 2, 3,}' },
  { category: 'Arrays', name: 'Array indented', source: 'array:\n  1\n  2\n  3' },

  // ============= LAMBDA EXPRESSIONS =============
  { category: 'Lambda', name: 'Single param', source: 'x => x + 1' },
  { category: 'Lambda', name: 'Multi param parens', source: '(x, y) => x + y' },
  { category: 'Lambda', name: 'Multi param no parens', source: 'x, y => x + y' },
  { category: 'Lambda', name: 'No params', source: '() => 42' },
  { category: 'Lambda', name: 'Complex body', source: 'x => { y := x + 1; y * 2 }' },

  // ============= OBJECT CONSTRUCTION =============
  { category: 'Objects', name: 'Empty object', source: 'Type{}' },
  { category: 'Objects', name: 'Single field', source: 'Point{x:=1}' },
  { category: 'Objects', name: 'Multiple fields', source: 'Point{x:=1, y:=2}' },
  { category: 'Objects', name: 'With spaces', source: 'Point{ x := 1 , y := 2 }' },
  { category: 'Objects', name: 'Trailing comma', source: 'Point{x:=1, y:=2,}' },
  { category: 'Objects', name: 'Nested object', source: 'Rect{p1:=Point{x:=0}, p2:=Point{x:=1}}' },

  // ============= MEMBER ACCESS =============
  { category: 'Members', name: 'Dot access', source: 'obj.field' },
  { category: 'Members', name: 'Chained dots', source: 'a.b.c.d' },
  { category: 'Members', name: 'Computed access', source: 'arr[0]' },
  { category: 'Members', name: 'Mixed access', source: 'obj.arr[0].field' },
  { category: 'Members', name: 'Complex index', source: 'arr[i + 1]' },

  // ============= CALL EXPRESSIONS =============
  { category: 'Calls', name: 'No args', source: 'func()' },
  { category: 'Calls', name: 'Single arg', source: 'func(x)' },
  { category: 'Calls', name: 'Multiple args', source: 'func(x, y, z)' },
  { category: 'Calls', name: 'With spaces', source: 'func( x , y , z )' },
  { category: 'Calls', name: 'Nested calls', source: 'f(g(h()))' },
  { category: 'Calls', name: 'Chained calls', source: 'obj.method1().method2()' },

  // ============= IF EXPRESSIONS =============
  { category: 'If', name: 'Simple if-then', source: 'if (x) then y' },
  { category: 'If', name: 'If-then-else', source: 'if (x) then y else z' },
  { category: 'If', name: 'No parens', source: 'if x then y else z' },
  { category: 'If', name: 'With blocks', source: 'if (x) then { a; b } else { c; d }' },
  { category: 'If', name: 'Indented then', source: 'if (x):\n  statement1\n  statement2' },
  { category: 'If', name: 'Indented if-then-else', source: 'if:\n  x\nthen:\n  y\nelse:\n  z' },

  // ============= FOR EXPRESSIONS =============
  { category: 'For', name: 'Simple for', source: 'for (x : items) { process(x) }' },
  { category: 'For', name: 'For with index', source: 'for (i -> x : items) { process(i, x) }' },
  { category: 'For', name: 'For no parens', source: 'for x : items { process(x) }' },
  { category: 'For', name: 'For indented', source: 'for:\n  items\ndo:\n  process' },
  { category: 'For', name: 'For single expr', source: 'for (x : items) process(x)' },

  // ============= LOOP EXPRESSIONS =============
  { category: 'Loop', name: 'Loop with block', source: 'loop { doWork() }' },
  { category: 'Loop', name: 'Loop single expr', source: 'loop doWork()' },
  { category: 'Loop', name: 'Loop indented', source: 'loop:\n  statement1\n  statement2' },

  // ============= BLOCK EXPRESSIONS =============
  { category: 'Block', name: 'Block indented', source: 'block:\n  statement1\n  statement2' },
  { category: 'Block', name: 'Block nested', source: 'block:\n  block:\n    inner' },

  // ============= CASE EXPRESSIONS =============
  { category: 'Case', name: 'Case braces', source: 'case(x) { 0 => a, 1 => b }' },
  { category: 'Case', name: 'Case indented', source: 'case(x):\n  0 => a\n  1 => b' },
  { category: 'Case', name: 'Case wildcard', source: 'case(x) { 0 => a, _ => b }' },
  { category: 'Case', name: 'Case identifiers', source: 'case(x) { Red => 1, Green => 2 }' },

  // ============= SET EXPRESSIONS =============
  { category: 'Set', name: 'Set simple', source: 'set x = 5' },
  { category: 'Set', name: 'Set member', source: 'set obj.field = value' },
  { category: 'Set', name: 'Set computed', source: 'set arr[i] = value' },

  // ============= CONTROL FLOW STATEMENTS =============
  { category: 'Control', name: 'Break', source: 'break' },
  { category: 'Control', name: 'Continue', source: 'continue' },
  { category: 'Control', name: 'Return', source: 'return' },
  { category: 'Control', name: 'Return value', source: 'return 42' },

  // ============= VARIABLE DECLARATIONS =============
  { category: 'Variables', name: 'Var simple', source: 'var x : int' },
  { category: 'Variables', name: 'Var with init', source: 'var x : int = 5' },
  { category: 'Variables', name: 'Var inferred', source: 'var x := 5' },
  { category: 'Variables', name: 'Var with spaces', source: 'var  x  :  int  =  5' },
  { category: 'Variables', name: 'Const simple', source: 'x := 5' },
  { category: 'Variables', name: 'Const typed', source: 'x : int = 5' },

  // ============= FUNCTION DECLARATIONS =============
  { category: 'Functions', name: 'Func no params', source: 'f() := 42' },
  { category: 'Functions', name: 'Func with params', source: 'f(x, y) := x + y' },
  { category: 'Functions', name: 'Func typed params', source: 'f(x:int, y:int) := x + y' },
  { category: 'Functions', name: 'Func return type', source: 'f():int = 42' },
  { category: 'Functions', name: 'Func full typed', source: 'f(x:int, y:int):int = x + y' },
  { category: 'Functions', name: 'Func default params', source: 'f(x:int=0, y:int=1) := x + y' },

  // ============= SPECIFIERS =============
  { category: 'Specifiers', name: 'Public specifier', source: 'f<public>() := 42' },
  { category: 'Specifiers', name: 'Multiple specifiers', source: 'f<public, decides>() := 42' },
  { category: 'Specifiers', name: 'Pre and post', source: 'f<public>()<decides> := 42' },
  { category: 'Specifiers', name: 'Scoped specifier', source: 'f<scoped(class)>() := 42' },
  { category: 'Specifiers', name: 'Var specifier', source: 'var x<public> : int = 5' },

  // ============= DATA STRUCTURES =============
  { category: 'DataStructures', name: 'Empty class', source: 'MyClass := class {}' },
  { category: 'DataStructures', name: 'Class with fields', source: 'MyClass := class { x:int; y:int }' },
  { category: 'DataStructures', name: 'Class indented', source: 'MyClass := class:\n  x:int\n  y:int' },
  { category: 'DataStructures', name: 'Class inheritance', source: 'Child := class(Parent) {}' },
  { category: 'DataStructures', name: 'Module empty', source: 'MyModule := module {}' },
  { category: 'DataStructures', name: 'Module with content', source: 'MyModule := module { x := 5 }' },
  { category: 'DataStructures', name: 'Interface', source: 'IFace := interface { Method():void }' },
  { category: 'DataStructures', name: 'Struct', source: 'Point := struct { x:float; y:float }' },
  { category: 'DataStructures', name: 'Enum simple', source: 'Color := enum { Red, Green, Blue }' },
  { category: 'DataStructures', name: 'Enum with values', source: 'Color := enum { Red=1, Green=2, Blue=3 }' },

  // ============= TYPE EXPRESSIONS =============
  { category: 'Types', name: 'Simple type', source: 'var x : int' },
  { category: 'Types', name: 'Optional type', source: 'var x : ?int' },
  { category: 'Types', name: 'Array type', source: 'var x : []int' },
  { category: 'Types', name: 'Multi-dim array', source: 'var x : [][]int' },
  { category: 'Types', name: 'Optional array', source: 'var x : ?[]int' },
  { category: 'Types', name: 'Qualified type', source: 'var x : Module.Type' },

  // ============= COMMENTS =============
  { category: 'Comments', name: 'Single line', source: 'x := 5  # comment' },
  { category: 'Comments', name: 'Multi line', source: 'x <# comment #> + y' },
  { category: 'Comments', name: 'Nested comments', source: 'x <# outer <# inner #> outer #> + y' },
  { category: 'Comments', name: 'Comment at start', source: '# comment\nx := 5' },
  { category: 'Comments', name: 'Multiple comments', source: '# comment 1\nx := 5  # comment 2\ny := 10  # comment 3' },

  // ============= WHITESPACE & INDENTATION =============
  { category: 'Whitespace', name: 'Leading spaces', source: '  x := 5' },
  { category: 'Whitespace', name: 'Trailing spaces', source: 'x := 5  ' },
  { category: 'Whitespace', name: 'Mixed spacing', source: 'x   :=   5' },
  { category: 'Whitespace', name: 'Tabs', source: '\tx := 5' },
  { category: 'Whitespace', name: 'Empty lines', source: 'x := 5\n\ny := 10' },
];

/**
 * Run all grammar tests
 */
function runGrammarAudit(): void {
  console.log(`\n${BOLD}Grammar Reconstruction Audit${RESET}`);
  console.log('=' .repeat(60) + '\n');

  const results: Map<string, {passed: number, total: number}> = new Map();
  let totalPassed = 0;
  let totalTests = 0;

  // Group tests by category
  const categories = new Map<string, TestCase[]>();
  for (const test of grammarTests) {
    if (!categories.has(test.category)) {
      categories.set(test.category, []);
    }
    categories.get(test.category)!.push(test);
  }

  // Run tests by category
  for (const [category, tests] of categories) {
    console.log(`${BLUE}${category}:${RESET}`);
    let categoryPassed = 0;

    for (const test of tests) {
      if (testReconstruction(test)) {
        categoryPassed++;
        totalPassed++;
      }
      totalTests++;
    }

    results.set(category, {passed: categoryPassed, total: tests.length});
    console.log(`  ${CYAN}Subtotal: ${categoryPassed}/${tests.length}${RESET}\n`);
  }

  // Summary
  console.log('=' .repeat(60));
  console.log(`\n${BOLD}Category Summary:${RESET}`);
  for (const [category, result] of results) {
    const percentage = ((result.passed / result.total) * 100).toFixed(1);
    const color = result.passed === result.total ? GREEN :
                  result.passed / result.total > 0.8 ? YELLOW : RED;
    console.log(`  ${category}: ${color}${result.passed}/${result.total}${RESET} (${percentage}%)`);
  }

  console.log('\n' + '=' .repeat(60));
  const percentage = ((totalPassed / totalTests) * 100).toFixed(1);
  const color = totalPassed === totalTests ? GREEN :
                totalPassed / totalTests > 0.9 ? YELLOW : RED;
  console.log(`${BOLD}Total: ${color}${totalPassed}/${totalTests}${RESET} tests passed (${percentage}%)\n`);

  // List problem areas
  if (totalPassed < totalTests) {
    console.log(`${BOLD}${RED}Problem Areas:${RESET}`);
    for (const [category, result] of results) {
      if (result.passed < result.total) {
        console.log(`  - ${category}: ${result.total - result.passed} failures`);
      }
    }
    console.log();
  }
}

// Run the audit
if (require.main === module) {
  runGrammarAudit();
}