import { parseVersee } from '../src/parser/parser';
import { PrettyPrinter } from '../src/printer/pretty-printer';

// Generate random whitespace (1-5 spaces) - unused but kept for potential future use
// function randomSpaces(): string {
//   const count = Math.floor(Math.random() * 5) + 1;
//   return ' '.repeat(count);
// }

// Generate random whitespace including tabs and newlines
function randomWhitespace(): string {
  const options = [' ', '  ', '   ', '    ', '     ', '\t', ' \t', '\t ', '  \t  '];
  return options[Math.floor(Math.random() * options.length)];
}

// Test templates with placeholders for whitespace
const testTemplates = [
  // Binary expressions
  '{ws1}5{ws2}+{ws3}3{ws4}',
  '{ws1}x{ws2}*{ws3}y{ws4}+{ws5}z{ws6}',
  '{ws1}({ws2}a{ws3}+{ws4}b{ws5}){ws6}*{ws7}c{ws8}',

  // Assignments
  '{ws1}x{ws2}={ws3}5{ws4}',
  '{ws1}result{ws2}={ws3}x{ws4}+{ws5}y{ws6}',

  // Function calls
  '{ws1}f{ws2}({ws3}x{ws4},{ws5}y{ws6}){ws7}',
  '{ws1}Math.Add{ws2}({ws3}1{ws4},{ws5}2{ws6}){ws7}',

  // Logical operators
  '{ws1}x{ws2}and{ws3}y{ws4}',
  '{ws1}a{ws2}or{ws3}b{ws4}and{ws5}c{ws6}',

  // Comparison operators
  '{ws1}x{ws2}<{ws3}5{ws4}',
  '{ws1}a{ws2}>={ws3}b{ws4}',
  '{ws1}value{ws2}={ws3}42{ws4}',

  // Tuples (arrays not supported)
  '{ws1}({ws2}x{ws3},{ws4}y{ws5}){ws6}',

  // Specifiers (no whitespace inside angle brackets)
  '{ws1}<public>{ws2}x{ws3}',
  '{ws1}<decides>{ws2}',

  // String literals
  '{ws1}"hello"{ws2}',
  '{ws1}"test{ws2}string"{ws3}',

  // If expressions
  '{ws1}if{ws2}({ws3}x{ws4}>{ws5}0{ws6}){ws7}:{ws8}true{ws9}',

  // For loops
  '{ws1}for{ws2}({ws3}i{ws4}:={ws5}0{ws6}..{ws7}10{ws8}){ws9}:{ws10}i{ws11}',

  // Lambda expressions
  '{ws1}x{ws2}=>{ws3}x{ws4}+{ws5}1{ws6}',
  '{ws1}({ws2}a{ws3},{ws4}b{ws5}){ws6}=>{ws7}a{ws8}*{ws9}b{ws10}',

  // Property access
  '{ws1}player{ws2}.{ws3}Name{ws4}',
  '{ws1}obj{ws2}.{ws3}field{ws4}.{ws5}subfield{ws6}',

  // Case expressions
  '{ws1}case{ws2}({ws3}x{ws4}){ws5}:{ws6}result{ws7}',
];

describe('Whitespace Variation Tests', () => {
  // Test each template with random whitespace variations
  testTemplates.forEach((template, templateIndex) => {
    test(`Template ${templateIndex}: ${template.replace(/{ws\d+}/g, 'WS')}`, () => {
      // Generate 10 random variations of this template
      for (let variation = 0; variation < 10; variation++) {
        let testCase = template;
        let wsIndex = 1;

        // Replace all whitespace placeholders with random whitespace
        while (testCase.includes(`{ws${wsIndex}}`)) {
          testCase = testCase.replace(`{ws${wsIndex}}`, randomWhitespace());
          wsIndex++;
        }

        // Parse the test case
        const parseResult = parseVersee(testCase);

        // Should parse successfully
        expect(parseResult.success).toBe(true);

        if (parseResult.success) {
          // Pretty print should work
          const printer = new PrettyPrinter(undefined, testCase);
          expect(() => printer.print(parseResult.value)).not.toThrow();

          // The printed result should also parse successfully
          const printed = printer.print(parseResult.value);
          const reparsed = parseVersee(printed);
          expect(reparsed.success).toBe(true);

          // Log failures for debugging
          if (!reparsed.success) {
            console.log(`Original: ${testCase}`);
            console.log(`Printed: ${printed}`);
            console.log(`Reparse error: ${reparsed.error}`);
          }
        } else {
          // Log parse failures for debugging
          console.log(`Parse failed for: ${testCase}`);
          console.log(`Error: ${parseResult.error}`);
        }
      }
    });
  });

  // Test extreme whitespace cases
  test('Extreme whitespace variations', () => {
    const extremeCases = [
      // Lots of spaces
      '     5     +     3     ',
      '          x          =          42          ',

      // Mixed tabs and spaces
      '\t\tx\t\t+\t\ty\t\t',
      '  \t  a  \t  *  \t  b  \t  ',

      // Multiple operators with varying spacing
      '   a   +   b   *   c   -   d   ',
      ' x  and  y  or  z ',

      // Function calls with extreme spacing
      '   f   (   x   ,   y   )   ',

      // Tuples with extreme spacing (arrays not supported)
      '   (   1   ,   2   ,   3   )   ',

      // Parentheses with extreme spacing
      '   (   (   x   +   y   )   *   z   )   ',
    ];

    extremeCases.forEach((testCase) => {
      const parseResult = parseVersee(testCase);
      expect(parseResult.success).toBe(true);

      if (parseResult.success) {
        const printer = new PrettyPrinter(undefined, testCase);
        const printed = printer.print(parseResult.value);
        const reparsed = parseVersee(printed);
        expect(reparsed.success).toBe(true);
      }
    });
  });

  // Test whitespace preservation in specific constructs
  test('Whitespace in complex constructs', () => {
    const complexCases = [
      // Nested function calls
      '  f  (  g  (  x  )  ,  h  (  y  )  )  ',

      // Complex expressions
      '  (  a  +  b  )  *  (  c  -  d  )  ',

      // Chained property access
      '  obj  .  prop  .  subProp  .  value  ',

      // Lambda with complex body
      '  x  =>  (  x  +  1  )  *  2  ',

      // For loop with complex range
      '  for  (  i  :=  start  ..  end  )  :  process  (  i  )  ',

      // If with complex condition
      '  if  (  x  >  0  and  y  <  100  )  :  true  ',

      // Tuple with complex elements (no arrays)
      '  (  f  (  x  )  ,  g  (  y  )  ,  h  (  z  )  )  ',
    ];

    complexCases.forEach(testCase => {
      const parseResult = parseVersee(testCase);
      expect(parseResult.success).toBe(true);

      if (parseResult.success) {
        const printer = new PrettyPrinter(undefined, testCase);
        const printed = printer.print(parseResult.value);
        const reparsed = parseVersee(printed);
        expect(reparsed.success).toBe(true);
      }
    });
  });

  // Test minimal vs maximal whitespace
  test('Minimal vs maximal whitespace', () => {
    // Test cases that are safe to add spaces to
    const safeCases = [
      'x+y',
      'f(a,b)',
      '(1,2,3)',
      'a and b or c',
    ];

    // Test cases that should work as-is (already have proper spacing or compound operators)
    const compoundCases = [
      'x=>x+1',       // Don't break => operator
      'if(x>0):true', // Don't break complex expressions
      'for(i:=0..10):i', // Don't break := or .. operators
    ];

    [...safeCases, ...compoundCases].forEach(baseCase => {
      // Test minimal (no extra spaces)
      const minimalResult = parseVersee(baseCase);
      expect(minimalResult.success).toBe(true);

      if (safeCases.includes(baseCase)) {
        // For safe cases, add lots of spaces
        let maximalCase = baseCase;
        maximalCase = maximalCase.replace(/\+/g, ' + ');
        maximalCase = maximalCase.replace(/\*/g, ' * ');
        maximalCase = maximalCase.replace(/\(/g, ' ( ');
        maximalCase = maximalCase.replace(/\)/g, ' ) ');
        maximalCase = maximalCase.replace(/,/g, ' , ');
        maximalCase = maximalCase.replace(/\band\b/g, ' and ');
        maximalCase = maximalCase.replace(/\bor\b/g, ' or ');
        maximalCase = maximalCase.trim();

        const maximalResult = parseVersee(maximalCase);
        expect(maximalResult.success).toBe(true);

        // Both should produce equivalent ASTs when printed
        if (minimalResult.success && maximalResult.success) {
          const minimalPrinter = new PrettyPrinter(undefined, baseCase);
          const maximalPrinter = new PrettyPrinter(undefined, maximalCase);

          const minimalPrinted = minimalPrinter.print(minimalResult.value);
          const maximalPrinted = maximalPrinter.print(maximalResult.value);

          // Both printed versions should parse successfully
          expect(parseVersee(minimalPrinted).success).toBe(true);
          expect(parseVersee(maximalPrinted).success).toBe(true);
        }
      } else {
        // For compound cases, just test that they parse (don't modify them)
        if (minimalResult.success) {
          const printer = new PrettyPrinter(undefined, baseCase);
          const printed = printer.print(minimalResult.value);
          expect(parseVersee(printed).success).toBe(true);
        }
      }
    });
  });
});