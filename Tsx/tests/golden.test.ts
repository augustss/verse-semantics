import { parseVersee } from '../src/parser/parser';

describe('Golden Tests - Replicating Haskell Parser Tests', () => {

  describe('Basic Literals', () => {
    // From syntax.verse - basic literals that should parse successfully
    const literalTests = [
      // Integers
      { input: '0', expected: 'Int', value: 0n },
      { input: '10', expected: 'Int', value: 10n },
      { input: '1234', expected: 'Int', value: 1234n },

      // Floats
      { input: '0.0', expected: 'Float', value: 0.0 },
      { input: '3.14', expected: 'Float', value: 3.14 },
      { input: '1.5', expected: 'Float', value: 1.5 },

      // Characters
      { input: "'a'", expected: 'Char', value: 'a' },
      { input: "'!'", expected: 'Char', value: '!' },
      { input: "'''", expected: 'Char', value: "'" },
      { input: "'\\n'", expected: 'Char', value: '\n' },
      { input: "'\\t'", expected: 'Char', value: '\t' },

      // Strings
      { input: '""', expected: 'String', value: '' },
      { input: '"hello world"', expected: 'String', value: 'hello world' },
      { input: '"a\\nb\\\\c"', expected: 'String', value: 'a\nb\\c' },

      // Booleans and keywords
      { input: 'true', expected: 'True' },
      { input: 'false', expected: 'False' },
      { input: 'fail', expected: 'Fail' },
    ];

    literalTests.forEach(({ input, expected, value }) => {
      test(`parses literal: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (value !== undefined) {
            switch (expected) {
              case 'Int':
                expect((result.value.value as any).value).toBe(value);
                break;
              case 'Float':
                expect((result.value.value as any).value).toBeCloseTo(value as number);
                break;
              case 'Char':
              case 'String':
                expect((result.value.value as any).value || (result.value.value as any).text).toBe(value);
                break;
            }
          }
        }
      });
    });
  });

  describe('Arithmetic Operators', () => {
    // From all.verse - arithmetic expressions
    const arithmeticTests = [
      // Basic operations
      { input: '1+2', expected: 'Add' },
      { input: '5-3', expected: 'Subtract' },
      { input: '3*4', expected: 'Multiply' },
      { input: '10/2', expected: 'Divide' },

      // Precedence tests
      { input: '1+2*3', expected: 'Add', rightKind: 'Multiply' },
      { input: '10-6/2', expected: 'Subtract', rightKind: 'Divide' },
      { input: '1*2+3', expected: 'Add', leftKind: 'Multiply' },

      // Chained operations
      { input: '1+2+3', expected: 'Add' },
      { input: '8-3-2', expected: 'Subtract' },
      { input: '2*3*4', expected: 'Multiply' },

      // Parentheses
      { input: '(1+2)*3', expected: 'Multiply' },
      { input: '2*(3+4)', expected: 'Multiply' },
      { input: '((1+2))', expected: 'List' },
    ];

    arithmeticTests.forEach(({ input, expected, leftKind, rightKind }) => {
      test(`parses arithmetic: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (leftKind && result.value.value.kind === expected && 'left' in result.value.value) {
            expect((result.value.value as any).left.value.kind).toBe(leftKind);
          }

          if (rightKind && result.value.value.kind === expected && 'right' in result.value.value) {
            expect((result.value.value as any).right.value.kind).toBe(rightKind);
          }
        }
      });
    });
  });

  describe('Unary Operators', () => {
    // From all.verse - prefix operators
    const unaryTests = [
      { input: '+1', expected: 'PrefixPlus' },
      { input: '-5', expected: 'PrefixMinus' },
      { input: '*x', expected: 'PrefixMultiply' },
      { input: '-+1', expected: 'PrefixMinus', innerKind: 'PrefixPlus' },
      { input: '++1', expected: 'PrefixPlus', innerKind: 'PrefixPlus' },
    ];

    unaryTests.forEach(({ input, expected, innerKind }) => {
      test(`parses unary: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (innerKind && 'expr' in result.value.value) {
            expect((result.value.value as any).expr.value.kind).toBe(innerKind);
          }
        }
      });
    });
  });

  describe('Comparison Operators', () => {
    // From all.verse - comparison operations
    const comparisonTests = [
      { input: 'x<y', expected: 'Less' },
      { input: 'x<=y', expected: 'LessEqual' },
      { input: 'x>y', expected: 'Greater' },
      { input: 'x>=y', expected: 'GreaterEqual' },
      { input: 'x=y', expected: 'Assign' },

      // Chained comparisons (from all.verse: X < Y < Z)
      { input: 'x<y<z', expected: 'Less', rightKind: 'Less' },
      { input: 'x>y>z', expected: 'Greater', rightKind: 'Greater' },
      { input: 'x>y<z', expected: 'Less', leftKind: 'Greater' },
    ];

    comparisonTests.forEach(({ input, expected, leftKind, rightKind }) => {
      test(`parses comparison: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (leftKind && 'left' in result.value.value) {
            expect((result.value.value as any).left.value.kind).toBe(leftKind);
          }

          if (rightKind && 'right' in result.value.value) {
            expect((result.value.value as any).right.value.kind).toBe(rightKind);
          }
        }
      });
    });
  });

  describe('Logical Operators', () => {
    // From all.verse - logical operations
    const logicalTests = [
      { input: 'x and y', expected: 'And' },
      { input: 'x or y', expected: 'Or' },
      { input: 'not x', expected: 'Not' },

      // Chained logical operations
      { input: 'x and y and z', expected: 'And', rightKind: 'And' },
      { input: 'x or y or z', expected: 'Or', rightKind: 'Or' },
      { input: 'not not x', expected: 'Not', innerKind: 'Not' },

      // Mixed logical operations (precedence: and > or)
      { input: 'x or y and z', expected: 'Or', rightKind: 'And' },
    ];

    logicalTests.forEach(({ input, expected, rightKind, innerKind }) => {
      test(`parses logical: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (rightKind && 'right' in result.value.value) {
            expect((result.value.value as any).right.value.kind).toBe(rightKind);
          }

          if (innerKind && 'expr' in result.value.value) {
            expect((result.value.value as any).expr.value.kind).toBe(innerKind);
          }
        }
      });
    });
  });

  describe('Specifiers (From Haskell tests)', () => {
    // From all.verse - specifier usage
    const specifierTests = [
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
    ];

    specifierTests.forEach(input => {
      test(`parses specifier: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe('Specifier');
          const specName = input.slice(1, -1).toLowerCase(); // Remove < >
          expect((result.value.value as any).spec).toBe(specName);
        }
      });
    });

    // Multiple specifiers
    test('parses multiple specifiers', () => {
      const result = parseVersee('<decides>, <succeeds>');
      expect(result.success).toBe(true);

      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(2);
          expect(result.value.value.elements[0].value.kind).toBe('Specifier');
          expect(result.value.value.elements[1].value.kind).toBe('Specifier');
        }
      }
    });
  });

  describe('Lists and Grouping', () => {
    // From all.verse and syntax.verse - list structures
    const listTests = [
      // Basic lists
      { input: '()', expected: 'List', length: 0 },
      { input: '(1)', expected: 'List', length: 1 },
      { input: '1,2,3', expected: 'List', length: 3 },
      { input: '1;2;3', expected: 'List', length: 3 },

      // Braces
      { input: '{}', expected: 'List', length: 0 },
      { input: '{1}', expected: 'List', length: 1 },
      { input: '{1,2,3}', expected: 'List', length: 3 },
      { input: '{1;2;3}', expected: 'List', length: 3 },

      // Mixed expressions in lists
      { input: 'x, true, "hello"', expected: 'List', length: 3 },
      { input: '1+2, 3*4', expected: 'List', length: 2 },
    ];

    listTests.forEach(({ input, expected, length }) => {
      test(`parses list: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (length !== undefined && result.value.value.kind === 'List') {
            expect(result.value.value.elements.length).toBe(length);
          }
        }
      });
    });
  });

  describe('Complex Operator Precedence', () => {
    // From all.verse - complex precedence tests
    const precedenceTests = [
      // Arithmetic vs comparison
      { input: '1+2 < 3*4', expected: 'Less', leftKind: 'Add', rightKind: 'Multiply' },
      { input: '5-1 > 2+1', expected: 'Greater', leftKind: 'Subtract', rightKind: 'Add' },

      // Comparison vs logical
      { input: 'x<5 and y>3', expected: 'And', leftKind: 'Less', rightKind: 'Greater' },
      { input: 'a=1 or b=2', expected: 'Or', leftKind: 'Assign', rightKind: 'Assign' },

      // Complex mixed precedence
      { input: '1+2*3 < 4 and true', expected: 'And' },
      { input: 'not x > 5', expected: 'Not' },
    ];

    precedenceTests.forEach(({ input, expected, leftKind, rightKind }) => {
      test(`handles precedence: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (leftKind && 'left' in result.value.value) {
            expect((result.value.value as any).left.value.kind).toBe(leftKind);
          }

          if (rightKind && 'right' in result.value.value) {
            expect((result.value.value as any).right.value.kind).toBe(rightKind);
          }
        }
      });
    });
  });

  describe('Identifiers and Variables', () => {
    // From all.verse and syntax.verse - identifier tests
    const identifierTests = [
      'x',
      'apa1',
      'verylongidentifier',
      '_var',
      'var123',
      'X',
      'ABC',
    ];

    identifierTests.forEach(input => {
      test(`parses identifier: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe('Pat');
        }
      });
    });
  });

  describe('Function Declarations', () => {
    // From all.verse and other test files - function declaration tests
    const funcDeclTests = [
      // Basic function declarations (WORKING)
      { input: 'f() := 1', expected: 'FuncDecl', isDefinition: true },
      { input: 'f() = 1', expected: 'Assign', isDefinition: false },
      { input: 'f(x) := x', expected: 'FuncDecl', paramCount: 1 },
      { input: 'g(x, y) := x + y', expected: 'FuncDecl', paramCount: 2 },

      // Function declarations with return types (from all.verse)
      { input: 'f():int = return', expected: 'FuncDecl', hasReturnType: true },
      { input: 'Func()<decides>:int = 1', expected: 'FuncDecl', hasReturnType: true, specifierCount: 1 },

      // Function declarations with specifiers (from all.verse)
      { input: 'f()<decides> := 1', expected: 'FuncDecl', specifierCount: 1 },
      { input: 'f()<decides><succeeds> := 1', expected: 'FuncDecl', specifierCount: 2 },
      { input: 'f3(o:O0)<transacts><decides>:int = return', expected: 'FuncDecl', specifierCount: 2, hasReturnType: true },

      // Complex function declarations (from all.verse)
      { input: 'MFun(X:int)<transacts>:float := X*10+1', expected: 'FuncDecl', hasReturnType: true, specifierCount: 1 },
      { input: 'F(X:int)<decides><closed> := X = 1', expected: 'FuncDecl', specifierCount: 2 },
    ];

    // Additional patterns from Haskell tests we should support
    // TODO: Uncomment when curried function support is implemented
    /*
    const advancedFuncTests = [
      // From execution/function tests - curried functions
      { input: 'F(X:int)(Y:int) := X + Y', expected: 'FuncDecl', description: 'curried function' },

      // From execution/function tests - parameter patterns
      { input: 'Plus1(X:any) := X + 1', expected: 'FuncDecl', description: 'typed parameter' },
      { input: 'F(X:Plus1) := X', expected: 'FuncDecl', description: 'complex parameter type' },

      // From verification/function tests
      { input: 'F(X:int)<decides> := X = 0', expected: 'FuncDecl', description: 'decides specifier' },
      { input: 'F(X:int)<closed><decides> := X = 1', expected: 'FuncDecl', description: 'multiple specifiers' },
    ];
    */

    funcDeclTests.forEach(({ input, expected, isDefinition, paramCount, hasReturnType, specifierCount }) => {
      test(`parses function declaration: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.value.value.kind).toBe(expected);

          if (result.value.value.kind === 'FuncDecl') {
            const decl = result.value.value.decl;

            if (isDefinition !== undefined) {
              expect(decl.isDefinition).toBe(isDefinition);
            }

            if (paramCount !== undefined) {
              expect(decl.params.length).toBe(paramCount);
            }

            if (hasReturnType !== undefined) {
              expect(decl.returnType !== undefined).toBe(hasReturnType);
            }

            if (specifierCount !== undefined) {
              expect(decl.specifiers.length).toBe(specifierCount);
            }
          }
        }
      });
    });

    // Test advanced function patterns (may not all work yet)
    // TODO: Uncomment when curried function support is implemented
    /*
    advancedFuncTests.forEach(({ input, expected, description }) => {
      test(`parses advanced function: ${description} - ${input}`, () => {
        const result = parseVersee(input);
        // Note: These may fail until we implement full support
        if (result.success) {
          expect(result.value.value.kind).toBe(expected);
        } else {
          // For now, just log that these advanced patterns need work
          console.log(`Advanced pattern not yet supported: ${input}`);
        }
      });
    });
    */
  });

  describe('Error Cases', () => {
    // Tests that should fail - from our understanding of the language
    const errorTests = [
      // Reserved words as identifiers
      'if',
      'then',
      'else',
      'var',
      'set',
      'decides', // specifier without < >

      // Malformed literals
      '"unclosed string',
      "'unclosed char",
      "''", // empty char

      // Invalid operators
      '1 === 2',
      'x && y', // should be 'and'
      'x || y', // should be 'or'

      // Mismatched brackets
      '(1 + 2',
      '1 + 2)',
      '{1, 2',
      '1, 2}',
      '[1, 2', // if brackets were supported

      // Invalid specifiers
      '<unknown>',
      '<>',
      '< decides >', // spaces inside
    ];

    errorTests.forEach(input => {
      test(`rejects invalid input: ${input}`, () => {
        const result = parseVersee(input);
        expect(result.success).toBe(false);
      });
    });
  });

});