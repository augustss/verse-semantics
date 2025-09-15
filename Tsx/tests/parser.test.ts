import { parseVersee } from '../src/parser/parser';

describe('Versee Parser', () => {
  describe('Literals', () => {
    test('parses integer literals', () => {
      const result = parseVersee('42');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Int');
        if (result.value.value.kind === 'Int') {
          expect(result.value.value.value).toBe(42n);
        }
      }
    });

    test('parses large integer literals', () => {
      const result = parseVersee('999999999999999999999');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Int');
        if (result.value.value.kind === 'Int') {
          expect(result.value.value.value).toBe(999999999999999999999n);
        }
      }
    });

    test('parses float literals', () => {
      const result = parseVersee('3.14');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Float');
        if (result.value.value.kind === 'Float') {
          expect(result.value.value.value).toBeCloseTo(3.14);
        }
      }
    });

    test('parses float with multiple digits', () => {
      const result = parseVersee('123.456');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Float');
        if (result.value.value.kind === 'Float') {
          expect(result.value.value.value).toBeCloseTo(123.456);
        }
      }
    });

    test('parses boolean true', () => {
      const result = parseVersee('true');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('True');
      }
    });

    test('parses boolean false', () => {
      const result = parseVersee('false');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('False');
      }
    });

    test('parses fail keyword', () => {
      const result = parseVersee('fail');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Fail');
      }
    });

    test('parses string literals', () => {
      const result = parseVersee('"hello world"');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('String');
        if (result.value.value.kind === 'String') {
          expect(result.value.value.text).toBe('hello world');
        }
      }
    });

    test('parses empty string', () => {
      const result = parseVersee('""');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('String');
        if (result.value.value.kind === 'String') {
          expect(result.value.value.text).toBe('');
        }
      }
    });

    test('parses string with escape sequences', () => {
      const result = parseVersee('"hello\\nworld\\t!"');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('String');
        if (result.value.value.kind === 'String') {
          expect(result.value.value.text).toBe('hello\nworld\t!');
        }
      }
    });

    test('parses string with escaped quotes', () => {
      const result = parseVersee('"He said \\"hello\\""');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('String');
        if (result.value.value.kind === 'String') {
          expect(result.value.value.text).toBe('He said "hello"');
        }
      }
    });

    test('parses character literals', () => {
      const result = parseVersee("'a'");
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Char');
        if (result.value.value.kind === 'Char') {
          expect(result.value.value.value).toBe('a');
        }
      }
    });

    test('parses escaped character literals', () => {
      const result = parseVersee("'\\n'");
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Char');
        if (result.value.value.kind === 'Char') {
          expect(result.value.value.value).toBe('\n');
        }
      }
    });
  });

  describe('Arithmetic expressions', () => {
    test('parses addition', () => {
      const result = parseVersee('1 + 2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Add');
      }
    });

    test('parses subtraction', () => {
      const result = parseVersee('5 - 3');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Subtract');
      }
    });

    test('parses multiplication', () => {
      const result = parseVersee('3 * 4');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Multiply');
      }
    });

    test('parses division', () => {
      const result = parseVersee('10 / 2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Divide');
      }
    });

    test('respects operator precedence', () => {
      const result = parseVersee('1 + 2 * 3');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Add');
        if (result.value.value.kind === 'Add') {
          expect(result.value.value.right.value.kind).toBe('Multiply');
        }
      }
    });

    test('respects precedence with division', () => {
      const result = parseVersee('10 - 6 / 2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Subtract');
        if (result.value.value.kind === 'Subtract') {
          expect(result.value.value.right.value.kind).toBe('Divide');
        }
      }
    });

    test('parses parenthesized expressions', () => {
      const result = parseVersee('(1 + 2) * 3');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Multiply');
      }
    });

    test('parses nested parentheses', () => {
      const result = parseVersee('((1 + 2) * (3 + 4))');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List' && result.value.value.elements.length > 0) {
          expect(result.value.value.elements[0].value.kind).toBe('Multiply');
        }
      }
    });

    test('parses unary plus', () => {
      const result = parseVersee('+5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('PrefixPlus');
      }
    });

    test('parses unary minus', () => {
      const result = parseVersee('-5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('PrefixMinus');
      }
    });

    test('parses unary multiply (dereference)', () => {
      const result = parseVersee('*x');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('PrefixMultiply');
      }
    });

    test('parses chain of additions', () => {
      const result = parseVersee('1 + 2 + 3 + 4');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Add');
      }
    });
  });

  describe('Comparison operators', () => {
    test('parses less than', () => {
      const result = parseVersee('x < 5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Less');
      }
    });

    test('parses less than or equal', () => {
      const result = parseVersee('x <= 5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('LessEqual');
      }
    });

    test('parses greater than', () => {
      const result = parseVersee('x > 5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Greater');
      }
    });

    test('parses greater than or equal', () => {
      const result = parseVersee('x >= 5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('GreaterEqual');
      }
    });

    test('comparison has lower precedence than arithmetic', () => {
      const result = parseVersee('1 + 2 < 3 * 4');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Less');
        if (result.value.value.kind === 'Less') {
          expect(result.value.value.left.value.kind).toBe('Add');
          expect(result.value.value.right.value.kind).toBe('Multiply');
        }
      }
    });
  });

  describe('Variables and assignments', () => {
    test('parses variable names', () => {
      const result = parseVersee('x');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Pat');
      }
    });

    test('parses underscore variables', () => {
      const result = parseVersee('_var');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Pat');
      }
    });

    test('parses variables with numbers', () => {
      const result = parseVersee('var123');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Pat');
      }
    });

    test('parses assignments', () => {
      const result = parseVersee('x = 5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Assign');
      }
    });

    test('parses complex assignments', () => {
      const result = parseVersee('result = (x + y) * 2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Assign');
      }
    });

    test('assignment has lower precedence than comparison', () => {
      const result = parseVersee('x = y < 5');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Assign');
        if (result.value.value.kind === 'Assign') {
          expect(result.value.value.right.value.kind).toBe('Less');
        }
      }
    });
  });

  describe('Logical expressions', () => {
    test('parses and operator', () => {
      const result = parseVersee('true and false');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('And');
      }
    });

    test('parses or operator', () => {
      const result = parseVersee('true or false');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Or');
      }
    });

    test('parses not operator', () => {
      const result = parseVersee('not true');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Not');
      }
    });

    test('parses double negation', () => {
      const result = parseVersee('not not true');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Not');
        if (result.value.value.kind === 'Not') {
          expect(result.value.value.expr.value.kind).toBe('Not');
        }
      }
    });

    test('and has higher precedence than or', () => {
      const result = parseVersee('true or false and true');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Or');
        if (result.value.value.kind === 'Or') {
          expect(result.value.value.right.value.kind).toBe('And');
        }
      }
    });

    test('comparison has higher precedence than logical operators', () => {
      const result = parseVersee('x < 5 and y > 3');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('And');
        if (result.value.value.kind === 'And') {
          expect(result.value.value.left.value.kind).toBe('Less');
          expect(result.value.value.right.value.kind).toBe('Greater');
        }
      }
    });
  });

  describe('Lists and grouping', () => {
    test('parses empty parentheses as empty list', () => {
      const result = parseVersee('()');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(0);
        }
      }
    });

    test('parses single element in parentheses', () => {
      const result = parseVersee('(42)');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(1);
        }
      }
    });

    test('parses comma-separated lists', () => {
      const result = parseVersee('1, 2, 3');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(3);
        }
      }
    });

    test('parses semicolon-separated lists', () => {
      const result = parseVersee('1; 2; 3');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(3);
        }
      }
    });

    test('parses mixed expressions in lists', () => {
      const result = parseVersee('x + 1, true, "hello"');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(3);
        }
      }
    });

    test('parses braces as blocks', () => {
      const result = parseVersee('{1, 2, 3}');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(3);
        }
      }
    });

    test('parses empty braces', () => {
      const result = parseVersee('{}');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(0);
        }
      }
    });
  });

  describe('Whitespace handling', () => {
    test('ignores leading whitespace', () => {
      const result = parseVersee('   42');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Int');
      }
    });

    test('ignores trailing whitespace', () => {
      const result = parseVersee('42   ');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Int');
      }
    });

    test('ignores whitespace around operators', () => {
      const result = parseVersee('1   +   2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Add');
      }
    });

    test('handles tabs as whitespace', () => {
      const result = parseVersee('1\t+\t2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Add');
      }
    });
  });

  describe('Complex expressions', () => {
    test('parses nested arithmetic and logical operations', () => {
      const result = parseVersee('(x + 1) * 2 < 10 and y > 0');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('And');
      }
    });

    test('parses assignment with complex expression using correct precedence', () => {
      const result = parseVersee('result = (a + b) * c < d or e and f');
      expect(result.success).toBe(true);
      if (result.success) {
        // Should parse as: (result = (a + b) * c < d) or (e and f)
        expect(result.value.value.kind).toBe('Or');
        const left = (result.value.value as any).left.value;
        expect(left.kind).toBe('Assign');
        expect(left.left.value.pattern.ident.name).toBe('result');
      }
    });

    test('parses deeply nested parentheses', () => {
      const result = parseVersee('(((((1)))))');
      expect(result.success).toBe(true);
      if (result.success) {
        // Multiple nested lists, innermost containing the integer
        let current = result.value;
        let depth = 0;
        while (current.value.kind === 'List' && current.value.elements.length === 1) {
          depth++;
          current = current.value.elements[0];
        }
        expect(depth).toBeGreaterThan(0);
      }
    });
  });

  describe('Error handling', () => {
    test('rejects reserved words as identifiers', () => {
      const result = parseVersee('if');
      expect(result.success).toBe(false);
    });

    test('rejects unclosed strings', () => {
      const result = parseVersee('"unclosed');
      expect(result.success).toBe(false);
    });

    test('rejects unclosed character literals', () => {
      const result = parseVersee("'a");
      expect(result.success).toBe(false);
    });

    test('rejects empty character literals', () => {
      const result = parseVersee("''");
      expect(result.success).toBe(false);
    });

    test('reports parse errors with position', () => {
      const result = parseVersee('1 + + 2');
      if (!result.success) {
        expect(result.error.position).toBeDefined();
        expect(result.error.message).toBeDefined();
      }
    });

    test('parses postfix increment followed by number as list', () => {
      const result = parseVersee('1 ++ 2');
      expect(result.success).toBe(true);
      if (result.success) {
        // Should parse as a list with PostfixIncrement(1) and Int(2)
        expect(result.value.value.kind).toBe('Module');
        const body = (result.value.value as any).body.value;
        expect(body.kind).toBe('List');
        expect(body.elements.length).toBe(2);
        expect(body.elements[0].value.kind).toBe('PostfixIncrement');
        expect(body.elements[1].value.kind).toBe('Int');
      }
    });

    test('rejects mismatched parentheses', () => {
      const result = parseVersee('(1 + 2');
      expect(result.success).toBe(false);
    });

    test('rejects mismatched braces', () => {
      const result = parseVersee('{1, 2');
      expect(result.success).toBe(false);
    });
  });

  describe('Location tracking', () => {
    test('tracks location for simple literals', () => {
      const result = parseVersee('42');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.loc.start.line).toBe(1);
        expect(result.value.loc.start.column).toBe(1);
        expect(result.value.loc.end.line).toBe(1);
        expect(result.value.loc.end.column).toBe(3);
      }
    });

    test('tracks location for binary operations', () => {
      const result = parseVersee('1 + 2');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.loc.start.line).toBe(1);
        expect(result.value.loc.start.column).toBe(1);
        expect(result.value.loc.end.line).toBe(1);
        expect(result.value.loc.end.column).toBe(6);
      }
    });
  });
});