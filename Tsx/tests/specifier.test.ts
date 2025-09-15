import { parseVersee } from '../src/parser/parser';

describe('Versee Specifier Parsing', () => {
  describe('Basic specifiers', () => {
    test('parses <decides> specifier', () => {
      const result = parseVersee('<decides>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('decides');
        }
      }
    });

    test('parses <succeeds> specifier', () => {
      const result = parseVersee('<succeeds>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('succeeds');
        }
      }
    });

    test('parses <fails> specifier', () => {
      const result = parseVersee('<fails>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('fails');
        }
      }
    });

    test('parses <transacts> specifier', () => {
      const result = parseVersee('<transacts>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('transacts');
        }
      }
    });

    test('parses <computes> specifier', () => {
      const result = parseVersee('<computes>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('computes');
        }
      }
    });

    test('parses <ambiguates> specifier', () => {
      const result = parseVersee('<ambiguates>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('ambiguates');
        }
      }
    });

    test('parses <reads> specifier', () => {
      const result = parseVersee('<reads>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('reads');
        }
      }
    });

    test('parses <writes> specifier', () => {
      const result = parseVersee('<writes>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('writes');
        }
      }
    });

    test('parses <allocates> specifier', () => {
      const result = parseVersee('<allocates>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('allocates');
        }
      }
    });

    test('parses <suspends> specifier', () => {
      const result = parseVersee('<suspends>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('suspends');
        }
      }
    });
  });

  describe('Specifier case handling', () => {
    test('handles uppercase specifiers', () => {
      const result = parseVersee('<DECIDES>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('decides');
        }
      }
    });

    test('handles mixed case specifiers', () => {
      const result = parseVersee('<Succeeds>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('Specifier');
        if (result.value.value.kind === 'Specifier') {
          expect(result.value.value.spec).toBe('succeeds');
        }
      }
    });
  });

  describe('Specifiers in lists', () => {
    test('parses multiple specifiers in a list', () => {
      const result = parseVersee('<decides>, <succeeds>, <fails>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(3);
          expect(result.value.value.elements[0].value.kind).toBe('Specifier');
          expect(result.value.value.elements[1].value.kind).toBe('Specifier');
          expect(result.value.value.elements[2].value.kind).toBe('Specifier');
        }
      }
    });

    test('parses specifiers in expressions', () => {
      const result = parseVersee('<reads>, <writes>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.value.kind).toBe('List');
        if (result.value.value.kind === 'List') {
          expect(result.value.value.elements.length).toBe(2);
        }
      }
    });
  });

  describe('Error handling', () => {
    test('rejects unknown specifiers', () => {
      const result = parseVersee('<unknown>');
      expect(result.success).toBe(false);
    });

    test('rejects empty angle brackets', () => {
      const result = parseVersee('<>');
      expect(result.success).toBe(false);
    });

    test('rejects unclosed angle brackets', () => {
      const result = parseVersee('<decides');
      expect(result.success).toBe(false);
    });

    test('rejects specifier without angle brackets', () => {
      const result = parseVersee('decides');
      expect(result.success).toBe(false);
    });

    test('rejects spaces inside angle brackets', () => {
      const result = parseVersee('< decides >');
      expect(result.success).toBe(false);
    });
  });

  describe('Location tracking', () => {
    test('tracks location for specifiers', () => {
      const result = parseVersee('<decides>');
      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.value.loc.start.line).toBe(1);
        expect(result.value.loc.start.column).toBe(1);
        expect(result.value.loc.end.line).toBe(1);
        expect(result.value.loc.end.column).toBe(10); // Length of '<decides>' is 9, so end column is 10
      }
    });
  });
});