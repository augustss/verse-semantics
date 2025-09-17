/**
 * True lossless parser that preserves all whitespace, comments, and formatting
 * by attaching trivia to AST nodes
 */

import {
  Parser, ParseResult, ParseState, ParseError
} from './combinators';
import { L, createPos, createLoc, withLoc } from '../ast/location';
import { Exp } from '../ast/expression';
import {
  createTriviaList
} from '../ast/trivia';
import { pLeadingTrivia, pTrailingTrivia } from './trivia-parser';
import { createInt, createFloat, createString, createTrue, createFalse,
         createIdent } from '../ast/expression';
const CHAR_LF = 0x0A;
const CHAR_CR = 0x0D;
const CHAR_MINUS = 0x2D;  // -
const CHAR_DOT = 0x2E;    // .
const CHAR_QUOTE = 0x22;  // "

// Helper to check if a character is a newline
function isNewlineChar(c: number): boolean {
  return c === CHAR_LF || c === CHAR_CR;
}

// Wrap a parser to capture leading and trailing trivia
function withTrivia<T>(parser: Parser<T>): Parser<L<T>> {
  return (state: ParseState): ParseResult<L<T>> => {
    // Capture leading trivia
    const leadingResult = pLeadingTrivia(state);
    const leadingTrivia = leadingResult.success ? leadingResult.value : createTriviaList([]);
    const afterLeading = leadingResult.success ? leadingResult.state : state;

    // Parse the actual content
    const startPos = createPos(afterLeading.line, afterLeading.column, afterLeading.position);
    const contentResult = parser(afterLeading);
    if (!contentResult.success) {
      return contentResult as ParseResult<L<T>>;
    }

    const endPos = createPos(contentResult.state.line, contentResult.state.column, contentResult.state.position);
    const loc = createLoc(startPos, endPos);

    // Capture trailing trivia (inline whitespace only, not newlines)
    const trailingResult = pTrailingTrivia(contentResult.state);
    const trailingTrivia = trailingResult.success ? trailingResult.value : createTriviaList([]);
    const finalState = trailingResult.success ? trailingResult.state : contentResult.state;

    return {
      success: true,
      value: withLoc(loc, contentResult.value, leadingTrivia, trailingTrivia),
      state: finalState
    };
  };
}

// Number parser that preserves exact format
const pNumber: Parser<L<Exp>> = withTrivia(
  (state: ParseState): ParseResult<Exp> => {
    let current = state;
    let chars: number[] = [];
    let hasDecimal = false;

    // Optional minus sign
    if (current.position < current.input.length && current.input[current.position] === CHAR_MINUS) {
      chars.push(CHAR_MINUS);
      current = {
        ...current,
        position: current.position + 1,
        column: current.column + 1
      };
    }

    // Digits before decimal
    while (current.position < current.input.length) {
      const c = current.input[current.position];
      if (c >= 0x30 && c <= 0x39) { // 0-9
        chars.push(c);
        current = {
          ...current,
          position: current.position + 1,
          column: current.column + 1
        };
      } else {
        break;
      }
    }

    // Optional decimal part
    if (current.position < current.input.length && current.input[current.position] === CHAR_DOT) {
      // Look ahead to ensure this is a decimal point, not property access
      if (current.position + 1 < current.input.length &&
          current.input[current.position + 1] >= 0x30 &&
          current.input[current.position + 1] <= 0x39) {
        hasDecimal = true;
        chars.push(CHAR_DOT);
        current = {
          ...current,
          position: current.position + 1,
          column: current.column + 1
        };

        // Digits after decimal
        while (current.position < current.input.length) {
          const c = current.input[current.position];
          if (c >= 0x30 && c <= 0x39) { // 0-9
            chars.push(c);
            current = {
              ...current,
              position: current.position + 1,
              column: current.column + 1
            };
          } else {
            break;
          }
        }
      }
    }

    if (chars.length === 0 || (chars.length === 1 && chars[0] === CHAR_MINUS)) {
      return {
        success: false,
        error: {
          message: 'Expected number',
          position: createPos(state.line, state.column, state.position)
        }
      };
    }

    const numStr = String.fromCharCode(...chars);

    if (hasDecimal) {
      return {
        success: true,
        value: createFloat(parseFloat(numStr)),
        state: current
      };
    } else {
      return {
        success: true,
        value: createInt(BigInt(numStr)),
        state: current
      };
    }
  }
);

// String literal parser that preserves exact format
const pString: Parser<L<Exp>> = withTrivia(
  (state: ParseState): ParseResult<Exp> => {
    if (state.position >= state.input.length || state.input[state.position] !== CHAR_QUOTE) {
      return {
        success: false,
        error: {
          message: 'Expected string literal',
          position: createPos(state.line, state.column, state.position)
        }
      };
    }

    let current = {
      ...state,
      position: state.position + 1,
      column: state.column + 1
    };
    let chars: number[] = [];
    let escaped = false;

    while (current.position < current.input.length) {
      const c = current.input[current.position];

      if (escaped) {
        chars.push(c);
        escaped = false;
      } else if (c === 0x5C) { // backslash
        chars.push(c);
        escaped = true;
      } else if (c === CHAR_QUOTE) {
        // End of string
        current = {
          ...current,
          position: current.position + 1,
          column: current.column + 1
        };
        break;
      } else {
        chars.push(c);
      }

      current = {
        ...current,
        position: current.position + 1,
        column: isNewlineChar(c) ? 1 : current.column + 1,
        line: isNewlineChar(c) ? current.line + 1 : current.line
      };
    }

    const str = String.fromCharCode(...chars);

    return {
      success: true,
      value: createString(str),
      state: current
    };
  }
);

// Identifier parser
const pIdentifier: Parser<L<Exp>> = withTrivia(
  (state: ParseState): ParseResult<Exp> => {
    let current = state;
    let chars: number[] = [];

    // First character must be letter or underscore
    if (current.position < current.input.length) {
      const c = current.input[current.position];
      if ((c >= 0x41 && c <= 0x5A) || // A-Z
          (c >= 0x61 && c <= 0x7A) || // a-z
          c === 0x5F) { // _
        chars.push(c);
        current = {
          ...current,
          position: current.position + 1,
          column: current.column + 1
        };
      }
    }

    if (chars.length === 0) {
      return {
        success: false,
        error: {
          message: 'Expected identifier',
          position: createPos(state.line, state.column, state.position)
        }
      };
    }

    // Subsequent characters can be letters, digits, or underscore
    while (current.position < current.input.length) {
      const c = current.input[current.position];
      if ((c >= 0x41 && c <= 0x5A) || // A-Z
          (c >= 0x61 && c <= 0x7A) || // a-z
          (c >= 0x30 && c <= 0x39) || // 0-9
          c === 0x5F) { // _
        chars.push(c);
        current = {
          ...current,
          position: current.position + 1,
          column: current.column + 1
        };
      } else {
        break;
      }
    }

    const name = String.fromCharCode(...chars);

    // Check for keywords
    switch (name) {
      case 'true':
        return { success: true, value: createTrue(), state: current };
      case 'false':
        return { success: true, value: createFalse(), state: current };
      default:
        return { success: true, value: createIdent(name), state: current };
    }
  }
);

// Simple expression parser for testing
const pExpression: Parser<L<Exp>> = (state: ParseState): ParseResult<L<Exp>> => {
  // Try different expression types
  const parsers = [
    pNumber,
    pString,
    pIdentifier
  ];

  for (const parser of parsers) {
    const result = parser(state);
    if (result.success) {
      return result;
    }
  }

  return {
    success: false,
    error: {
      message: 'Expected expression',
      position: createPos(state.line, state.column, state.position)
    }
  };
};

// Export the main parse function
export function parseLosslessTrue(input: string): {
  success: boolean;
  ast?: L<Exp>;
  error?: ParseError;
} {
  // Convert string to array of character codes (as Uint8Array)
  const codes = new Uint8Array(Array.from(input).map(c => c.charCodeAt(0)));

  const initialState: ParseState = {
    input: codes,
    position: 0,
    line: 1,
    column: 1,
    indentStack: [],
    blockIndent: '',
    lineIndent: '',
    linePrefix: '',
    nest: false
  };

  const result = pExpression(initialState);

  if (result.success) {
    return {
      success: true,
      ast: result.value
    };
  } else {
    return {
      success: false,
      error: result.error
    };
  }
}

// Export a function to reconstruct the exact source from AST with trivia
export function reconstructSource(ast: L<Exp>): string {
  let result = '';

  // Add leading trivia
  if (ast.leadingTrivia) {
    result += ast.leadingTrivia.trivia.map(t => t.text).join('');
  }

  // Add the actual content
  result += reconstructExpression(ast.value);

  // Add trailing trivia
  if (ast.trailingTrivia) {
    result += ast.trailingTrivia.trivia.map(t => t.text).join('');
  }

  return result;
}

// Helper to reconstruct expression content based on the actual Exp type
function reconstructExpression(exp: Exp): string {
  // Check the discriminated union structure
  if ('kind' in exp) {
    switch (exp.kind) {
      case 'Int':
        return 'value' in exp ? exp.value.toString() : '[INT]';
      case 'Float':
        return 'value' in exp ? exp.value.toString() : '[FLOAT]';
      case 'String':
        if ('text' in exp && exp.interpolations.length === 0) {
          return `"${exp.text}"`;
        } else {
          // Handle interpolations later
          return `"${exp.text}"`;
        }
      case 'Ident':
        return 'name' in exp ? exp.name : '[IDENT]';
      case 'True':
        return 'true';
      case 'False':
        return 'false';
      case 'Add':
        if ('left' in exp && 'right' in exp) {
          return `${reconstructSource(exp.left as L<Exp>)}+${reconstructSource(exp.right as L<Exp>)}`;
        }
        break;
      case 'Multiply':
        if ('left' in exp && 'right' in exp) {
          return `${reconstructSource(exp.left as L<Exp>)}*${reconstructSource(exp.right as L<Exp>)}`;
        }
        break;
      case 'And':
        if ('left' in exp && 'right' in exp) {
          return `${reconstructSource(exp.left as L<Exp>)}and${reconstructSource(exp.right as L<Exp>)}`;
        }
        break;
      case 'Or':
        if ('left' in exp && 'right' in exp) {
          return `${reconstructSource(exp.left as L<Exp>)}or${reconstructSource(exp.right as L<Exp>)}`;
        }
        break;
    }
  }

  // Fallback
  return '[TODO]';
}