import { Pos, L, createPos, createLoc, withLoc } from '../ast/location';

export interface ParseError {
  position: Pos;
  message: string;
  expected?: string[];
}

export interface ParseState {
  input: Uint8Array;
  position: number;
  line: number;
  column: number;
  indentStack: string[];
  blockIndent: string;
  lineIndent: string;
  linePrefix: string;
  nest: boolean;
}

export type ParseResult<T> =
  | { success: true; value: T; state: ParseState }
  | { success: false; error: ParseError };

export type Parser<T> = (state: ParseState) => ParseResult<T>;

// Basic combinators
export function succeed<T>(value: T): Parser<T> {
  return (state) => ({ success: true, value, state });
}

export function fail(message: string): Parser<never> {
  return (state) => ({
    success: false,
    error: {
      position: createPos(state.line, state.column, state.position),
      message
    }
  });
}

export function map<A, B>(parser: Parser<A>, fn: (a: A) => B): Parser<B> {
  return (state) => {
    const result = parser(state);
    if (!result.success) return result;
    return { success: true, value: fn(result.value), state: result.state };
  };
}

export function flatMap<A, B>(parser: Parser<A>, fn: (a: A) => Parser<B>): Parser<B> {
  return (state) => {
    const result = parser(state);
    if (!result.success) return result;
    return fn(result.value)(result.state);
  };
}

export function sequence<T>(parsers: Parser<T>[]): Parser<T[]> {
  return (state) => {
    const results: T[] = [];
    let currentState = state;

    for (const parser of parsers) {
      const result = parser(currentState);
      if (!result.success) return result;
      results.push(result.value);
      currentState = result.state;
    }

    return { success: true, value: results, state: currentState };
  };
}

export function choice<T>(parsers: Parser<T>[]): Parser<T> {
  return (state) => {
    const errors: ParseError[] = [];

    for (const parser of parsers) {
      const result = parser(state);
      if (result.success) return result;
      errors.push(result.error);
    }

    return {
      success: false,
      error: {
        position: createPos(state.line, state.column, state.position),
        message: `None of ${parsers.length} alternatives succeeded`,
        expected: errors.flatMap(e => e.expected || [])
      }
    };
  };
}

export function optional<T>(parser: Parser<T>): Parser<T | null> {
  return choice([
    map(parser, x => x as T | null),
    succeed(null)
  ]);
}

export function many<T>(parser: Parser<T>): Parser<T[]> {
  return (state) => {
    const results: T[] = [];
    let currentState = state;

    while (true) {
      const result = parser(currentState);
      if (!result.success) break;
      results.push(result.value);
      currentState = result.state;
    }

    return { success: true, value: results, state: currentState };
  };
}

export function many1<T>(parser: Parser<T>): Parser<T[]> {
  return flatMap(parser, first =>
    map(many(parser), rest => [first, ...rest])
  );
}

export function sepBy<T, S>(parser: Parser<T>, separator: Parser<S>): Parser<T[]> {
  return choice([
    sepBy1(parser, separator),
    succeed([])
  ]);
}

export function sepBy1<T, S>(parser: Parser<T>, separator: Parser<S>): Parser<T[]> {
  return flatMap(parser, first =>
    map(many(flatMap(separator, () => parser)), rest => [first, ...rest])
  );
}

export function between<L, R, T>(left: Parser<L>, right: Parser<R>, parser: Parser<T>): Parser<T> {
  return flatMap(left, () =>
    flatMap(parser, value =>
      map(right, () => value)
    )
  );
}

export function lookahead<T>(parser: Parser<T>): Parser<T> {
  return (state) => {
    const result = parser(state);
    if (!result.success) return result;
    return { success: true, value: result.value, state }; // Don't advance state
  };
}

export function notFollowedBy<T>(parser: Parser<T>): Parser<null> {
  return (state) => {
    const result = parser(state);
    if (result.success) {
      return {
        success: false,
        error: {
          position: createPos(state.line, state.column, state.position),
          message: 'Unexpected match'
        }
      };
    }
    return { success: true, value: null, state };
  };
}

export function withLocation<T>(parser: Parser<T>): Parser<L<T>> {
  return (state) => {
    const startPos = createPos(state.line, state.column, state.position);
    const result = parser(state);
    if (!result.success) return result;
    const endPos = createPos(result.state.line, result.state.column, result.state.position);
    const loc = createLoc(startPos, endPos);
    return {
      success: true,
      value: withLoc(loc, result.value),
      state: result.state
    };
  };
}

export function lazy<T>(fn: () => Parser<T>): Parser<T> {
  return (state) => fn()(state);
}

// String/character parsers
export function satisfy(predicate: (char: number) => boolean): Parser<number> {
  return (state) => {
    if (state.position >= state.input.length) {
      return {
        success: false,
        error: {
          position: createPos(state.line, state.column, state.position),
          message: 'Unexpected end of input'
        }
      };
    }

    const char = state.input[state.position];
    if (!predicate(char)) {
      return {
        success: false,
        error: {
          position: createPos(state.line, state.column, state.position),
          message: `Unexpected character: ${String.fromCharCode(char)}`
        }
      };
    }

    const newState = { ...state, position: state.position + 1 };
    if (char === 0x0A) { // newline
      newState.line++;
      newState.column = 1;
    } else {
      newState.column++;
    }

    return { success: true, value: char, state: newState };
  };
}

export function char(ch: string): Parser<string> {
  const code = ch.charCodeAt(0);
  return map(satisfy(c => c === code), () => ch);
}

export function string(str: string): Parser<string> {
  return (state) => {
    const bytes = new TextEncoder().encode(str);

    if (state.position + bytes.length > state.input.length) {
      return {
        success: false,
        error: {
          position: createPos(state.line, state.column, state.position),
          message: `Expected "${str}" but reached end of input`
        }
      };
    }

    for (let i = 0; i < bytes.length; i++) {
      if (state.input[state.position + i] !== bytes[i]) {
        return {
          success: false,
          error: {
            position: createPos(state.line, state.column, state.position),
            message: `Expected "${str}"`
          }
        };
      }
    }

    let newState = { ...state, position: state.position + bytes.length };
    // Update line and column based on the string content
    for (const byte of bytes) {
      if (byte === 0x0A) {
        newState.line++;
        newState.column = 1;
      } else {
        newState.column++;
      }
    }

    return { success: true, value: str, state: newState };
  };
}

export function eof(): Parser<null> {
  return (state) => {
    if (state.position >= state.input.length) {
      return { success: true, value: null, state };
    }
    return {
      success: false,
      error: {
        position: createPos(state.line, state.column, state.position),
        message: 'Expected end of file'
      }
    };
  };
}

// Helper to create initial parse state
export function createParseState(input: string | Uint8Array): ParseState {
  const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
  return {
    input: bytes,
    position: 0,
    line: 1,
    column: 1,
    indentStack: [],
    blockIndent: '',
    lineIndent: '',
    linePrefix: '',
    nest: true
  };
}

// Main parse function
export function parse<T>(parser: Parser<T>, input: string | Uint8Array): ParseResult<T> {
  const state = createParseState(input);
  return parser(state);
}