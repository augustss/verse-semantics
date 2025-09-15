// Trivia-aware parser combinators for lossless parsing
import {
  Parser, ParseResult, ParseState, map, flatMap, satisfy, string, many,
  optional, choice, succeed, fail, withLocation
} from './combinators';
import { L, Loc, Position, createPos, createLoc, withLoc } from '../ast/location';
import {
  Trivia, TriviaList, TriviaKind,
  createWhitespace, createNewline, createLineComment, createBlockComment,
  createTriviaList, combine
} from '../ast/trivia';

// Character predicates
const isSpace = (c: number): boolean => c === 0x20 || c === 0x09; // space, tab
const isNewline = (c: number): boolean => c === 0x0A || c === 0x0D; // LF, CR

// Parse a single trivia token
const pWhitespaceTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.offset);
  let current = state;
  let chars: number[] = [];

  while (current.offset < current.input.length) {
    const c = current.input.charCodeAt(current.offset);
    if (!isSpace(c)) break;

    chars.push(c);
    current = {
      ...current,
      offset: current.offset + 1,
      column: current.column + 1
    };
  }

  if (chars.length === 0) {
    return { success: false, error: { message: 'No whitespace', position: current.offset } };
  }

  const end = createPos(current.line, current.column, current.offset);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createWhitespace(text, start, end),
    newState: current
  };
};

const pNewlineTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.offset);
  let current = state;
  let chars: number[] = [];

  while (current.offset < current.input.length) {
    const c = current.input.charCodeAt(current.offset);
    if (!isNewline(c)) break;

    chars.push(c);
    current = {
      ...current,
      offset: current.offset + 1,
      column: c === 0x0A ? 1 : current.column, // Reset column on LF
      line: c === 0x0A ? current.line + 1 : current.line
    };

    // Handle CRLF as single newline
    if (c === 0x0D && current.offset < current.input.length &&
        current.input.charCodeAt(current.offset) === 0x0A) {
      chars.push(0x0A);
      current = {
        ...current,
        offset: current.offset + 1,
        column: 1,
        line: current.line + 1
      };
    }
    break; // One newline sequence at a time
  }

  if (chars.length === 0) {
    return { success: false, error: { message: 'No newline', position: current.offset } };
  }

  const end = createPos(current.line, current.column, current.offset);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createNewline(text, start, end),
    newState: current
  };
};

const pLineCommentTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.offset);

  // Check for '#' at start
  if (state.offset >= state.input.length || state.input.charCodeAt(state.offset) !== 0x23) {
    return { success: false, error: { message: 'No line comment', position: state.offset } };
  }

  let current = state;
  let chars: number[] = [];

  // Consume until newline or end of input
  while (current.offset < current.input.length) {
    const c = current.input.charCodeAt(current.offset);
    if (isNewline(c)) break;

    chars.push(c);
    current = {
      ...current,
      offset: current.offset + 1,
      column: current.column + 1
    };
  }

  const end = createPos(current.line, current.column, current.offset);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createLineComment(text, start, end),
    newState: current
  };
};

const pBlockCommentTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.offset);

  // Check for '<#' at start
  if (state.offset + 1 >= state.input.length ||
      state.input.charCodeAt(state.offset) !== 0x3C ||
      state.input.charCodeAt(state.offset + 1) !== 0x23) {
    return { success: false, error: { message: 'No block comment', position: state.offset } };
  }

  let current = {
    ...state,
    offset: state.offset + 2,
    column: state.column + 2
  };
  let chars: number[] = [0x3C, 0x23]; // Include opening '<#'

  // Find closing '#>'
  while (current.offset + 1 < current.input.length) {
    const c = current.input.charCodeAt(current.offset);
    chars.push(c);

    if (c === 0x23 && current.input.charCodeAt(current.offset + 1) === 0x3E) {
      // Found '#>', include it and finish
      chars.push(0x3E);
      current = {
        ...current,
        offset: current.offset + 2,
        column: isNewline(c) ? 1 : current.column + 2,
        line: isNewline(c) ? current.line + 1 : current.line
      };
      break;
    }

    current = {
      ...current,
      offset: current.offset + 1,
      column: isNewline(c) ? 1 : current.column + 1,
      line: isNewline(c) ? current.line + 1 : current.line
    };
  }

  const end = createPos(current.line, current.column, current.offset);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createBlockComment(text, start, end),
    newState: current
  };
};

// Parse any single trivia token
const pSingleTrivia: Parser<Trivia> = choice([
  pBlockCommentTrivia,
  pLineCommentTrivia,
  pNewlineTrivia,
  pWhitespaceTrivia
]);

// Parse a list of trivia tokens
export const pTrivia: Parser<TriviaList> = map(
  many(pSingleTrivia),
  trivia => createTriviaList(trivia)
);

// Parse leading trivia before a token
export const pLeadingTrivia: Parser<TriviaList> = pTrivia;

// Parse trailing trivia after a token (typically just spaces, no newlines)
export const pTrailingTrivia: Parser<TriviaList> = map(
  many(pWhitespaceTrivia),
  trivia => createTriviaList(trivia)
);

// Trivia-aware lexeme combinator
export function triviaLexeme<T>(parser: Parser<T>): Parser<L<T>> {
  return flatMap(
    pLeadingTrivia,
    leadingTrivia => flatMap(
      withLocation(parser),
      locatedValue => map(
        pTrailingTrivia,
        trailingTrivia => withLoc(
          locatedValue.loc,
          locatedValue.value,
          leadingTrivia,
          trailingTrivia
        )
      )
    )
  );
}

// Skip trivia only (for compatibility with existing parser)
export const skipTrivia: Parser<null> = map(pTrivia, () => null);