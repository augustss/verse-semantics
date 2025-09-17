// Trivia-aware parser combinators for lossless parsing
import {
  Parser, ParseResult, ParseState, map, flatMap, many, choice, withLocation
} from './combinators';
import { L, createPos, withLoc } from '../ast/location';
import {
  Trivia, TriviaList,
  createWhitespace, createNewline, createLineComment, createBlockComment,
  createTriviaList
} from '../ast/trivia';

// Character predicates
const isSpace = (c: number): boolean => c === 0x20 || c === 0x09; // space, tab
const isNewline = (c: number): boolean => c === 0x0A || c === 0x0D; // LF, CR

// Parse a single trivia token
const pWhitespaceTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.position);
  let current = state;
  let chars: number[] = [];

  while (current.position < current.input.length) {
    const c = current.input[current.position];
    if (!isSpace(c)) break;

    chars.push(c);
    current = {
      ...current,
      position: current.position + 1,
      column: current.column + 1
    };
  }

  if (chars.length === 0) {
    return {
      success: false,
      error: {
        message: 'No whitespace',
        position: createPos(current.line, current.column, current.position)
      }
    };
  }

  const end = createPos(current.line, current.column, current.position);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createWhitespace(text, start, end),
    state: current
  };
};

const pNewlineTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.position);
  let current = state;
  let chars: number[] = [];

  while (current.position < current.input.length) {
    const c = current.input[current.position];
    if (!isNewline(c)) break;

    // Handle CRLF as single newline sequence but preserve both characters for lossless parsing
    if (c === 0x0D && current.position + 1 < current.input.length &&
        current.input[current.position + 1] === 0x0A) {
      // CRLF sequence - preserve both characters
      // console.log('DEBUG: Found CRLF, preserving both characters');
      chars.push(0x0D); // CR
      chars.push(0x0A); // LF
      current = {
        ...current,
        position: current.position + 2, // Skip both CR and LF
        column: 1,
        line: current.line + 1
      };
    } else {
      // Single character newline (LF or standalone CR)
      chars.push(c);
      current = {
        ...current,
        position: current.position + 1,
        column: c === 0x0A ? 1 : current.column, // Reset column on LF
        line: c === 0x0A ? current.line + 1 : current.line
      };
    }
    break; // One newline sequence at a time
  }

  if (chars.length === 0) {
    return {
      success: false,
      error: {
        message: 'No newline',
        position: createPos(current.line, current.column, current.position)
      }
    };
  }

  const end = createPos(current.line, current.column, current.position);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createNewline(text, start, end),
    state: current
  };
};

const pLineCommentTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.position);

  // Check for '#' at start
  if (state.position >= state.input.length || state.input[state.position] !== 0x23) {
    return { success: false, error: { message: 'No line comment', position: createPos(state.line, state.column, state.position) } };
  }

  let current = state;
  let chars: number[] = [];

  // Consume until newline or end of input
  while (current.position < current.input.length) {
    const c = current.input[current.position];
    if (isNewline(c)) break;

    chars.push(c);
    current = {
      ...current,
      position: current.position + 1,
      column: current.column + 1
    };
  }

  const end = createPos(current.line, current.column, current.position);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createLineComment(text, start, end),
    state: current
  };
};

const pBlockCommentTrivia: Parser<Trivia> = (state: ParseState): ParseResult<Trivia> => {
  const start = createPos(state.line, state.column, state.position);

  // Check for '<#' at start
  if (state.position + 1 >= state.input.length ||
      state.input[state.position] !== 0x3C ||
      state.input[state.position + 1] !== 0x23) {
    return { success: false, error: { message: 'No block comment', position: createPos(state.line, state.column, state.position) } };
  }

  let current = {
    ...state,
    position: state.position + 2,
    column: state.column + 2
  };
  let chars: number[] = [0x3C, 0x23]; // Include opening '<#'

  // Find closing '#>'
  while (current.position + 1 < current.input.length) {
    const c = current.input[current.position];
    chars.push(c);

    if (c === 0x23 && current.input[current.position + 1] === 0x3E) {
      // Found '#>', include it and finish
      chars.push(0x3E);
      current = {
        ...current,
        position: current.position + 2,
        column: isNewline(c) ? 1 : current.column + 2,
        line: isNewline(c) ? current.line + 1 : current.line
      };
      break;
    }

    current = {
      ...current,
      position: current.position + 1,
      column: isNewline(c) ? 1 : current.column + 1,
      line: isNewline(c) ? current.line + 1 : current.line
    };
  }

  const end = createPos(current.line, current.column, current.position);
  const text = String.fromCharCode(...chars);

  return {
    success: true,
    value: createBlockComment(text, start, end),
    state: current
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