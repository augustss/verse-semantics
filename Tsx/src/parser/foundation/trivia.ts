/**
 * Trivia parsing (whitespace, comments)
 */

import * as PC from '../../parser-combinators';
import { isNewline, isWhitespace } from './helpers';

// Parse line comment starting with # or ## (doc comments)
export const lineComment: PC.Parser<string> = (state) => {
  if (state.position < state.input.length && state.input[state.position] === '#') {
    // Check if it's a doc comment (##) or regular comment (#)
    // Both are handled the same way for parsing purposes
    let startIndex = state.position;
    let endIndex = state.position + 1;

    // If it's a doc comment, include the second #
    if (endIndex < state.input.length && state.input[endIndex] === '#') {
      endIndex++;
    }

    // Find the end of the line or end of input
    while (endIndex < state.input.length && !isNewline(state.input[endIndex])) {
      endIndex++;
    }
    // Include the newline if present
    if (endIndex < state.input.length && isNewline(state.input[endIndex])) {
      endIndex++;
    }
    const comment = state.input.slice(startIndex, endIndex);
    return {
      success: true,
      value: comment,
      state: { ...state, position: endIndex }
    };
  }
  return { success: false, error: 'Not a line comment', state };
};

// Parse multiline comment <# ... #>
export const multilineComment: PC.Parser<string> = (state) => {
  if (state.position + 2 <= state.input.length &&
      state.input.slice(state.position, state.position + 2) === '<#') {

    // Handle nested comments by counting opening and closing delimiters
    let depth = 1;
    let pos = state.position + 2;
    const startPos = state.position;

    while (pos < state.input.length && depth > 0) {
      // Check for nested opening delimiter
      if (pos + 2 <= state.input.length &&
          state.input.slice(pos, pos + 2) === '<#') {
        depth++;
        pos += 2;
      }
      // Check for closing delimiter
      else if (pos + 2 <= state.input.length &&
               state.input.slice(pos, pos + 2) === '#>') {
        depth--;
        pos += 2;
      }
      else {
        pos++;
      }
    }

    if (depth === 0) {
      const comment = state.input.slice(startPos, pos);
      return {
        success: true,
        value: comment,
        state: { ...state, position: pos }
      };
    }
  }

  return { success: false, error: 'Not a multiline comment or unclosed comment', state };
};

// Parse any kind of trivia (whitespace, comments)
export const trivia: PC.Parser<string> = (state) => {
  let result = '';
  let currentState = state;

  while (currentState.position < currentState.input.length) {
    // Try whitespace
    const wsResult = PC.regex(/\s+/)(currentState);
    if (wsResult.success) {
      result += wsResult.value;
      currentState = wsResult.state;
      continue;
    }

    // Try line comment
    const lineCommentResult = lineComment(currentState);
    if (lineCommentResult.success) {
      result += lineCommentResult.value;
      currentState = lineCommentResult.state;
      continue;
    }

    // Try multiline comment
    const multilineCommentResult = multilineComment(currentState);
    if (multilineCommentResult.success) {
      result += multilineCommentResult.value;
      currentState = multilineCommentResult.state;
      continue;
    }

    // No more trivia
    break;
  }

  return {
    success: true,
    value: result,
    state: currentState
  };
};