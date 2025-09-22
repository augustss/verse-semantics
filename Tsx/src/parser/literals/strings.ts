/**
 * String literal parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { trivia } from '../foundation/trivia';

// String literal parser
export const stringLiteral: PC.Parser<AST.StringLiteral> = (state) => {
  const startPos = state.position;

  // First, parse any leading trivia
  const leadingTriviaResult = trivia(state);
  let currentState = leadingTriviaResult.state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Check for opening quote
  if (currentState.position >= currentState.input.length || currentState.input[currentState.position] !== '"') {
    return { success: false, error: 'Expected string literal', state };
  }

  // Consume opening quote
  const openQuotePos = currentState.position;
  currentState = { ...currentState, position: currentState.position + 1 };

  // Parse string content (handle escaped quotes)
  let content = '';
  let escaped = false;

  while (currentState.position < currentState.input.length) {
    const char = currentState.input[currentState.position];

    if (escaped) {
      // Handle escape sequences
      switch (char) {
        case 'n':
          content += '\n';
          break;
        case 't':
          content += '\t';
          break;
        case 'r':
          content += '\r';
          break;
        case '\\':
          content += '\\';
          break;
        case '"':
          content += '"';
          break;
        default:
          // Invalid escape sequence
          return { success: false, error: `Invalid escape sequence: \\${char}`, state };
      }
      escaped = false;
    } else if (char === '\\') {
      escaped = true;
    } else if (char === '"') {
      // Found closing quote
      break;
    } else {
      content += char;
    }

    currentState = { ...currentState, position: currentState.position + 1 };
  }

  // Check for closing quote
  if (currentState.position >= currentState.input.length ||
      currentState.input[currentState.position] !== '"') {
    return { success: false, error: 'Unterminated string literal', state };
  }

  // Consume closing quote
  const closeQuotePos = currentState.position;
  currentState = { ...currentState, position: currentState.position + 1 };

  // Parse trailing trivia
  const trailingTriviaResult = trivia(currentState);
  const trailingTrivia = trailingTriviaResult.success ? trailingTriviaResult.value : '';
  const finalState = trailingTriviaResult.success ? trailingTriviaResult.state : currentState;

  // Create the full text including quotes
  const fullText = state.input.slice(openQuotePos, closeQuotePos + 1);

  const token = AST.token(
    fullText,
    content, // The actual string value without quotes
    { leading: leadingTrivia, trailing: trailingTrivia },
    { start: startPos, end: finalState.position }
  );

  return {
    success: true,
    value: AST.stringLiteral(token, { start: startPos, end: finalState.position }),
    state: finalState
  };
};