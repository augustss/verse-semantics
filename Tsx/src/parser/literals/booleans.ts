/**
 * Boolean literal parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { trivia } from '../foundation/trivia';

// Boolean literal parser
export const booleanLiteral: PC.Parser<AST.BooleanLiteral> = (state) => {
  const startPos = state.position;

  // Parse leading trivia
  const leadingTriviaResult = trivia(state);
  const afterTrivia = leadingTriviaResult.success ? leadingTriviaResult.state : state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Try to match 'true' or 'false'
  const trueResult = PC.string('true')(afterTrivia);
  const falseResult = PC.string('false')(afterTrivia);

  let matched: { value: boolean; text: string; newState: PC.ParserState } | null = null;

  if (trueResult.success) {
    // Make sure it's not part of a longer identifier
    const nextPos = trueResult.state.position;
    if (nextPos >= trueResult.state.input.length ||
        !/[a-zA-Z0-9_]/.test(trueResult.state.input[nextPos])) {
      matched = { value: true, text: 'true', newState: trueResult.state };
    }
  }

  if (!matched && falseResult.success) {
    // Make sure it's not part of a longer identifier
    const nextPos = falseResult.state.position;
    if (nextPos >= falseResult.state.input.length ||
        !/[a-zA-Z0-9_]/.test(falseResult.state.input[nextPos])) {
      matched = { value: false, text: 'false', newState: falseResult.state };
    }
  }

  if (!matched) {
    return { success: false, error: 'Expected boolean literal', state };
  }

  // Parse trailing trivia
  const trailingResult = trivia(matched.newState);
  const trailingTrivia = trailingResult.success ? trailingResult.value : '';
  const finalState = trailingResult.success ? trailingResult.state : matched.newState;

  const token = AST.token(
    matched.text,
    matched.value,
    { leading: leadingTrivia, trailing: trailingTrivia },
    { start: startPos, end: finalState.position }
  );

  return {
    success: true,
    value: AST.booleanLiteral(token, { start: startPos, end: finalState.position }),
    state: finalState
  };
};