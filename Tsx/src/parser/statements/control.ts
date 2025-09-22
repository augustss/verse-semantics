/**
 * Control flow statements (break, continue)
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';

// Break statement
export const breakStatement: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Parse 'break' with trivia (this handles leading trivia)
  const breakResult = withTriviaLiteral('break', PC.string('break'))(state);
  if (!breakResult.success) return { success: false, error: 'Not a break statement', state };

  // Check it's not part of a longer identifier (check after the keyword in the parsed token)
  const afterKeyword = breakResult.state.position;
  if (afterKeyword < state.input.length && /[a-zA-Z0-9_]/.test(state.input[afterKeyword])) {
    return { success: false, error: 'break is part of identifier', state };
  }

  return {
    success: true,
    value: AST.breakStatement(
      breakResult.value,
      { start: startPos, end: breakResult.state.position }
    ),
    state: breakResult.state
  };
};

// Continue statement
export const continueStatement: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Parse 'continue' with trivia (this handles leading trivia)
  const continueResult = withTriviaLiteral('continue', PC.string('continue'))(state);
  if (!continueResult.success) return { success: false, error: 'Not a continue statement', state };

  // Check it's not part of a longer identifier (check after the keyword in the parsed token)
  const afterKeyword = continueResult.state.position;
  if (afterKeyword < state.input.length && /[a-zA-Z0-9_]/.test(state.input[afterKeyword])) {
    return { success: false, error: 'continue is part of identifier', state };
  }

  return {
    success: true,
    value: AST.continueStatement(
      continueResult.value,
      { start: startPos, end: continueResult.state.position }
    ),
    state: continueResult.state
  };
};