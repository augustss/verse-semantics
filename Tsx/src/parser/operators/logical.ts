/**
 * Logical operator parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';

// Logical operators (and, or)
export const logicalOp: PC.Parser<AST.Token<'and' | 'or'>> = (state) => {
  // Try 'and' - check boundary before consuming trivia
  const andCheck = PC.string('and')(state);
  if (andCheck.success) {
    const nextPos = andCheck.state.position;
    if (nextPos >= andCheck.state.input.length ||
        !/[a-zA-Z0-9_]/.test(andCheck.state.input[nextPos])) {
      // Boundary check passed, now consume with trivia
      const andResult = withTriviaLiteral('and', PC.string('and'))(state);
      if (andResult.success) {
        return andResult as PC.ParserResult<AST.Token<'and' | 'or'>>;
      }
    }
  }

  // Try 'or' - check boundary before consuming trivia
  const orCheck = PC.string('or')(state);
  if (orCheck.success) {
    const nextPos = orCheck.state.position;
    if (nextPos >= orCheck.state.input.length ||
        !/[a-zA-Z0-9_]/.test(orCheck.state.input[nextPos])) {
      // Boundary check passed, now consume with trivia
      const orResult = withTriviaLiteral('or', PC.string('or'))(state);
      if (orResult.success) {
        return orResult as PC.ParserResult<AST.Token<'and' | 'or'>>;
      }
    }
  }

  return { success: false, error: 'Expected and or or', state };
};

// Not operator
export const notOp: PC.Parser<AST.Token<'not'>> = (state) => {
  // Check boundary before consuming trivia
  const notCheck = PC.string('not')(state);
  if (notCheck.success) {
    const nextPos = notCheck.state.position;
    if (nextPos >= notCheck.state.input.length ||
        !/[a-zA-Z0-9_]/.test(notCheck.state.input[nextPos])) {
      // Boundary check passed, now consume with trivia
      const notResult = withTriviaLiteral('not', PC.string('not'))(state);
      if (notResult.success) {
        return notResult;
      }
    }
  }

  return { success: false, error: 'Expected not', state };
};