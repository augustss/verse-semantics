/**
 * Arithmetic operator parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';

// Addition operators (+, -)
export const addOp: PC.Parser<AST.Token<'+' | '-'>> = (state) => {
  const plusResult = withTriviaLiteral('+', PC.char('+'))(state);
  if (plusResult.success) {
    return plusResult as PC.ParserResult<AST.Token<'+' | '-'>>;
  }

  const minusResult = withTriviaLiteral('-', PC.char('-'))(state);
  if (minusResult.success) {
    return minusResult as PC.ParserResult<AST.Token<'+' | '-'>>;
  }

  return { success: false, error: 'Expected + or -', state };
};

// Multiplication operators (*, /, %)
export const mulOp: PC.Parser<AST.Token<'*' | '/' | '%'>> = (state) => {
  const starResult = withTriviaLiteral('*', PC.char('*'))(state);
  if (starResult.success) {
    return starResult as PC.ParserResult<AST.Token<'*' | '/' | '%'>>;
  }

  const slashResult = withTriviaLiteral('/', PC.char('/'))(state);
  if (slashResult.success) {
    return slashResult as PC.ParserResult<AST.Token<'*' | '/' | '%'>>;
  }

  const percentResult = withTriviaLiteral('%', PC.char('%'))(state);
  if (percentResult.success) {
    return percentResult as PC.ParserResult<AST.Token<'*' | '/' | '%'>>;
  }

  return { success: false, error: 'Expected *, /, or %', state };
};

// Unary minus operator
export const unaryMinusOp: PC.Parser<AST.Token<'-'>> = (state) => {
  return withTriviaLiteral('-', PC.char('-'))(state);
};