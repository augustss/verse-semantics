/**
 * Punctuation and bracket parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';

// Parentheses
export const leftParen: PC.Parser<AST.Token<'('>> = (state) => {
  return withTriviaLiteral('(', PC.char('('))(state);
};

export const rightParen: PC.Parser<AST.Token<')'>> = (state) => {
  return withTriviaLiteral(')', PC.char(')'))(state);
};

// Braces
export const leftBrace: PC.Parser<AST.Token<'{'>> = (state) => {
  return withTriviaLiteral('{', PC.char('{'))(state);
};

export const rightBrace: PC.Parser<AST.Token<'}'>> = (state) => {
  return withTriviaLiteral('}', PC.char('}'))(state);
};

// Brackets
export const leftBracket: PC.Parser<AST.Token<'['>> = (state) => {
  return withTriviaLiteral('[', PC.char('['))(state);
};

export const rightBracket: PC.Parser<AST.Token<']'>> = (state) => {
  return withTriviaLiteral(']', PC.char(']'))(state);
};

// Other punctuation
export const comma: PC.Parser<AST.Token<','>> = (state) => {
  return withTriviaLiteral(',', PC.char(','))(state);
};

export const semicolon: PC.Parser<AST.Token<';'>> = (state) => {
  return withTriviaLiteral(';', PC.char(';'))(state);
};

export const colon: PC.Parser<AST.Token<':'>> = (state) => {
  return withTriviaLiteral(':', PC.char(':'))(state);
};

export const dot: PC.Parser<AST.Token<'.'>> = (state) => {
  return withTriviaLiteral('.', PC.char('.'))(state);
};

// Range operator for for loops
export const rangeOp: PC.Parser<AST.Token<'..'>> = (state) => {
  // Check for exactly '..' but not '...'
  const dotDotResult = PC.string('..')(state);
  if (!dotDotResult.success) {
    return dotDotResult;
  }

  // Check that the next character is not another dot
  const nextPos = dotDotResult.state.position;
  if (nextPos < dotDotResult.state.input.length && dotDotResult.state.input[nextPos] === '.') {
    // This is '...' or more, reject it
    return { success: false, error: 'Range operator must be exactly two dots (..), not three or more', state };
  }

  return withTriviaLiteral('..', PC.string('..'))(state);
};

// Arrow operator for lambdas
export const arrowOp: PC.Parser<AST.Token<'=>'>> = (state) => {
  return withTriviaLiteral('=>', PC.string('=>'))(state);
};

// Right arrow operator for for loops
export const rightArrowOp: PC.Parser<AST.Token<'->'>> = (state) => {
  return withTriviaLiteral('->', PC.string('->'))(state);
};

// Assignment operators
export const assignOp: PC.Parser<AST.Token<':=' | '='>> = (state) => {
  const colonEqualsResult = withTriviaLiteral(':=', PC.string(':='))(state);
  if (colonEqualsResult.success) {
    return colonEqualsResult as PC.ParserResult<AST.Token<':=' | '='>>;
  }

  // Check for '=' but not '=>'
  const equalsCheck = PC.char('=')(state);
  if (equalsCheck.success) {
    // Make sure it's not followed by '>'
    const nextPos = equalsCheck.state.position;
    if (nextPos < equalsCheck.state.input.length && equalsCheck.state.input[nextPos] === '>') {
      // This is '=>', not '='
      return { success: false, error: 'Expected := or =', state };
    }
    // Now parse with trivia
    const equalsResult = withTriviaLiteral('=', PC.char('='))(state);
    if (equalsResult.success) {
      return equalsResult as PC.ParserResult<AST.Token<':=' | '='>>;
    }
  }

  return { success: false, error: 'Expected := or =', state };
};

// At symbol for decorators
export const atSymbol: PC.Parser<AST.Token<'@'>> = (state) => {
  return withTriviaLiteral('@', PC.char('@'))(state);
};