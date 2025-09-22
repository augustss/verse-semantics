/**
 * Variable declaration parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral, withTrivia } from '../foundation/tokens';
import { variable } from '../literals/identifiers';
import { colon, assignOp } from '../operators/punctuation';
import { modularExpr } from '../expressions/core';
import { trivia } from '../foundation/trivia';

// Parse a specifier like <public>, <path(...)>, <mut>, etc.
const parseSpecifier: PC.Parser<AST.Specifier> = (state) => {
  const startPos = state.position;

  // Parse leading trivia
  const leadingTriviaResult = trivia(state);
  const afterTrivia = leadingTriviaResult.success ? leadingTriviaResult.state : state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Must start with <
  if (afterTrivia.position >= afterTrivia.input.length ||
      afterTrivia.input[afterTrivia.position] !== '<') {
    return { success: false, error: 'Expected < for specifier', state };
  }

  // Find matching >
  let currentPos = afterTrivia.position + 1;
  let depth = 1;

  while (currentPos < afterTrivia.input.length && depth > 0) {
    const ch = afterTrivia.input[currentPos];
    if (ch === '<') depth++;
    else if (ch === '>') depth--;
    if (depth > 0) currentPos++;
  }

  if (depth !== 0 || currentPos >= afterTrivia.input.length) {
    return { success: false, error: 'Unclosed specifier', state };
  }

  // Extract the specifier content (including < and >)
  const specifierText = afterTrivia.input.slice(afterTrivia.position, currentPos + 1);

  // Parse trailing trivia
  const afterSpec = { ...afterTrivia, position: currentPos + 1 };
  const trailingTriviaResult = trivia(afterSpec);
  const trailingTrivia = trailingTriviaResult.success ? trailingTriviaResult.value : '';
  const finalState = trailingTriviaResult.success ? trailingTriviaResult.state : afterSpec;

  // For now, create a simplified specifier structure
  // Parse the content between < and > to get the name and any arguments
  const innerContent = specifierText.slice(1, -1); // Remove < and >

  // Create tokens for the AST with proper types
  const leftAngleToken = AST.token('<' as const, '<', { leading: leadingTrivia, trailing: '' }, { start: startPos, end: afterTrivia.position + 1 }) as AST.Token<'<'>;
  const rightAngleToken = AST.token('>' as const, '>', { leading: '', trailing: trailingTrivia }, { start: currentPos, end: finalState.position }) as AST.Token<'>'>;

  // For simplicity, treat the entire inner content as the name
  // TODO: Parse arguments like path(config.json) properly
  const nameToken = AST.token(innerContent, innerContent, { leading: '', trailing: '' }, { start: afterTrivia.position + 1, end: currentPos });

  return {
    success: true,
    value: AST.specifier(
      leftAngleToken,
      nameToken,
      undefined, // leftParen
      undefined, // argument
      undefined, // rightParen
      rightAngleToken,
      { start: startPos, end: finalState.position }
    ),
    state: finalState
  };
};

// Variable declaration
export const varDeclaration: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'var' keyword with boundary check
  const varCheck = PC.string('var')(state);
  if (!varCheck.success) return { success: false, error: 'Not a var declaration', state };

  // Check it's not part of a longer identifier
  const nextPos = varCheck.state.position;
  if (nextPos < varCheck.state.input.length && /[a-zA-Z0-9_]/.test(varCheck.state.input[nextPos])) {
    return { success: false, error: 'var is part of identifier', state };
  }

  // Parse 'var' with trivia
  const varResult = withTriviaLiteral('var', PC.string('var'))(state);
  if (!varResult.success) return varResult;

  // Parse variable name
  const nameResult = variable(varResult.state);
  if (!nameResult.success) return { success: false, error: 'Expected variable name after var', state };

  // Optional: Parse specifier like <public>, <mut>, etc.
  let specifierResult: PC.ParserResult<AST.Specifier> | null = null;
  let currentState = nameResult.state;

  const specResult = parseSpecifier(currentState);
  if (specResult.success) {
    specifierResult = specResult;
    currentState = specResult.state;
  }

  // Parse colon
  const colonResult = colon(currentState);
  if (!colonResult.success) return { success: false, error: 'Expected : after variable name', state };

  // Parse type (as identifier)
  const typeResult = variable(colonResult.state);
  if (!typeResult.success) return { success: false, error: 'Expected type after :', state };

  // Parse assignment
  const assignResult = assignOp(typeResult.state);
  if (!assignResult.success) return { success: false, error: 'Expected = after type', state };

  // Parse initial value
  const valueResult = modularExpr(assignResult.state);
  if (!valueResult.success) return { success: false, error: 'Expected value after =', state };

  return {
    success: true,
    value: AST.variableDeclaration(
      varResult.value,
      nameResult.value.token,
      specifierResult ? specifierResult.value : undefined,
      colonResult.value,
      typeResult.value.token,
      assignResult.value as AST.Token<'='>,
      valueResult.value,
      { start: startPos, end: valueResult.state.position }
    ),
    state: valueResult.state
  };
};