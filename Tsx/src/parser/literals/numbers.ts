/**
 * Number literal parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { trivia } from '../foundation/trivia';

// Integer literal parser
export const integer: PC.Parser<AST.IntegerLiteral> = (state) => {
  const startPos = state.position;

  // First, parse any leading trivia
  const leadingTriviaResult = trivia(state);
  let currentState = leadingTriviaResult.state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Look ahead to make sure this isn't actually a float
  const afterTrivia = currentState;
  const rangeCheckRegex = /^(\d+)\.\./.exec(afterTrivia.input.slice(afterTrivia.position));
  if (rangeCheckRegex) {
    // This is the start of a range expression, parse just the integer part
    const intRegex = /^\d+/.exec(afterTrivia.input.slice(afterTrivia.position));
    if (!intRegex) {
      return { success: false, error: 'Expected integer', state };
    }

    const newPos = afterTrivia.position + intRegex[0].length;
    const trailingState = { ...afterTrivia, position: newPos };
    const trailingResult = trivia(trailingState);
    const trailingTrivia = trailingResult.success ? trailingResult.value : '';
    const finalState = trailingResult.success ? trailingResult.state : trailingState;

    const text = intRegex[0];
    const token = AST.token(
      text,
      parseInt(text, 10),
      { leading: leadingTrivia, trailing: trailingTrivia },
      { start: startPos, end: finalState.position }
    );

    return {
      success: true,
      value: AST.integerLiteral(token, { start: startPos, end: finalState.position }),
      state: finalState
    };
  }

  // Check for float patterns and reject them
  const floatRegex = /^(\d+\.\d*|\.\d+|\d+\.[eE][+-]?\d+|\d+\.\d*[eE][+-]?\d+)/;
  const floatMatch = floatRegex.exec(afterTrivia.input.slice(afterTrivia.position));
  if (floatMatch) {
    return { success: false, error: 'Expected integer but found float', state };
  }

  // Parse pure integer
  const intRegex = /^\d+/;
  const match = intRegex.exec(afterTrivia.input.slice(afterTrivia.position));
  if (!match) {
    return { success: false, error: 'Expected integer literal', state };
  }

  const newPos = afterTrivia.position + match[0].length;

  // Get trailing trivia
  const trailingState = { ...afterTrivia, position: newPos };
  const trailingResult = trivia(trailingState);
  const trailingTrivia = trailingResult.success ? trailingResult.value : '';
  const finalState = trailingResult.success ? trailingResult.state : trailingState;

  const text = match[0];
  const token = AST.token(
    text,
    parseInt(text, 10),
    { leading: leadingTrivia, trailing: trailingTrivia },
    { start: startPos, end: finalState.position }
  );

  return {
    success: true,
    value: AST.integerLiteral(token, { start: startPos, end: finalState.position }),
    state: finalState
  };
};

// Float literal parser
export const floatLiteral: PC.Parser<AST.FloatLiteral> = (state) => {
  const startPos = state.position;

  // First, parse any leading trivia
  const leadingTriviaResult = trivia(state);
  const afterTrivia = leadingTriviaResult.success ? leadingTriviaResult.state : state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Check if we have a number followed by .. (range operator)
  const rangeCheckRegex = /^(\d+)\.\./.exec(afterTrivia.input.slice(afterTrivia.position));
  if (rangeCheckRegex) {
    // This is the start of a range expression, not a float
    return { success: false, error: 'Not a float literal (range operator detected)', state };
  }

  // Match floating point patterns
  const floatRegex = /^(?:(?:\d+\.\d*|\.\d+)(?:[eE][+-]?\d+)?|\d+\.(?!\.)|\d+[eE][+-]?\d+)/;
  const match = floatRegex.exec(afterTrivia.input.slice(afterTrivia.position));
  if (!match) {
    return { success: false, error: 'Not a float literal', state };
  }

  // Make sure it's actually a float (has a dot or exponent)
  if (!match[0].includes('.') && !match[0].toLowerCase().includes('e')) {
    return { success: false, error: 'Not a float literal', state };
  }

  const newPos = afterTrivia.position + match[0].length;

  // Get trailing trivia
  const trailingState = { ...afterTrivia, position: newPos };
  const trailingResult = trivia(trailingState);
  const trailingTrivia = trailingResult.success ? trailingResult.value : '';
  const finalState = trailingResult.success ? trailingResult.state : trailingState;

  const text = match[0];
  const token = AST.token(
    text,
    parseFloat(text),
    { leading: leadingTrivia, trailing: trailingTrivia },
    { start: startPos, end: finalState.position }
  );

  return {
    success: true,
    value: AST.floatLiteral(token, { start: startPos, end: finalState.position }),
    state: finalState
  };
};