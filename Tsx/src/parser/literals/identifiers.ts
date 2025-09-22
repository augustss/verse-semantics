/**
 * Identifier parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { trivia } from '../foundation/trivia';

// Simple identifier parser
export const identifier = (state: PC.ParserState): PC.ParserResult<string> => {
  const startPos = state.position;
  let currentPos = startPos;

  if (currentPos >= state.input.length) {
    return { success: false, error: 'Expected identifier', state };
  }

  // First character must be letter or underscore
  const firstChar = state.input[currentPos];
  if (!/[a-zA-Z_]/.test(firstChar)) {
    return { success: false, error: 'Expected identifier', state };
  }

  currentPos++;

  // Rest can be alphanumeric or underscore
  while (currentPos < state.input.length && /[a-zA-Z0-9_]/.test(state.input[currentPos])) {
    currentPos++;
  }

  const id = state.input.slice(startPos, currentPos);

  // Check if it's a reserved keyword
  const reservedKeywords = [
    'var', 'if', 'then', 'else', 'for', 'case', 'block', 'using',
    'module', 'class', 'interface', 'enum', 'struct', 'function',
    'return', 'break', 'continue', 'not', 'and', 'or', 'array',
    'true', 'false'
  ];

  if (reservedKeywords.includes(id)) {
    return { success: false, error: `Unexpected keyword: ${id}`, state };
  }

  return {
    success: true,
    value: id,
    state: { ...state, position: currentPos }
  };
};

// Variable parser (identifier as an expression)
export const variable: PC.Parser<AST.Variable> = (state) => {
  const startPos = state.position;

  // Parse leading trivia
  const leadingTriviaResult = trivia(state);
  const afterTrivia = leadingTriviaResult.success ? leadingTriviaResult.state : state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Parse identifier name (including potential specifiers)
  let nameEnd = afterTrivia.position;
  let nameText = '';
  let currentState = afterTrivia;

  // Check for specifiers BEFORE the identifier like <public>, <mut>, etc.
  let specifierPrefix = '';
  while (currentState.position < currentState.input.length &&
         currentState.input[currentState.position] === '<') {
    // Find the matching >
    let specEnd = currentState.position + 1;
    let depth = 1;

    // Handle nested brackets in specifiers like <path(config.json)>
    while (specEnd < currentState.input.length && depth > 0) {
      if (currentState.input[specEnd] === '<') depth++;
      else if (currentState.input[specEnd] === '>') depth--;
      if (depth > 0) specEnd++;
    }

    if (specEnd < currentState.input.length && depth === 0) {
      // Include the specifier as a prefix
      specifierPrefix += currentState.input.slice(currentState.position, specEnd + 1);
      currentState = { ...currentState, position: specEnd + 1 };

      // Skip any whitespace after the specifier
      while (currentState.position < currentState.input.length &&
             /\s/.test(currentState.input[currentState.position])) {
        specifierPrefix += currentState.input[currentState.position];
        currentState = { ...currentState, position: currentState.position + 1 };
      }
    } else {
      break; // Unclosed specifier
    }
  }

  // Parse base identifier
  const idResult = identifier(currentState);
  if (!idResult.success) return idResult;

  nameText = specifierPrefix + idResult.value;
  nameEnd = idResult.state.position;
  currentState = idResult.state;

  // Check for specifiers AFTER the identifier like <public>, <private>, etc.
  while (currentState.position < currentState.input.length &&
         currentState.input[currentState.position] === '<') {
    // Find the matching >
    let specEnd = currentState.position + 1;
    while (specEnd < currentState.input.length &&
           currentState.input[specEnd] !== '>') {
      specEnd++;
    }
    if (specEnd < currentState.input.length) {
      // Include the specifier in the name
      nameText += currentState.input.slice(currentState.position, specEnd + 1);
      nameEnd = specEnd + 1;
      currentState = { ...currentState, position: specEnd + 1 };
    } else {
      break; // Unclosed specifier
    }
  }

  // Parse trailing trivia
  const trailingTriviaResult = trivia(currentState);
  const trailingTrivia = trailingTriviaResult.success ? trailingTriviaResult.value : '';
  const finalState = trailingTriviaResult.success ? trailingTriviaResult.state : currentState;

  const nameToken = AST.token(
    nameText,
    nameText,
    { leading: leadingTrivia, trailing: trailingTrivia },
    { start: startPos, end: finalState.position }
  );

  return {
    success: true,
    value: AST.variable(nameToken, { start: startPos, end: finalState.position }),
    state: finalState
  };
};

// Member name parser - like identifier but allows keywords for member access
export const memberName = (state: PC.ParserState): PC.ParserResult<string> => {
  const startPos = state.position;
  let currentPos = startPos;

  if (currentPos >= state.input.length) {
    return { success: false, error: 'Expected member name', state };
  }

  // First character must be letter or underscore
  const firstChar = state.input[currentPos];
  if (!/[a-zA-Z_]/.test(firstChar)) {
    return { success: false, error: 'Expected member name', state };
  }

  currentPos++;

  // Rest can be alphanumeric or underscore
  while (currentPos < state.input.length && /[a-zA-Z0-9_]/.test(state.input[currentPos])) {
    currentPos++;
  }

  const id = state.input.slice(startPos, currentPos);

  // Don't reject keywords for member names - allow obj.continue(), obj.class(), etc.
  return {
    success: true,
    value: id,
    state: { ...state, position: currentPos }
  };
};