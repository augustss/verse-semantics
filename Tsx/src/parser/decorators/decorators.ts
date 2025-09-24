/**
 * Decorator parsing (@editable, @public, etc.)
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { atSymbol, leftParen, rightParen, comma, leftBrace, rightBrace } from '../operators/punctuation';
import { variable } from '../literals/identifiers';

// Forward declaration for expression parser to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

/**
 * Parse a single decorator: @name or @name(args)
 */
export const decorator: PC.Parser<AST.Decorator> = (state) => {
  const startPos = state.position;

  // Parse @ symbol
  const atResult = atSymbol(state);
  if (!atResult.success) return atResult;

  // Parse decorator name (identifier)
  const nameResult = variable(atResult.state);
  if (!nameResult.success) return { success: false, error: 'Expected decorator name after @', state };

  let currentState = nameResult.state;
  let leftParenToken: AST.Token<'('> | undefined;
  let rightParenToken: AST.Token<')'> | undefined;
  let args: AST.Expr[] | undefined;
  let commas: AST.Token<','>[] | undefined;

  // Check for optional arguments: @name(arg1, arg2) or @name{key := value}
  // First try parentheses
  const leftParenResult = leftParen(currentState);
  if (leftParenResult.success) {
    leftParenToken = leftParenResult.value;
    currentState = leftParenResult.state;

    // Parse arguments
    args = [];
    commas = [];

    // Check for empty argument list
    const rightParenResult = rightParen(currentState);
    if (rightParenResult.success) {
      rightParenToken = rightParenResult.value;
      currentState = rightParenResult.state;
    } else {
      // Parse arguments
      while (true) {
        const argResult = getExpr()(currentState);
        if (!argResult.success) break;

        args.push(argResult.value);
        currentState = argResult.state;

        // Check for comma
        const commaResult = comma(currentState);
        if (commaResult.success) {
          commas.push(commaResult.value);
          currentState = commaResult.state;
        } else {
          break;
        }
      }

      // Expect closing paren
      const rightParenResult2 = rightParen(currentState);
      if (!rightParenResult2.success) {
        return { success: false, error: 'Expected ) after decorator arguments', state };
      }
      rightParenToken = rightParenResult2.value;
      currentState = rightParenResult2.state;
    }
  } else {
    // Try braces for @editable {ToolTip := "text"} style
    const leftBraceResult = leftBrace(currentState);
    if (leftBraceResult.success) {
      currentState = leftBraceResult.state;

      // Parse the brace content as an expression
      // This could be empty {} or contain key := value pairs
      const rightBraceCheck = rightBrace(currentState);
      if (rightBraceCheck.success) {
        // Empty braces @editable {}
        currentState = rightBraceCheck.state;
      } else {
        // Parse the content as an expression
        const contentResult = getExpr()(currentState);
        if (contentResult.success) {
          // Store as single argument
          args = [contentResult.value];
          currentState = contentResult.state;
        }

        // Expect closing brace
        const rightBraceResult = rightBrace(currentState);
        if (!rightBraceResult.success) {
          return { success: false, error: 'Expected } after decorator brace content', state };
        }
        currentState = rightBraceResult.state;
      }
    }
  }

  return {
    success: true,
    value: AST.decorator(
      atResult.value,
      nameResult.value.token,
      { start: startPos, end: currentState.position },
      leftParenToken,
      args,
      commas,
      rightParenToken
    ),
    state: currentState
  };
};

/**
 * Parse multiple decorators
 */
export const decorators: PC.Parser<AST.Decorator[]> = (state) => {
  const decoratorList: AST.Decorator[] = [];
  let currentState = state;

  while (true) {
    const decoratorResult = decorator(currentState);
    if (!decoratorResult.success) break;

    decoratorList.push(decoratorResult.value);
    currentState = decoratorResult.state;
  }

  return {
    success: true,
    value: decoratorList,
    state: currentState
  };
};