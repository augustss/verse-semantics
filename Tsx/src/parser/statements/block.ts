/**
 * Block statement parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { leftBrace, rightBrace, colon, semicolon } from '../operators/punctuation';
import { withTriviaLiteral } from '../foundation/tokens';
import { trivia } from '../foundation/trivia';
import { parseIndentedStatements, statementsToBody } from './shared-indented';

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

// Parse a single statement (expression optionally followed by semicolon, or just semicolon for empty statement)
const parseStatement: PC.Parser<AST.Statement> = (state) => {
  const startPos = state.position;

  // Check for empty statement (just semicolon)
  const emptySemiResult = semicolon(state);
  if (emptySemiResult.success) {
    return {
      success: true,
      value: AST.statement(
        undefined, // No expression for empty statement
        emptySemiResult.value,
        { start: startPos, end: emptySemiResult.state.position }
      ),
      state: emptySemiResult.state
    };
  }

  // Parse the expression
  const exprResult = getExpr()(state);
  if (!exprResult.success) return exprResult;

  let currentState = exprResult.state;

  // Check for optional semicolon
  const semiResult = semicolon(currentState);
  if (semiResult.success) {
    return {
      success: true,
      value: AST.statement(
        exprResult.value,
        semiResult.value,
        { start: startPos, end: semiResult.state.position }
      ),
      state: semiResult.state
    };
  }

  // No semicolon, statement ends at expression
  return {
    success: true,
    value: AST.statement(
      exprResult.value,
      undefined,
      { start: startPos, end: currentState.position }
    ),
    state: currentState
  };
};

// Block expressions
export const blockExpression: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'block:' syntax (indentation style)
  const blockCheck = PC.string('block')(state);
  if (blockCheck.success) {
    // Check it's not part of a longer identifier
    const nextPos = blockCheck.state.position;
    if (nextPos < blockCheck.state.input.length && /[a-zA-Z0-9_]/.test(blockCheck.state.input[nextPos])) {
      return { success: false, error: 'block is part of identifier', state };
    }

    const blockResult = withTriviaLiteral('block', PC.string('block'))(state);
    if (blockResult.success) {
      // Expect colon for indentation style
      // For indented blocks, we need a special colon parser that doesn't consume newlines as trailing trivia
      const colonStartPos = blockResult.state.position;
      const colonCharResult = PC.char(':')(blockResult.state);
      if (!colonCharResult.success) {
        return { success: false, error: 'Expected : after block', state };
      }

      // After the colon, capture trailing whitespace up to and including the newline,
      // but not the indentation of the next line
      let colonTrailing = '';
      let pos = colonCharResult.state.position;

      // First, capture any spaces/tabs on the same line
      while (pos < colonCharResult.state.input.length) {
        const ch = colonCharResult.state.input[pos];
        if (ch === ' ' || ch === '\t') {
          colonTrailing += ch;
          pos++;
        } else {
          break;
        }
      }

      // Now capture the newline if present
      if (pos < colonCharResult.state.input.length) {
        const ch = colonCharResult.state.input[pos];
        if (ch === '\n') {
          colonTrailing += ch;
          pos++;
        } else if (ch === '\r') {
          colonTrailing += ch;
          pos++;
          // Handle CRLF
          if (pos < colonCharResult.state.input.length && colonCharResult.state.input[pos] === '\n') {
            colonTrailing += '\n';
            pos++;
          }
        }
      }

      // Create colon token with trailing trivia including newline
      const colonToken: AST.Token<':'> = {
        text: ':',
        value: ':' as const,
        trivia: { leading: '', trailing: colonTrailing },
        span: { start: colonStartPos, end: pos }
      };

      // Use shared indented statement parser
      const indentedResult = parseIndentedStatements(colonCharResult.state, getExpr);
      if (!indentedResult.success) {
        return indentedResult as PC.ParserResult<AST.Expr>;
      }

      // Convert expressions to statements for block
      const statements = indentedResult.value.map(expr =>
        AST.statement(expr, undefined, expr.span)
      );

      // Create block with indentation style
      return {
        success: true,
        value: AST.block(
          'indentation',
          statements,
          blockResult.value,
          undefined,
          undefined,
          colonToken,
          { start: startPos, end: indentedResult.state.position }
        ),
        state: indentedResult.state
      };
    }
  }

  // Try brace style block without 'block' keyword
  const lbraceResult = leftBrace(state);
  if (lbraceResult.success) {
    const statements: AST.Statement[] = [];
    let currentState = lbraceResult.state;

    // Parse statements until we hit closing brace
    while (true) {
      // Try to parse a statement first
      const stmtResult = parseStatement(currentState);
      if (stmtResult.success) {
        statements.push(stmtResult.value);
        currentState = stmtResult.state;

        // Skip trivia between statements
        const triviaResult = trivia(currentState);
        if (triviaResult.success) {
          currentState = triviaResult.state;
        }
        continue;
      }

      // If we can't parse a statement, check for closing brace
      const rbraceResult = rightBrace(currentState);
      if (rbraceResult.success) {
        // Create block with braces style
        return {
          success: true,
          value: AST.block(
            'braces',
            statements,
            undefined,
            lbraceResult.value,
            rbraceResult.value,
            undefined,
            { start: startPos, end: rbraceResult.state.position }
          ),
          state: rbraceResult.state
        };
      }

      // If we can't parse a statement or find closing brace, break
      break;
    }
  }

  return { success: false, error: 'Not a block expression', state };
};