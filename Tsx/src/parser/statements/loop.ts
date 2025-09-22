/**
 * Loop statement parsing
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

// Loop statements: loop: body
export const loopStatement: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'loop' keyword with boundary check
  const loopCheck = PC.string('loop')(state);
  if (!loopCheck.success) return { success: false, error: 'Not a loop statement', state };

  // Check it's not part of a longer identifier
  const nextPos = loopCheck.state.position;
  if (nextPos < loopCheck.state.input.length && /[a-zA-Z0-9_]/.test(loopCheck.state.input[nextPos])) {
    return { success: false, error: 'loop is part of identifier', state };
  }

  // Parse 'loop' with trivia
  const loopResult = withTriviaLiteral('loop', PC.string('loop'))(state);
  if (!loopResult.success) return loopResult;

  let bodyState = loopResult.state;

  // Check for brace or colon style
  const lbraceResult = leftBrace(bodyState);
  if (lbraceResult.success) {
    // Brace style - check for empty block first
    const rbraceCheck = rightBrace(lbraceResult.state);
    if (rbraceCheck.success) {
      // Empty block
      const emptyBody = AST.emptyExpression({ start: lbraceResult.state.position, end: lbraceResult.state.position });
      return {
        success: true,
        value: AST.loopStatement(
          loopResult.value,
          'braces',
          emptyBody,
          { start: startPos, end: rbraceCheck.state.position },
          lbraceResult.value,
          undefined,
          rbraceCheck.value
        ),
        state: rbraceCheck.state
      };
    }

    // Non-empty block - parse multiple statements
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
        // Create appropriate body expression
        let bodyExpr: AST.Expr;
        if (statements.length === 0) {
          bodyExpr = AST.emptyExpression({ start: lbraceResult.state.position, end: currentState.position });
        } else if (statements.length === 1 && !statements[0].expr) {
          // Single empty statement
          bodyExpr = AST.emptyExpression({ start: lbraceResult.state.position, end: currentState.position });
        } else if (statements.length === 1 && statements[0].expr) {
          // Single expression statement
          bodyExpr = statements[0].expr;
        } else {
          // Multiple statements - create a block
          bodyExpr = AST.block(
            'braces',
            statements,
            undefined,
            lbraceResult.value,
            rbraceResult.value,
            undefined,
            { start: lbraceResult.state.position, end: rbraceResult.state.position }
          );
        }

        return {
          success: true,
          value: AST.loopStatement(
            loopResult.value,
            'braces',
            bodyExpr,
            { start: startPos, end: rbraceResult.state.position },
            lbraceResult.value,
            undefined,
            rbraceResult.value
          ),
          state: rbraceResult.state
        };
      }

      // If we can't parse a statement or find closing brace, break
      break;
    }

    return { success: false, error: 'Expected } after loop body', state };
  }

  // Try colon/indentation style
  const bodyColonResult = colon(bodyState);
  if (bodyColonResult.success) {
    // Use shared indented statement parser
    const indentedResult = parseIndentedStatements(bodyColonResult.state, getExpr);
    if (!indentedResult.success) {
      return indentedResult as PC.ParserResult<AST.Expr>;
    }

    // Convert to appropriate body expression
    const bodyExpr = statementsToBody(
      indentedResult.value,
      bodyColonResult.state.position,
      indentedResult.state.position
    );

    return {
      success: true,
      value: AST.loopStatement(
        loopResult.value,
        'indentation',
        bodyExpr,
        { start: startPos, end: indentedResult.state.position },
        undefined,
        bodyColonResult.value,
        undefined
      ),
      state: indentedResult.state
    };
  }

  return { success: false, error: 'Expected { or : after loop', state };
};