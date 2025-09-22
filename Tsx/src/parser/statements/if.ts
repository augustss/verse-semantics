/**
 * If/then/else expression parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { leftParen, rightParen, leftBrace, rightBrace, colon } from '../operators/punctuation';
import { withTriviaLiteral } from '../foundation/tokens';
import { trivia } from '../foundation/trivia';
import { parseIndentedStatements, statementsToBody } from './shared-indented';

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

// Parse indented statements after a colon (delegates to shared implementation)
const parseIndentedBlock = (state: PC.ParserState): PC.ParserResult<AST.Expr> => {
  const startPos = state.position;

  const result = parseIndentedStatements(state, getExpr);
  if (!result.success) {
    return result as PC.ParserResult<AST.Expr>;
  }

  const blockExpr = statementsToBody(result.value, startPos, result.state.position);

  return {
    success: true,
    value: blockExpr,
    state: result.state
  };
};

// Parse a single statement (helper function)
const parseStatement = (state: PC.ParserState): PC.ParserResult<AST.Statement> => {
  const startPos = state.position;

  // Parse the expression
  const exprResult = getExpr()(state);
  if (!exprResult.success) return exprResult;

  // For now, we don't handle semicolons in IF block statements
  return {
    success: true,
    value: AST.statement(
      exprResult.value,
      undefined,
      { start: startPos, end: exprResult.state.position }
    ),
    state: exprResult.state
  };
};

// If/then/else expressions
export const ifExpression: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'if' keyword with boundary check
  const ifCheck = PC.string('if')(state);
  if (!ifCheck.success) return { success: false, error: 'Not an if expression', state };

  // Check it's not part of a longer identifier
  const nextPos = ifCheck.state.position;
  if (nextPos < ifCheck.state.input.length && /[a-zA-Z0-9_]/.test(ifCheck.state.input[nextPos])) {
    return { success: false, error: 'if is part of identifier', state };
  }

  // Parse 'if' with trivia
  const ifResult = withTriviaLiteral('if', PC.string('if'))(state);
  if (!ifResult.success) return ifResult;

  let condState = ifResult.state;

  // Check for optional parentheses
  let leftParenToken: AST.Token<'('> | undefined;
  let rightParenToken: AST.Token<')'> | undefined;

  const lpResult = leftParen(condState);
  if (lpResult.success) {
    leftParenToken = lpResult.value;
    condState = lpResult.state;
  }

  // Parse condition expression
  const condResult = getExpr()(condState);
  if (!condResult.success) return condResult;

  condState = condResult.state;

  // Close parentheses if opened
  if (leftParenToken) {
    const rpResult = rightParen(condState);
    if (rpResult.success) {
      rightParenToken = rpResult.value;
      condState = rpResult.state;
    }
  }

  // Check for 'then' keyword or colon
  const thenCheck = PC.string('then')(condState);
  let thenToken: AST.Token<'then'> | undefined;
  let colonToken: AST.Token<':'> | undefined;
  let thenExprResult: PC.ParserResult<AST.Expr>;
  let currentState: PC.ParserState;
  let thenLbrace: AST.Token<'{'> | undefined;
  let thenRbrace: AST.Token<'}'> | undefined;

  if (thenCheck.success) {
    // Check boundary
    const thenNextPos = thenCheck.state.position;
    if (thenNextPos < thenCheck.state.input.length && /[a-zA-Z0-9_]/.test(thenCheck.state.input[thenNextPos])) {
      return { success: false, error: 'then is part of identifier', state };
    }

    const thenParseResult = withTriviaLiteral('then', PC.string('then'))(condState);
    if (!thenParseResult.success) return thenParseResult;

    thenToken = thenParseResult.value as AST.Token<'then'>;
    let thenState = thenParseResult.state;

    // Check for colon after 'then' (indentation style): if(condition) then:
    const colonResult = colon(thenState);
    if (colonResult.success) {
      colonToken = colonResult.value;

      // Parse indented then branch - handle multiple statements as a block
      const blockResult = parseIndentedBlock(colonResult.state);
      if (!blockResult.success) return blockResult;

      thenExprResult = blockResult;
      currentState = blockResult.state;
    } else {
      // Parse then branch - delegate to expression parser which handles blocks
      thenExprResult = getExpr()(thenState);
      if (!thenExprResult.success) return thenExprResult;

      currentState = thenExprResult.state;
    }
  } else {
    // Try colon syntax
    const colonResult2 = colon(condState);
    if (!colonResult2.success) return { success: false, error: 'Expected then or : after if condition', state };

    colonToken = colonResult2.value;

    // Create a dummy then token for colon syntax (required by AST)
    thenToken = {
      text: 'then',
      value: 'then',
      trivia: { leading: '', trailing: '' },
      span: { start: colonToken.span.start, end: colonToken.span.start }
    } as AST.Token<'then'>;

    // Parse indented then branch - handle multiple statements as a block
    const blockResult = parseIndentedBlock(colonResult2.state);
    if (!blockResult.success) return blockResult;

    thenExprResult = blockResult;
    currentState = blockResult.state;
  }

  // Check for optional 'else' branch
  // First skip any trivia (whitespace, comments) before looking for 'else'
  const triviaResult = trivia(currentState);
  const triviaSkippedState = triviaResult.success ? triviaResult.state : currentState;

  // Now check if 'else' matches at the position after trivia (for boundary checking)
  const elseStringCheck = PC.string('else')(triviaSkippedState);
  if (elseStringCheck.success) {
    // Check boundary - make sure 'else' is not part of a longer identifier
    const afterElsePos = elseStringCheck.state.position;
    if (afterElsePos < elseStringCheck.state.input.length &&
        /[a-zA-Z0-9_]/.test(elseStringCheck.state.input[afterElsePos])) {
      // 'else' is part of a longer identifier, skip else parsing
    } else {
      // 'else' is a standalone keyword, now parse it with trivia from the original position (includes leading trivia)
      const elseResult = withTriviaLiteral('else', PC.string('else'))(currentState);
      if (elseResult.success) {
        let elseState = elseResult.state;

        // Parse else branch - support both braces and colon syntax
        let elseLbrace: AST.Token<'{'> | undefined;
        let elseRbrace: AST.Token<'}'> | undefined;
        let elseColonToken: AST.Token<':'> | undefined;
        let elseExprResult: PC.ParserResult<AST.Expr>;
        let elseEndState: PC.ParserState;
        let elseStyle: 'braces' | 'indentation';

        // Try colon style first: else: ...
        const elseColonResult = colon(elseState);
        if (elseColonResult.success) {
          elseColonToken = elseColonResult.value;

          const elseBlockResult = parseIndentedBlock(elseColonResult.state);
          if (elseBlockResult.success) {
            elseExprResult = elseBlockResult;
            elseEndState = elseBlockResult.state;
            elseStyle = 'indentation';
          } else {
            return elseBlockResult;
          }
        } else {
          // Parse else branch - delegate to expression parser which handles blocks
          elseExprResult = getExpr()(elseState);
          if (elseExprResult.success) {
            elseEndState = elseExprResult.state;
            elseStyle = 'braces'; // Default style
          } else {
            return elseExprResult;
          }
        }

        // Create else clause
        const elseClauseObj = AST.elseClause(
          elseResult.value,
          { start: elseResult.value.span.start, end: elseEndState.position },
          undefined, // No else-if
          elseStyle,
          elseLbrace,
          elseColonToken,
          elseExprResult.value,
          elseRbrace
        );

        // Create if expression with else branch
        const style = colonToken ? 'indentation' : 'braces';

        return {
          success: true,
          value: AST.ifExpression(
            ifResult.value,
            leftParenToken,
            condResult.value,
            rightParenToken,
            thenToken!,
            style,
            thenExprResult.value,
            { start: startPos, end: elseEndState.position },
            thenLbrace,
            colonToken,
            thenRbrace,
            elseClauseObj
          ),
          state: elseEndState
        };
      }
    }
  }

  // Create if expression without else branch
  const style = colonToken ? 'indentation' : 'braces';
  const endPos = currentState.position;

  return {
    success: true,
    value: AST.ifExpression(
      ifResult.value,
      leftParenToken,
      condResult.value,
      rightParenToken,
      thenToken as AST.Token<'then'>,
      style,
      thenExprResult.value,
      { start: startPos, end: endPos },
      thenLbrace,
      colonToken,
      thenRbrace,
      undefined // No else clause
    ),
    state: currentState
  };
};