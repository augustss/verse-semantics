/**
 * Case expression parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { leftParen, rightParen, leftBrace, rightBrace, colon, comma, arrowOp } from '../operators/punctuation';
import { withTriviaLiteral } from '../foundation/tokens';
import { parseIndentedStatements } from './shared-indented';
import { modularExprNoLambda } from '../expressions/core';

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

// Parse a single case branch
const parseCaseBranch: PC.Parser<AST.CaseBranch> = (state, style?: 'braces' | 'indentation') => {
  const startPos = state.position;

  // Try to parse underscore for default case
  const underscoreResult = withTriviaLiteral('_', PC.char('_'))(state);
  let pattern: AST.Expr | AST.Token<'_'>;
  let afterPattern = state;

  if (underscoreResult.success) {
    // Default case with _
    pattern = underscoreResult.value;
    afterPattern = underscoreResult.state;
  } else {
    // Parse pattern expression (restricted to avoid consuming lambda expressions)
    // For case patterns, we only want to parse basic expressions like identifiers,
    // literals, etc., but not lambda expressions

    // Try to parse just an identifier first
    const identifierResult = withTriviaLiteral('identifier', PC.regex(/^[a-zA-Z_][a-zA-Z0-9_]*/))(state);

    if (identifierResult.success) {
      // Create a Variable AST node for the identifier
      pattern = AST.variable(
        identifierResult.value,
        { start: state.position, end: identifierResult.state.position }
      );
      afterPattern = identifierResult.state;
    } else {
      // Fall back to expression parsing for more complex patterns
      // Use the no-lambda parser to avoid consuming => as part of a lambda
      const exprResult = modularExprNoLambda()(state);

      if (!exprResult.success) {
        return { success: false, error: 'Expected pattern in case branch', state };
      }

      pattern = exprResult.value;
      afterPattern = exprResult.state;
    }
  }

  // Expect arrow =>
  const arrowResult = arrowOp(afterPattern);
  if (!arrowResult.success) return { success: false, error: 'Expected => after case pattern', state };

  // Parse body
  let body: AST.Expr | undefined;
  let endState = arrowResult.state;

  // For indentation style, check if body is on next line and indented
  if (style === 'indentation') {
    // Check if we have a newline after =>
    let checkPos = arrowResult.state.position;
    while (checkPos < arrowResult.state.input.length &&
           /[ \t]/.test(arrowResult.state.input[checkPos])) {
      checkPos++;
    }

    if (checkPos < arrowResult.state.input.length &&
        (arrowResult.state.input[checkPos] === '\n' || arrowResult.state.input[checkPos] === '\r')) {
      // Body is on the next line(s), parse as indented block
      const { parseIndentedStatements } = require('./shared-indented');

      // Move to next line
      let bodyState = arrowResult.state;
      while (bodyState.position < bodyState.input.length &&
             bodyState.input[bodyState.position] !== '\n') {
        bodyState = { ...bodyState, position: bodyState.position + 1 };
      }
      if (bodyState.position < bodyState.input.length) {
        bodyState = { ...bodyState, position: bodyState.position + 1 }; // Skip newline
      }

      // Parse indented statements
      const indentedResult = parseIndentedStatements(bodyState, getExpr);
      if (indentedResult.success && indentedResult.value.length > 0) {
        // If we have multiple statements, wrap in a block
        if (indentedResult.value.length > 1) {
          body = AST.block(
            indentedResult.value,
            { start: indentedResult.value[0].span.start, end: indentedResult.state.position }
          );
        } else {
          body = indentedResult.value[0];
        }
        endState = indentedResult.state;
      }
    } else {
      // Body is on the same line, parse as single expression
      if (!getExpr) {
        return { success: false, error: 'Expression parser not initialized for body', state };
      }
      const bodyResult = getExpr()(arrowResult.state);
      if (bodyResult.success) {
        body = bodyResult.value;
        endState = bodyResult.state;
      }
    }
  } else {
    // Brace style - parse single expression on same line
    // Check if there's a body (not just immediately followed by comma or closing brace)
    const nextCharResult = PC.peek(endState);
    if (nextCharResult.success && nextCharResult.value !== ',' && nextCharResult.value !== '}') {
      if (!getExpr) {
        return { success: false, error: 'Expression parser not initialized for body', state };
      }
      const bodyResult = getExpr()(arrowResult.state);
      if (bodyResult.success) {
        body = bodyResult.value;
        endState = bodyResult.state;
      }
    }
  }

  return {
    success: true,
    value: AST.caseBranch(
      pattern,
      arrowResult.value,
      { start: startPos, end: endState.position },
      body
    ),
    state: endState
  };
};

// Case expression
export const caseExpression: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'case' keyword with boundary check
  const caseCheck = PC.string('case')(state);
  if (!caseCheck.success) {
    return { success: false, error: 'Not a case expression', state };
  }

  // Check it's not part of a longer identifier
  const nextPos = caseCheck.state.position;
  if (nextPos < caseCheck.state.input.length && /[a-zA-Z0-9_]/.test(caseCheck.state.input[nextPos])) {
    return { success: false, error: 'case is part of identifier', state };
  }

  // Parse 'case' with trivia
  const caseResult = withTriviaLiteral('case', PC.string('case'))(state);
  if (!caseResult.success) {
    return caseResult;
  }

  // Expect left parenthesis
  const lpResult = leftParen(caseResult.state);
  if (!lpResult.success) {
    return { success: false, error: 'Expected ( after case', state };
  }

  // Parse expression to match
  if (!getExpr) {
    return { success: false, error: 'Expression parser not initialized', state };
  }
  const exprResult = getExpr()(lpResult.state);
  if (!exprResult.success) {
    return { success: false, error: 'Expected expression in case', state };
  }

  // Expect right parenthesis
  const rpResult = rightParen(exprResult.state);
  if (!rpResult.success) {
    return { success: false, error: 'Expected ) after case expression', state };
  }

  let bodyState = rpResult.state;
  const branches: AST.CaseBranch[] = [];
  const commas: AST.Token<','>[] = [];
  let style: 'braces' | 'indentation' | 'parentheses' = 'braces';

  // Check for brace or colon style
  const lbraceResult = leftBrace(bodyState);
  if (lbraceResult.success) {
    // Brace style
    style = 'braces';
    bodyState = lbraceResult.state;

    // Parse branches
    while (true) {
      // Skip trivia first
      const triviaResult = PC.optional(PC.regex(/^[\s\n\r]*/m))(bodyState);
      if (triviaResult.success) {
        bodyState = triviaResult.state;
      }

      // Check for closing brace using peek (don't consume trivia)
      const peekResult = PC.peek(bodyState);
      if (peekResult.success && peekResult.value === '}') {
        // Found closing brace - but case expressions must have at least one branch
        if (branches.length === 0) {
          return { success: false, error: 'Case expression must have at least one branch', state };
        }

        // Parse closing brace properly
        const rbraceResult = rightBrace(bodyState);
        if (rbraceResult.success) {
          return {
            success: true,
            value: AST.caseExpression(
              caseResult.value,
              lpResult.value,
              exprResult.value,
              rpResult.value,
              style,
              branches,
              commas,
              { start: startPos, end: rbraceResult.state.position },
              lbraceResult.value,
              undefined,
              rbraceResult.value
            ),
            state: rbraceResult.state
          };
        }
      }

      // Parse a branch
      const branchResult = parseCaseBranch(bodyState, style);
      if (!branchResult.success) break;

      branches.push(branchResult.value);
      bodyState = branchResult.state;

      // Check for comma
      const commaResult = comma(bodyState);
      if (commaResult.success) {
        commas.push(commaResult.value);
        bodyState = commaResult.state;
      } else {
        // No comma, skip trivia and check for closing brace
        const triviaResult2 = PC.optional(PC.regex(/^[\s\n\r]*/m))(bodyState);
        if (triviaResult2.success) {
          bodyState = triviaResult2.state;
        }
        const rbraceResult = rightBrace(bodyState);
        if (rbraceResult.success) {
          return {
            success: true,
            value: AST.caseExpression(
              caseResult.value,
              lpResult.value,
              exprResult.value,
              rpResult.value,
              style,
              branches,
              commas,
              { start: startPos, end: rbraceResult.state.position },
              lbraceResult.value,
              undefined,
              rbraceResult.value
            ),
            state: rbraceResult.state
          };
        }
        break;
      }
    }

    // If we're here, we failed to parse branches in brace style
    return { success: false, error: 'Failed to parse case branches', state };
  }

  // Try colon/indentation style
  const colonResult = colon(bodyState);
  if (colonResult.success) {
    style = 'indentation';
    bodyState = colonResult.state;

    // Skip to next line and get base indentation
    while (bodyState.position < bodyState.input.length &&
           bodyState.input[bodyState.position] !== '\n') {
      bodyState = { ...bodyState, position: bodyState.position + 1 };
    }
    if (bodyState.position < bodyState.input.length) {
      bodyState = { ...bodyState, position: bodyState.position + 1 }; // Skip newline
    }

    let baseIndent: number | undefined = undefined;

    // Parse indented branches
    while (bodyState.position < bodyState.input.length) {
      const lineStart = bodyState.position;

      // Count indentation
      let indentLevel = 0;
      while (bodyState.position < bodyState.input.length) {
        const char = bodyState.input[bodyState.position];
        if (char === ' ') {
          indentLevel++;
          bodyState = { ...bodyState, position: bodyState.position + 1 };
        } else if (char === '\t') {
          indentLevel += 4;
          bodyState = { ...bodyState, position: bodyState.position + 1 };
        } else {
          break;
        }
      }

      // Check for empty line
      if (bodyState.position >= bodyState.input.length ||
          bodyState.input[bodyState.position] === '\n' ||
          bodyState.input[bodyState.position] === '\r') {
        if (bodyState.position < bodyState.input.length) {
          bodyState = { ...bodyState, position: bodyState.position + 1 };
        }
        continue;
      }

      // Check indentation level
      if (baseIndent === undefined) {
        if (indentLevel === 0) {
          // No indentation means we're done
          bodyState = { ...bodyState, position: lineStart };
          break;
        }
        baseIndent = indentLevel;
      } else if (indentLevel < baseIndent) {
        // Dedented - we're done
        bodyState = { ...bodyState, position: lineStart };
        break;
      }

      // Parse branch starting from line start to include indentation as trivia
      const branchStateWithIndent = { ...bodyState, position: lineStart };
      const branchResult = parseCaseBranch(branchStateWithIndent, style);

      if (!branchResult.success) {
        break;
      }

      branches.push(branchResult.value);
      bodyState = branchResult.state;

      // Move to next line
      while (bodyState.position < bodyState.input.length &&
             bodyState.input[bodyState.position] !== '\n' &&
             bodyState.input[bodyState.position] !== '\r') {
        bodyState = { ...bodyState, position: bodyState.position + 1 };
      }
      if (bodyState.position < bodyState.input.length) {
        bodyState = { ...bodyState, position: bodyState.position + 1 };
      }
    }

    if (branches.length === 0) {
      return { success: false, error: 'Expected case branches after :', state };
    }

    return {
      success: true,
      value: AST.caseExpression(
        caseResult.value,
        lpResult.value,
        exprResult.value,
        rpResult.value,
        style,
        branches,
        commas,
        { start: startPos, end: bodyState.position },
        undefined,
        colonResult.value,
        undefined
      ),
      state: bodyState
    };
  }

  return { success: false, error: 'Expected { or : after case header', state };
};