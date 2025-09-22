/**
 * Case expression parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { leftParen, rightParen, leftBrace, rightBrace, colon, comma, arrowOp } from '../operators/punctuation';
import { withTriviaLiteral } from '../foundation/tokens';

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

// Parse a single case branch
const parseCaseBranch: PC.Parser<AST.CaseBranch> = (state) => {
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
      // Fall back to full expression parsing for more complex patterns
      if (!getExpr) {
        return { success: false, error: 'Expression parser not initialized', state };
      }

      // Use a limited expression parser that stops at arrows
      // We'll use peek to check if there's an arrow nearby
      const peekAhead = state.input.slice(state.position, state.position + 50);
      const arrowIndex = peekAhead.indexOf('=>');

      if (arrowIndex === -1) {
        return { success: false, error: 'Expected => in case branch', state };
      }

      // Parse only up to the arrow
      const beforeArrowText = peekAhead.slice(0, arrowIndex).trim();
      const patternResult = getExpr()({ input: beforeArrowText, position: 0 });

      if (!patternResult.success) {
        return { success: false, error: 'Expected pattern in case branch', state };
      }

      pattern = patternResult.value;
      afterPattern = { ...state, position: state.position + beforeArrowText.length };

      // Skip any whitespace before the arrow
      while (afterPattern.position < state.input.length &&
             /\s/.test(state.input[afterPattern.position])) {
        afterPattern = { ...afterPattern, position: afterPattern.position + 1 };
      }
    }
  }

  // Expect arrow =>
  const arrowResult = arrowOp(afterPattern);
  if (!arrowResult.success) return { success: false, error: 'Expected => after case pattern', state };

  // Parse body (optional for default case)
  let body: AST.Expr | undefined;
  let endState = arrowResult.state;

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
      const branchResult = parseCaseBranch(bodyState);
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

    // Use simpler parsing approach - just parse branches sequentially
    // Skip whitespace and newlines to get to first branch
    while (bodyState.position < bodyState.input.length) {
      const char = bodyState.input[bodyState.position];
      if (char === ' ' || char === '\t' || char === '\n' || char === '\r') {
        bodyState = { ...bodyState, position: bodyState.position + 1 };
      } else {
        break;
      }
    }

    // Parse first branch (required)
    const firstBranchResult = parseCaseBranch(bodyState);
    if (!firstBranchResult.success) {
      return { success: false, error: 'Expected case branch after :', state };
    }

    branches.push(firstBranchResult.value);
    bodyState = firstBranchResult.state;

    // Parse additional branches
    while (true) {
      // Skip whitespace and newlines
      while (bodyState.position < bodyState.input.length) {
        const char = bodyState.input[bodyState.position];
        if (char === ' ' || char === '\t' || char === '\n' || char === '\r') {
          bodyState = { ...bodyState, position: bodyState.position + 1 };
        } else {
          break;
        }
      }

      // Check if we're at the end of input
      if (bodyState.position >= bodyState.input.length) {
        break;
      }

      // Try to parse another branch
      const nextBranchResult = parseCaseBranch(bodyState);
      if (!nextBranchResult.success) {
        break;
      }

      branches.push(nextBranchResult.value);
      bodyState = nextBranchResult.state;
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