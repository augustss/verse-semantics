/**
 * For loop expression parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { leftParen, rightParen, leftBrace, rightBrace, colon, rightArrowOp, semicolon } from '../operators/punctuation';
import { withTriviaLiteral } from '../foundation/tokens';
import { trivia } from '../foundation/trivia';
import { variable as parseVariable } from '../literals/identifiers';
import { parseIndentedStatements, statementsToBody } from './shared-indented';
import { parseStatement } from './block';

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

// For loop expressions
export const forExpression: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'for' keyword with boundary check
  const forCheck = PC.string('for')(state);
  if (!forCheck.success) return { success: false, error: 'Not a for expression', state };

  // Check it's not part of a longer identifier
  const nextPos = forCheck.state.position;
  if (nextPos < forCheck.state.input.length && /[a-zA-Z0-9_]/.test(forCheck.state.input[nextPos])) {
    return { success: false, error: 'for is part of identifier', state };
  }

  // Parse 'for' with trivia
  const forResult = withTriviaLiteral('for', PC.string('for'))(state);
  if (!forResult.success) return forResult;

  // Expect left parenthesis
  const lpResult = leftParen(forResult.state);
  if (!lpResult.success) return { success: false, error: 'Expected ( after for', state };

  // Parse iterator - could be "var" or "index -> var"
  let currentState = lpResult.state;
  let index: AST.Token<string> | undefined;
  let arrow: AST.Token<'->'> | undefined;
  let variable: AST.Token<string>;

  // First try to parse potential index variable
  const firstVarResult = parseVariable(currentState);
  if (!firstVarResult.success) return { success: false, error: 'Expected iterator variable', state };

  // Check if this is followed by an arrow
  const arrowResult = rightArrowOp(firstVarResult.state);
  if (arrowResult.success) {
    // This is the "index -> var" pattern
    index = firstVarResult.value.token;
    arrow = arrowResult.value;

    // Parse the actual iterator variable after the arrow
    const secondVarResult = parseVariable(arrowResult.state);
    if (!secondVarResult.success) return { success: false, error: 'Expected variable after ->', state };

    variable = secondVarResult.value.token;
    currentState = secondVarResult.state;
  } else {
    // This is just the simple "var" pattern
    variable = firstVarResult.value.token;
    currentState = firstVarResult.state;
  }

  // Create ForIterator
  const iterator = AST.forIterator(
    variable,
    { start: firstVarResult.value.span.start, end: currentState.position },
    index,
    arrow
  );

  // Expect colon
  const colonResult = colon(currentState);
  if (!colonResult.success) return { success: false, error: 'Expected : after iterator', state };

  // Parse iterable expression (range, array, etc.)
  const iterableResult = getExpr()(colonResult.state);
  if (!iterableResult.success) return { success: false, error: 'Expected iterable expression', state };

  // Expect right parenthesis
  const rpResult = rightParen(iterableResult.state);
  if (!rpResult.success) return { success: false, error: 'Expected ) after iterable', state };

  let bodyState = rpResult.state;

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
        value: AST.forExpression(
          forResult.value,
          lpResult.value,
          iterator,
          colonResult.value,
          iterableResult.value,
          rpResult.value,
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

    // Non-empty block - parse as statements with semicolons
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
        // Convert statements to appropriate body expression
        const blockBody = statements.length === 0
          ? AST.emptyExpression({ start: lbraceResult.state.position, end: lbraceResult.state.position })
          : statements.length === 1
            ? statements[0].expr || AST.emptyExpression({ start: lbraceResult.state.position, end: lbraceResult.state.position })
            : AST.block(
                'braces',
                statements,
                undefined,
                undefined, // No left brace in body (parent owns it)
                undefined, // No right brace in body (parent owns it)
                undefined,
                { start: lbraceResult.state.position, end: rbraceResult.state.position }
              );

        return {
          success: true,
          value: AST.forExpression(
            forResult.value,
            lpResult.value,
            iterator,
            colonResult.value,
            iterableResult.value,
            rpResult.value,
            'braces',
            blockBody,
            { start: startPos, end: rbraceResult.state.position },
            lbraceResult.value,
            undefined,
            rbraceResult.value
          ),
          state: rbraceResult.state
        };
      }

      // Neither statement nor closing brace found - error
      return { success: false, error: 'Expected statement or } in for loop body', state };
    }
  }

  // Try colon/indentation style
  const bodyColonResult = colon(bodyState);
  if (bodyColonResult.success) {
    // Use shared indented statement parser - let it handle the trivia after colon
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
      value: AST.forExpression(
        forResult.value,
        lpResult.value,
        iterator,
        colonResult.value,
        iterableResult.value,
        rpResult.value,
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

  return { success: false, error: 'Expected { or : after for header', state };
};