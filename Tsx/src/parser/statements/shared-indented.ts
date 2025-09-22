/**
 * Shared indented body parsing for block:, if:, then:, else:, for:, loop:, etc.
 * All these constructs share the same indented statement list parsing logic.
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';

/**
 * Parse indented statements after a colon.
 * Handles both same-line syntax (e.g., "if x then: result") and
 * multi-line indented syntax with proper empty line support.
 */
export const parseIndentedStatements = (
  state: PC.ParserState,
  getExpr: () => PC.Parser<AST.Expr>
): PC.ParserResult<AST.Expr[]> => {
  let currentState = state;

  // Check if we're already at an indented statement (colon parser consumed all trivia)
  const currentChar = currentState.position < currentState.input.length ? currentState.input[currentState.position] : '';
  const alreadyAtStatement = currentChar && !/[\s]/.test(currentChar);  // Not whitespace

  // If we're not already at a statement, check for same-line syntax
  if (!alreadyAtStatement) {
    // Skip only spaces/tabs after colon (not newlines)
    let spacesAfterColon = 0;
    let tempPos = currentState.position;
    while (tempPos < currentState.input.length &&
           (currentState.input[tempPos] === ' ' || currentState.input[tempPos] === '\t')) {
      spacesAfterColon++;
      tempPos++;
    }

    // If there's non-whitespace content on the same line, try to parse it
    if (tempPos < currentState.input.length &&
        currentState.input[tempPos] !== '\n' &&
        currentState.input[tempPos] !== '\r') {
      // Advance past the spaces
      currentState = { ...currentState, position: tempPos };
      const exprResult = getExpr()(currentState);
      if (exprResult.success) {
        return {
          success: true,
          value: [exprResult.value],
          state: exprResult.state
        };
      }
      // Reset if same-line parse failed
      currentState = state;
    }
  }

  // Multi-line indented syntax
  const statements: AST.Expr[] = [];
  let baseIndent: number | undefined = undefined;


  if (alreadyAtStatement) {
    // We're already at a statement. The colon parser consumed trivia including indentation.
    // We need to figure out the base indentation by looking backwards.
    let lookBack = currentState.position - 1;
    let indentCount = 0;

    // Count spaces/tabs immediately before current position
    while (lookBack >= 0) {
      const ch = currentState.input[lookBack];
      if (ch === ' ') {
        indentCount++;
        lookBack--;
      } else if (ch === '\t') {
        indentCount += 4;
        lookBack--;
      } else if (ch === '\n' || ch === '\r') {
        // Found the newline, we have our indentation count
        break;
      } else {
        // Found non-whitespace before newline, reset count
        indentCount = 0;
        lookBack--;
      }
    }

    // Set base indent if we found any
    baseIndent = indentCount > 0 ? indentCount : undefined;
  } else {
    // Normal case - skip to start of next line
    while (currentState.position < currentState.input.length &&
           currentState.input[currentState.position] !== '\n') {
      currentState = { ...currentState, position: currentState.position + 1 };
    }
    if (currentState.position < currentState.input.length) {
      currentState = { ...currentState, position: currentState.position + 1 }; // Skip newline
    }
  }

  // Process lines until we dedent or reach end
  while (currentState.position < currentState.input.length) {
    // Find the start and end of the current line
    const lineStart = currentState.position;

    // Count indentation and capture it
    let indentLevel = 0;
    let indentString = '';
    const indentStart = currentState.position;

    // Special case: if we started with alreadyAtStatement and this is the first iteration,
    // we need to use the pre-calculated base indent as the current indent
    if (alreadyAtStatement && statements.length === 0 && baseIndent !== undefined) {
      indentLevel = baseIndent;
      // Don't advance position - we're already at the statement
    } else {
      // Normal indentation counting
      while (currentState.position < currentState.input.length) {
        const char = currentState.input[currentState.position];
        if (char === ' ') {
          indentLevel++;
          indentString += char;
          currentState = { ...currentState, position: currentState.position + 1 };
        } else if (char === '\t') {
          indentLevel += 4;
          indentString += char;
          currentState = { ...currentState, position: currentState.position + 1 };
        } else {
          break;
        }
      }
    }

    // Check what comes after indentation
    if (currentState.position >= currentState.input.length) {
      // End of input
      break;
    }

    const nextChar = currentState.input[currentState.position];
    if (nextChar === '\n' || nextChar === '\r') {
      // Empty line - skip it
      currentState = { ...currentState, position: currentState.position + 1 };
      continue;
    }

    // Non-empty line - check indentation level
    if (baseIndent === undefined) {
      // First non-empty line sets the base indentation
      if (indentLevel === 0) {
        // No indentation means we're done
        currentState = { ...currentState, position: lineStart };
        break;
      }
      baseIndent = indentLevel;
    } else if (indentLevel < baseIndent) {
      // Dedented - we're done with this block
      currentState = { ...currentState, position: lineStart };
      break;
    }

    // Find the end of this line
    let lineEnd = currentState.position;
    while (lineEnd < currentState.input.length &&
           currentState.input[lineEnd] !== '\n' &&
           currentState.input[lineEnd] !== '\r') {
      lineEnd++;
    }

    // Parse expression on this line with limited scope
    // Reset position to include indentation as leading trivia
    const exprStateWithIndent = { ...currentState, position: indentStart };
    const exprResult = getExpr()(exprStateWithIndent);

    if (!exprResult.success) {
      // Can't parse expression - we're done
      break;
    }


    // Check if this expression starts with a brace (indicating a multiline block)
    // Look for the first non-whitespace character after indentation
    let firstNonWhitespace = currentState.position;
    while (firstNonWhitespace < currentState.input.length &&
           /[ \t]/.test(currentState.input[firstNonWhitespace])) {
      firstNonWhitespace++;
    }

    const startsWithBrace = firstNonWhitespace < currentState.input.length &&
                           currentState.input[firstNonWhitespace] === '{';

    // Check if this is an if expression that might have multi-line structure
    const isIfExpression = exprResult.value.type === 'IfExpression';

    // Make sure the expression didn't consume beyond the current line
    // UNLESS it's a braced block OR an if expression (which can have else clauses on subsequent lines)
    if (!startsWithBrace && !isIfExpression && exprResult.state.position > lineEnd) {
      // Limit the expression's extent to the current line
      exprResult.state = { ...exprResult.state, position: lineEnd };
    }

    statements.push(exprResult.value);

    // Move to the next position after the expression
    // For braced blocks and if expressions, use the expression's end position
    // For single-line expressions, move to the end of the current line
    if (startsWithBrace || isIfExpression) {
      // The expression consumed multiple lines, continue from where it ended
      currentState = exprResult.state;
    } else {
      // Move to the start of the next line
      currentState = { ...currentState, position: lineEnd };
      if (currentState.position < currentState.input.length) {
        const ch = currentState.input[currentState.position];
        if (ch === '\n') {
          currentState = { ...currentState, position: currentState.position + 1 };
        } else if (ch === '\r') {
          currentState = { ...currentState, position: currentState.position + 1 };
          if (currentState.position < currentState.input.length &&
              currentState.input[currentState.position] === '\n') {
            currentState = { ...currentState, position: currentState.position + 1 };
          }
        }
      }
    }

  }


  return {
    success: true,
    value: statements,
    state: currentState
  };
};

/**
 * Convert an array of expressions to an appropriate AST body node.
 * - Empty array -> EmptyExpression
 * - Single expression -> Return as-is
 * - Multiple expressions -> Block with statements
 */
export const statementsToBody = (
  statements: AST.Expr[],
  startPos: number,
  endPos: number
): AST.Expr => {
  if (statements.length === 0) {
    return AST.emptyExpression({ start: startPos, end: endPos });
  } else if (statements.length === 1) {
    return statements[0];
  } else {
    // Convert expressions to statements for block
    const blockStatements = statements.map(expr =>
      AST.statement(expr, undefined, expr.span)
    );
    return AST.block(
      'indentation',
      blockStatements,
      undefined, // no block keyword
      undefined, // no left brace
      undefined, // no right brace
      undefined, // no colon (parent owns it)
      { start: startPos, end: endPos }
    );
  }
};