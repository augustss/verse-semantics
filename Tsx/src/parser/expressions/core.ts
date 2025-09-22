/**
 * Core expression parsing logic using modular components
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';

// Import modular components
import { integer, floatLiteral } from '../literals/numbers';
import { booleanLiteral } from '../literals/booleans';
import { stringLiteral } from '../literals/strings';
import { variable, memberName } from '../literals/identifiers';
import { withTriviaLiteral } from '../foundation/tokens';
import { addOp, mulOp, unaryMinusOp } from '../operators/arithmetic';
import { compareOp } from '../operators/comparison';
import { logicalOp, notOp } from '../operators/logical';
import { leftParen, rightParen, leftBrace, rightBrace, leftBracket, rightBracket, comma, assignOp, arrowOp, dot, rangeOp, colon } from '../operators/punctuation';
import { trivia } from '../foundation/trivia';
import { ifExpression, setExprParser as setIfExprParser } from '../statements/if';
import { blockExpression, setExprParser as setBlockExprParser } from '../statements/block';
import { forExpression, setExprParser as setForExprParser } from '../statements/for';
import { breakStatement, continueStatement } from '../statements/control';
import { setStatement, setExprParser } from '../statements/set';
import { loopStatement, setExprParser as setLoopExprParser } from '../statements/loop';
import { varDeclaration } from '../statements/var';
import { caseExpression, setExprParser as setCaseExprParser } from '../statements/case';
import { classExpression, setExprParser as setClassExprParser } from '../statements/class';

// Forward declarations for recursive grammar
let expr: PC.Parser<AST.Expr>;
let assignmentExpr: PC.Parser<AST.Expr>;
let primaryExpr: PC.Parser<AST.Expr>;
let callExpr: PC.Parser<AST.Expr>;
let unaryExpr: PC.Parser<AST.Expr>;
let term: PC.Parser<AST.Expr>;
let additiveExpr: PC.Parser<AST.Expr>;
let comparisonExpr: PC.Parser<AST.Expr>;
let logicalExpr: PC.Parser<AST.Expr>;

// Primary expressions (literals, variables, parenthesized expressions, arrays, if/then/else, blocks, for)
primaryExpr = PC.choice(
  // Variable declarations (must come before other keywords)
  varDeclaration as PC.Parser<AST.Expr>,

  // Control flow statements (must come before other keywords)
  breakStatement as PC.Parser<AST.Expr>,
  continueStatement as PC.Parser<AST.Expr>,
  setStatement as PC.Parser<AST.Expr>,
  loopStatement as PC.Parser<AST.Expr>,

  // Case expressions (must come before other keywords)
  caseExpression as PC.Parser<AST.Expr>,

  // Class expressions (must come before other keywords)
  classExpression as PC.Parser<AST.Expr>,

  // For expressions (must come before other keywords)
  forExpression as PC.Parser<AST.Expr>,

  // Block expressions (must come before other keywords)
  blockExpression as PC.Parser<AST.Expr>,

  // If expressions (must come before other keywords)
  ifExpression as PC.Parser<AST.Expr>,

  // Literals
  floatLiteral as PC.Parser<AST.Expr>,
  integer as PC.Parser<AST.Expr>,
  booleanLiteral as PC.Parser<AST.Expr>,
  stringLiteral as PC.Parser<AST.Expr>,

  // Array literals (must come before variable to match 'array{' first)
  (state) => {
    const startPos = state.position;

    // Try 'array{...}' syntax
    const arrayCheck = PC.string('array')(state);
    if (arrayCheck.success) {
      const lbraceResult = leftBrace(arrayCheck.state);
      if (lbraceResult.success) {
        const elements: AST.Expr[] = [];
        const commas: AST.Token<','>[] = [];
        let elemState = lbraceResult.state;

        // Check for empty array
        const rbraceResult = rightBrace(elemState);
        if (rbraceResult.success) {
          return {
            success: true,
            value: AST.arrayConstruction(
              AST.token('array', 'array', { leading: '', trailing: '' }, { start: startPos, end: arrayCheck.state.position }),
              'braces',
              elements,
              commas,
              { start: startPos, end: rbraceResult.state.position },
              lbraceResult.value,
              undefined,
              rbraceResult.value
            ),
            state: rbraceResult.state
          };
        }

        // Parse array elements
        while (true) {
          const elemResult = expr(elemState);
          if (!elemResult.success) break;

          elements.push(elemResult.value);
          elemState = elemResult.state;

          // Check for comma
          const commaResult = comma(elemState);
          if (commaResult.success) {
            commas.push(commaResult.value);
            elemState = commaResult.state;
          } else {
            break;
          }
        }

        // Expect closing brace
        const rbraceResult2 = rightBrace(elemState);
        if (rbraceResult2.success) {
          return {
            success: true,
            value: AST.arrayConstruction(
              AST.token('array', 'array', { leading: '', trailing: '' }, { start: startPos, end: arrayCheck.state.position }),
              'braces',
              elements,
              commas,
              { start: startPos, end: rbraceResult2.state.position },
              lbraceResult.value,
              undefined,
              rbraceResult2.value
            ),
            state: rbraceResult2.state
          };
        }
      }
    }


    return { success: false, error: 'Not an array literal', state };
  },

  // Bare brace array literals (shorthand for array{...})
  (state) => {
    const startPos = state.position;
    const lbraceResult = leftBrace(state);
    if (!lbraceResult.success) return { success: false, error: 'Not a bare brace array', state };

    const elements: AST.Expr[] = [];
    const commas: AST.Token<','>[] = [];
    let elemState = lbraceResult.state;

    // Check for empty array
    const rbraceResult = rightBrace(elemState);
    if (rbraceResult.success) {
      return {
        success: true,
        value: AST.arrayConstruction(
          undefined, // No array keyword for bare brace syntax
          'braces',
          elements,
          commas,
          { start: startPos, end: rbraceResult.state.position },
          lbraceResult.value,
          undefined,
          rbraceResult.value
        ),
        state: rbraceResult.state
      };
    }

    // Parse array elements
    while (true) {
      const elemResult = expr(elemState);
      if (!elemResult.success) break;

      elements.push(elemResult.value);
      elemState = elemResult.state;

      // Check for comma
      const commaResult = comma(elemState);
      if (commaResult.success) {
        commas.push(commaResult.value);
        elemState = commaResult.state;
      } else {
        break;
      }
    }

    // Expect closing brace
    const rbraceResult2 = rightBrace(elemState);
    if (rbraceResult2.success) {
      return {
        success: true,
        value: AST.arrayConstruction(
          undefined, // No array keyword for bare brace syntax
          'braces',
          elements,
          commas,
          { start: startPos, end: rbraceResult2.state.position },
          lbraceResult.value,
          undefined,
          rbraceResult2.value
        ),
        state: rbraceResult2.state
      };
    }

    return { success: false, error: 'Expected } after array elements', state };
  },

  variable as PC.Parser<AST.Expr>,

  // Parenthesized expressions and unit value
  (state) => {
    const startPos = state.position;
    const lpResult = leftParen(state);
    if (!lpResult.success) return lpResult;

    // Check for empty parentheses (unit value)
    const rpResult = rightParen(lpResult.state);
    if (rpResult.success) {
      // This is the unit value ()
      return {
        success: true,
        value: AST.emptyExpression(
          { start: startPos, end: rpResult.state.position }
        ),
        state: rpResult.state
      };
    }

    const exprResult = expr(lpResult.state);
    if (!exprResult.success) return exprResult;

    const rpResult2 = rightParen(exprResult.state);
    if (!rpResult2.success) return rpResult2;

    return {
      success: true,
      value: AST.parenthesized(
        lpResult.value,
        exprResult.value,
        rpResult2.value,
        { start: startPos, end: rpResult2.state.position }
      ),
      state: rpResult2.state
    };
  }
);

// Call expressions (function calls, object construction, member access, etc.)
callExpr = (state) => {
  const startPos = state.position;
  let currentExpr = primaryExpr(state);
  if (!currentExpr.success) return currentExpr;

  let currentState = currentExpr.state;
  let left = currentExpr.value;

  while (true) {
    // Try object construction first (Point{x:=1, y:=2})
    const lbraceResult = leftBrace(currentState);
    if (lbraceResult.success) {
      // Only proceed with object construction if left is a variable or application (type name or generic type)
      if (left.type !== 'Variable' && left.type !== 'Application') {
        // Not a variable or application, so this is not object construction - try other patterns
        break;
      }

      // Parse field assignments
      const fields: AST.FieldAssignment[] = [];
      let fieldState = lbraceResult.state;

      // Check for empty object
      const rbraceResult = rightBrace(fieldState);
      if (rbraceResult.success) {
        left = AST.objectConstruction(
          left as AST.Variable | AST.Application,
          'braces',
          fields,
          lbraceResult.value,
          rbraceResult.value,
          undefined,
          { start: startPos, end: rbraceResult.state.position }
        );
        currentState = rbraceResult.state;
        continue;
      }

      // Parse field assignments
      let hasValidFields = false;
      while (true) {
        // Parse field name
        const nameResult = variable(fieldState);
        if (!nameResult.success) {
          // If we haven't parsed any valid fields and can't parse a field name,
          // this might be malformed object construction
          if (!hasValidFields) {
            return { success: false, error: 'Invalid object construction: expected field name', state };
          }
          break;
        }

        // Check for field declaration syntax (name:type = value) or assignment syntax (name := value)
        let colonToken: AST.Token<':'> | undefined;
        let typeToken: AST.Token<string> | undefined;
        let assignToken: AST.Token<':=' | '='> | undefined;
        let nextState = nameResult.state;
        let fieldParseSucceeded = false;

        // Try to parse colon for field declaration
        const colonResult = colon(nextState);
        if (colonResult.success) {
          colonToken = colonResult.value;
          nextState = colonResult.state;

          // Parse type annotation
          const typeResult = variable(nextState);
          if (typeResult.success) {
            typeToken = typeResult.value.token;
            nextState = typeResult.state;

            // Expect = operator for field declaration
            const assignResult = assignOp(nextState);
            if (assignResult.success && assignResult.value.value === '=') {
              assignToken = assignResult.value;
              nextState = assignResult.state;
              fieldParseSucceeded = true;
            }
          }
        }

        // If field declaration parsing failed, try assignment syntax (name := value)
        if (!fieldParseSucceeded) {
          nextState = nameResult.state; // Reset to after field name
          const assignResult = assignOp(nextState);
          if (assignResult.success) {
            assignToken = assignResult.value;
            nextState = assignResult.state;
            fieldParseSucceeded = true;
            // Clear field declaration tokens since we're using assignment syntax
            colonToken = undefined;
            typeToken = undefined;
          }
        }

        // If neither field declaration nor assignment syntax worked, this isn't valid object construction
        if (!fieldParseSucceeded || !assignToken) {
          // If we haven't parsed any valid fields and can't parse this field, give up on object construction
          if (!hasValidFields) {
            break;
          }
          // Otherwise, we have some valid fields already, so just stop parsing more fields
          break;
        }

        // Parse field value
        const valueResult = expr(nextState);
        if (!valueResult.success) {
          // If we can't parse the field value, stop parsing fields
          break;
        }

        // Track commas for field assignments (will be undefined for last field)
        fields.push(AST.fieldAssignment(
          nameResult.value.token,
          assignToken,
          valueResult.value,
          undefined, // Comma will be set if we find one
          { start: nameResult.value.span.start, end: valueResult.state.position },
          colonToken,
          typeToken
        ));
        fieldState = valueResult.state;
        hasValidFields = true;

        // Check for comma or newline-separated continuation
        const commaResult = comma(fieldState);
        if (commaResult.success) {
          // Update the last field with the comma
          if (fields.length > 0) {
            const lastField = fields[fields.length - 1];
            fields[fields.length - 1] = AST.fieldAssignment(
              lastField.name,
              lastField.assignOp,
              lastField.value,
              commaResult.value,
              lastField.span,
              lastField.colon,
              lastField.typeAnnotation
            );
          }
          fieldState = commaResult.state;
        } else {
          // No comma found, check if there's a newline followed by another field
          // Skip trivia (including newlines) and see if we can parse another field
          const triviaResult = trivia(fieldState);
          let nextState = triviaResult.success ? triviaResult.state : fieldState;

          // Try to parse another field name to see if we should continue
          const nextNameResult = variable(nextState);
          if (nextNameResult.success) {
            // There's another field after trivia, continue parsing
            fieldState = nextState;
          } else {
            // No more fields, break
            break;
          }
        }
      }

      // Expect closing brace
      const rbraceResult2 = rightBrace(fieldState);
      if (rbraceResult2.success) {
        left = AST.objectConstruction(
          left as AST.Variable | AST.Application,
          'braces',
          fields,
          lbraceResult.value,
          rbraceResult2.value,
          undefined,
          { start: startPos, end: rbraceResult2.state.position }
        );
        currentState = rbraceResult2.state;
        continue;
      } else {
        // We have a variable followed by { but can't find closing }
        return { success: false, error: 'Invalid object construction: expected closing }', state };
      }
    }

    // Try function call
    const lpResult = leftParen(currentState);
    if (lpResult.success) {
      // Parse arguments
      const args: AST.Expr[] = [];
      const commas: AST.Token<','>[] = [];
      let argState = lpResult.state;

      // Check for empty parameter list
      const rpResult = rightParen(argState);
      if (rpResult.success) {
        left = AST.application(
          left,
          lpResult.value,
          args,
          commas,
          rpResult.value,
          { start: startPos, end: rpResult.state.position }
        );
        currentState = rpResult.state;
        continue;
      }

      // Parse arguments
      while (true) {
        const argResult = expr(argState);
        if (!argResult.success) break;

        args.push(argResult.value);
        argState = argResult.state;

        // Check for comma
        const commaResult = comma(argState);
        if (commaResult.success) {
          // After a comma, we must have another argument (no trailing commas)
          // So peek ahead to see if there's a valid argument
          const nextArgResult = expr(commaResult.state);
          if (!nextArgResult.success) {
            // No argument after comma - this is a trailing comma error
            break;
          }
          commas.push(commaResult.value);
          argState = commaResult.state;
        } else {
          break;
        }
      }

      // Expect closing paren
      const rpResult2 = rightParen(argState);
      if (rpResult2.success) {
        left = AST.application(
          left,
          lpResult.value,
          args,
          commas,
          rpResult2.value,
          { start: startPos, end: rpResult2.state.position }
        );
        currentState = rpResult2.state;
        continue;
      }
    }

    // Try member access
    const dotResult = dot(currentState);
    if (dotResult.success) {
      // Parse the member name (allow keywords like continue, class, etc.)
      const memberNameResult = memberName(dotResult.state);
      if (memberNameResult.success) {
        // Parse trailing trivia after the member name
        const trailingTriviaResult = trivia(memberNameResult.state);
        const trailingTrivia = trailingTriviaResult.success ? trailingTriviaResult.value : '';
        const finalState = trailingTriviaResult.success ? trailingTriviaResult.state : memberNameResult.state;

        // Create a token with proper trivia for the member name
        const memberToken = AST.token(
          memberNameResult.value,
          memberNameResult.value,
          { leading: '', trailing: trailingTrivia },
          { start: dotResult.state.position, end: finalState.position }
        );

        left = AST.memberAccess(
          left,
          dotResult.value,
          memberToken,
          { start: startPos, end: finalState.position }
        );
        currentState = finalState;
        continue;
      }
    }

    // Try array/map indexing
    const lbracketResult = leftBracket(currentState);
    if (lbracketResult.success) {
      // Parse index expression (or multiple expressions for multi-dimensional arrays)
      const indices: AST.Expr[] = [];
      const commas: AST.Token<','>[] = [];
      let indexState = lbracketResult.state;

      // Check for empty brackets first
      const rbracketResult = rightBracket(indexState);
      if (rbracketResult.success) {
        // Empty square brackets: f[]
        if (left.type === 'Variable') {
          // Convert Variable to FunctionCall with square brackets
          left = AST.functionCall(
            left.token,
            lbracketResult.value,
            indices,
            commas,
            rbracketResult.value,
            { start: startPos, end: rbracketResult.state.position }
          );
        } else {
          // Convert to Application with square brackets
          left = AST.application(
            left,
            lbracketResult.value,
            indices,
            commas,
            rbracketResult.value,
            { start: startPos, end: rbracketResult.state.position }
          );
        }
        currentState = rbracketResult.state;
        continue;
      }

      // Parse arguments for non-empty brackets
      while (true) {
        const indexResult = expr(indexState);
        if (!indexResult.success) break;

        indices.push(indexResult.value);
        indexState = indexResult.state;

        // Check for comma (for multi-dimensional arrays)
        const commaResult = comma(indexState);
        if (commaResult.success) {
          // After a comma, we must have another argument (no trailing commas)
          // So peek ahead to see if there's a valid argument
          const nextIndexResult = expr(commaResult.state);
          if (!nextIndexResult.success) {
            // No argument after comma - this is a trailing comma error
            break;
          }
          commas.push(commaResult.value);
          indexState = commaResult.state;
        } else {
          break;
        }
      }

      // Expect closing bracket
      const rbracketResult2 = rightBracket(indexState);
      if (rbracketResult2.success && indices.length > 0) {
        if (indices.length === 1 && commas.length === 0) {
          // Single index without comma: regular index access like arr[0]
          left = AST.indexAccess(
            left,
            lbracketResult.value,
            indices[0],
            rbracketResult2.value,
            { start: startPos, end: rbracketResult2.state.position }
          );
        } else {
          // Multiple indices or trailing comma: function call with square brackets like f[x, y]
          if (left.type === 'Variable') {
            // Convert Variable to FunctionCall with square brackets
            left = AST.functionCall(
              left.token,
              lbracketResult.value,
              indices,
              commas,
              rbracketResult2.value,
              { start: startPos, end: rbracketResult2.state.position }
            );
          } else {
            // Convert to Application with square brackets
            left = AST.application(
              left,
              lbracketResult.value,
              indices,
              commas,
              rbracketResult2.value,
              { start: startPos, end: rbracketResult2.state.position }
            );
          }
        }
        currentState = rbracketResult2.state;
        continue;
      }
    }

    // Try colon-style object construction (Point: x := 1; y := 2)
    const colonResult = colon(currentState);
    if (colonResult.success && left.type === 'Variable') {
      // Parse indented field assignments
      const fields: AST.FieldAssignment[] = [];
      let fieldState = colonResult.state;

      // Parse field assignments on indented lines
      while (true) {
        // Parse field name
        const nameResult = variable(fieldState);
        if (!nameResult.success) break;

        // Expect := operator for field assignment
        const assignResult = assignOp(nameResult.state);
        if (!assignResult.success || assignResult.value.value !== ':=') break;

        // Parse field value
        const valueResult = expr(assignResult.state);
        if (!valueResult.success) break;

        // Create field assignment
        fields.push(AST.fieldAssignment(
          nameResult.value.token,
          assignResult.value as AST.Token<':='>,
          valueResult.value,
          undefined, // No commas in indented style
          { start: nameResult.value.span.start, end: valueResult.state.position },
          undefined, // colon - for simple assignments, there's no colon
          undefined  // typeAnnotation - for simple assignments, there's no type
        ));

        fieldState = valueResult.state;
      }

      // Create object construction if we found fields OR if we're at end of input/line (empty object)
      if (fields.length > 0 || fieldState.position >= fieldState.input.length ||
          fieldState.input[fieldState.position] === '\n' || fieldState.input[fieldState.position] === '\r') {
        left = AST.objectConstruction(
          left as AST.Variable | AST.Application,
          'indentation',
          fields,
          undefined, // No left brace for indented style
          undefined, // No right brace for indented style
          colonResult.value,
          { start: startPos, end: fieldState.position }
        );
        currentState = fieldState;
        continue;
      }
    }

    // No more calls found
    break;
  }

  return { success: true, value: left, state: currentState };
};

// Unary expressions (-, not)
unaryExpr = PC.choice(
  // Unary minus
  (state) => {
    const startPos = state.position;
    const opResult = unaryMinusOp(state);
    if (!opResult.success) return opResult;

    const operandResult = unaryExpr(opResult.state);
    if (!operandResult.success) return operandResult;

    return {
      success: true,
      value: AST.unaryOp(
        opResult.value,
        operandResult.value,
        { start: startPos, end: operandResult.state.position }
      ),
      state: operandResult.state
    };
  },
  // Logical not
  (state) => {
    const startPos = state.position;
    const opResult = notOp(state);
    if (!opResult.success) return opResult;

    const operandResult = unaryExpr(opResult.state);
    if (!operandResult.success) return operandResult;

    return {
      success: true,
      value: AST.unaryOp(
        opResult.value,
        operandResult.value,
        { start: startPos, end: operandResult.state.position }
      ),
      state: operandResult.state
    };
  },
  callExpr
);

// Term expressions (*, /, %)
term = (state) => {
  const startPos = state.position;
  const leftResult = unaryExpr(state);
  if (!leftResult.success) return leftResult;

  let currentState = leftResult.state;
  let left = leftResult.value;

  while (true) {
    const opResult = mulOp(currentState);
    if (!opResult.success) break;

    const rightResult = unaryExpr(opResult.state);
    if (!rightResult.success) break;

    left = AST.binaryOp(
      left,
      opResult.value,
      rightResult.value,
      { start: startPos, end: rightResult.state.position }
    );
    currentState = rightResult.state;
  }

  return { success: true, value: left, state: currentState };
};

// Additive expressions (+, -)
additiveExpr = (state) => {
  const startPos = state.position;
  const leftResult = term(state);
  if (!leftResult.success) return leftResult;

  let currentState = leftResult.state;
  let left = leftResult.value;

  while (true) {
    const opResult = addOp(currentState);
    if (!opResult.success) break;

    const rightResult = term(opResult.state);
    if (!rightResult.success) break;

    left = AST.binaryOp(
      left,
      opResult.value,
      rightResult.value,
      { start: startPos, end: rightResult.state.position }
    );
    currentState = rightResult.state;
  }

  return { success: true, value: left, state: currentState };
};

// Range expressions (..)
const rangeExpr: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;
  const leftResult = additiveExpr(state);
  if (!leftResult.success) return leftResult;

  // Check for range operator
  const rangeResult = rangeOp(leftResult.state);
  if (!rangeResult.success) {
    return leftResult;
  }

  // Parse the right side
  const rightResult = additiveExpr(rangeResult.state);
  if (!rightResult.success) return rightResult;

  return {
    success: true,
    value: AST.rangeExpression(
      leftResult.value,
      rangeResult.value,
      rightResult.value,
      { start: startPos, end: rightResult.state.position }
    ),
    state: rightResult.state
  };
};

// Comparison expressions (>, <, >=, <=, ==, !=)
comparisonExpr = (state) => {
  const startPos = state.position;
  const leftResult = rangeExpr(state);
  if (!leftResult.success) return leftResult;

  let currentState = leftResult.state;
  let left = leftResult.value;

  while (true) {
    const opResult = compareOp(currentState);
    if (!opResult.success) break;

    const rightResult = rangeExpr(opResult.state);
    if (!rightResult.success) break;

    left = AST.binaryOp(
      left,
      opResult.value,
      rightResult.value,
      { start: startPos, end: rightResult.state.position }
    );
    currentState = rightResult.state;
  }

  return { success: true, value: left, state: currentState };
};

// Logical expressions (and, or)
logicalExpr = (state) => {
  const startPos = state.position;
  const leftResult = comparisonExpr(state);
  if (!leftResult.success) return leftResult;

  let currentState = leftResult.state;
  let left = leftResult.value;

  while (true) {
    const opResult = logicalOp(currentState);
    if (!opResult.success) break;

    const rightResult = comparisonExpr(opResult.state);
    if (!rightResult.success) break;

    left = AST.binaryOp(
      left,
      opResult.value,
      rightResult.value,
      { start: startPos, end: rightResult.state.position }
    );
    currentState = rightResult.state;
  }

  return { success: true, value: left, state: currentState };
};

// Parse a single specifier like <public>, <inline>, <suspends>, etc.
const specifierParser = (state: PC.ParserState): PC.ParserResult<AST.Specifier> => {
  const startPos = state.position;

  // Parse leading trivia
  const leadingTriviaResult = trivia(state);
  const afterTrivia = leadingTriviaResult.success ? leadingTriviaResult.state : state;
  const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';

  // Must start with <
  if (afterTrivia.position >= afterTrivia.input.length ||
      afterTrivia.input[afterTrivia.position] !== '<') {
    return { success: false, error: 'Expected < for specifier', state };
  }

  // Find matching >
  let currentPos = afterTrivia.position + 1;
  let depth = 1;

  while (currentPos < afterTrivia.input.length && depth > 0) {
    const ch = afterTrivia.input[currentPos];
    if (ch === '<') depth++;
    else if (ch === '>') depth--;
    if (depth > 0) currentPos++;
  }

  if (depth !== 0 || currentPos >= afterTrivia.input.length) {
    return { success: false, error: 'Unclosed specifier', state };
  }

  // Extract the specifier content (including < and >)
  const specifierText = afterTrivia.input.slice(afterTrivia.position, currentPos + 1);
  const innerContent = specifierText.slice(1, -1); // Remove < and >

  // Parse trailing trivia
  const afterSpec = { ...afterTrivia, position: currentPos + 1 };
  const trailingTriviaResult = trivia(afterSpec);
  const trailingTrivia = trailingTriviaResult.success ? trailingTriviaResult.value : '';
  const finalState = trailingTriviaResult.success ? trailingTriviaResult.state : afterSpec;

  // Create tokens for the AST with proper types
  const leftAngleToken = AST.token('<' as const, '<', { leading: leadingTrivia, trailing: '' }, { start: startPos, end: afterTrivia.position + 1 }) as AST.Token<'<'>;
  const rightAngleToken = AST.token('>' as const, '>', { leading: '', trailing: trailingTrivia }, { start: currentPos, end: finalState.position }) as AST.Token<'>'>;
  const nameToken = AST.token(innerContent, innerContent, { leading: '', trailing: '' }, { start: afterTrivia.position + 1, end: currentPos });

  return {
    success: true,
    value: AST.specifier(
      leftAngleToken,
      nameToken,
      undefined, // leftParen
      undefined, // argument
      undefined, // rightParen
      rightAngleToken,
      { start: startPos, end: finalState.position }
    ),
    state: finalState
  };
};

// Parse function declaration with specifiers: f <spec1> <spec2> () := body
const parseFunctionWithSpecifiers = (state: PC.ParserState): PC.ParserResult<AST.Expr> => {
  const startPos = state.position;

  // Parse function name
  const nameResult = variable(state);
  if (!nameResult.success) return nameResult;

  // Parse specifiers
  const specifiers: AST.Specifier[] = [];
  let currentState = nameResult.state;

  // Parse specifiers like <public>, <inline>, etc.
  while (true) {
    const specResult = specifierParser(currentState);
    if (!specResult.success) break;

    specifiers.push(specResult.value);
    currentState = specResult.state;
  }

  // Parse left parenthesis
  const leftParenResult = leftParen(currentState);
  if (!leftParenResult.success) return { success: false, error: 'Expected ( after function name', state };

  // Parse parameters
  const params: AST.FunctionParam[] = [];
  const commas: AST.Token<','>[] = [];
  currentState = leftParenResult.state;

  // Parse parameter list
  while (true) {
    // Try to parse a parameter (name : type)
    const paramNameResult = variable(currentState);
    if (!paramNameResult.success) break;

    // Check for colon and type
    const colonResult = colon(paramNameResult.state);
    let paramType: AST.Expr | undefined;
    let colonToken: AST.Token<':'> | undefined;
    let afterParam = paramNameResult.state;

    if (colonResult.success) {
      const typeResult = modularExpr(colonResult.state);
      if (typeResult.success) {
        paramType = typeResult.value;
        colonToken = colonResult.value;
        afterParam = typeResult.state;
      }
    }

    params.push(AST.functionParam(
      paramNameResult.value.token,
      colonToken,
      paramType,
      { start: paramNameResult.value.span.start, end: afterParam.position }
    ));
    currentState = afterParam;

    // Check for comma
    const commaResult = comma(currentState);
    if (!commaResult.success) break;

    commas.push(commaResult.value);
    currentState = commaResult.state;
  }

  // Assign commas to parameters
  for (let i = 0; i < commas.length && i < params.length; i++) {
    params[i].comma = commas[i];
  }

  // Parse right parenthesis
  const rightParenResult = rightParen(currentState);
  if (!rightParenResult.success) return { success: false, error: 'Expected ) after parameters', state };
  currentState = rightParenResult.state;

  // Parse post-parenthesis specifiers like <suspends>, <decides>
  const postParenSpecifiers: AST.Specifier[] = [];
  while (true) {
    const specResult = specifierParser(currentState);
    if (!specResult.success) break;
    postParenSpecifiers.push(specResult.value);
    currentState = specResult.state;
  }

  // Optional: Parse return type (: type)
  let colonToken: AST.Token<':'> | undefined;
  let returnTypeToken: AST.Token<string> | undefined;

  const colonResult = colon(currentState);
  if (colonResult.success) {
    const returnTypeResult = variable(colonResult.state);
    if (returnTypeResult.success) {
      colonToken = colonResult.value;
      returnTypeToken = returnTypeResult.value.token;
      currentState = returnTypeResult.state;
    }
  }

  // Parse assignment operator (:= or =)
  const assignResult = assignOp(currentState);
  if (!assignResult.success) {
    const equalsResult = withTriviaLiteral('=', PC.string('='))(currentState);
    if (!equalsResult.success) {
      return { success: false, error: 'Expected := or = after function signature', state };
    }
    currentState = equalsResult.state;

    // Parse function body
    const bodyResult = assignmentExpr(currentState);
    if (!bodyResult.success) return { success: false, error: 'Expected function body', state };

    return {
      success: true,
      value: AST.functionDeclaration(
        nameResult.value.token,
        specifiers,
        postParenSpecifiers.length > 0 ? postParenSpecifiers : undefined,
        leftParenResult.value,
        params,
        rightParenResult.value,
        colonToken,
        returnTypeToken,
        equalsResult.value,
        bodyResult.value,
        { start: startPos, end: bodyResult.state.position }
      ),
      state: bodyResult.state
    };
  } else {
    // Parse function body
    const bodyResult = assignmentExpr(assignResult.state);
    if (!bodyResult.success) return { success: false, error: 'Expected function body', state };

    return {
      success: true,
      value: AST.functionDeclaration(
        nameResult.value.token,
        specifiers,
        postParenSpecifiers.length > 0 ? postParenSpecifiers : undefined,
        leftParenResult.value,
        params,
        rightParenResult.value,
        colonToken,
        returnTypeToken,
        assignResult.value,
        bodyResult.value,
        { start: startPos, end: bodyResult.state.position }
      ),
      state: bodyResult.state
    };
  }
};

// Lambda and assignment expressions
assignmentExpr = (state) => {
  const startPos = state.position;

  // First check if this might be a typed constant declaration (x : type = value)
  // We need to peek ahead to see if we have the pattern: identifier : identifier =
  const identResult = variable(state);
  if (identResult.success) {
    // Check for colon (type annotation)
    const colonResult = colon(identResult.state);
    if (colonResult.success) {
      // Try to parse type name
      const typeResult = variable(colonResult.state);
      if (typeResult.success) {
        // Check for = (not :=)
        const equalsResult = withTriviaLiteral('=', PC.string('='))(typeResult.state);
        if (equalsResult.success) {
          // Parse the value
          const valueResult = assignmentExpr(equalsResult.state);
          if (valueResult.success) {
            // Create a ConstDeclaration with type annotation
            return {
              success: true,
              value: AST.constDeclaration(
                identResult.value.token,
                colonResult.value,
                typeResult.value.token,
                equalsResult.value,
                valueResult.value,
                { start: startPos, end: valueResult.state.position }
              ),
              state: valueResult.state
            };
          }
        }
      }
    }
  }

  // Normal parsing path
  const leftResult = logicalExpr(state);
  if (!leftResult.success) return leftResult;

  // Check if this is a function declaration with return type: f() : type = body
  if (leftResult.value.type === 'Application') {
    // Parse post-parentheses specifiers (like <suspends>, <transacts>, etc.)
    const postParenSpecifiers: AST.Specifier[] = [];
    let currentState = leftResult.state;

    // Parse all post-parentheses specifiers
    while (true) {
      const specResult = specifierParser(currentState);
      if (!specResult.success) break;
      postParenSpecifiers.push(specResult.value);
      currentState = specResult.state;
    }

    const colonResult = colon(currentState);
    if (colonResult.success) {
      // Parse return type
      const returnTypeResult = variable(colonResult.state);
      if (returnTypeResult.success) {
        // Check for = (not :=)
        const equalsResult = withTriviaLiteral('=', PC.string('='))(returnTypeResult.state);
        if (equalsResult.success) {
          // Parse function body
          const bodyResult = assignmentExpr(equalsResult.state);
          if (bodyResult.success) {
            // Convert to FunctionDeclaration
            const app = leftResult.value;
            // The function name should be in app.func if it's a Variable
            if (app.func.type === 'Variable') {
              const params: AST.FunctionParam[] = [];
              for (let i = 0; i < app.args.length; i++) {
                const arg = app.args[i];

                // Handle both simple variables and typed parameters
                if (arg.type === 'Variable') {
                  // Simple parameter: name
                  params.push(AST.functionParam(
                    arg.token,
                    undefined,
                    undefined,
                    { start: arg.span.start, end: arg.span.end }
                  ));
                  if (i < app.args.length - 1) {
                    params[params.length - 1].comma = app.commas[i];
                  }
                } else if (arg.type === 'ConstDeclaration') {
                  // Typed parameter: name:type (parsed as ConstDeclaration)
                  const constDecl = arg as AST.ConstDeclaration;
                  params.push(AST.functionParam(
                    constDecl.name,
                    constDecl.colon,
                    constDecl.typeName,
                    { start: arg.span.start, end: arg.span.end }
                  ));
                  if (i < app.args.length - 1) {
                    params[params.length - 1].comma = app.commas[i];
                  }
                }
              }

              return {
                success: true,
                value: AST.functionDeclaration(
                  app.func.token,
                  [],  // no pre-parentheses specifiers for now
                  postParenSpecifiers.length > 0 ? postParenSpecifiers : undefined,
                  app.leftParen as AST.Token<'('>,
                  params,
                  app.rightParen as AST.Token<')'>,
                  colonResult.value,
                  returnTypeResult.value.token,
                  equalsResult.value,
                  bodyResult.value,
                  { start: startPos, end: bodyResult.state.position }
                ),
                state: bodyResult.state
              };
            }
          }
        }
      }
    }
  }

  // Check for function declaration with specifiers: f <spec1> <spec2> () := body
  const functionResult = parseFunctionWithSpecifiers(state);
  if (functionResult.success) {
    return functionResult;
  }

  // Check for arrow (lambda)
  const arrowResult = arrowOp(leftResult.state);
  if (arrowResult.success) {
    // This is a lambda expression
    // The left side should be a simple variable (the parameter)
    if (leftResult.value.type === 'Variable') {
      const bodyResult = assignmentExpr(arrowResult.state);
      if (bodyResult.success) {
        return {
          success: true,
          value: AST.lambdaExpression(
            leftResult.value.token,
            arrowResult.value,
            bodyResult.value,
            { start: startPos, end: bodyResult.state.position }
          ),
          state: bodyResult.state
        };
      }
    }
    // If not a valid lambda, fall through to try assignment
  }

  // Check for assignment operator
  const opResult = assignOp(leftResult.state);
  if (!opResult.success) {
    // No assignment, just return the logical expression
    return leftResult;
  }

  // Validate assignment target - only variables should be assignable
  if (leftResult.value.type !== 'Variable') {
    return { success: false, error: `Invalid assignment target: ${leftResult.value.type}`, state };
  }

  // Check for reserved keywords that cannot be assigned to
  const variableAst = leftResult.value as AST.Variable;
  const varName = variableAst.token.value;
  const reservedKeywords = [
    'true', 'false', 'continue', 'break', 'class', 'if', 'then', 'else',
    'for', 'case', 'using', 'module', 'interface', 'enum', 'struct',
    'function', 'return', 'not', 'and', 'or', 'array', 'var', 'block'
  ];

  if (reservedKeywords.includes(varName)) {
    return { success: false, error: `Cannot assign to reserved keyword: ${varName}`, state };
  }

  const rightResult = assignmentExpr(opResult.state);
  if (!rightResult.success) return rightResult;

  return {
    success: true,
    value: AST.assignment(
      leftResult.value,
      opResult.value,
      rightResult.value,
      { start: startPos, end: rightResult.state.position }
    ),
    state: rightResult.state
  };
};

// Top-level expression
expr = assignmentExpr;

// Set up the expression parser reference for if, block, for, case, class, set, and loop statements
setIfExprParser(() => expr);
setBlockExprParser(() => expr);
setForExprParser(() => expr);
setCaseExprParser(() => expr);
setClassExprParser(() => expr);
setExprParser(() => expr);
setLoopExprParser(() => expr);

// Export the modular expression parser
export const modularExpr = expr;

/**
 * Parse an expression using modular components
 */
export const parseExpressionModular = (input: string, verbose = false): AST.Expr | null => {
  const result = PC.runParser(expr, input);

  if (result.success) {
    return result.value;
  } else {
    if (verbose) {
      console.log('❌ Modular parsing failed:', result.error);
    }
    return null;
  }
};