/**
 * Set statement parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';
import { variable } from '../literals/identifiers';

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
};

// Set statements: set variable = expression
export const setStatement: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Try 'set' keyword with boundary check
  const setCheck = PC.string('set')(state);
  if (!setCheck.success) return { success: false, error: 'Not a set statement', state };

  // Check it's not part of a longer identifier
  const nextPos = setCheck.state.position;
  if (nextPos < setCheck.state.input.length && /[a-zA-Z0-9_]/.test(setCheck.state.input[nextPos])) {
    return { success: false, error: 'set is part of identifier', state };
  }

  // Parse 'set' with trivia
  const setResult = withTriviaLiteral('set', PC.string('set'))(state);
  if (!setResult.success) return setResult;

  // Parse the variable name
  const variableResult = variable(setResult.state);
  if (!variableResult.success) return { success: false, error: 'Expected variable name after set', state };

  // Parse the assignment operator (= only, not :=)
  const equalsResult = withTriviaLiteral('=', PC.char('='))(variableResult.state);
  if (!equalsResult.success) return { success: false, error: 'Expected = after variable in set statement', state };

  // Parse the expression
  const exprResult = getExpr()(equalsResult.state);
  if (!exprResult.success) return { success: false, error: 'Expected expression after = in set statement', state };

  return {
    success: true,
    value: AST.setStatement(
      setResult.value,
      variableResult.value.token,
      equalsResult.value,
      exprResult.value,
      { start: startPos, end: exprResult.state.position }
    ),
    state: exprResult.state
  };
};