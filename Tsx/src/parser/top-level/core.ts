/**
 * Top-level parsing functions for statements and programs
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { modularExpr } from '../expressions/core';
import { trivia } from '../foundation/trivia';
import { usingStatement, topLevelDeclaration } from './declarations';

// Helper function to attach leading trivia to the first token in an expression
function attachLeadingTrivia(expr: AST.Expr, leadingTrivia: string): AST.Expr {
  // Helper to update a token's leading trivia
  const updateTokenTrivia = (token: AST.Token<any>): AST.Token<any> => ({
    ...token,
    trivia: {
      ...token.trivia,
      leading: leadingTrivia + token.trivia.leading
    }
  });

  // Handle different expression types by finding their first token
  switch (expr.type) {
    case 'BreakExpression':
      return {
        ...expr,
        keyword: updateTokenTrivia(expr.keyword)
      };

    case 'ContinueExpression':
      return {
        ...expr,
        keyword: updateTokenTrivia(expr.keyword)
      };

    case 'Variable':
      return {
        ...expr,
        token: updateTokenTrivia(expr.token)
      };

    case 'IntegerLiteral':
      return {
        ...expr,
        token: updateTokenTrivia(expr.token)
      };

    case 'FloatLiteral':
      return {
        ...expr,
        token: updateTokenTrivia(expr.token)
      };

    case 'BooleanLiteral':
      return {
        ...expr,
        token: updateTokenTrivia(expr.token)
      };

    case 'StringLiteral':
      return {
        ...expr,
        token: updateTokenTrivia(expr.token)
      };

    case 'Parenthesized':
      return {
        ...expr,
        leftParen: updateTokenTrivia(expr.leftParen)
      };

    case 'Block':
      if (expr.leftBrace) {
        return {
          ...expr,
          leftBrace: updateTokenTrivia(expr.leftBrace)
        };
      } else if (expr.keyword) {
        return {
          ...expr,
          keyword: updateTokenTrivia(expr.keyword)
        };
      }
      return expr;

    case 'IfExpression':
      return {
        ...expr,
        ifKeyword: updateTokenTrivia(expr.ifKeyword)
      };

    case 'ForExpression':
      return {
        ...expr,
        forKeyword: updateTokenTrivia(expr.forKeyword)
      };

    case 'CaseExpression':
      return {
        ...expr,
        caseKeyword: updateTokenTrivia(expr.caseKeyword)
      };

    case 'ArrayConstruction':
      return {
        ...expr,
        arrayKeyword: expr.arrayKeyword ? updateTokenTrivia(expr.arrayKeyword) : expr.arrayKeyword
      };

    case 'ObjectConstruction':
      // For ObjectConstruction, we need to handle the typeExpr which can be Variable or Application
      if (expr.typeExpr.type === 'Variable') {
        return {
          ...expr,
          typeExpr: {
            ...expr.typeExpr,
            token: updateTokenTrivia(expr.typeExpr.token)
          }
        };
      } else {
        // For Application types, we recursively attach trivia to the function name
        return {
          ...expr,
          typeExpr: attachLeadingTrivia(expr.typeExpr, leadingTrivia) as AST.Variable | AST.Application
        };
      }

    case 'ClassExpression':
      return {
        ...expr,
        keyword: updateTokenTrivia(expr.keyword)
      };

    case 'VariableDeclaration':
      return {
        ...expr,
        varKeyword: updateTokenTrivia(expr.varKeyword)
      };

    // For binary operations, attach to the left operand recursively
    case 'BinaryOp':
      return {
        ...expr,
        left: attachLeadingTrivia(expr.left, leadingTrivia)
      };

    case 'UnaryOp':
      return {
        ...expr,
        operator: updateTokenTrivia(expr.operator)
      };

    case 'FunctionCall':
      return {
        ...expr,
        name: updateTokenTrivia(expr.name)
      };

    case 'Assignment':
      return {
        ...expr,
        target: attachLeadingTrivia(expr.target, leadingTrivia)
      };

    case 'Application':
      return {
        ...expr,
        func: attachLeadingTrivia(expr.func, leadingTrivia)
      };

    case 'RangeExpression':
      return {
        ...expr,
        start: attachLeadingTrivia(expr.start, leadingTrivia)
      };

    // For other expression types, return as-is
    // Could be extended to handle more types as needed
    default:
      return expr;
  }
}

/**
 * Parse a program (collection of using statements and top-level declarations)
 */
const programParser: PC.Parser<AST.Program> = (state) => {
  const startPos = state.position;
  const usingStatements: AST.UsingStatement[] = [];
  const declarations: (AST.TopLevelDeclaration | AST.FunctionDeclaration | AST.ConstDeclaration | AST.Expr)[] = [];
  let currentState = state;

  // Capture leading trivia for lossless parsing
  let leadingTriviaText = '';
  const leadingTriviaResult = trivia(currentState);
  if (leadingTriviaResult.success) {
    leadingTriviaText = state.input.substring(state.position, leadingTriviaResult.state.position);
    currentState = leadingTriviaResult.state;
  }

  // Parse using statements
  while (true) {
    const usingResult = usingStatement(currentState);
    if (!usingResult.success) break;

    usingStatements.push(usingResult.value);
    currentState = usingResult.state;

    // Skip trivia after using statement
    const triviaResult = trivia(currentState);
    if (triviaResult.success) {
      currentState = triviaResult.state;
    }
  }

  // Parse top-level declarations
  while (currentState.position < currentState.input.length) {
    // Skip trivia
    const triviaResult = trivia(currentState);
    if (triviaResult.success) {
      currentState = triviaResult.state;
    }

    // Check if we're at the end
    if (currentState.position >= currentState.input.length) {
      break;
    }

    // Try to parse a top-level declaration
    const declResult = topLevelDeclaration(currentState);
    if (declResult.success) {
      declarations.push(declResult.value);
      currentState = declResult.state;
    } else {
      // If we can't parse a declaration, try to parse an expression
      // This handles cases where the file is just an expression
      const exprResult = modularExpr(currentState);
      if (exprResult.success) {
        // Block expressions are not valid at the top level
        if (exprResult.value.type === 'Block') {
          break;
        }

        // Add the successfully parsed expression to declarations
        declarations.push(exprResult.value);
        currentState = exprResult.state;

        // If we consumed the entire input, we're done
        if (currentState.position >= currentState.input.length) {
          break;
        }
      } else {
        // Can't parse anything else, break
        break;
      }
    }
  }

  // Capture trailing trivia for lossless parsing (only whitespace and comments, not unparsed code)
  let trailingTriviaText = '';
  const triviaResult = trivia(currentState);
  if (triviaResult.success) {
    trailingTriviaText = state.input.substring(currentState.position, triviaResult.state.position);
    currentState = triviaResult.state;
  }

  // If there's still remaining input after parsing trivia, it's an error
  if (currentState.position < state.input.length) {
    return {
      success: false,
      error: `Unexpected input: ${state.input.substring(currentState.position)}`,
      state
    };
  }

  // Only return success if we've parsed something meaningful
  if (usingStatements.length > 0 || declarations.length > 0) {
    return {
      success: true,
      value: AST.program(
        usingStatements,
        declarations,
        { start: startPos, end: currentState.position },
        leadingTriviaText,
        trailingTriviaText
      ),
      state: currentState
    };
  }

  return {
    success: false,
    error: 'Failed to parse program',
    state
  };
};

// For backward compatibility with existing expression parser
export const parseExpression = (input: string, quiet: boolean = false): AST.Expr | null => {
  // Create a parser that skips leading trivia and then parses the expression
  const exprWithTrivia: PC.Parser<AST.Expr> = (state) => {
    // Skip leading trivia first
    const triviaResult = trivia(state);
    const startState = triviaResult.success ? triviaResult.state : state;

    // Parse the expression starting from after leading trivia
    const exprResult = modularExpr(startState);
    if (!exprResult.success) {
      return exprResult;
    }

    // If we had leading trivia, we need to attach it to the first token in the result
    if (triviaResult.success && triviaResult.value) {
      // Attach the leading trivia to the result expression
      const updatedExpr = attachLeadingTrivia(exprResult.value, triviaResult.value);
      return {
        success: true,
        value: updatedExpr,
        state: exprResult.state
      };
    }

    return exprResult;
  };

  const result = PC.runParser(exprWithTrivia, input);
  if (!result.success && !quiet)
    console.log('❌ Expression parsing failed:', result.error);

  // Check that all input was consumed - if not, it's an error (e.g., "x && y" should fail)
  if (result.success && result.state.position !== input.length) {
    if (!quiet) {
      const remaining = input.slice(result.state.position);
      console.log(`❌ Expression parsing failed: unexpected remaining input "${remaining}"`);
    }
    return null;
  }

  return result.success ? result.value : null;
};

export const parse = (input: string, quiet?: boolean): AST.Expr | AST.Program | null => {
  // First try to parse as a program (only if it has declarations or using statements)
  const programResult = PC.runParser(programParser, input);
  if (programResult.success &&
      programResult.state.position === input.length &&
      (programResult.value.declarations.length > 0 || programResult.value.usingStatements.length > 0)) {
    return programResult.value;
  }
  // Fall back to expression parsing
  return parseExpression(input, quiet);
};

export const parseTopLevel = (input: string, quiet: boolean = false): AST.Program | null => {
  const result = PC.runParser(programParser, input);
  return result.success && result.state.position === input.length ? result.value : null;
};

export const parseProgram = (input: string, quiet: boolean = false): AST.Program | null => {
  return parseTopLevel(input, quiet);
};