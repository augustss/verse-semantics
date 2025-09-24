/**
 * Lambda Expression Parser
 *
 * This module handles parsing of lambda (arrow function) expressions.
 *
 * Lambda syntax:
 * - Single parameter: x => x + 1
 * - Multiple parameters: (x, y) => x + y
 * - No parameters: () => 42
 *
 * Key features:
 * - Lookahead detection to identify lambda patterns
 * - Parameter list parsing with proper validation
 * - Arrow token offset tracking for source reconstruction
 */

import { Token, TokenType } from '../../lexer/token';
import { ParserState, ParseResult, ParseError } from '../parser-state';
import * as AST from '../ast';

/**
 * Parser for lambda (arrow function) expressions.
 *
 * Handles various forms of lambda syntax:
 * - Single parameter without parentheses: x => x * 2
 * - Multiple parameters with parentheses: (x, y) => x + y
 * - Zero parameters: () => 42
 */
export class LambdaParser {
  /**
   * Parse a lambda expression or fall through to logical OR.
   *
   * This method performs lookahead to determine if we have a lambda expression.
   * Lambda detection requires finding the arrow operator (=>) after parameters.
   *
   * Grammar:
   *   lambda = identifier "=>" logical_or
   *          | "(" parameter_list ")" "=>" logical_or
   *          | logical_or
   *
   * Examples:
   *   x => x * 2         -> LambdaExpression { parameters: [x], body: x * 2 }
   *   (a, b) => a + b    -> LambdaExpression { parameters: [a, b], body: a + b }
   *   () => 42           -> LambdaExpression { parameters: [], body: 42 }
   *   regular_expr       -> falls through to parseLogicalOr
   *
   * @param state Current parser state
   * @param parseIdentifier Parser for identifier parameters
   * @param parseLogicalOr Parser for the body expression (or fallback)
   * @param parseLambdaExpression Parser for the actual lambda expression
   * @returns Parsed expression and new state
   */
  parseLambda(
    state: ParserState,
    parseIdentifier: (state: ParserState) => ParseResult<AST.IdentifierExpression>,
    parseLogicalOr: (state: ParserState) => ParseResult<AST.Expression>,
    parseLambdaExpression: (state: ParserState) => ParseResult<AST.LambdaExpression>
  ): ParseResult<AST.Expression> {
    state = state.skipTrivia();
    const token = state.current();

    if (!token) {
      return parseLogicalOr(state);
    }

    // Check for single-parameter lambda: identifier =>
    if (token.type === TokenType.IDENTIFIER) {
      const next = state.advance().skipTrivia().current();
      if (next && next.type === TokenType.OPERATOR && next.content === '=>') {
        // Found lambda expression
        return parseLambdaExpression(state);
      }
    }

    // Check for parenthesized parameter list: (...) =>
    if (token.type === TokenType.OPERATOR && token.content === '(') {
      let lookahead = state.advance().skipTrivia();
      let paramCount = 0;
      let isLambda = false;

      // Look ahead to find if this is a lambda parameter list
      while (!lookahead.isAtEnd()) {
        const current = lookahead.current();
        if (!current) break;

        // Check for closing parenthesis
        if (current.type === TokenType.OPERATOR && current.content === ')') {
          // Check if arrow follows the closing paren
          const afterParen = lookahead.advance().skipTrivia().current();
          if (afterParen && afterParen.type === TokenType.OPERATOR && afterParen.content === '=>') {
            isLambda = true;
          }
          break;
        }

        // Check for parameter (must be identifier)
        if (current.type === TokenType.IDENTIFIER) {
          paramCount++;
          lookahead = lookahead.advance().skipTrivia();

          // Check for comma (more parameters)
          const next = lookahead.current();
          if (next && next.type === TokenType.OPERATOR && next.content === ',') {
            lookahead = lookahead.advance().skipTrivia();
          }
        } else {
          // Not a valid parameter list
          break;
        }
      }

      if (isLambda) {
        return parseLambdaExpression(state);
      }
    }

    // Not a lambda, parse as logical OR expression
    return parseLogicalOr(state);
  }

  /**
   * Parse a lambda expression after determining it is one.
   *
   * This is called after the lookahead confirms we have a lambda.
   * It handles the actual parsing of parameters and body.
   *
   * Grammar:
   *   lambda_expression = parameter_spec "=>" body
   *   parameter_spec = identifier | "(" parameter_list ")"
   *   parameter_list = (identifier ("," identifier)*)?
   *   body = lambda_expression | logical_or
   *
   * Examples:
   *   x => x * 2           -> Single parameter, simple body
   *   (x, y) => x + y      -> Multiple parameters
   *   () => getValue()     -> No parameters
   *   n => n > 0 ? n : 0   -> Complex body expression
   *
   * @param state Current parser state
   * @param parseIdentifier Parser for parameter identifiers
   * @param parseBody Parser for the body expression (supports nested lambdas)
   * @returns Parsed lambda expression and new state
   * @throws ParseError if lambda syntax is malformed
   */
  parseLambdaExpression(
    state: ParserState,
    parseIdentifier: (state: ParserState) => ParseResult<AST.IdentifierExpression>,
    parseBody: (state: ParserState) => ParseResult<AST.Expression>
  ): ParseResult<AST.LambdaExpression> {
    state = state.skipTrivia();
    const startToken = state.current();

    if (!startToken) {
      throw new ParseError('Expected lambda expression', state.position);
    }

    const params: AST.IdentifierExpression[] = [];
    const parameterSeparatorOffsets: number[] = [];

    // Parse parameter(s)
    if (startToken.type === TokenType.OPERATOR && startToken.content === '(') {
      // Parenthesized parameter list
      state = state.advance();

      // Parse comma-separated parameters
      while (!state.isAtEnd() && !(state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')')) {
        const paramResult = parseIdentifier(state);
        params.push(paramResult.node);
        state = paramResult.state.skipTrivia();

        // Check for comma (more parameters)
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
          parameterSeparatorOffsets.push(state.currentOffset());
          state = state.advance();
        } else if (!(state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')')) {
          throw new ParseError('Expected , or )', state.position, state.current() || undefined);
        }
      }

      // Verify closing parenthesis
      if (!(state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')')) {
        throw new ParseError('Expected )', state.position, state.current() || undefined);
      }
      state = state.advance();

      // Collect trailing tokens after closing paren

    } else if (startToken.type === TokenType.IDENTIFIER) {
      // Single parameter without parentheses
      const paramResult = parseIdentifier(state);
      params.push(paramResult.node);
      state = paramResult.state;
    } else {
      throw new ParseError('Expected parameter list or identifier', state.position, startToken);
    }

    // Parse arrow operator (no need to skip trivia if we collected trailing tokens)
    state = state.skipTrivia();
    const arrowOffset = state.currentOffset();
    const arrowToken = state.current();
    if (!arrowToken || arrowToken.type !== TokenType.OPERATOR || arrowToken.content !== '=>') {
      throw new ParseError('Expected =>', state.position, arrowToken || undefined);
    }
    state = state.advance();

    // Parse lambda body (can be another lambda for nested lambdas)
    const bodyResult = parseBody(state);

    // Create lambda expression node with arrow offset
    const node: AST.LambdaExpression = {
      type: 'LambdaExpression',
      parameters: params,
      body: bodyResult.node,
      arrowOffset,
      parameterSeparatorOffsets
    };

    return { node, state: bodyResult.state };
  }
}