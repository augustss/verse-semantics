/**
 * Operator Expression Parser
 *
 * This module handles parsing of expressions involving operators.
 *
 * Unary operators:
 * - Arithmetic: - (negation)
 * - Logical: not
 *
 * Binary operators:
 * - Arithmetic: +, -, *, /, %
 * - Logical: and, or (keyword operators)
 * - Comparison: <, <=, >, >=
 * - Keyword operators: is, as, in
 * - Range: ..
 *
 * Special operators:
 * - Assignment: := (right-associative)
 *
 * Postfix operators:
 * - Member access: . (dot notation)
 * - Indexing: [] (computed access)
 * - Function call: ()
 * - Object construction: {} (only on identifiers)
 *
 * All operators store their token offset for source reconstruction.
 */

import { Token, TokenType } from '../../lexer/token';
import { ParserState, ParseResult, ParseError } from '../parser-state';
import * as AST from '../ast';

/**
 * Parser for operator-based expressions.
 *
 * Handles operator precedence and associativity for:
 * - Arithmetic operations
 * - Logical operations
 * - Comparison operations
 * - Member access
 * - Function calls
 * - Assignment
 */
export class OperatorParser {
  /**
   * Parse a unary expression.
   *
   * Grammar:
   *   unary = ("-" | "not") unary
   *         | postfix
   *
   * Examples:
   *   -42           -> UnaryExpression { operator: "-", operand: 42 }
   *   not flag      -> UnaryExpression { operator: "not", operand: flag }
   *   --x           -> UnaryExpression { operator: "-", operand: UnaryExpression { operator: "-", operand: x } }
   *
   * @param state Current parser state
   * @param parseUnary Recursive callback for nested unary expressions
   * @param parsePostfix Callback to parse postfix expressions when no unary operator
   * @returns Parsed expression and new state
   */
  parseUnary(
    state: ParserState,
    parseUnary: (state: ParserState) => ParseResult<AST.Expression>,
    parsePostfix: (state: ParserState) => ParseResult<AST.Expression>
  ): ParseResult<AST.Expression> {
    state = state.skipTrivia();
    const token = state.current();

    // Check for unary operators
    if (token && ((token.type === TokenType.OPERATOR && (token.content === '-' || token.content === '*')) ||
                  (token.type === TokenType.IDENTIFIER && token.content === 'not'))) {
      const operatorOffset = state.currentOffset();
      const operator = token;
      state = state.advance();

      // Collect trailing tokens after operator

      // Parse the operand (which could be another unary expression)
      const operandResult = parseUnary(state);

      // Create appropriate expression node based on operator
      if (operator.content === '*') {
        // Tuple expansion operator
        const node: AST.TupleExpansionExpression = {
          type: 'TupleExpansionExpression',
          tuple: operandResult.node,
          operatorOffset
        };
        return { node, state: operandResult.state };
      } else {
        // Regular unary expression
        const node: AST.UnaryExpression = {
          type: 'UnaryExpression',
          operator: operator.content,
          operand: operandResult.node,
          operatorOffset
        };
        return { node, state: operandResult.state };
      }
    }

    // No unary operator, parse as postfix expression
    return parsePostfix(state);
  }

  /**
   * Parse a binary operator expression with left associativity.
   *
   * This is a generic method used by different precedence levels.
   * It handles left-associative binary operators.
   *
   * Grammar template:
   *   expr = left_expr (operator right_expr)*
   *
   * @param state Current parser state
   * @param parseLeft Parser for left operand (higher precedence)
   * @param parseRight Parser for right operand (same or lower precedence)
   * @param operatorContents Array of operator strings to match
   * @returns Parsed expression and new state
   */
  parseBinaryOp(
    state: ParserState,
    parseLeft: (state: ParserState) => ParseResult<AST.Expression>,
    parseRight: (state: ParserState) => ParseResult<AST.Expression>,
    operatorContents: string[]
  ): ParseResult<AST.Expression> {
    // Parse left operand
    let leftResult = parseLeft(state);
    state = leftResult.state;

    // Keep parsing while we find more operators at this precedence level
    while (true) {
      state = state.skipTrivia();

      // Check for line continuation: if we see a newline, look ahead
      if (state.current()?.type === TokenType.NEWLINE) {
        let lookahead = state.advance().skipTrivia();

        // Skip additional newlines
        while (lookahead.current()?.type === TokenType.NEWLINE) {
          lookahead = lookahead.advance().skipTrivia();
        }

        const nextToken = lookahead.current();
        if (nextToken &&
            ((nextToken.type === TokenType.OPERATOR && operatorContents.includes(nextToken.content)) ||
             (nextToken.type === TokenType.IDENTIFIER && operatorContents.includes(nextToken.content)))) {
          // Found operator on next line, continue with that state
          state = lookahead;
        } else {
          // No operator on next line, done with this expression
          return leftResult;
        }
      }

      const token = state.current();

      // Check for binary operator (can be OPERATOR or IDENTIFIER for keyword operators)
      if (!token) {
        return leftResult;
      }

      const isOperator = (token.type === TokenType.OPERATOR && operatorContents.includes(token.content)) ||
                         (token.type === TokenType.IDENTIFIER && operatorContents.includes(token.content));

      if (!isOperator) {
        // No more matching operators, return accumulated result
        return leftResult;
      }

      // Found operator, parse right operand
      const operatorOffset = state.currentOffset();
      const operator = token;
      state = state.advance();

      const rightResult = parseRight(state);

      // Create binary expression node (left-associative) with token offset
      const node: AST.BinaryExpression = {
        type: 'BinaryExpression',
        left: leftResult.node,
        operator: operator.content,
        right: rightResult.node,
        operatorOffset
      };

      // Update left result for next iteration
      leftResult = { node, state: rightResult.state };
      state = rightResult.state;
    }
  }

  /**
   * Parse an assignment expression.
   *
   * Grammar:
   *   assignment = range_expr (":=" assignment | "=" assignment)?
   *
   * Right-associative to allow chaining: a := b := c or a = b = c
   *
   * Examples:
   *   x := 42       -> AssignmentExpression { left: x, operator: ":=", right: 42 }
   *   x = 42        -> AssignmentExpression { left: x, operator: "=", right: 42 }
   *   a := b := 5   -> AssignmentExpression { left: a, right: AssignmentExpression { left: b, right: 5 } }
   *   a = b = 5     -> AssignmentExpression { left: a, right: AssignmentExpression { left: b, right: 5 } }
   *
   * @param state Current parser state
   * @param parseRange Parser for range expressions
   * @param parseAssignment Recursive callback for right-associativity
   * @returns Parsed expression and new state
   */
  parseAssignment(
    state: ParserState,
    parseRange: (state: ParserState) => ParseResult<AST.Expression>,
    parseAssignment: (state: ParserState) => ParseResult<AST.Expression>
  ): ParseResult<AST.Expression> {
    // Parse left side (could be identifier, member access, etc.)
    const leftResult = parseRange(state);
    state = leftResult.state.skipTrivia();

    // Check for line continuation with assignment operator
    if (state.current()?.type === TokenType.NEWLINE) {
      let lookahead = state.advance().skipTrivia();

      // Skip additional newlines
      while (lookahead.current()?.type === TokenType.NEWLINE) {
        lookahead = lookahead.advance().skipTrivia();
      }

      const nextToken = lookahead.current();
      if (nextToken && nextToken.type === TokenType.OPERATOR &&
          (nextToken.content === ':=' || nextToken.content === '=')) {
        // Found assignment operator on next line
        state = lookahead;
      } else {
        // No assignment operator on next line
        return leftResult;
      }
    }

    // Check for assignment operator (both := and =)
    const operatorOffset = state.currentOffset();
    const token = state.current();
    if (!token || token.type !== TokenType.OPERATOR ||
        (token.content !== ':=' && token.content !== '=')) {
      // No assignment, return left expression
      return leftResult;
    }

    const operator = token;
    state = state.advance();

    // Collect trailing tokens after operator

    // Validate that left side is a valid lvalue
    if (!this.isValidLValue(leftResult.node)) {
      throw new ParseError(`Invalid left-hand side in assignment`, state.position, operator);
    }

    // Parse right side (recursively for right-associativity)
    const rightResult = parseAssignment(state);

    // Create assignment expression node
    const node: AST.AssignmentExpression = {
      type: 'AssignmentExpression',
      left: leftResult.node,
      operator: operator.content,
      right: rightResult.node,
      operatorOffset
    };

    return { node, state: rightResult.state };
  }

  /**
   * Check if an expression is a valid left-hand side for assignment.
   * Valid lvalues are:
   * - Identifiers (variables)
   * - Member expressions (obj.prop, obj[key])
   */
  private isValidLValue(expr: AST.Expression): boolean {
    switch (expr.type) {
      case 'Identifier':
        return true;
      case 'MemberExpression':
        return true;
      default:
        return false;
    }
  }

  /**
   * Parse a range expression.
   *
   * Grammar:
   *   range = lambda_expr (".." lambda_expr)?
   *
   * Examples:
   *   1..10         -> BinaryExpression { left: 1, operator: "..", right: 10 }
   *   start..end    -> BinaryExpression { left: start, operator: "..", right: end }
   *
   * @param state Current parser state
   * @param parseLambda Parser for lambda expressions
   * @returns Parsed expression and new state
   */
  parseRange(
    state: ParserState,
    parseLambda: (state: ParserState) => ParseResult<AST.Expression>
  ): ParseResult<AST.Expression> {
    // Parse left operand
    const leftResult = parseLambda(state);
    state = leftResult.state.skipTrivia();

    // Check for line continuation with range operator
    if (state.current()?.type === TokenType.NEWLINE) {
      let lookahead = state.advance().skipTrivia();

      // Skip additional newlines
      while (lookahead.current()?.type === TokenType.NEWLINE) {
        lookahead = lookahead.advance().skipTrivia();
      }

      const nextToken = lookahead.current();
      if (nextToken && nextToken.type === TokenType.OPERATOR && nextToken.content === '..') {
        // Found range operator on next line
        state = lookahead;
      } else {
        // No range operator on next line
        return leftResult;
      }
    }

    // Check for range operator
    const operatorOffset = state.currentOffset();
    const token = state.current();
    if (!token || token.type !== TokenType.OPERATOR || token.content !== '..') {
      return leftResult;
    }

    const operator = token;
    state = state.advance();

    // Collect trailing tokens after operator

    // Parse right operand
    const rightResult = parseLambda(state);

    // Create range expression as binary expression
    const node: AST.RangeExpression = {
      type: 'RangeExpression',
      start: leftResult.node,
      end: rightResult.node,
      operatorOffset
    };

    return { node, state: rightResult.state };
  }

  /**
   * Parse postfix expressions (member access, indexing, function calls).
   *
   * Grammar:
   *   postfix = primary_expr postfix_op*
   *   postfix_op = "." identifier
   *              | "[" expression "]"
   *              | "(" argument_list ")"
   *
   * Examples:
   *   obj.prop          -> MemberExpression { object: obj, property: prop }
   *   arr[0]            -> MemberExpression { object: arr, property: 0, computed: true }
   *   func(a, b)        -> CallExpression { callee: func, arguments: [a, b] }
   *   Point{x:=1, y:=2} -> ObjectConstructorExpression { typeName: "Point", fields: [...] }
   *   obj.method()      -> CallExpression { callee: MemberExpression { obj.method }, arguments: [] }
   *   matrix[i][j]      -> MemberExpression { object: MemberExpression { matrix[i] }, property: j }
   *
   * @param state Current parser state
   * @param parsePrimary Parser for primary expressions
   * @param parseIdentifier Parser for identifiers (used in member access)
   * @param parseExpression Parser for general expressions (used in indexing and arguments)
   * @returns Parsed expression and new state
   */
  parsePostfix(
    state: ParserState,
    parsePrimary: (state: ParserState) => ParseResult<AST.Expression>,
    parseIdentifier: (state: ParserState) => ParseResult<AST.IdentifierExpression>,
    parseExpression: (state: ParserState) => ParseResult<AST.Expression>
  ): ParseResult<AST.Expression> {
    // Parse the primary expression first
    const primaryResult = parsePrimary(state);
    let node = primaryResult.node;
    state = primaryResult.state;

    // Apply postfix operators in a loop
    while (!state.isAtEnd()) {
      state = state.skipTrivia();
      const token = state.current();

      if (!token) break;

      if (token.type === TokenType.OPERATOR && token.content === '.') {
        // Member access: obj.property
        const dotOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        // Skip newlines after dot (for line continuation like x.\ny)
        while (state.current()?.type === TokenType.NEWLINE) {
          state = state.advance().skipTrivia();
        }

        const propertyResult = parseIdentifier(state);

        node = {
          type: 'MemberExpression',
          object: node,
          property: propertyResult.node,
          computed: false,  // Direct property access
          dotOffset
        } as AST.MemberExpression;

        state = propertyResult.state;

      } else if (token.type === TokenType.OPERATOR && token.content === '[') {
        // Brackets can be either:
        // 1. Function call: f[] or f[1, 2, 3] (Verse-style)
        // 2. Member access: arr[index] (single expression)
        const openBracketOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        // Check if immediately closed (empty brackets)
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ']') {
          // f[] - treat as function call with no arguments
          const closeBracketOffset = state.currentOffset();
          state = state.advance();

          node = {
            type: 'CallExpression',
            callee: node,
            arguments: [],
            openParenOffset: openBracketOffset,  // Using bracket offsets as paren offsets
            closeParenOffset: closeBracketOffset,
            argumentSeparatorOffsets: []
          } as AST.CallExpression;
        } else {
          // Could be either function call with arguments or member access
          // We need to parse potentially multiple comma-separated expressions
          const args: AST.Expression[] = [];
          const argumentSeparatorOffsets: number[] = [];

          // Parse first expression
          const firstResult = parseExpression(state);
          args.push(firstResult.node);
          state = firstResult.state.skipTrivia();

          // Check if there's a comma (indicating multiple arguments)
          let isCallExpression = false;
          const next = state.current();
          if (next && next.type === TokenType.OPERATOR && next.content === ',') {
            // Multiple arguments - this is a function call
            isCallExpression = true;
            argumentSeparatorOffsets.push(state.currentOffset());
            state = state.advance().skipTrivia();

            // Parse remaining arguments
            while (!state.isAtEnd()) {
              // Check for trailing comma
              if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ']') {
                // Trailing comma - this is an error
                throw new ParseError('Trailing comma not allowed', state.position, state.current() || undefined);
              }

              // Parse argument
              const argResult = parseExpression(state);
              args.push(argResult.node);
              state = argResult.state.skipTrivia();

              // Check for comma or closing bracket
              const current = state.current();
              if (current && current.type === TokenType.OPERATOR && current.content === ',') {
                argumentSeparatorOffsets.push(state.currentOffset());
                state = state.advance().skipTrivia();
                // Check for trailing comma
                if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ']') {
                  // Trailing comma - this is an error
                  throw new ParseError('Trailing comma not allowed in array access', state.position, state.current() || undefined);
                }
              } else if (!current || current.type !== TokenType.OPERATOR || current.content !== ']') {
                throw new ParseError('Expected , or ]', state.position, current || undefined);
              } else {
                break;
              }
            }
          }

          // Expect closing bracket
          const closeBracketOffset = state.currentOffset();
          const closeBracket = state.current();
          if (!closeBracket || closeBracket.type !== TokenType.OPERATOR || closeBracket.content !== ']') {
            throw new ParseError('Expected ]', state.position, closeBracket || undefined);
          }
          state = state.advance();

          if (isCallExpression || args.length > 1) {
            // Multiple arguments or explicitly marked as call - treat as function call
            node = {
              type: 'CallExpression',
              callee: node,
              arguments: args,
              openParenOffset: openBracketOffset,
              closeParenOffset: closeBracketOffset,
              argumentSeparatorOffsets
            } as AST.CallExpression;
          } else {
            // Single argument without comma - treat as member access
            node = {
              type: 'MemberExpression',
              object: node,
              property: args[0],
              computed: true,
              openBracketOffset,
              closeBracketOffset
            } as AST.MemberExpression;
          }
        }

      } else if (token.type === TokenType.OPERATOR && token.content === '(') {
        // Function call: func(args)
        const openParenOffset = state.currentOffset();
        state = state.advance();
        const args: AST.Expression[] = [];
        const argumentSeparatorOffsets: number[] = [];

        // Parse argument list
        while (!state.isAtEnd()) {
          state = state.skipTrivia();
          const current = state.current();

          // Check for empty argument list or end of arguments
          if (current && current.type === TokenType.OPERATOR && current.content === ')') {
            break;
          }

          // Parse an argument
          const argResult = parseExpression(state);
          args.push(argResult.node);
          state = argResult.state.skipTrivia();

          // Check for comma (more arguments) or closing paren
          const next = state.current();
          if (next && next.type === TokenType.OPERATOR && next.content === ',') {
            argumentSeparatorOffsets.push(state.currentOffset());
            state = state.advance().skipTrivia();
            // Check if this was a trailing comma (followed by closing paren)
            if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')') {
              // Trailing comma - this is an error
              throw new ParseError('Trailing comma not allowed', state.position, state.current() || undefined);
            }
          } else if (!next || next.type !== TokenType.OPERATOR || next.content !== ')') {
            throw new ParseError('Expected , or )', state.position, next || undefined);
          }
        }

        // Expect closing parenthesis
        const closeParenOffset = state.currentOffset();
        const closeParen = state.current();
        if (!closeParen || closeParen.type !== TokenType.OPERATOR || closeParen.content !== ')') {
          throw new ParseError('Expected )', state.position, closeParen || undefined);
        }
        state = state.advance();

        // Check if this could be tuple element access (single argument)
        // Tuple access: expr(index) where index is an expression
        if (args.length === 1 && argumentSeparatorOffsets.length === 0) {
          // This could be tuple access - create TupleAccessExpression
          node = {
            type: 'TupleAccessExpression',
            tuple: node,
            index: args[0],
            openParenOffset,
            closeParenOffset
          } as AST.TupleAccessExpression;
        } else {
          // Regular function call with multiple arguments or no arguments
          node = {
            type: 'CallExpression',
            callee: node,
            arguments: args,
            openParenOffset,
            closeParenOffset,
            argumentSeparatorOffsets
          } as AST.CallExpression;
        }

      } else if (token.type === TokenType.OPERATOR && token.content === '{' &&
                 node.type === 'Identifier') {
        // Object construction: Point{x:=1, y:=2}
        // Only allow on identifiers (not on complex expressions)
        const identifierNode = node as AST.IdentifierExpression;
        const openBraceOffset = state.currentOffset();
        state = state.advance();
        const fields: AST.ObjectField[] = [];
        const fieldSeparatorOffsets: number[] = [];

        // Parse field list
        while (!state.isAtEnd()) {
          state = state.skipTrivia();

          // Skip newlines within the object constructor
          while (state.current()?.type === TokenType.NEWLINE) {
            state = state.advance().skipTrivia();
          }

          const current = state.current();

          // Check for empty field list or end of fields
          if (current && current.type === TokenType.OPERATOR && current.content === '}') {
            break;
          }

          // Parse a field: name := value
          const nameResult = parseIdentifier(state);
          state = nameResult.state.skipTrivia();

          // Expect := operator
          const assignOffset = state.currentOffset();
          const assignToken = state.current();
          if (!assignToken || assignToken.type !== TokenType.OPERATOR || assignToken.content !== ':=') {
            throw new ParseError('Expected := in object field', state.position, assignToken || undefined);
          }
          state = state.advance();

          // Parse field value
          const valueResult = parseExpression(state);

          const field: AST.ObjectField = {
            type: 'ObjectField',
            name: nameResult.node.name,
            nameOffset: nameResult.node.tokenOffset,
            assignOffset,
            value: valueResult.node
          };

          fields.push(field);
          state = valueResult.state.skipTrivia();

          // Check for comma, newline (as separator), or closing brace
          const next = state.current();
          if (next && next.type === TokenType.OPERATOR && next.content === ',') {
            fieldSeparatorOffsets.push(state.currentOffset());
            state = state.advance().skipTrivia();

            // Skip newlines after comma
            while (state.current()?.type === TokenType.NEWLINE) {
              state = state.advance().skipTrivia();
            }

            // Check if this was a trailing comma (followed by closing brace)
            if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}') {
              // Trailing comma - don't parse another field
              break;
            }
          } else if (next && next.type === TokenType.NEWLINE) {
            // Newline as separator
            fieldSeparatorOffsets.push(state.currentOffset());
            state = state.advance().skipTrivia();

            // Skip additional newlines
            while (state.current()?.type === TokenType.NEWLINE) {
              state = state.advance().skipTrivia();
            }
          } else if (!next || next.type !== TokenType.OPERATOR || next.content !== '}') {
            throw new ParseError('Expected ,, newline, or }', state.position, next || undefined);
          }
        }

        // Expect closing brace
        const closeBraceOffset = state.currentOffset();
        const closeBrace = state.current();
        if (!closeBrace || closeBrace.type !== TokenType.OPERATOR || closeBrace.content !== '}') {
          throw new ParseError('Expected }', state.position, closeBrace || undefined);
        }
        state = state.advance();

        node = {
          type: 'ObjectConstructorExpression',
          typeName: identifierNode.name,
          typeNameOffset: identifierNode.tokenOffset,
          fields,
          openBraceOffset,
          closeBraceOffset,
          fieldSeparatorOffsets
        } as AST.ObjectConstructorExpression;

      } else if (token.type === TokenType.OPERATOR && token.content === '?') {
        // Query operator for option types: x?
        const operatorOffset = state.currentOffset();
        state = state.advance();

        // Create unary expression with postfix notation
        node = {
          type: 'UnaryExpression',
          operator: '?',
          operand: node,
          operatorOffset
        } as AST.UnaryExpression;
      } else {
        // No more postfix operators
        break;
      }
    }

    return { node, state };
  }
}