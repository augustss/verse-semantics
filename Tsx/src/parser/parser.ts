/**
 * Verse Parser - Main Orchestrator
 *
 * A recursive descent parser for the Verse language that coordinates
 * multiple specialized parser modules to build an Abstract Syntax Tree (AST).
 *
 * The parser follows these design principles:
 * - Modular architecture with specialized parsers for different constructs
 * - Immutable state management for easy backtracking
 * - Operator precedence handled through recursive descent levels
 * - Token stream offsets stored in AST nodes for source reconstruction
 * - Indentation-sensitive parsing for block structures
 *
 * Key Features:
 * - AST nodes store token offsets instead of token references
 * - Position information can be reconstructed from token offsets
 * - Supports both braced and indented syntax for arrays and blocks
 * - Tracks indentation context for proper block parsing
 *
 * Operator Precedence (lowest to highest):
 * 1. Assignment (:=) - right associative
 * 2. Range (..)
 * 3. Lambda (=>)
 * 4. Logical OR (or)
 * 5. Logical AND (and)
 * 6. Comparison (<, <=, >, >=)
 * 7. Addition (+, -)
 * 8. Multiplication (*, /, %)
 * 9. Unary (-, not)
 * 10. Postfix (., [], ())
 * 11. Primary (literals, identifiers, parentheses, compounds, arrays)
 */

import { TokenStream } from '../lexer/tokenstream';
import { Token, TokenType } from '../lexer/token';
import * as AST from './ast';
import { ParserState, ParseResult, ParseError } from './parser-state';
import { LiteralParser } from './parsers/literal-parser';
import { OperatorParser } from './parsers/operator-parser';
import { CompoundParser } from './parsers/compound-parser';
import { LambdaParser } from './parsers/lambda-parser';
import { DeclarationParser } from './parsers/declaration-parser';

// Re-export parser state types for consumers
export { ParserState, ParseResult, ParseError } from './parser-state';

/**
 * Main parser class that orchestrates all sub-parsers.
 *
 * This class serves as the central coordinator, delegating to specialized
 * parser modules while maintaining the overall parsing flow and precedence.
 */
export class Parser {
  // Specialized parser instances
  private literalParser: LiteralParser;
  private operatorParser: OperatorParser;
  private compoundParser: CompoundParser;
  private lambdaParser: LambdaParser;
  private declarationParser: DeclarationParser;

  constructor() {
    // Initialize all parser modules
    this.literalParser = new LiteralParser();
    this.operatorParser = new OperatorParser();
    // Compound parser needs access to general expression parsing
    this.compoundParser = new CompoundParser(this.parseExpression.bind(this));
    this.lambdaParser = new LambdaParser();
    // Declaration parser needs expression and indented compound parsers
    this.declarationParser = new DeclarationParser(
      this.parseExpression.bind(this),
      this.parseIdentedCompound.bind(this)
    );
  }

  /**
   * Main entry point for parsing any expression.
   * Starts at the lowest precedence level (assignment).
   *
   * The parser uses recursive descent with each method handling
   * a specific precedence level, calling higher precedence parsers
   * for operands.
   */
  parseExpression(state: ParserState): ParseResult<AST.Expression> {
    return this.parseAssignment(state);
  }

  /**
   * Parse assignment expressions (lowest precedence).
   * Right-associative to support chaining: a := b := c
   */
  private parseAssignment(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseAssignment(
      state,
      this.parseRange.bind(this),
      this.parseAssignment.bind(this)  // Recursive for right-associativity
    );
  }

  /**
   * Parse range expressions (e.g., 1..10).
   * Used for creating ranges of values.
   */
  private parseRange(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseRange(
      state,
      this.parseLambda.bind(this)
    );
  }

  /**
   * Parse lambda expressions or fall through to logical OR.
   * Performs lookahead to detect lambda syntax.
   */
  private parseLambda(state: ParserState): ParseResult<AST.Expression> {
    return this.lambdaParser.parseLambda(
      state,
      this.parseIdentifier.bind(this),
      this.parseLogicalOr.bind(this),
      this.parseLambdaExpression.bind(this)
    );
  }

  /**
   * Parse the actual lambda expression after detection.
   */
  private parseLambdaExpression(state: ParserState): ParseResult<AST.LambdaExpression> {
    return this.lambdaParser.parseLambdaExpression(
      state,
      this.parseIdentifier.bind(this),
      this.parseLambda.bind(this)  // Allow nested lambdas in the body
    );
  }

  /**
   * Parse logical OR expressions (or).
   * Left-associative: a or b or c -> ((a or b) or c)
   */
  private parseLogicalOr(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseBinaryOp(
      state,
      this.parseLogicalAnd.bind(this),
      this.parseLogicalAnd.bind(this),
      ['or']
    );
  }

  /**
   * Parse logical AND expressions (and).
   * Left-associative: a and b and c -> ((a and b) and c)
   */
  private parseLogicalAnd(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseBinaryOp(
      state,
      this.parseEquality.bind(this),
      this.parseEquality.bind(this),
      ['and']
    );
  }

  /**
   * Parse equality expressions (==, !=).
   * Non-associative: equality comparisons don't typically chain.
   */
  private parseEquality(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseBinaryOp(
      state,
      this.parseComparison.bind(this),
      this.parseComparison.bind(this),
      ['==', '!=']
    );
  }

  /**
   * Parse comparison expressions (<, <=, >, >=).
   * Non-associative in practice (chaining is rare).
   */
  private parseComparison(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseBinaryOp(
      state,
      this.parseAddition.bind(this),
      this.parseAddition.bind(this),
      ['<', '<=', '>', '>=']
    );
  }

  /**
   * Parse addition and subtraction (+, -).
   * Left-associative: a + b - c -> ((a + b) - c)
   */
  private parseAddition(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseBinaryOp(
      state,
      this.parseMultiplication.bind(this),
      this.parseMultiplication.bind(this),
      ['+', '-']
    );
  }

  /**
   * Parse multiplication, division, and modulo (*, /, %).
   * Left-associative: a * b / c -> ((a * b) / c)
   */
  private parseMultiplication(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseBinaryOp(
      state,
      this.parseUnary.bind(this),
      this.parseUnary.bind(this),
      ['*', '/', '%']
    );
  }

  /**
   * Parse unary expressions (-, not).
   * Right-associative: --x -> -(-(x))
   */
  private parseUnary(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parseUnary(
      state,
      this.parseUnary.bind(this),  // Recursive for right-associativity
      this.parsePostfix.bind(this)
    );
  }

  /**
   * Parse postfix expressions (member access, indexing, calls).
   * Left-associative: obj.prop.method() -> ((obj.prop).method)()
   */
  private parsePostfix(state: ParserState): ParseResult<AST.Expression> {
    return this.operatorParser.parsePostfix(
      state,
      this.parsePrimary.bind(this),
      this.parseIdentifier.bind(this),
      this.parseExpression.bind(this)
    );
  }

  /**
   * Parse primary expressions (highest precedence).
   *
   * Primary expressions are the fundamental building blocks of the language:
   *
   * LITERALS:
   * - Numbers: 42, 3.14, -5
   * - Strings: "hello", "world"
   * - Booleans: true, false
   *
   * IDENTIFIERS:
   * - Variables: x, myVar, _internal
   * - Keywords used as identifiers: array := [1,2,3]
   * - Decorators: @editable, @public
   *
   * CONTROL FLOW CONSTRUCTS:
   * - if expressions: if(condition) then: result
   * - case expressions: case(x): 0 => "zero", _ => "other"
   * - for loops: for(i : 0..10): process(i)
   * - loop expressions: loop: break
   * - block expressions: block: x := 1; x + 1
   *
   * COLLECTIONS:
   * - Array expressions: array{1, 2, 3} or array: item1; item2
   * - Compound expressions: {expr1; expr2; result}
   *
   * GROUPING:
   * - Parenthesized expressions: (a + b) * c
   *
   * DATA STRUCTURES:
   * - Anonymous classes/structs: class { field := value }
   * - Object construction: Point{x := 1, y := 2}
   *
   * CONTROL FLOW STATEMENTS:
   * - break, return (parsed as special expressions)
   */
  private parsePrimary(state: ParserState): ParseResult<AST.Expression> {
    state = state.skipTrivia();

    // Skip leading newlines (for cases like "<# comment #>\nx + y")
    while (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance().skipTrivia();
    }

    const token = state.current();

    if (!token) {
      throw new ParseError('Unexpected end of input', state.position);
    }

    // Number or string literal
    if (token.type === TokenType.INTEGER ||
        token.type === TokenType.FLOAT ||
        token.type === TokenType.STRING ||
        token.type === TokenType.INVALID_STRING) {
      return this.parseLiteral(state);
    }

    // Handle 'var' keyword for variable declarations
    if (token.type === TokenType.DECL_KEYWORD && token.content === 'var') {
      return this.parseVariableDeclaration(state);
    }

    // Handle 'set' keyword for set expressions
    if (token.type === TokenType.DECL_KEYWORD && token.content === 'set') {
      return this.parseSetExpression(state);
    }

    // Handle SPECIFIER token - likely a declaration with pre-specifiers
    if (token.type === TokenType.SPECIFIER) {
      // Try to parse as declaration
      try {
        return this.declarationParser.parseDeclaration(state);
      } catch (e) {
        // If it's not a valid declaration, throw the original error
        throw new ParseError(`Unexpected token: ${token.type}`, state.position, token);
      }
    }

    // Data structure keywords (class, struct, interface, etc.)
    if (token.type === TokenType.DATA_STRUCTURE_KEYWORD) {
      // Parse as anonymous data structure expression
      // This allows expressions like: MyClass := class { }
      return this.parseAnonymousDataStructure(state);
    }

    // Handle TYPE_KEYWORD as identifiers in expression context
    if (token.type === TokenType.TYPE_KEYWORD) {
      // Special case: option{ expression } syntax
      if (token.content === 'option') {
        return this.parseOptionExpression(state);
      }
      // Other type keywords can be used as identifiers in expressions
      return this.parseIdentifier(state);
    }

    // Handle RESERVED_WORD tokens - some are control flow statements, others can be identifiers
    if (token.type === TokenType.RESERVED_WORD) {
      // CONTROL FLOW STATEMENTS
      // These create special expression nodes for flow control within loops and functions

      if (token.content === 'break') {
        // Break statement: immediately exit current loop
        // Example: for(i : 0..10) { if(i > 5) then { break } }
        const tokenOffset = state.currentOffset();
        state = state.advance();
        const node: AST.BreakExpression = {
          type: 'BreakExpression',
          tokenOffset
        };
        return { node, state };
      }


      if (token.content === 'return') {
        // Return statement: exit function with optional value
        // Examples: return, return 42, return x + y
        const tokenOffset = state.currentOffset();
        state = state.advance();

        // Look ahead to determine if there's a return value
        // We check for tokens that could start an expression
        const nextState = state.skipTrivia();
        const nextToken = nextState.current();

        // Check for expression-starting tokens
        if (nextToken &&
            (nextToken.type === TokenType.IDENTIFIER ||
             nextToken.type === TokenType.INTEGER ||
             nextToken.type === TokenType.FLOAT ||
             nextToken.type === TokenType.STRING ||
             (nextToken.type === TokenType.OPERATOR &&
              (nextToken.content === '(' || nextToken.content === '{' ||
               nextToken.content === '[' || nextToken.content === '-')))) {
          // Parse the return value expression
          const valueResult = this.parseExpression(nextState);
          const node: AST.ReturnExpression = {
            type: 'ReturnExpression',
            tokenOffset,
            value: valueResult.node
          };
          return { node, state: valueResult.state };
        }

        // No return value (void return)
        const node: AST.ReturnExpression = {
          type: 'ReturnExpression',
          tokenOffset
        };
        return { node, state };
      }

      // CONCURRENT PROGRAMMING CONSTRUCTS
      if (token.content === 'spawn') {
        return this.parseSpawnExpression(state);
      }
      if (token.content === 'race') {
        return this.parseRaceExpression(state);
      }
      if (token.content === 'sync') {
        return this.parseSyncExpression(state);
      }
      if (token.content === 'branch') {
        return this.parseBranchExpression(state);
      }

      // OTHER RESERVED WORDS
      // Words like 'do', 'while', 'yield', etc. are reserved for future use
      // but can currently be used as identifiers in expressions for compatibility
      // Example: while := true (valid identifier usage)
      return this.parseIdentifier(state);
    }

    // Try parsing declarations first, then fall back to identifier
    if (token.type === TokenType.IDENTIFIER) {
      // Check if this is a decorator (starts with @)
      if (token.content.startsWith('@')) {
        // This is likely a decorator for a declaration
        // Let the declaration parser handle it
        return this.declarationParser.parseDeclaration(state);
      }

      // Try to parse as declaration first
      const declarationResult = this.tryParseDeclaration(state);
      if (declarationResult) {
        return declarationResult;
      }

      // Check for object construction pattern: identifier : indented-content
      // This handles cases like "Person:" with indented fields
      const idResult = this.parseIdentifier(state);
      let checkState = idResult.state.skipTrivia();

      // If the identifier is followed by ':' and then newline/indented content,
      // parse it as an object construction with indented fields
      if (checkState.current()?.type === TokenType.OPERATOR && checkState.current()?.content === ':') {
        const colonOffset = checkState.currentOffset();
        checkState = checkState.advance();

        // Skip trailing whitespace/comments on the same line
        while (checkState.current() && (checkState.current()!.type === TokenType.SPACE ||
                                        checkState.current()!.type === TokenType.TAB ||
                                        checkState.current()!.type === TokenType.COMMENT ||
                                        checkState.current()!.type === TokenType.TRIVIA)) {
          checkState = checkState.advance();
        }

        // If we see a newline, parse the indented content
        if (checkState.current()?.type === TokenType.NEWLINE) {
          // Parse as object construction with indented fields
          const fieldsResult = this.parseIndentedExpressionList(checkState);

          // Create an object construction node
          // For now, we'll represent this as an IdentedCompoundExpression with the identifier
          const node: AST.IdentedCompoundExpression = {
            type: 'IdentedCompoundExpression',
            expressions: fieldsResult.node.type === 'CompoundExpression'
              ? (fieldsResult.node as AST.CompoundExpression).expressions
              : [fieldsResult.node],
            keywordOffset: idResult.node.tokenOffset,
            colonOffset: colonOffset,
            separatorOffsets: fieldsResult.node.type === 'CompoundExpression'
              ? (fieldsResult.node as AST.CompoundExpression).separatorOffsets
              : [],
            baseIndentation: 0
          };

          return { node, state: fieldsResult.state };
        }
      }

      // Otherwise, just return the identifier
      return idResult;
    }

    // Block-forming keyword (if:, then:, array:, etc.)
    if (token.type === TokenType.BLOCK_FORMING_KEYWORD) {
      // Special handling for 'array'
      if (token.content === 'array') {
        // Check if this is being used as an identifier (array := ...)
        // or as an array literal (array{} or array:)
        const savedState = state;
        const lookAheadState = state.advance().skipTrivia();
        const next = lookAheadState.current();
        if (next && next.type === TokenType.OPERATOR &&
            (next.content === ':=' || next.content === '=' || next.content === '+=' ||
             next.content === '-=' || next.content === '*=' || next.content === '/=')) {
          // Treat 'array' as an identifier when followed by assignment
          const idNode: AST.IdentifierExpression = {
            type: 'Identifier',
            name: token.content,
            tokenOffset: savedState.currentOffset()
          };
          return { node: idNode, state: savedState.advance() };
        }
        // Otherwise parse as array literal
        return this.parseArray(savedState);
      }
      // Special handling for 'if'
      if (token.content === 'if') {
        return this.parseIfExpression(state);
      }
      // Special handling for 'for'
      if (token.content === 'for') {
        return this.parseForExpression(state);
      }
      // Special handling for 'case'
      if (token.content === 'case') {
        // Case is always a keyword, never an identifier
        // Parse as case expression
        return this.parseCaseExpression(state);
      }
      // Special handling for 'loop'
      if (token.content === 'loop') {
        // Loop is always a keyword, never an identifier
        // Parse as loop expression
        return this.parseLoopExpression(state);
      }
      // Special handling for 'block'
      if (token.content === 'block') {
        // Parse block: using indented compound parser which handles semicolons
        const compoundResult = this.parseIdentedCompound(state);

        // Wrap the indented compound in a BlockExpression
        const compound = compoundResult.node;
        const blockExpr: AST.BlockExpression = {
          type: 'BlockExpression',
          body: compound,
          blockOffset: compound.keywordOffset,
          colonOffset: compound.colonOffset
        };

        return { node: blockExpr, state: compoundResult.state };
      }

      // Other block-forming keywords are handled elsewhere (if, for)
      return this.parseIdentedCompound(state);
    }

    // Parenthesized expression or tuple literal
    if (token.type === TokenType.OPERATOR && token.content === '(') {
      const openParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Check for empty tuple/parentheses
      const nextToken = state.current();
      if (nextToken?.type === TokenType.OPERATOR && nextToken.content === ')') {
        const closeParenOffset = state.currentOffset();
        state = state.advance();

        // Empty parentheses represent an empty tuple
        const node: AST.TupleExpression = {
          type: 'TupleExpression',
          elements: [],
          openParenOffset,
          closeParenOffset,
          separatorOffsets: []
        };
        return { node, state };
      }

      // Parse the first expression
      const firstExprResult = this.parseExpression(state);
      state = firstExprResult.state.skipTrivia();

      // Check if this is a tuple (has commas) or a parenthesized expression
      const elements: AST.Expression[] = [firstExprResult.node];
      const separatorOffsets: number[] = [];

      // Look for comma-separated elements to determine if it's a tuple
      while (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance().skipTrivia();

        // Parse the next element
        const nextExprResult = this.parseExpression(state);
        elements.push(nextExprResult.node);
        state = nextExprResult.state.skipTrivia();
      }

      const closeParenOffset = state.currentOffset();
      const closeParen = state.current();
      if (!closeParen || closeParen.type !== TokenType.OPERATOR || closeParen.content !== ')') {
        throw new ParseError('Expected )', state.position, closeParen || undefined);
      }
      state = state.advance();

      // If we have more than one element, it's a tuple
      if (elements.length > 1) {
        const node: AST.TupleExpression = {
          type: 'TupleExpression',
          elements,
          openParenOffset,
          closeParenOffset,
          separatorOffsets
        };
        return { node, state };
      } else {
        // Single element, it's a parenthesized expression
        const node: AST.ParenthesizedExpression = {
          type: 'ParenthesizedExpression',
          expression: elements[0],
          openParenOffset,
          closeParenOffset
        };
        return { node, state };
      }
    }

    // Compound expression
    if (token.type === TokenType.OPERATOR && token.content === '{') {
      return this.parseCompoundExpression(state);
    }

    throw new ParseError(`Unexpected token: ${token.type}`, state.position, token);
  }

  /**
   * Parse a literal expression (public for special use cases).
   */
  parseLiteral(state: ParserState): ParseResult<AST.LiteralExpression> {
    return this.literalParser.parseLiteral(state);
  }

  /**
   * Parse an identifier expression.
   */
  private parseIdentifier(state: ParserState): ParseResult<AST.IdentifierExpression> {
    return this.literalParser.parseIdentifier(state);
  }

  /**
   * Parse an array expression (array{...} or array:).
   * Handles both syntaxes for arrays in Verse.
   *
   * array{...} - Braced array with comma-separated elements
   * array: - Indented array with newline-separated elements
   *
   * Stores offsets for:
   * - arrayKeywordOffset: Position of 'array' keyword
   * - openBraceOffset/colonOffset: Position of { or :
   * - closeBraceOffset: Position of } (for braced syntax)
   * - separatorOffsets: Positions of commas or newlines
   */
  private parseArray(state: ParserState): ParseResult<AST.ArrayExpression> {
    const arrayKeywordOffset = state.currentOffset();
    const arrayToken = state.current();

    if (!arrayToken || arrayToken.content !== 'array') {
      throw new ParseError('Expected array keyword', state.position);
    }

    state = state.advance().skipTrivia();
    const next = state.current();

    if (!next) {
      throw new ParseError('Expected { or : after array', state.position);
    }

    // array{...} syntax
    if (next.type === TokenType.OPERATOR && next.content === '{') {
      const openBraceOffset = state.currentOffset();
      state = state.advance();

      const elements: AST.Expression[] = [];
      const separatorOffsets: number[] = [];

      // Parse array elements
      while (!state.isAtEnd()) {
        state = state.skipTrivia();

        // Check for closing brace
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}') {
          break;
        }

        // Parse element
        const elementResult = this.parseExpression(state);
        elements.push(elementResult.node);
        state = elementResult.state.skipTrivia();

        // Check for comma or closing brace
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
          separatorOffsets.push(state.currentOffset());
          state = state.advance();
        } else if (!(state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}')) {
          throw new ParseError('Expected , or }', state.position, state.current() || undefined);
        }
      }

      // Verify closing brace
      const closeBraceOffset = state.currentOffset();
      const closeBrace = state.current();
      if (!closeBrace || closeBrace.type !== TokenType.OPERATOR || closeBrace.content !== '}') {
        throw new ParseError('Expected }', state.position, closeBrace || undefined);
      }
      state = state.advance();

      const node: AST.ArrayExpression = {
        type: 'ArrayExpression',
        elements,
        arrayKeywordOffset,
        openBraceOffset,
        closeBraceOffset,
        separatorOffsets
      };

      return { node, state };
    }

    // array: syntax (indented list)
    if (next.type === TokenType.OPERATOR && next.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented list of elements
      const result = this.parseIdentedList(state);

      const node: AST.ArrayExpression = {
        type: 'ArrayExpression',
        elements: result.elements,
        arrayKeywordOffset,
        colonOffset,
        separatorOffsets: result.separatorOffsets
      };

      return { node, state: result.state };
    }

    throw new ParseError('Expected { or : after array', state.position, next);
  }

  /**
   * Parse a set expression.
   *
   * Form: set x = value
   * Used for reassigning mutable variables.
   */
  private parseSetExpression(state: ParserState): ParseResult<AST.SetExpression> {
    const setOffset = state.currentOffset();
    const setToken = state.current();

    if (!setToken || setToken.type !== TokenType.DECL_KEYWORD || setToken.content !== 'set') {
      throw new ParseError('Expected set keyword', state.position, setToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Parse target expression (variable or member access)
    const targetResult = this.parsePostfix(state);
    state = targetResult.state.skipTrivia();

    // Handle line continuation before equals
    while (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance().skipTrivia();
    }

    // Expect equals operator
    const equalsOffset = state.currentOffset();
    const equalsToken = state.current();
    if (!equalsToken || equalsToken.type !== TokenType.OPERATOR || equalsToken.content !== '=') {
      throw new ParseError('Expected = after target in set expression', state.position, equalsToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Handle line continuation after equals
    while (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance().skipTrivia();
    }

    // Parse value expression
    const valueResult = this.parseExpression(state);

    const node: AST.SetExpression = {
      type: 'SetExpression',
      target: targetResult.node,
      value: valueResult.node,
      setOffset,
      equalsOffset
    };

    return { node, state: valueResult.state };
  }

  /**
   * Parse an option expression: option{ expression }
   */
  private parseOptionExpression(state: ParserState): ParseResult<AST.OptionExpression> {
    const optionOffset = state.currentOffset();
    const optionToken = state.current();

    if (!optionToken || optionToken.type !== TokenType.TYPE_KEYWORD || optionToken.content !== 'option') {
      throw new ParseError('Expected option keyword', state.position, optionToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Expect opening brace
    const openBraceOffset = state.currentOffset();
    const openBraceToken = state.current();
    if (!openBraceToken || openBraceToken.type !== TokenType.OPERATOR || openBraceToken.content !== '{') {
      throw new ParseError('Expected { after option keyword', state.position, openBraceToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Parse the inner expression
    const valueResult = this.parseExpression(state);
    state = valueResult.state.skipTrivia();

    // Expect closing brace
    const closeBraceOffset = state.currentOffset();
    const closeBraceToken = state.current();
    if (!closeBraceToken || closeBraceToken.type !== TokenType.OPERATOR || closeBraceToken.content !== '}') {
      throw new ParseError('Expected } to close option expression', state.position, closeBraceToken || undefined);
    }

    state = state.advance();

    const node: AST.OptionExpression = {
      type: 'OptionExpression',
      optionOffset,
      value: valueResult.node,
      openBraceOffset,
      closeBraceOffset
    };

    return { node, state };
  }

  /**
   * Parse a for-loop expression.
   *
   * Forms:
   * - for '(' <identifier> ':' <expression> ')' <expression>
   * - for '(' <identifier> ':' <expression> ')' ':' <indented-expression-list>
   * - for ':' <indented-expression-list> ['do' ':' <indented-expression-list>]
   */
  private parseForExpression(state: ParserState): ParseResult<AST.ForExpression> {
    const forOffset = state.currentOffset();
    const forToken = state.current();

    if (!forToken || forToken.content !== 'for') {
      throw new ParseError('Expected for keyword', state.position, forToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for indented form (for:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Handle indented for: form
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list (which contains the loop spec)
      const specResult = this.parseIndentedExpressionList(state);
      state = specResult.state.skipTrivia();

      let doOffset: number | undefined;
      let body: AST.Expression | undefined;

      // Check for optional 'do:' with indented expression list
      if (state.current()?.type === TokenType.IDENTIFIER && state.current()?.content === 'do') {
        doOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const bodyResult = this.parseIndentedExpressionList(state);
          body = bodyResult.node;
          state = bodyResult.state;
        } else {
          throw new ParseError('Expected : after do', state.position, state.current() || undefined);
        }
      }

      // For indented for: form without explicit iteration variable
      // The entire indented block is the body, and we create a dummy iterable
      // In the actual Verse language, this would be parsed differently
      const dummyIterable: AST.IdentifierExpression = {
        type: 'Identifier',
        name: '_implicit_',
        tokenOffset: colonOffset
      };

      const node: AST.ForExpression = {
        type: 'ForExpression',
        variable: '_',  // Implicit iteration variable
        variableOffset: colonOffset,
        iterable: body ? specResult.node : dummyIterable,  // If there's a do:, spec is the iterable
        body: body || specResult.node,  // Use do: body if present, otherwise spec is the body
        forOffset,
        colonOffset,
        doOffset
      };

      return { node, state };
    }

    // Check for parenthesized form
    let openParenOffset: number | undefined;
    let closeParenOffset: number | undefined;

    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '(') {
      openParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Skip comments inside parens
      while (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance().skipTrivia();
      }

      // Parse variable(s) - either "x" or "i -> x"
      let variable: string;
      let variableOffset: number;
      let indexVariable: string | undefined;
      let indexVariableOffset: number | undefined;
      let arrowOffset: number | undefined;

      const firstVarOffset = state.currentOffset();
      const firstVarToken = state.current();
      if (!firstVarToken || firstVarToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected variable name in for loop', state.position, firstVarToken || undefined);
      }
      const firstVariable = firstVarToken.content;
      state = state.advance().skipTrivia();

      // Also skip TRIVIA tokens (spaces combined by combineTrivia)
      while (state.current()?.type === TokenType.TRIVIA) {
        state = state.advance();
      }

      // Check if this is the "i -> x" syntax
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '->') {
        // This is "i -> x" syntax
        indexVariable = firstVariable;
        indexVariableOffset = firstVarOffset;
        arrowOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        // Parse the value variable
        variableOffset = state.currentOffset();
        const valueVarToken = state.current();
        if (!valueVarToken || valueVarToken.type !== TokenType.IDENTIFIER) {
          throw new ParseError('Expected value variable name after ->', state.position, valueVarToken || undefined);
        }
        variable = valueVarToken.content;
        state = state.advance().skipTrivia();

        // Also skip TRIVIA tokens
        while (state.current()?.type === TokenType.TRIVIA) {
          state = state.advance();
        }
      } else {
        // This is just "x" syntax
        variable = firstVariable;
        variableOffset = firstVarOffset;
      }

      // Expect : between variable(s) and iterable
      const colonOffset = state.currentOffset();
      if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== ':') {
        throw new ParseError('Expected : after loop variable', state.position, state.current() || undefined);
      }
      state = state.advance().skipTrivia();

      // Also skip TRIVIA tokens (spaces combined by combineTrivia)
      while (state.current()?.type === TokenType.TRIVIA) {
        state = state.advance();
      }

      // Parse iterable expression
      const iterableResult = this.parseExpression(state);
      state = iterableResult.state.skipTrivia();

      // Expect closing paren
      if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== ')') {
        throw new ParseError('Expected ) after for range', state.position, state.current() || undefined);
      }
      closeParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Check for ':' followed by indented expression list
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
        const bodyColonOffset = state.currentOffset();
        state = state.advance();

        // Parse indented expression list
        const bodyResult = this.parseIndentedExpressionList(state);

        const node: AST.ForExpression = {
          type: 'ForExpression',
          variable,
          variableOffset,
          indexVariable,
          indexVariableOffset,
          arrowOffset,
          iterable: iterableResult.node,
          body: bodyResult.node,
          forOffset,
          openParenOffset,
          closeParenOffset,
          colonOffset
        };

        return { node, state: bodyResult.state };
      }

      // Otherwise parse body as a single expression
      const bodyResult = this.parseExpression(state);

      const node: AST.ForExpression = {
        type: 'ForExpression',
        variable,
        variableOffset,
        indexVariable,
        indexVariableOffset,
        arrowOffset,
        iterable: iterableResult.node,
        body: bodyResult.node,
        forOffset,
        openParenOffset,
        closeParenOffset,
        colonOffset
      };

      return { node, state: bodyResult.state };
    }

    // Non-parenthesized form not supported for for-loops
    throw new ParseError('Expected ( or : after for keyword', state.position, state.current() || undefined);
  }

  /**
   * Parse a loop expression.
   *
   * Forms:
   * - loop <expression>
   * - loop: <indented-expression-list>
   */
  private parseLoopExpression(state: ParserState): ParseResult<AST.LoopExpression> {
    const loopOffset = state.currentOffset();
    const loopToken = state.current();

    if (!loopToken || loopToken.type !== TokenType.BLOCK_FORMING_KEYWORD || loopToken.content !== 'loop') {
      throw new ParseError('Expected loop keyword', state.position, loopToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for indented form (loop:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list
      const bodyResult = this.parseIndentedExpressionList(state);

      const node: AST.LoopExpression = {
        type: 'LoopExpression',
        body: bodyResult.node,
        loopOffset,
        colonOffset
      };

      return { node, state: bodyResult.state };
    }

    // Otherwise parse body as a single expression
    const bodyResult = this.parseExpression(state);

    const node: AST.LoopExpression = {
      type: 'LoopExpression',
      body: bodyResult.node,
      loopOffset
    };

    return { node, state: bodyResult.state };
  }

  /**
   * Parse a spawn expression.
   *
   * Forms:
   * - spawn{expression}
   * - spawn: <indented-expression-list>
   */
  private parseSpawnExpression(state: ParserState): ParseResult<AST.SpawnExpression> {
    const spawnOffset = state.currentOffset();
    const spawnToken = state.current();

    if (!spawnToken || spawnToken.content !== 'spawn') {
      throw new ParseError('Expected spawn keyword', state.position, spawnToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for brace form (spawn{...})
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '{') {
      const openBraceOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse expression inside braces
      const bodyResult = this.parseExpression(state);
      state = bodyResult.state.skipTrivia();

      // Expect closing brace
      if (!state.current() || state.current()?.content !== '}') {
        throw new ParseError('Expected }', state.position, state.current() || undefined);
      }
      const closeBraceOffset = state.currentOffset();
      state = state.advance();

      const node: AST.SpawnExpression = {
        type: 'SpawnExpression',
        body: bodyResult.node,
        spawnOffset,
        openBraceOffset,
        closeBraceOffset
      };

      return { node, state };
    }

    // Check for indented form (spawn:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list
      const bodyResult = this.parseIndentedExpressionList(state);

      const node: AST.SpawnExpression = {
        type: 'SpawnExpression',
        body: bodyResult.node,
        spawnOffset,
        colonOffset
      };

      return { node, state: bodyResult.state };
    }

    // No body form - just spawn as a statement
    throw new ParseError('Expected { or : after spawn', state.position, state.current() || undefined);
  }

  /**
   * Parse a race expression.
   *
   * Forms:
   * - race: <indented-expression-list>
   */
  private parseRaceExpression(state: ParserState): ParseResult<AST.RaceExpression> {
    const raceOffset = state.currentOffset();
    const raceToken = state.current();

    if (!raceToken || raceToken.content !== 'race') {
      throw new ParseError('Expected race keyword', state.position, raceToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for indented form (race:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list
      const bodyResult = this.parseIndentedExpressionList(state);

      // Extract branches from compound expression
      let branches: AST.Expression[];
      if (bodyResult.node.type === 'CompoundExpression' || bodyResult.node.type === 'IdentedCompoundExpression') {
        branches = (bodyResult.node as any).expressions || [];
      } else {
        branches = [bodyResult.node];
      }

      const node: AST.RaceExpression = {
        type: 'RaceExpression',
        branches,
        raceOffset,
        colonOffset
      };

      return { node, state: bodyResult.state };
    }

    throw new ParseError('Expected : after race', state.position, state.current() || undefined);
  }

  /**
   * Parse a sync expression.
   *
   * Forms:
   * - sync: <indented-expression-list>
   */
  private parseSyncExpression(state: ParserState): ParseResult<AST.SyncExpression> {
    const syncOffset = state.currentOffset();
    const syncToken = state.current();

    if (!syncToken || syncToken.content !== 'sync') {
      throw new ParseError('Expected sync keyword', state.position, syncToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for indented form (sync:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list
      const bodyResult = this.parseIndentedExpressionList(state);

      // Extract operations from compound expression
      let operations: AST.Expression[];
      if (bodyResult.node.type === 'CompoundExpression' || bodyResult.node.type === 'IdentedCompoundExpression') {
        operations = (bodyResult.node as any).expressions || [];
      } else {
        operations = [bodyResult.node];
      }

      const node: AST.SyncExpression = {
        type: 'SyncExpression',
        operations,
        syncOffset,
        colonOffset
      };

      return { node, state: bodyResult.state };
    }

    throw new ParseError('Expected : after sync', state.position, state.current() || undefined);
  }

  /**
   * Parse a branch expression.
   *
   * Forms:
   * - branch: <indented-expression-list>
   * - branch{expression1, expression2}
   */
  private parseBranchExpression(state: ParserState): ParseResult<AST.BranchExpression> {
    const branchOffset = state.currentOffset();
    const branchToken = state.current();

    if (!branchToken || branchToken.content !== 'branch') {
      throw new ParseError('Expected branch keyword', state.position, branchToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for brace form (branch{...})
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '{') {
      const openBraceOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse expressions inside braces
      const branches: AST.Expression[] = [];
      while (state.current() && state.current()?.content !== '}') {
        const exprResult = this.parseExpression(state);
        branches.push(exprResult.node);
        state = exprResult.state.skipTrivia();

        // Check for comma separator
        if (state.current()?.content === ',') {
          state = state.advance().skipTrivia();
        }
      }

      // Expect closing brace
      if (!state.current() || state.current()?.content !== '}') {
        throw new ParseError('Expected }', state.position, state.current() || undefined);
      }
      const closeBraceOffset = state.currentOffset();
      state = state.advance();

      const node: AST.BranchExpression = {
        type: 'BranchExpression',
        branches,
        branchOffset,
        openBraceOffset,
        closeBraceOffset
      };

      return { node, state };
    }

    // Check for indented form (branch:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list
      const bodyResult = this.parseIndentedExpressionList(state);

      // Extract branches from compound expression
      let branches: AST.Expression[];
      if (bodyResult.node.type === 'CompoundExpression' || bodyResult.node.type === 'IdentedCompoundExpression') {
        branches = (bodyResult.node as any).expressions || [];
      } else {
        branches = [bodyResult.node];
      }

      const node: AST.BranchExpression = {
        type: 'BranchExpression',
        branches,
        branchOffset,
        colonOffset
      };

      return { node, state: bodyResult.state };
    }

    throw new ParseError('Expected { or : after branch', state.position, state.current() || undefined);
  }

  /**
   * Helper method to continue parsing if expression after the condition
   */
  private parseIfWithCondition(state: ParserState, ifOffset: number, condition: AST.Expression): ParseResult<AST.IfExpression> {
    // Look for then/else
    let thenBranch: AST.Expression | undefined;
    let thenOffset: number | undefined;
    let elseBranch: AST.Expression | undefined;
    let elseOffset: number | undefined;

    // Skip trivia to check for 'then' keyword or ':'
    if (!state.indentationSensitive) {
      state = state.skipTrivia();
    }

    // Check for ':' after condition (indented form without explicit 'then')
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Parse indented body as implicit then branch
      state = state.advance();
      const thenResult = this.parseIndentedExpressionList(state);
      thenBranch = thenResult.node;
      state = thenResult.state.skipTrivia();

      // Check for 'else:' with indented expression list
      if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
        elseOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const elseResult = this.parseIndentedExpressionList(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        } else {
          // else without colon - parse as regular expression
          const elseResult = this.parseExpression(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        }
      }
    }
    // Check for 'then' keyword
    else if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'then') {
      thenOffset = state.currentOffset();
      state = state.advance();
      if (!state.indentationSensitive) {
        state = state.skipTrivia();
      }

      // Check if 'then' is followed by ':' for indented form
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
        state = state.advance();
        const thenResult = this.parseIndentedExpressionList(state);
        thenBranch = thenResult.node;
        state = thenResult.state.skipTrivia();
      } else {
        // Parse then expression normally
        const thenResult = this.parseExpression(state);
        thenBranch = thenResult.node;
        state = state.indentationSensitive ? thenResult.state : thenResult.state.skipTrivia();
      }

      // Check for 'else' keyword
      if (!state.indentationSensitive) {
        state = state.skipTrivia();
      }

      if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
        elseOffset = state.currentOffset();
        state = state.advance();

        if (!state.indentationSensitive) {
          state = state.skipTrivia();
        }

        // Check if 'else' is followed by ':' for indented form
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const elseResult = this.parseIndentedExpressionList(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        } else {
          // Parse else expression normally
          const elseResult = this.parseExpression(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        }
      }
    }

    const node: AST.IfExpression = {
      type: 'IfExpression',
      condition,
      ifOffset,
      thenBranch,
      thenOffset,
      elseBranch,
      elseOffset
    };

    return { node, state };
  }

  /**
   * Parse a block expression.
   *
   * Form:
   * - block: <indented-expression-list>
   */
  private parseBlockExpression(state: ParserState): ParseResult<AST.BlockExpression> {
    const blockOffset = state.currentOffset();
    const blockToken = state.current();

    if (!blockToken || blockToken.type !== TokenType.BLOCK_FORMING_KEYWORD || blockToken.content !== 'block') {
      throw new ParseError('Expected block keyword', state.position, blockToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Expect ':' for indented form
    if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== ':') {
      throw new ParseError('Expected : after block', state.position, state.current() || undefined);
    }
    const colonOffset = state.currentOffset();
    state = state.advance();

    // Parse indented expression list
    const bodyResult = this.parseIndentedExpressionList(state);

    const node: AST.BlockExpression = {
      type: 'BlockExpression',
      body: bodyResult.node,
      blockOffset,
      colonOffset
    };

    return { node, state: bodyResult.state };
  }

  /**
   * Parse a case expression - pattern matching construct.
   *
   * Case expressions provide pattern matching against a scrutinee value:
   *
   * BRACED FORM:
   * case(value) {
   *   0 => "zero",
   *   1 => "one",
   *   _ => "other"
   * }
   *
   * INDENTED FORM:
   * case(value):
   *   0 => "zero"
   *   1 => "one"
   *   _ => "other"
   *
   * REQUIREMENTS:
   * - Must have at least one branch
   * - Each branch must have a pattern (literal, identifier, or wildcard '_')
   * - Each branch must have a non-empty body expression after '=>'
   * - Empty branches (pattern =>) are rejected as errors
   * - In indented form, no content allowed on same line as ':'
   *
   * SUPPORTED PATTERNS:
   * - Literals: 0, "text", true
   * - Identifiers: x, myVar
   * - Wildcard: _ (matches anything, typically used as default)
   *
   * NOT YET SUPPORTED:
   * - Arithmetic patterns: x + 1, a * b
   * - Complex expressions as patterns
   */
  private parseCaseExpression(state: ParserState): ParseResult<AST.CaseExpression> {
    const caseOffset = state.currentOffset();
    const caseToken = state.current();

    if (!caseToken || caseToken.content !== 'case') {
      throw new ParseError('Expected case keyword', state.position, caseToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Expect '('
    if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== '(') {
      throw new ParseError('Expected ( after case', state.position, state.current() || undefined);
    }
    const openParenOffset = state.currentOffset();
    state = state.advance().skipTrivia();

    // Parse scrutinee expression
    const scrutineeResult = this.parseExpression(state);
    state = scrutineeResult.state.skipTrivia();

    // Expect ')'
    if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== ')') {
      throw new ParseError('Expected ) after case expression', state.position, state.current() || undefined);
    }
    const closeParenOffset = state.currentOffset();
    state = state.advance().skipTrivia();

    // Check for brace form or indented form
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '{') {
      // Brace form: case(x) { 0 => a, 1 => b }
      const openBraceOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      const branches: AST.CaseBranch[] = [];

      // Parse branches
      while (state.current() && !(state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}')) {
        const branchResult = this.parseCaseBranch(state);
        branches.push(branchResult.node);
        state = branchResult.state.skipTrivia();

        // Check for comma separator
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
          state = state.advance().skipTrivia();
        }
      }

      // Expect '}'
      if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== '}') {
        throw new ParseError('Expected } to close case expression', state.position, state.current() || undefined);
      }
      const closeBraceOffset = state.currentOffset();
      state = state.advance();

      // Case expressions must have at least one branch
      if (branches.length === 0) {
        throw new ParseError('Case expression must have at least one branch', state.position);
      }

      const node: AST.CaseExpression = {
        type: 'CaseExpression',
        scrutinee: scrutineeResult.node,
        branches,
        caseOffset,
        openParenOffset,
        closeParenOffset,
        openBraceOffset,
        closeBraceOffset
      };

      return { node, state };

    } else if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Indented form: case(x): \n  0 => a \n  1 => b
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Skip any trailing whitespace/comments on the same line
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }

      // Require a newline after the colon for indented form
      if (state.current()?.type !== TokenType.NEWLINE && !state.isAtEnd()) {
        throw new ParseError('Case expression with : requires content on next line', state.position, state.current() || undefined);
      }

      // Parse indented case branches
      const branchesResult = this.parseIndentedCaseBranches(state);

      // Case expressions must have at least one branch
      if (branchesResult.branches.length === 0) {
        throw new ParseError('Case expression must have at least one branch', branchesResult.state.position);
      }

      const node: AST.CaseExpression = {
        type: 'CaseExpression',
        scrutinee: scrutineeResult.node,
        branches: branchesResult.branches,
        caseOffset,
        openParenOffset,
        closeParenOffset,
        colonOffset
      };

      return { node, state: branchesResult.state };

    } else {
      throw new ParseError('Expected { or : after case expression', state.position, state.current() || undefined);
    }
  }

  /**
   * Parse a single case branch (pattern => expression).
   */
  private parseCaseBranch(state: ParserState): ParseResult<AST.CaseBranch> {
    // Parse pattern (literal, identifier, or wildcard)
    let pattern: AST.Expression | '_';

    if (state.current()?.type === TokenType.IDENTIFIER && state.current()?.content === '_') {
      // Wildcard pattern
      pattern = '_';
      state = state.advance();
      // Skip only non-newline trivia in indentation context
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }
    } else if (state.current()?.type === TokenType.INTEGER ||
               state.current()?.type === TokenType.FLOAT ||
               state.current()?.type === TokenType.STRING ||
               state.current()?.type === TokenType.INVALID_STRING) {
      // Literal pattern
      const literalResult = this.parseLiteral(state);
      pattern = literalResult.node;
      state = literalResult.state;
      // Skip only non-newline trivia in indentation context
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }
    } else if (state.current()?.type === TokenType.IDENTIFIER) {
      // Identifier pattern
      const idResult = this.parseIdentifier(state);
      pattern = idResult.node;
      state = idResult.state;
      // Skip only non-newline trivia in indentation context
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }
    } else {
      throw new ParseError('Expected pattern in case branch', state.position, state.current() || undefined);
    }

    // Expect '=>'
    if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== '=>') {
      throw new ParseError('Expected => in case branch', state.position, state.current() || undefined);
    }
    const arrowOffset = state.currentOffset();
    state = state.advance();
    // Skip only non-newline trivia in indentation context
    while (state.current() && (state.current()!.type === TokenType.SPACE ||
                               state.current()!.type === TokenType.TAB ||
                               state.current()!.type === TokenType.COMMENT ||
                               state.current()!.type === TokenType.TRIVIA)) {
      state = state.advance();
    }

    // Parse body expression
    // Check if body is on next line (indented block) or same line
    let bodyResult: ParseResult<AST.Expression>;
    if (state.current()?.type === TokenType.NEWLINE) {
      // Body is on next line - parse as indented block
      bodyResult = this.parseIndentedExpressionList(state);

      // Check if the result is an empty compound expression (no indented content found)
      if (bodyResult.node.type === 'CompoundExpression' &&
          (bodyResult.node as AST.CompoundExpression).expressions.length === 0) {
        throw new ParseError('Expected expression after => in case branch (empty case branch not allowed)', state.position);
      }
    } else if (state.current() &&
               state.current()!.type !== TokenType.OPERATOR &&
               state.current()!.type !== TokenType.NEWLINE &&
               !state.isAtEnd()) {
      // Body is on same line - parse single expression
      // But check that we actually have something to parse
      // (not another operator like '}' or ',')
      bodyResult = this.parseExpression(state);
    } else {
      // No body expression after =>
      // This includes cases where we immediately see '}', ',', EOF, or just whitespace
      throw new ParseError('Expected expression after => in case branch', state.position, state.current() || undefined);
    }

    // After parsing the body, skip any trailing whitespace on the same line
    // but DO NOT skip newlines as they're significant in indented contexts
    let bodyState = bodyResult.state;
    while (bodyState.current() &&
           (bodyState.current()!.type === TokenType.SPACE ||
            bodyState.current()!.type === TokenType.TAB)) {
      bodyState = bodyState.advance();
    }

    const node: AST.CaseBranch = {
      type: 'CaseBranch',
      pattern,
      body: bodyResult.node,
      arrowOffset
    };

    return { node, state: bodyState };
  }

  /**
   * Parse indented case branches.
   */
  private parseIndentedCaseBranches(state: ParserState): { branches: AST.CaseBranch[], state: ParserState } {
    // Skip any trailing whitespace on the current line (but not newlines)
    while (state.current() && (state.current()!.type === TokenType.SPACE ||
                               state.current()!.type === TokenType.TAB ||
                               state.current()!.type === TokenType.COMMENT ||
                               state.current()!.type === TokenType.TRIVIA)) {
      state = state.advance();
    }

    // Skip empty lines (lines with only whitespace/comments) to find actual content
    while (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance();
      // Skip whitespace and comments on this line
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }
      // If we hit another newline, this was an empty line, continue
      if (state.current()?.type === TokenType.NEWLINE) {
        continue;
      }
      // Otherwise we found content, break
      break;
    }

    // Now look for the indentation of the first content line
    const nextLineIndent = state.current()?.position.column;

    if (nextLineIndent === undefined || nextLineIndent === null || state.isAtEnd()) {
      throw new ParseError('Expected indented case branches', state.position);
    }

    // Enter indentation context with the detected indentation level
    state = state.enterIndentationContext(nextLineIndent);

    const branches: AST.CaseBranch[] = [];

    // Parse multiple branches separated by newlines
    while (!state.isAtEnd()) {
      // Skip any whitespace and trivia at the start of the line
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }

      // Get current position to check indentation
      const currentToken = state.current();
      if (!currentToken) break;

      // Check if we've outdented
      if (currentToken.position.column < nextLineIndent) {
        // Outdented from the block - we're done
        break;
      }

      // Skip any remaining trivia before parsing
      while (state.current() && (state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }

      // If we hit a newline after comments, skip it and continue
      if (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance();
        continue;
      }

      // Parse a case branch
      const branchResult = this.parseCaseBranch(state);
      branches.push(branchResult.node);
      state = branchResult.state;

      // The state is now positioned after the body expression
      // Skip any trailing whitespace/comments on the same line (already done in parseCaseBranch)
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }

      // Check for newline to continue with next branch
      if (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance();

        // Skip completely empty lines
        while (state.current()?.type === TokenType.NEWLINE) {
          state = state.advance();
        }
        // Continue to parse next branch
      } else if (state.isAtEnd()) {
        // End of input, we're done
        break;
      } else {
        // There's more content on the same line - could be another branch
        // Continue loop to try parsing it
        continue;
      }
    }

    if (branches.length === 0) {
      throw new ParseError('Expected at least one case branch', state.position);
    }

    // Exit the indentation context
    state = state.exitIndentationContext();

    return { branches, state };
  }

  /**
   * Parse an if-expression.
   *
   * Forms:
   * - if <expression> [then <expression> [else <expression>]]
   * - if: <indented-expression-list> [then: <indented-expression-list> [else: <indented-expression-list>]]
   */
  private parseIfExpression(state: ParserState): ParseResult<AST.IfExpression> {
    const ifOffset = state.currentOffset();
    const ifToken = state.current();

    if (!ifToken || ifToken.content !== 'if') {
      throw new ParseError('Expected if keyword', state.position, ifToken || undefined);
    }

    state = state.advance().skipTrivia();

    // Check for parenthesized condition with dot format
    // Pattern: if (condition). expressions
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '(') {
      const openParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse the condition inside parentheses
      const condResult = this.parseExpression(state);
      state = condResult.state.skipTrivia();

      // Check for closing paren
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')') {
        const closeParenOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        // Now check for dot
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '.') {
          // This is dot format!
          const dotOffset = state.currentOffset();
          state = state.advance().skipTrivia();

          // Parse expressions separated by semicolons until we hit 'else' or end
          const expressions: AST.Expression[] = [];

          while (state.current()) {
            // Check if we've hit 'else'
            if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
              break;
            }

            // Parse an expression
            const exprResult = this.parseExpression(state);
            expressions.push(exprResult.node);
            state = exprResult.state.skipTrivia();

            // Check for semicolon separator
            if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ';') {
              state = state.advance().skipTrivia();
            } else if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
              // Reached else, stop
              break;
            } else {
              // No more expressions
              break;
            }
          }

          let thenBranch: AST.Expression | undefined;
          let elseBranch: AST.Expression | undefined;
          let elseOffset: number | undefined;
          let elseDotOffset: number | undefined;

          // If we have expressions, create the then branch
          if (expressions.length === 1) {
            thenBranch = expressions[0];
          } else if (expressions.length > 1) {
            // Create a compound expression
            thenBranch = {
              type: 'CompoundExpression',
              expressions,
              openBraceOffset: dotOffset,
              closeBraceOffset: state.currentOffset(),
              separatorOffsets: []
            } as AST.CompoundExpression;
          }

          // Check for 'else' with dot format
          if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
            elseOffset = state.currentOffset();
            state = state.advance().skipTrivia();

            // Check for '.' after else
            if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '.') {
              elseDotOffset = state.currentOffset();
              state = state.advance().skipTrivia();

              // Parse expressions after else.
              const elseExpressions: AST.Expression[] = [];

              while (state.current()) {
                // Parse an expression
                const exprResult = this.parseExpression(state);
                elseExpressions.push(exprResult.node);
                state = exprResult.state.skipTrivia();

                // Check for semicolon separator
                if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ';') {
                  state = state.advance().skipTrivia();
                } else {
                  // No more expressions
                  break;
                }
              }

              // If we have expressions, create the else branch
              if (elseExpressions.length === 1) {
                elseBranch = elseExpressions[0];
              } else if (elseExpressions.length > 1) {
                // Create a compound expression
                elseBranch = {
                  type: 'CompoundExpression',
                  expressions: elseExpressions,
                  openBraceOffset: elseDotOffset,
                  closeBraceOffset: state.currentOffset(),
                  separatorOffsets: []
                } as AST.CompoundExpression;
              }
            }
          }

          // Create a parenthesized expression for the condition to preserve parentheses
          const parenCondition: AST.ParenthesizedExpression = {
            type: 'ParenthesizedExpression',
            expression: condResult.node,
            openParenOffset,
            closeParenOffset
          };

          const node: AST.IfExpression = {
            type: 'IfExpression',
            condition: parenCondition,
            ifOffset,
            thenBranch,
            dotOffset,
            elseBranch,
            elseOffset,
            elseDotOffset
          };

          return { node, state };
        }

        // Not dot format, continue with normal parsing
        // Create a parenthesized expression and continue
        const parenCondition: AST.ParenthesizedExpression = {
          type: 'ParenthesizedExpression',
          expression: condResult.node,
          openParenOffset,
          closeParenOffset: state.currentOffset() - 1
        };

        // Not dot format, fall back to normal parsing with parenthesized condition
        return this.parseIfWithCondition(state, ifOffset, parenCondition);
      }
    }

    // Fall back to the original parsing for non-parenthesized conditions

    // Check for indented form (if:)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Handle indented if: form
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Parse indented expression list as condition
      const condResult = this.parseIndentedExpressionList(state);
      state = condResult.state.skipTrivia();

      let thenBranch: AST.Expression | undefined;
      let thenOffset: number | undefined;
      let elseBranch: AST.Expression | undefined;
      let elseOffset: number | undefined;

      // Check for 'then:' with indented expression list
      if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'then') {
        thenOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const thenResult = this.parseIndentedExpressionList(state);
          thenBranch = thenResult.node;
          state = thenResult.state.skipTrivia();

          // Check for 'else:' with indented expression list
          if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
            elseOffset = state.currentOffset();
            state = state.advance().skipTrivia();

            if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
              state = state.advance();
              const elseResult = this.parseIndentedExpressionList(state);
              elseBranch = elseResult.node;
              state = elseResult.state;
            }
          }
        }
      }
      // Check for 'else:' directly (without explicit 'then')
      // In this case, there is NO then branch - the condition block is just the condition
      else if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
        elseOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const elseResult = this.parseIndentedExpressionList(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        } else {
          // else without colon - parse as regular expression
          const elseResult = this.parseExpression(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        }
      }

      const node: AST.IfExpression = {
        type: 'IfExpression',
        condition: condResult.node,
        ifOffset,
        thenBranch,
        thenOffset,
        elseBranch,
        elseOffset
      };

      return { node, state };
    }

    // Parse non-indented form: if <expression> [then <expression> [else <expression>]]
    const condResult = this.parseExpression(state);
    // Only skip trivia if not in indentation-sensitive context
    state = state.indentationSensitive ? condResult.state : condResult.state.skipTrivia();

    // Look for then/else
    let thenBranch: AST.Expression | undefined;
    let thenOffset: number | undefined;
    let elseBranch: AST.Expression | undefined;
    let elseOffset: number | undefined;

    // Skip trivia to check for 'then' keyword or ':'
    if (!state.indentationSensitive) {
      state = state.skipTrivia();
    }

    // Check for ':' after condition (indented form without explicit 'then')
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Parse indented body as implicit then branch
      state = state.advance();
      const thenResult = this.parseIndentedExpressionList(state);
      thenBranch = thenResult.node;
      state = thenResult.state.skipTrivia();

      // Check for 'else:' with indented expression list
      if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
        elseOffset = state.currentOffset();
        state = state.advance().skipTrivia();

        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const elseResult = this.parseIndentedExpressionList(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        } else {
          // else without colon - parse as regular expression
          const elseResult = this.parseExpression(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        }
      }
    }
    // Check for 'then' keyword
    else if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'then') {
      thenOffset = state.currentOffset();
      state = state.advance();
      if (!state.indentationSensitive) {
        state = state.skipTrivia();
      }

      // Check if 'then' is followed by ':' for indented form
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
        state = state.advance();
        const thenResult = this.parseIndentedExpressionList(state);
        thenBranch = thenResult.node;
        state = thenResult.state.skipTrivia();
      } else {
        // Parse then expression normally
        const thenResult = this.parseExpression(state);
        thenBranch = thenResult.node;
        // Only skip trivia if we're not in an indentation-sensitive context
        // (in compound expressions, newlines are significant separators)
        state = state.indentationSensitive ? thenResult.state : thenResult.state.skipTrivia();
      }

      // Skip trivia to check for 'else' keyword
      if (!state.indentationSensitive) {
        state = state.skipTrivia();
      }

      // Check for 'else' keyword
      if (state.current()?.type === TokenType.BLOCK_FORMING_KEYWORD && state.current()?.content === 'else') {
        elseOffset = state.currentOffset();
        state = state.advance();
        if (!state.indentationSensitive) {
          state = state.skipTrivia();
        }

        // Check if 'else' is followed by ':' for indented form
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          state = state.advance();
          const elseResult = this.parseIndentedExpressionList(state);
          elseBranch = elseResult.node;
          state = elseResult.state;
        } else {
          // Parse else expression normally
          const elseResult = this.parseExpression(state);
          elseBranch = elseResult.node;
          // Don't skip trivia in indentation-sensitive contexts
          state = state.indentationSensitive ? elseResult.state : elseResult.state;
        }
      }
    }

    const node: AST.IfExpression = {
      type: 'IfExpression',
      condition: condResult.node,
      ifOffset,
      thenBranch,
      thenOffset,
      elseBranch,
      elseOffset
    };

    return { node, state };
  }

  /**
   * Parse an anonymous data structure (class, struct, interface, etc.)
   *
   * Examples:
   * - class { }
   * - class(BaseClass) { field := 1 }
   * - interface { Method(): void }
   */
  private parseAnonymousDataStructure(state: ParserState): ParseResult<AST.Expression> {
    const kindOffset = state.currentOffset();
    const kindToken = state.current();

    if (!kindToken || kindToken.type !== TokenType.DATA_STRUCTURE_KEYWORD) {
      throw new ParseError('Expected data structure keyword', state.position, kindToken || undefined);
    }

    const kind = kindToken.content;
    state = state.advance().skipTrivia();

    // For now, create a simple identifier expression for the class keyword
    // In a full implementation, this would create a proper ClassExpression node
    const node: AST.IdentifierExpression = {
      type: 'Identifier',
      name: kind,
      tokenOffset: kindOffset
    };

    // Check for inheritance/parameters (BaseClass)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '(') {
      // Skip the parenthesized content for now
      let parenDepth = 1;
      state = state.advance();
      while (!state.isAtEnd() && parenDepth > 0) {
        if (state.current()?.type === TokenType.OPERATOR) {
          if (state.current()?.content === '(') parenDepth++;
          else if (state.current()?.content === ')') parenDepth--;
        }
        state = state.advance();
      }
      state = state.skipTrivia();
    }

    // Check for body { } or :
    const bodyToken = state.current();
    if (bodyToken && bodyToken.type === TokenType.OPERATOR && bodyToken.content === '{') {
      // Parse braced body to validate its contents
      state = state.advance();

      // Use the declaration parser to validate the body contents
      if (kind === 'enum') {
        // For enums, validate enum member syntax
        this.validateEnumBody(state);
      } else {
        // For classes/interfaces/etc, validate declaration syntax
        this.validateDataStructureBody(state, '}', kind);
      }

      // Skip to the closing brace
      let braceDepth = 1;
      while (!state.isAtEnd() && braceDepth > 0) {
        if (state.current()?.type === TokenType.OPERATOR) {
          if (state.current()?.content === '{') braceDepth++;
          else if (state.current()?.content === '}') braceDepth--;
        }
        state = state.advance();
      }
    } else if (bodyToken && bodyToken.type === TokenType.OPERATOR && bodyToken.content === ':') {
      // Parse indented content to validate it
      state = state.advance();
      const nextLineIndent = state.getNextLineIndentation();
      if (nextLineIndent !== null) {
        state = state.enterIndentationContext(nextLineIndent);
        // Skip to first declaration
        state = state.skipTrivia();
        while (state.current()?.type === TokenType.NEWLINE) {
          state = state.advance().skipTrivia();
        }

        if (kind === 'enum') {
          this.validateIndentedEnumBody(state, nextLineIndent);
        } else {
          this.validateIndentedDataStructureBody(state, nextLineIndent, kind);
        }

        // Skip the remaining indented content
        while (!state.isAtEnd() && state.currentIndentationLevel >= nextLineIndent) {
          state = state.advance();
        }
        state = state.exitIndentationContext();
      }
    } else {
      throw new ParseError('Expected { or : for data structure body', state.position, state.current() || undefined);
    }

    return { node, state };
  }

  /**
   * Parse an indented expression list after a colon.
   * Can be either multiple expressions with newlines OR just a newline (empty).
   * Used for if:, then:, else:, for:, block: constructs.
   */
  private parseIndentedExpressionList(state: ParserState): ParseResult<AST.Expression> {
    // Skip any trailing whitespace on the current line (but not newlines)
    while (state.current() && (state.current()!.type === TokenType.SPACE ||
                               state.current()!.type === TokenType.TAB ||
                               state.current()!.type === TokenType.COMMENT ||
                               state.current()!.type === TokenType.TRIVIA)) {
      state = state.advance();
    }

    // Skip empty lines (lines with only whitespace/comments) to find actual content
    while (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance();
      // Skip whitespace and comments on this line
      while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                 state.current()!.type === TokenType.TAB ||
                                 state.current()!.type === TokenType.COMMENT ||
                                 state.current()!.type === TokenType.TRIVIA)) {
        state = state.advance();
      }
      // If we hit another newline, this was an empty line, continue
      if (state.current()?.type === TokenType.NEWLINE) {
        continue;
      }
      // Otherwise we found content, break
      break;
    }

    // Now look for the indentation of the first content line
    const nextLineIndent = state.current()?.position.column;

    if (nextLineIndent === undefined || nextLineIndent === null || state.isAtEnd()) {
      // No indented content - return empty compound
      const emptyCompound: AST.CompoundExpression = {
        type: 'CompoundExpression',
        expressions: [],
        openBraceOffset: state.currentOffset(),
        closeBraceOffset: state.currentOffset(),
        separatorOffsets: []
      };
      return { node: emptyCompound, state };
    }

    // Enter indentation context with the detected indentation level
    state = state.enterIndentationContext(nextLineIndent);

    const expressions: AST.Expression[] = [];
    const separatorOffsets: number[] = [];

    // Parse multiple expressions separated by newlines
    let isDone = false;
    while (!state.isAtEnd() && !isDone) {
      // Skip empty lines within the indented block
      while (state.current()?.type === TokenType.NEWLINE) {
        const newlineOffset = state.currentOffset();
        state = state.advance();

        // Skip whitespace and comments
        while (state.current() && (state.current()!.type === TokenType.SPACE ||
                                   state.current()!.type === TokenType.TAB ||
                                   state.current()!.type === TokenType.COMMENT ||
                                   state.current()!.type === TokenType.TRIVIA)) {
          state = state.advance();
        }

        // If we hit another newline, continue to skip more empty lines
        if (state.current()?.type === TokenType.NEWLINE) {
          continue;
        }

        // If there's no current token, look ahead to see if the block truly ends
        if (!state.current()) {
          // Reached EOF, exit the block
          isDone = true;
          break;
        }

        // Check if we've dedented out of this block
        if (state.hasDedented(state.current()!.position.column)) {
          // We've dedented, exit the block
          isDone = true;
          break;
        }

        // We found content at the right indentation, add the separator for the previous expression if we have one
        if (expressions.length > 0) {
          separatorOffsets.push(newlineOffset);
        }
        break;
      }

      if (isDone) break;

      // Check if we're done
      const currentToken = state.current();
      if (!currentToken) {
        break;
      }
      // Check if we've dedented out of this block
      if (state.hasDedented(currentToken.position.column)) {
        // Dedented - exit the block
        break;
      }
      // Check for inconsistent indentation (over-indented)
      if (currentToken.position.column > nextLineIndent) {
        // Over-indented - this is an error, exit the block leaving this token unparsed
        break;
      }

      // Check for 'else' or 'then' at the base indentation level
      // These should terminate the expression list as they belong to a parent if statement
      if (currentToken.position.column === nextLineIndent &&
          currentToken.type === TokenType.BLOCK_FORMING_KEYWORD &&
          (currentToken.content === 'else' || currentToken.content === 'then')) {
        // These keywords at base indentation terminate the expression list
        break;
      }

      // Parse expression
      const exprResult = this.parseExpression(state);
      expressions.push(exprResult.node);
      state = exprResult.state;

      // Skip any trailing trivia on the same line (e.g., comments)
      while (state.current() && state.current()!.type === TokenType.TRIVIA) {
        state = state.advance();
      }

      // Check for semicolon separator on the same line
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ';') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();

        // Skip any trailing trivia after semicolon
        while (state.current() && state.current()!.type === TokenType.TRIVIA) {
          state = state.advance();
        }
      }
    }

    state = state.exitIndentationContext();

    // If we have a single expression, return it directly
    if (expressions.length === 1 && separatorOffsets.length === 0) {
      return { node: expressions[0], state };
    }

    // Otherwise, wrap in a compound expression
    const compound: AST.CompoundExpression = {
      type: 'CompoundExpression',
      expressions,
      openBraceOffset: 0, // Virtual offset for indented compound
      closeBraceOffset: 0, // Virtual offset for indented compound
      separatorOffsets
    };

    return { node: compound, state };
  }

  /**
   * Parse a branch body for then/else.
   * Can be {compound}, : indented, or expression.
   */
  private parseBranchBody(state: ParserState, keyword: string): ParseResult<AST.Expression> {
    state = state.skipTrivia();

    // Check for braced body
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '{') {
      return this.parseCompoundExpression(state);
    }

    // Check for indented body
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      const colonOffset = state.currentOffset();
      state = state.advance();

      // Look ahead to find the indentation of the next line
      const nextLineIndent = state.getNextLineIndentation();

      if (nextLineIndent !== null) {
        // Enter indentation context and parse the body as compound
        state = state.enterIndentationContext(nextLineIndent);
        state = state.skipTrivia();
        if (state.current()?.type === TokenType.NEWLINE) {
          state = state.advance().skipTrivia();
        }

        // Parse multiple expressions in the indented block
        const expressions: AST.Expression[] = [];
        const separatorOffsets: number[] = [];

        while (!state.isAtEnd()) {
          const currentToken = state.current();
          if (!currentToken) break;

          // Check if we're still indented
          if (currentToken.position.column < nextLineIndent) {
            break;
          }

          // Parse expression
          const exprResult = this.parseExpression(state);
          expressions.push(exprResult.node);
          state = exprResult.state.skipTrivia();

          // Check for newline separator
          if (state.current()?.type === TokenType.NEWLINE) {
            separatorOffsets.push(state.currentOffset());
            state = state.advance().skipTrivia();
          } else if (!state.isAtEnd() && state.current() && state.current()!.position.column >= nextLineIndent) {
            // Continue if still indented
            continue;
          } else {
            break;
          }
        }

        state = state.exitIndentationContext();

        // Create an indented compound expression
        const node: AST.IdentedCompoundExpression = {
          type: 'IdentedCompoundExpression',
          expressions,
          keywordOffset: colonOffset - 1,  // Approximate position
          colonOffset,
          separatorOffsets,
          baseIndentation: nextLineIndent
        };

        return { node, state };
      }

      throw new ParseError(`Expected indented expression after ${keyword}:`, state.position);
    }

    // Otherwise parse as expression
    return this.parseExpression(state);
  }

  /**
   * Parse an indented list of expressions.
   * Used for array: syntax and other indented lists.
   *
   * Uses indentation tracking to determine block boundaries:
   * - Looks ahead to find the indentation of the first element
   * - Enters indentation-sensitive parsing mode
   * - Stops when encountering a line with less indentation
   *
   * Returns:
   * - elements: The parsed expressions
   * - separatorOffsets: Offsets of newlines/commas between elements
   * - state: Parser state after the indented block
   */
  private parseIdentedList(state: ParserState): { elements: AST.Expression[], separatorOffsets: number[], state: ParserState } {
    const elements: AST.Expression[] = [];
    const separatorOffsets: number[] = [];

    // Look ahead to find the indentation of the next line
    const nextLineIndent = state.getNextLineIndentation();
    if (nextLineIndent === null) {
      // No indented content found
      return { elements, separatorOffsets, state };
    }

    // Enter indentation-sensitive context
    state = state.enterIndentationContext(nextLineIndent);

    // Skip to the first element
    state = state.skipTrivia();
    if (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance().skipTrivia();
    }

    while (!state.isAtEnd()) {
      // Check if we're still indented
      const currentToken = state.current();
      if (!currentToken) break;

      // The indentation context will handle stopping at unindented lines
      if (currentToken.position.column < nextLineIndent) {
        break;
      }

      // Parse element
      const elementResult = this.parseExpression(state);
      elements.push(elementResult.node);
      state = elementResult.state;

      // Skip any trailing trivia on the line
      state = state.skipTrivia();

      // Check for newline or comma separator
      if (state.current()?.type === TokenType.NEWLINE) {
        separatorOffsets.push(state.currentOffset());
        state = state.advance().skipTrivia();
      } else if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      } else {
        // No more separators, we're done
        break;
      }
    }

    // Exit indentation context
    state = state.exitIndentationContext();

    return { elements, separatorOffsets, state };
  }

  /**
   * Parse a brace-delimited compound expression.
   */
  private parseCompoundExpression(state: ParserState): ParseResult<AST.CompoundExpression> {
    return this.compoundParser.parseCompoundExpression(state);
  }

  /**
   * Parse an indented compound expression after block-forming keywords.
   */
  private parseIdentedCompound(state: ParserState): ParseResult<AST.IdentedCompoundExpression> {
    return this.compoundParser.parseIdentedCompound(state);
  }

  /**
   * Try to parse a declaration, return null if it's not a declaration.
   * This uses lookahead to determine if we have a declaration pattern.
   */
  private tryParseDeclaration(state: ParserState): ParseResult<AST.Expression> | null {
    // Look ahead to see if this looks like a declaration
    // Patterns:
    // - identifier : type
    // - identifier := value
    // - identifier < specifiers > : type
    // - identifier < specifiers > := value
    // - identifier ( params ) := body  (function)
    // - identifier ( params ) : returnType = body  (function)

    const originalPosition = state.position;
    let lookahead = state.skipTrivia();

    // Must start with identifier
    if (!lookahead.current() || lookahead.current()!.type !== TokenType.IDENTIFIER) {
      return null;
    }

    lookahead = lookahead.advance().skipTrivia();

    // Skip any specifiers after the identifier
    while (lookahead.current() && lookahead.current()!.type === TokenType.SPECIFIER) {
      lookahead = lookahead.advance().skipTrivia();
    }

    // Check for function parameters first
    const next = lookahead.current();
    if (!next) return null;

    // If we see ( then it's likely a function
    if (next.type === TokenType.OPERATOR && next.content === '(') {
      try {
        return this.declarationParser.parseDeclaration(state);
      } catch (e: any) {
        // Debug: log why declaration parsing failed
        // console.error('Declaration parsing failed:', e.message);
        return null;
      }
    }

    // Skip any specifiers (there could be multiple)
    let currentToken: Token | null | undefined = next;
    while (currentToken && (currentToken.type === TokenType.SPECIFIER ||
                    (currentToken.type === TokenType.OPERATOR && currentToken.content === '<'))) {
      if (currentToken.type === TokenType.SPECIFIER) {
        lookahead = lookahead.advance().skipTrivia();
      }
      // If we see < (specifier list), skip over it
      else if (currentToken.type === TokenType.OPERATOR && currentToken.content === '<') {
        lookahead = lookahead.advance();
        // Skip to closing >
        let depth = 1;
        while (!lookahead.isAtEnd() && depth > 0) {
          const current = lookahead.current();
          if (!current) break;
          if (current.type === TokenType.OPERATOR) {
            if (current.content === '<') depth++;
            else if (current.content === '>') depth--;
          }
          lookahead = lookahead.advance();
        }
        lookahead = lookahead.skipTrivia();
      }
      // Update currentToken for the while condition
      currentToken = lookahead.current();
    }

    const token = lookahead.current();
    if (!token) return null;

    // Check for declaration patterns
    if (token.type === TokenType.OPERATOR && (token.content === ':' || token.content === ':=')) {
      // Check for object construction pattern: identifier : newline (with indented content)
      // This heuristic helps distinguish "Person:" (object construction) from "x: int" (type annotation)
      if (token.content === ':') {
        // Look ahead past the colon
        const afterColon = lookahead.advance();

        // Skip any trailing whitespace/comments on the same line
        let checkPos = afterColon;
        while (checkPos.current() && (checkPos.current()!.type === TokenType.SPACE ||
                                      checkPos.current()!.type === TokenType.TAB ||
                                      checkPos.current()!.type === TokenType.COMMENT ||
                                      checkPos.current()!.type === TokenType.TRIVIA)) {
          checkPos = checkPos.advance();
        }

        // If we see a newline immediately after the colon (possibly with trailing comments),
        // this is likely an object construction, not a type annotation
        if (checkPos.current()?.type === TokenType.NEWLINE) {
          // This looks like object construction (e.g., "Person:" with indented fields)
          // Don't treat it as a declaration - return null to let it be parsed as an identifier
          return null;
        }
      }

      // For := followed by data structure keywords, parse as data structure
      if (token.content === ':=') {
        lookahead = lookahead.advance().skipTrivia();
        const afterAssign = lookahead.current();
        if (afterAssign && afterAssign.type === TokenType.DATA_STRUCTURE_KEYWORD) {
          // Always parse as data structure when we see these keywords after :=
          // The data structure parser will handle () for inheritance, {}, or : for body
          try {
            return this.declarationParser.parseDataStructureDeclaration(state);
          } catch (error: any) {
            // If this is an enum validation error or missing body error, throw it instead of falling back
            if (error.message &&
                (error.message.includes('Enum must have at least one member') ||
                 error.message.includes('Expected { or : for data structure body'))) {
              throw error;
            }
            return null;
          }
        }
      }

      // This looks like a regular declaration, parse it
      try {
        return this.declarationParser.parseConstantDeclaration(state);
      } catch {
        return null;
      }
    }

    return null;
  }

  /**
   * Parse a variable declaration starting with 'var'.
   */
  private parseVariableDeclaration(state: ParserState): ParseResult<AST.Expression> {
    return this.declarationParser.parseVariableDeclaration(state);
  }

  /**
   * Parse any declaration (constant, variable, function, data structure).
   */
  parseDeclaration(state: ParserState): ParseResult<AST.Declaration> {
    return this.declarationParser.parseDeclaration(state);
  }

  /**
   * Validate data structure body contents without full parsing.
   * This ensures the body contains valid declarations, not arbitrary expressions.
   */
  private validateDataStructureBody(state: ParserState, endToken: string, kind: string): void {
    state = state.skipTrivia();

    while (!state.isAtEnd()) {
      const token = state.current();
      if (token && token.type === TokenType.OPERATOR && token.content === endToken) {
        break; // Found end token
      }

      if (!token) break;

      // Skip trivia and empty lines
      if (token.type === TokenType.TRIVIA || token.type === TokenType.NEWLINE ||
          token.type === TokenType.SPACE || token.type === TokenType.TAB) {
        state = state.advance();
        continue;
      }

      // Validate that we have valid declaration starting tokens
      if (token.type === TokenType.IDENTIFIER ||
          token.type === TokenType.DECL_KEYWORD ||
          (token.type === TokenType.OPERATOR && token.content === '<') || // specifiers
          (token.type === TokenType.OPERATOR && token.content === '@')) { // decorators
        // Valid start of declaration - parse and validate it
        try {
          const declResult = this.declarationParser.parseDeclaration(state, { kind });
          state = declResult.state.skipTrivia();
        } catch (error) {
          throw error; // Re-throw validation errors
        }
      } else {
        // Invalid token for data structure body
        throw new ParseError(`Invalid ${kind} member - expected declaration but found ${token.type}:'${token.content}'`,
                           state.position, token);
      }
    }
  }

  /**
   * Validate indented data structure body.
   */
  private validateIndentedDataStructureBody(state: ParserState, baseIndent: number, kind: string): void {
    while (!state.isAtEnd() && state.currentIndentationLevel >= baseIndent) {
      const token = state.current();
      if (!token) break;

      // Skip trivia and empty lines
      if (token.type === TokenType.TRIVIA || token.type === TokenType.NEWLINE ||
          token.type === TokenType.SPACE || token.type === TokenType.TAB) {
        state = state.advance();
        continue;
      }

      // Validate declaration
      if (token.type === TokenType.IDENTIFIER ||
          token.type === TokenType.DECL_KEYWORD ||
          (token.type === TokenType.OPERATOR && token.content === '<') ||
          (token.type === TokenType.OPERATOR && token.content === '@')) {
        try {
          const declResult = this.declarationParser.parseDeclaration(state, { kind });
          state = declResult.state.skipTrivia();
        } catch (error) {
          throw error;
        }
      } else {
        throw new ParseError(`Invalid ${kind} member - expected declaration but found ${token.type}:'${token.content}'`,
                           state.position, token);
      }
    }
  }

  /**
   * Validate enum body contents (simplified validation).
   */
  private validateEnumBody(state: ParserState): void {
    // Simple validation for enum members - just check for identifiers separated by commas
    state = state.skipTrivia();

    while (!state.isAtEnd()) {
      const token = state.current();
      if (token && token.type === TokenType.OPERATOR && token.content === '}') {
        break;
      }

      if (!token) break;

      if (token.type === TokenType.TRIVIA || token.type === TokenType.NEWLINE ||
          token.type === TokenType.SPACE || token.type === TokenType.TAB) {
        state = state.advance();
        continue;
      }

      if (token.type === TokenType.IDENTIFIER) {
        state = state.advance().skipTrivia();
        // Optional comma or equals (for enum values)
        if (state.current()?.type === TokenType.OPERATOR) {
          if (state.current()?.content === '=') {
            // Skip enum value
            state = state.advance();
            // Skip the value expression (simplified - just skip to comma or end)
            while (state.current() &&
                   !(state.current()!.type === TokenType.OPERATOR &&
                     (state.current()!.content === ',' || state.current()!.content === '}'))) {
              state = state.advance();
            }
          }
          if (state.current()?.content === ',') {
            state = state.advance().skipTrivia();
          }
        }
      } else {
        throw new ParseError(`Invalid enum member - expected identifier but found ${token.type}:'${token.content}'`,
                           state.position, token);
      }
    }
  }

  /**
   * Validate indented enum body.
   */
  private validateIndentedEnumBody(state: ParserState, baseIndent: number): void {
    // Similar to above but for indented enum members
    while (!state.isAtEnd() && state.currentIndentationLevel >= baseIndent) {
      const token = state.current();
      if (!token) break;

      if (token.type === TokenType.TRIVIA || token.type === TokenType.NEWLINE ||
          token.type === TokenType.SPACE || token.type === TokenType.TAB) {
        state = state.advance();
        continue;
      }

      if (token.type === TokenType.IDENTIFIER) {
        state = state.advance().skipTrivia();
        // Handle optional value assignment
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '=') {
          state = state.advance();
          // Skip value (simplified)
          while (state.current() && state.currentIndentationLevel >= baseIndent &&
                 state.current()!.type !== TokenType.NEWLINE) {
            state = state.advance();
          }
        }
      } else {
        throw new ParseError(`Invalid enum member - expected identifier but found ${token.type}:'${token.content}'`,
                           state.position, token);
      }
    }
  }
}

/**
 * Factory function to create a parser instance.
 * Provides a clean API for parser creation.
 */
export function createParser(): Parser {
  return new Parser();
}

/**
 * Convenience function to create a parser state from a token stream.
 * Initializes the state at the beginning of the stream.
 */
export function createParserState(tokens: TokenStream): ParserState {
  return new ParserState(tokens);
}

/**
 * Parse an expression from a string.
 *
 * This is the most common entry point for parsing.
 * Automatically handles lexing and trivia combination.
 *
 * @param source The source code to parse
 * @returns The parsed expression AST
 * @throws ParseError if the expression is malformed
 */
export function parseExpression(source: string): AST.Expression {
  // Lex the source into tokens
  const tokens = TokenStream.fromString(source);
  // Combine trivia for cleaner parsing
  tokens.combineTrivia();

  // Create parser and state
  const parser = createParser();
  const state = createParserState(tokens);

  // Parse the expression
  const result = parser.parseExpression(state);

  // Verify we consumed all input
  if (!result.state.isAtEnd()) {
    const remaining = result.state.current();
    if (remaining && !remaining.isEOF()) {
      throw new ParseError(`Unexpected token after expression: ${remaining.type}`, result.state.position, remaining);
    }
  }

  return result.node;
}

