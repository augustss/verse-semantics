/**
 * Range-Enhanced Parser
 *
 * This demonstrates how to integrate source range tracking into
 * the existing parser with minimal changes.
 */

import { TokenStream, TokenType } from '../lexer';
import { EnhancedParserState, SourceRange, withRangeTracking } from './enhanced-parser-state';
import * as AST from './ast';
import { ParseError } from './parser';

/**
 * AST nodes with source ranges
 */
export interface RangedASTNode extends AST.ASTNode {
  sourceRange?: SourceRange;
}

export interface RangedExpression extends AST.Expression {
  sourceRange?: SourceRange;
}

/**
 * Parse result with enhanced state
 */
interface RangedParseResult<T> {
  node: T & { sourceRange?: SourceRange };
  state: EnhancedParserState;
}

/**
 * Range-enhanced parser for Verse
 */
export class RangeEnhancedParser {
  private source: string;

  constructor(source: string) {
    this.source = source;
  }

  /**
   * Parse a complete program with source ranges
   */
  parseProgram(): RangedASTNode {
    const stream = TokenStream.fromString(this.source);
    const state = EnhancedParserState.fromTokenStream(stream, this.source);
    const startState = state;

    const declarations: RangedASTNode[] = [];
    let currentState = state.skipTrivia();

    while (!currentState.isAtEnd()) {
      const declResult = this.parseTopLevelDeclaration(currentState);
      if (declResult) {
        declarations.push(declResult.node);
        currentState = declResult.state.skipTrivia();
      } else {
        break;
      }
    }

    return {
      type: 'Program',
      declarations,
      sourceRange: currentState.markRange(startState)
    } as RangedASTNode;
  }

  /**
   * Parse top-level declaration with range tracking
   */
  private parseTopLevelDeclaration(state: EnhancedParserState): RangedParseResult<RangedASTNode> | null {
    const token = state.current();
    if (!token) return null;

    // Dispatch based on token type
    if (token.type === TokenType.IDENTIFIER) {
      // Could be constant declaration or function
      return this.parseDeclaration(state);
    } else if (token.content === 'var') {
      return this.parseVariableDeclaration(state);
    }

    // Try as expression
    return this.parseExpression(state);
  }

  /**
   * Parse declaration (constant or function)
   */
  private parseDeclaration = withRangeTracking((state: EnhancedParserState) => {
    const startState = state;
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      return null;
    }

    const name = nameToken.content;
    const nameOffset = state.currentOffset();
    state = state.advance().skipTrivia();

    // Check for := or =
    const opToken = state.current();
    if (!opToken || opToken.type !== TokenType.OPERATOR) {
      return null;
    }

    if (opToken.content === ':=') {
      // Constant declaration
      const assignOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      const valueResult = this.parseExpression(state);
      if (!valueResult) {
        throw new ParseError('Expected expression after :=', state.position);
      }

      const node: RangedASTNode = {
        type: 'ConstantDeclaration',
        name,
        nameOffset,
        assignOffset,
        initializer: valueResult.node
      } as any;

      return { node, state: valueResult.state };
    }

    return null;
  });

  /**
   * Parse variable declaration with range tracking
   */
  private parseVariableDeclaration = withRangeTracking((state: EnhancedParserState) => {
    const varToken = state.current();
    if (!varToken || varToken.content !== 'var') {
      return null;
    }

    const varOffset = state.currentOffset();
    state = state.advance().skipTrivia();

    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected identifier after var', state.position);
    }

    const name = nameToken.content;
    const nameOffset = state.currentOffset();
    state = state.advance().skipTrivia();

    // Parse type annotation if present
    let colonOffset: number | undefined;
    let declaredType: any | undefined;

    if (state.current()?.content === ':') {
      colonOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse type (simplified)
      const typeToken = state.current();
      if (typeToken && typeToken.type === TokenType.IDENTIFIER) {
        declaredType = {
          type: 'TypeExpression',
          typeName: typeToken.content,
          typeNameOffset: state.currentOffset()
        };
        state = state.advance().skipTrivia();
      }
    }

    // Parse initializer if present
    let equalsOffset: number | undefined;
    let initializer: RangedExpression | undefined;

    if (state.current()?.content === '=') {
      equalsOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      const initResult = this.parseExpression(state);
      if (!initResult) {
        throw new ParseError('Expected expression after =', state.position);
      }
      initializer = initResult.node;
      state = initResult.state;
    }

    const node: RangedASTNode = {
      type: 'VariableDeclaration',
      varOffset,
      name,
      nameOffset,
      colonOffset,
      declaredType,
      equalsOffset,
      initializer
    } as any;

    return { node, state };
  });

  /**
   * Parse expression with range tracking
   */
  private parseExpression(state: EnhancedParserState): RangedParseResult<RangedExpression> | null {
    // This would dispatch to various expression parsers
    // For demonstration, let's parse simple cases

    const token = state.current();
    if (!token) return null;

    // Number literal
    if (token.type === TokenType.NUMBER) {
      return this.parseLiteral(state);
    }

    // String literal
    if (token.type === TokenType.STRING) {
      return this.parseLiteral(state);
    }

    // Identifier
    if (token.type === TokenType.IDENTIFIER) {
      return this.parseIdentifier(state);
    }

    // Binary expression
    return this.parseBinaryExpression(state);
  }

  /**
   * Parse literal with range tracking
   */
  private parseLiteral = withRangeTracking((state: EnhancedParserState) => {
    const token = state.current();
    if (!token) return null;

    let value: any;
    let literalType: string;

    if (token.type === TokenType.NUMBER) {
      value = parseFloat(token.content);
      literalType = token.content.includes('.') ? 'float' : 'integer';
    } else if (token.type === TokenType.STRING) {
      value = token.content;
      literalType = 'string';
    } else {
      return null;
    }

    const node: RangedExpression = {
      type: 'Literal',
      value,
      literalType,
      tokenOffset: state.currentOffset()
    } as any;

    return { node, state: state.advance() };
  });

  /**
   * Parse identifier with range tracking
   */
  private parseIdentifier = withRangeTracking((state: EnhancedParserState) => {
    const token = state.current();
    if (!token || token.type !== TokenType.IDENTIFIER) {
      return null;
    }

    const node: RangedExpression = {
      type: 'Identifier',
      name: token.content,
      tokenOffset: state.currentOffset()
    } as any;

    return { node, state: state.advance() };
  });

  /**
   * Parse binary expression with range tracking
   */
  private parseBinaryExpression(state: EnhancedParserState): RangedParseResult<RangedExpression> | null {
    const startState = state;

    // Parse left operand
    const leftResult = this.parsePrimaryExpression(state);
    if (!leftResult) return null;

    state = leftResult.state.skipTrivia();

    // Check for operator
    const opToken = state.current();
    if (!opToken || opToken.type !== TokenType.OPERATOR) {
      return leftResult;
    }

    const validOps = ['+', '-', '*', '/', '>', '<', '>=', '<=', '==', '!='];
    if (!validOps.includes(opToken.content)) {
      return leftResult;
    }

    const operator = opToken.content;
    const operatorOffset = state.currentOffset();
    state = state.advance().skipTrivia();

    // Parse right operand
    const rightResult = this.parseExpression(state);
    if (!rightResult) {
      throw new ParseError(`Expected expression after ${operator}`, state.position);
    }

    const node: RangedExpression = {
      type: 'BinaryExpression',
      left: leftResult.node,
      operator,
      operatorOffset,
      right: rightResult.node,
      sourceRange: rightResult.state.markRange(startState)
    } as any;

    return { node, state: rightResult.state };
  }

  /**
   * Parse primary expression (literal or identifier)
   */
  private parsePrimaryExpression(state: EnhancedParserState): RangedParseResult<RangedExpression> | null {
    const token = state.current();
    if (!token) return null;

    if (token.type === TokenType.NUMBER || token.type === TokenType.STRING) {
      return this.parseLiteral(state);
    }

    if (token.type === TokenType.IDENTIFIER) {
      return this.parseIdentifier(state);
    }

    return null;
  }
}

/**
 * Perfect reconstructor using source ranges
 */
export class RangeBasedReconstructor {
  private source: string;

  constructor(source: string) {
    this.source = source;
  }

  /**
   * Reconstruct any node with a source range
   */
  reconstruct(node: RangedASTNode): string {
    if (!node.sourceRange) {
      throw new Error(`Node ${node.type} lacks source range`);
    }

    // Perfect reconstruction - just extract the source!
    return this.source.substring(
      node.sourceRange.startOffset,
      node.sourceRange.endOffset
    );
  }

  /**
   * Reconstruct with modifications
   */
  transform(node: RangedASTNode, transformer: (node: RangedASTNode) => RangedASTNode): string {
    const transformed = transformer(node);

    if (transformed === node && node.sourceRange) {
      // No change - use original source
      return this.reconstruct(node);
    }

    // Node was modified - need to reconstruct from AST
    // This would use a traditional AST-based reconstructor
    return this.reconstructFromAST(transformed);
  }

  private reconstructFromAST(node: RangedASTNode): string {
    // Simplified AST reconstruction for demo
    switch (node.type) {
      case 'Literal':
        const lit = node as any;
        return lit.literalType === 'string' ? `"${lit.value}"` : String(lit.value);

      case 'Identifier':
        return (node as any).name;

      case 'BinaryExpression':
        const bin = node as any;
        return `${this.reconstructFromAST(bin.left)} ${bin.operator} ${this.reconstructFromAST(bin.right)}`;

      default:
        return '';
    }
  }
}