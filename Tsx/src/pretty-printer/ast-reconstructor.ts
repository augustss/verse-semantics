/**
 * AST-based Source Reconstruction
 *
 * This module provides utilities to reconstruct source code from AST nodes
 * while preserving all original formatting, comments, and whitespace.
 *
 * The reconstruction process uses token offsets stored in AST nodes to
 * retrieve and reconstruct the exact original source, including all trivia
 * (whitespace and comments) that appears between significant tokens.
 */

import { TokenStream } from '../lexer/tokenstream';
import { Token, TokenType } from '../lexer/token';
import * as AST from '../parser/ast';

/**
 * Options for AST reconstruction
 */
export interface ReconstructionOptions {
  /** Whether to include trailing trivia after the last token */
  includeTrailingTrivia?: boolean;
  /** Custom token stream if different from original */
  tokenStream?: TokenStream;
}

/**
 * AST Reconstructor
 *
 * Reconstructs source code from AST nodes by using stored token offsets
 * to retrieve original tokens and their surrounding trivia.
 */
export class ASTReconstructor {
  private tokens: Token[];
  private source: string;
  private lastOffset: number = -1;
  private result: string = '';

  constructor(source: string, tokenStream?: TokenStream) {
    this.source = source;
    const stream = tokenStream || TokenStream.fromString(source);
    this.tokens = stream.getAllTokens();
  }

  /**
   * Reconstruct source from an AST node
   */
  reconstruct(node: AST.ASTNode, options?: ReconstructionOptions): string {
    this.result = '';
    this.lastOffset = -1;
    this.reconstructNode(node);

    if (options?.includeTrailingTrivia) {
      this.appendRemainingTrivia();
    }

    return this.result;
  }

  /**
   * Reconstruct source from multiple AST nodes (e.g., a program)
   */
  reconstructProgram(nodes: AST.ASTNode[], options?: ReconstructionOptions): string {
    this.result = '';
    this.lastOffset = -1;

    for (const node of nodes) {
      this.reconstructNode(node);
    }

    if (options?.includeTrailingTrivia) {
      this.appendRemainingTrivia();
    }

    return this.result;
  }

  /**
   * Append a token at the given offset, including preceding trivia
   */
  private appendToken(offset: number): void {
    if (offset < 0 || offset >= this.tokens.length) return;

    // Append all trivia between lastOffset and current offset
    if (this.lastOffset >= 0) {
      for (let i = this.lastOffset + 1; i < offset; i++) {
        const token = this.tokens[i];
        if (this.isTrivia(token)) {
          this.result += token.content;
        }
      }
    } else {
      // First token - include all leading trivia
      for (let i = 0; i < offset; i++) {
        const token = this.tokens[i];
        if (this.isTrivia(token)) {
          this.result += token.content;
        }
      }
    }

    // Append the actual token
    const token = this.tokens[offset];
    this.result += token.content;
    this.lastOffset = offset;
  }

  /**
   * Append any remaining trivia after the last token
   */
  private appendRemainingTrivia(): void {
    if (this.lastOffset >= 0) {
      for (let i = this.lastOffset + 1; i < this.tokens.length; i++) {
        const token = this.tokens[i];
        if (this.isTrivia(token) || token.type === TokenType.EOF) {
          this.result += token.content;
        }
      }
    }
  }

  /**
   * Check if a token is trivia (whitespace or comment)
   */
  private isTrivia(token: Token): boolean {
    return token.type === TokenType.SPACE ||
           token.type === TokenType.TAB ||
           token.type === TokenType.NEWLINE ||
           token.type === TokenType.COMMENT ||
           token.type === TokenType.MULTILINE_COMMENT ||
           token.type === TokenType.TRIVIA;
  }

  /**
   * Reconstruct a specific AST node
   */
  private reconstructNode(node: AST.ASTNode): void {
    switch (node.type) {
      case 'Program':
        this.reconstructProgramNode(node as any);
        break;

      case 'Literal':
        this.reconstructLiteral(node as AST.LiteralExpression);
        break;

      case 'Identifier':
        this.reconstructIdentifier(node as AST.IdentifierExpression);
        break;

      case 'BinaryExpression':
        this.reconstructBinary(node as AST.BinaryExpression);
        break;

      case 'UnaryExpression':
        this.reconstructUnary(node as AST.UnaryExpression);
        break;

      case 'ParenthesizedExpression':
        this.reconstructParenthesized(node as AST.ParenthesizedExpression);
        break;

      case 'AssignmentExpression':
        this.reconstructAssignment(node as AST.AssignmentExpression);
        break;

      case 'MemberExpression':
        this.reconstructMember(node as AST.MemberExpression);
        break;

      case 'CallExpression':
        this.reconstructCall(node as AST.CallExpression);
        break;

      case 'ObjectConstructorExpression':
        this.reconstructObjectConstructor(node as AST.ObjectConstructorExpression);
        break;

      case 'ArrayExpression':
        this.reconstructArray(node as AST.ArrayExpression);
        break;

      case 'RangeExpression':
        this.reconstructRange(node as AST.RangeExpression);
        break;

      case 'LambdaExpression':
        this.reconstructLambda(node as AST.LambdaExpression);
        break;

      case 'CompoundExpression':
        this.reconstructCompound(node as AST.CompoundExpression);
        break;

      case 'IdentedCompoundExpression':
        this.reconstructIdentedCompound(node as AST.IdentedCompoundExpression);
        break;

      case 'SetExpression':
        this.reconstructSet(node as AST.SetExpression);
        break;

      case 'ForExpression':
        this.reconstructFor(node as AST.ForExpression);
        break;

      case 'IfExpression':
        this.reconstructIf(node as AST.IfExpression);
        break;

      case 'LoopExpression':
        this.reconstructLoop(node as AST.LoopExpression);
        break;

      case 'BlockExpression':
        this.reconstructBlock(node as AST.BlockExpression);
        break;

      case 'CaseExpression':
        this.reconstructCase(node as AST.CaseExpression);
        break;

      case 'IdentedCompoundExpression':
        this.reconstructIndentedCompound(node as AST.IdentedCompoundExpression);
        break;

      case 'BreakExpression':
        this.reconstructBreak(node as AST.BreakExpression);
        break;

      case 'ContinueExpression':
        this.reconstructContinue(node as AST.ContinueExpression);
        break;

      case 'ReturnExpression':
        this.reconstructReturn(node as AST.ReturnExpression);
        break;

      case 'ConstantDeclaration':
        this.reconstructConstant(node as AST.ConstantDeclaration);
        break;

      case 'VariableDeclaration':
        this.reconstructVariable(node as AST.VariableDeclaration);
        break;

      case 'FunctionDeclaration':
        this.reconstructFunction(node as AST.FunctionDeclaration);
        break;

      case 'DataStructureDeclaration':
        this.reconstructDataStructure(node as AST.DataStructureDeclaration);
        break;

      case 'TypeExpression':
        this.reconstructType(node as AST.TypeExpression);
        break;

      case 'SpecifierList':
        this.reconstructSpecifiers(node as AST.SpecifierList);
        break;

      case 'Parameter':
        this.reconstructParameter(node as AST.Parameter);
        break;

      case 'EnumMember':
        this.reconstructEnumMember(node as AST.EnumMember);
        break;
    }
  }

  // Individual node reconstruction methods

  private reconstructLiteral(node: AST.LiteralExpression): void {
    // Special handling for string literals - need to add quotes back
    if (node.literalType === 'string') {
      // Get the actual token to preserve original quotes if possible
      const token = this.tokens[node.tokenOffset];
      if (token && token.type === TokenType.STRING) {
        // The token content doesn't have quotes, so we need to add them
        // For now, use double quotes (could be enhanced to detect original)
        this.appendTokenWithPrefix(node.tokenOffset, '"', '"');
        return;
      }
    }
    this.appendToken(node.tokenOffset);
  }

  /**
   * Append a token with prefix and suffix (for string quotes)
   */
  private appendTokenWithPrefix(offset: number, prefix: string, suffix: string): void {
    if (offset < 0 || offset >= this.tokens.length) return;

    // Append all trivia between lastOffset and current offset
    if (this.lastOffset >= 0) {
      for (let i = this.lastOffset + 1; i < offset; i++) {
        const token = this.tokens[i];
        if (this.isTrivia(token)) {
          this.result += token.content;
        }
      }
    } else {
      // First token - include all leading trivia
      for (let i = 0; i < offset; i++) {
        const token = this.tokens[i];
        if (this.isTrivia(token)) {
          this.result += token.content;
        }
      }
    }

    // Append the token with prefix and suffix
    const token = this.tokens[offset];
    this.result += prefix + token.content + suffix;
    this.lastOffset = offset;
  }

  private reconstructIdentifier(node: AST.IdentifierExpression): void {
    this.appendToken(node.tokenOffset);
  }

  private reconstructBinary(node: AST.BinaryExpression): void {
    this.reconstructNode(node.left);
    this.appendToken(node.operatorOffset);
    this.reconstructNode(node.right);
  }

  private reconstructUnary(node: AST.UnaryExpression): void {
    this.appendToken(node.operatorOffset);
    this.reconstructNode(node.operand);
  }

  private reconstructParenthesized(node: AST.ParenthesizedExpression): void {
    this.appendToken(node.openParenOffset);
    this.reconstructNode(node.expression);
    this.appendToken(node.closeParenOffset);
  }

  private reconstructAssignment(node: AST.AssignmentExpression): void {
    this.reconstructNode(node.left);
    this.appendToken(node.operatorOffset);
    this.reconstructNode(node.right);
  }

  private reconstructMember(node: AST.MemberExpression): void {
    this.reconstructNode(node.object);
    if (node.computed) {
      if (node.openBracketOffset !== undefined) {
        this.appendToken(node.openBracketOffset);
      }
      this.reconstructNode(node.property);
      if (node.closeBracketOffset !== undefined) {
        this.appendToken(node.closeBracketOffset);
      }
    } else {
      if (node.dotOffset !== undefined) {
        this.appendToken(node.dotOffset);
      }
      this.reconstructNode(node.property);
    }
  }

  private reconstructCall(node: AST.CallExpression): void {
    this.reconstructNode(node.callee);
    this.appendToken(node.openParenOffset);

    for (let i = 0; i < node.arguments.length; i++) {
      this.reconstructNode(node.arguments[i]);
      if (i < node.argumentSeparatorOffsets.length) {
        this.appendToken(node.argumentSeparatorOffsets[i]);
      }
    }

    this.appendToken(node.closeParenOffset);
  }

  private reconstructObjectConstructor(node: AST.ObjectConstructorExpression): void {
    this.appendToken(node.typeNameOffset);
    this.appendToken(node.openBraceOffset);

    for (let i = 0; i < node.fields.length; i++) {
      const field = node.fields[i];
      this.appendToken(field.nameOffset);
      this.appendToken(field.assignOffset);
      this.reconstructNode(field.value);

      if (i < node.fieldSeparatorOffsets.length) {
        this.appendToken(node.fieldSeparatorOffsets[i]);
      }
    }

    this.appendToken(node.closeBraceOffset);
  }

  private reconstructArray(node: AST.ArrayExpression): void {
    if (node.arrayKeywordOffset !== undefined) {
      this.appendToken(node.arrayKeywordOffset);
    }

    if (node.openBraceOffset !== undefined) {
      this.appendToken(node.openBraceOffset);

      // After opening brace, append everything until first element
      if (node.elements.length > 0) {
        const firstElemOffset = this.getFirstTokenOffset(node.elements[0]);
        this.appendTokensUntil(firstElemOffset);
      }

      // Reconstruct each element and separators between them
      for (let i = 0; i < node.elements.length; i++) {
        this.reconstructNode(node.elements[i]);

        // After each element (except the last), append everything until next element
        if (i < node.elements.length - 1) {
          const nextElemOffset = this.getFirstTokenOffset(node.elements[i + 1]);
          this.appendTokensUntil(nextElemOffset);
        }
      }

      // After last element, append everything until closing brace
      if (node.closeBraceOffset !== undefined) {
        this.appendTokensUntil(node.closeBraceOffset);
        this.appendToken(node.closeBraceOffset);
      }
    } else if (node.colonOffset !== undefined) {
      this.appendToken(node.colonOffset);

      // For indented arrays, reconstruct elements with everything between them
      if (node.elements.length > 0) {
        const firstElemOffset = this.getFirstTokenOffset(node.elements[0]);
        this.appendTokensUntil(firstElemOffset);
      }

      for (let i = 0; i < node.elements.length; i++) {
        this.reconstructNode(node.elements[i]);

        if (i < node.elements.length - 1) {
          const nextElemOffset = this.getFirstTokenOffset(node.elements[i + 1]);
          this.appendTokensUntil(nextElemOffset);
        }
      }
    }
  }

  private reconstructRange(node: AST.RangeExpression): void {
    this.reconstructNode(node.start);
    this.appendToken(node.operatorOffset);
    this.reconstructNode(node.end);
  }

  private reconstructLambda(node: AST.LambdaExpression): void {
    // Check if parameters are parenthesized
    if (node.openParenOffset !== undefined) {
      this.appendToken(node.openParenOffset);
    }

    for (let i = 0; i < node.parameters.length; i++) {
      this.reconstructNode(node.parameters[i]);
      if (i < node.parameterSeparatorOffsets.length) {
        this.appendToken(node.parameterSeparatorOffsets[i]);
      }
    }

    if (node.closeParenOffset !== undefined) {
      this.appendToken(node.closeParenOffset);
    }

    this.appendToken(node.arrowOffset);
    this.reconstructNode(node.body);
  }

  private reconstructIdentedCompound(node: AST.IdentedCompoundExpression): void {
    // IdentedCompoundExpression is similar to indented CompoundExpression
    // Reconstruct each statement
    for (let i = 0; i < node.statements.length; i++) {
      this.reconstructNode(node.statements[i]);

      // Add trivia between statements
      if (i < node.statements.length - 1) {
        const currentEnd = this.getLastTokenOffset(node.statements[i]);
        const nextStart = this.getFirstTokenOffset(node.statements[i + 1]);
        this.appendTokensUntil(nextStart);
      }
    }
  }

  private reconstructCompound(node: AST.CompoundExpression): void {
    // Check if this is a braced compound or indented compound
    // openBraceOffset === 0 && closeBraceOffset === 0 indicates an indented form
    const isIndented = node.openBraceOffset === 0 && node.closeBraceOffset === 0;

    if (!isIndented) {
      // Braced compound - append the opening brace
      this.appendToken(node.openBraceOffset);

      // After opening brace, append everything until first expression
      if (node.expressions.length > 0) {
        const firstExprOffset = this.getFirstTokenOffset(node.expressions[0]);
        this.appendTokensUntil(firstExprOffset);
      }
    } else {
      // Indented compound - append everything from current position to first expression
      if (node.expressions.length > 0) {
        const firstExprOffset = this.getFirstTokenOffset(node.expressions[0]);
        this.appendTokensUntil(firstExprOffset);
      }
    }

    // Reconstruct each expression and separators between them
    for (let i = 0; i < node.expressions.length; i++) {
      this.reconstructNode(node.expressions[i]);

      // After each expression (except the last), append everything until next expression
      if (i < node.expressions.length - 1) {
        const nextExprOffset = this.getFirstTokenOffset(node.expressions[i + 1]);
        this.appendTokensUntil(nextExprOffset);
      }
    }

    // After last expression, for braced compounds append everything until closing brace
    if (!isIndented) {
      this.appendTokensUntil(node.closeBraceOffset);
      this.appendToken(node.closeBraceOffset);
    }
  }

  /**
   * Get the last token offset of an AST node
   */
  private getLastTokenOffset(node: AST.ASTNode): number {
    // This needs to return the last token offset for any node type
    switch (node.type) {
      case 'Literal':
        return (node as AST.LiteralExpression).tokenOffset;
      case 'Identifier':
        return (node as AST.IdentifierExpression).tokenOffset;
      case 'ParenthesizedExpression':
        return (node as AST.ParenthesizedExpression).closeParenOffset;
      case 'BinaryExpression':
        return this.getLastTokenOffset((node as AST.BinaryExpression).right);
      case 'UnaryExpression':
        return this.getLastTokenOffset((node as AST.UnaryExpression).operand);
      case 'CallExpression':
        return (node as AST.CallExpression).closeParenOffset;
      case 'MemberExpression':
        const member = node as AST.MemberExpression;
        if (member.computed && member.closeBracketOffset !== undefined) {
          return member.closeBracketOffset;
        }
        return this.getLastTokenOffset(member.property);
      case 'AssignmentExpression':
        return this.getLastTokenOffset((node as AST.AssignmentExpression).right);
      case 'CompoundExpression':
        const compound = node as AST.CompoundExpression;
        return compound.closeBraceOffset;
      case 'IdentedCompoundExpression':
        const identedCompound = node as AST.IdentedCompoundExpression;
        if (identedCompound.statements.length > 0) {
          return this.getLastTokenOffset(identedCompound.statements[identedCompound.statements.length - 1]);
        }
        return 0;
      case 'ArrayExpression':
        const arr = node as AST.ArrayExpression;
        if (arr.closeBraceOffset !== undefined) {
          return arr.closeBraceOffset;
        }
        if (arr.elements.length > 0) {
          return this.getLastTokenOffset(arr.elements[arr.elements.length - 1]);
        }
        return arr.colonOffset ?? arr.openBraceOffset ?? 0;
      case 'ObjectConstructorExpression':
        return (node as AST.ObjectConstructorExpression).closeBraceOffset;
      case 'RangeExpression':
        return this.getLastTokenOffset((node as AST.RangeExpression).end);
      case 'LambdaExpression':
        return this.getLastTokenOffset((node as AST.LambdaExpression).body);
      case 'IfExpression':
        const ifExpr = node as AST.IfExpression;
        if (ifExpr.elseBranch) {
          return this.getLastTokenOffset(ifExpr.elseBranch);
        }
        if (ifExpr.thenBranch) {
          return this.getLastTokenOffset(ifExpr.thenBranch);
        }
        return this.getLastTokenOffset(ifExpr.condition);
      case 'ForExpression':
        return this.getLastTokenOffset((node as AST.ForExpression).body);
      case 'LoopExpression':
        return this.getLastTokenOffset((node as AST.LoopExpression).body);
      case 'BlockExpression':
        return this.getLastTokenOffset((node as AST.BlockExpression).body);
      case 'CaseExpression':
        const caseExpr = node as AST.CaseExpression;
        if (caseExpr.closeBraceOffset !== undefined) {
          return caseExpr.closeBraceOffset;
        }
        if (caseExpr.branches.length > 0) {
          const lastBranch = caseExpr.branches[caseExpr.branches.length - 1];
          return this.getLastTokenOffset(lastBranch.body);
        }
        return caseExpr.closeParenOffset;
      default:
        // For other nodes, try to use getFirstTokenOffset as fallback
        return this.getFirstTokenOffset(node);
    }
  }

  /**
   * Get the first token offset of an AST node
   */
  private getFirstTokenOffset(node: AST.ASTNode): number {
    // This needs to return the first token offset for any node type
    switch (node.type) {
      case 'Literal':
        return (node as AST.LiteralExpression).tokenOffset;
      case 'Identifier':
        return (node as AST.IdentifierExpression).tokenOffset;
      case 'VariableDeclaration':
        const varDecl = node as AST.VariableDeclaration;
        // Check for decorators first
        const decoratorOffsets = (varDecl as any).decoratorOffsets;
        if (decoratorOffsets && decoratorOffsets.length > 0) {
          return decoratorOffsets[0];
        }
        return varDecl.varOffset;
      case 'BinaryExpression':
        return this.getFirstTokenOffset((node as AST.BinaryExpression).left);
      case 'UnaryExpression':
        return (node as AST.UnaryExpression).operatorOffset;
      case 'ParenthesizedExpression':
        return (node as AST.ParenthesizedExpression).openParenOffset;
      case 'CompoundExpression':
        return (node as AST.CompoundExpression).openBraceOffset;
      case 'ArrayExpression':
        const arr = node as AST.ArrayExpression;
        return arr.arrayKeywordOffset ?? arr.openBraceOffset ?? 0;
      case 'SetExpression':
        return (node as AST.SetExpression).setOffset;
      case 'BreakExpression':
        return (node as AST.BreakExpression).tokenOffset;
      case 'ContinueExpression':
        return (node as AST.ContinueExpression).tokenOffset;
      case 'ReturnExpression':
        return (node as AST.ReturnExpression).tokenOffset;
      case 'CallExpression':
        return this.getFirstTokenOffset((node as AST.CallExpression).callee);
      case 'MemberExpression':
        return this.getFirstTokenOffset((node as AST.MemberExpression).object);
      case 'AssignmentExpression':
        return this.getFirstTokenOffset((node as AST.AssignmentExpression).left);
      case 'LambdaExpression':
        const lambda = node as AST.LambdaExpression;
        if (lambda.parameters.length > 0) {
          return this.getFirstTokenOffset(lambda.parameters[0]);
        }
        return lambda.arrowOffset;
      case 'IfExpression':
        return (node as AST.IfExpression).ifOffset;
      case 'ForExpression':
        return (node as AST.ForExpression).forOffset;
      case 'LoopExpression':
        return (node as AST.LoopExpression).loopOffset;
      case 'BlockExpression':
        return (node as AST.BlockExpression).blockOffset;
      case 'CaseExpression':
        return (node as AST.CaseExpression).caseOffset;
      case 'RangeExpression':
        return this.getFirstTokenOffset((node as AST.RangeExpression).start);
      case 'ObjectConstructorExpression':
        return (node as AST.ObjectConstructorExpression).typeNameOffset;
      case 'ConstantDeclaration':
        const constDecl = node as AST.ConstantDeclaration;
        const constDecoratorOffsets = (constDecl as any).decoratorOffsets;
        if (constDecoratorOffsets && constDecoratorOffsets.length > 0) {
          return constDecoratorOffsets[0];
        }
        return constDecl.nameOffset;
      case 'FunctionDeclaration':
        const funcDecl = node as AST.FunctionDeclaration;
        const funcDecoratorOffsets = (funcDecl as any).decoratorOffsets;
        if (funcDecoratorOffsets && funcDecoratorOffsets.length > 0) {
          return funcDecoratorOffsets[0];
        }
        return funcDecl.nameOffset;
      case 'DataStructureDeclaration':
        const dataDecl = node as AST.DataStructureDeclaration;
        const dataDecoratorOffsets = (dataDecl as any).decoratorOffsets;
        if (dataDecoratorOffsets && dataDecoratorOffsets.length > 0) {
          return dataDecoratorOffsets[0];
        }
        return dataDecl.nameOffset;
      case 'IdentedCompoundExpression':
        const identedCompound = node as AST.IdentedCompoundExpression;
        if (identedCompound.statements.length > 0) {
          return this.getFirstTokenOffset(identedCompound.statements[0]);
        }
        return 0;
      default:
        // Fallback - this shouldn't happen if all node types are handled
        console.warn(`Unknown node type in getFirstTokenOffset: ${node.type}`);
        return 0;
    }
  }

  /**
   * Append all tokens from current position until the target offset (exclusive)
   */
  private appendTokensUntil(targetOffset: number): void {
    while (this.lastOffset + 1 < targetOffset && this.lastOffset + 1 < this.tokens.length) {
      this.appendToken(this.lastOffset + 1);
    }
  }

  /**
   * Reconstruct a Program (complete file with using statements and declarations)
   */
  private reconstructProgramNode(program: any): void {
    // Program type is from top-level-parser, not in main AST types
    // Structure: { type: 'Program', initialTrivia: [], usingStatements: [], declarations: [] }

    // Start from the beginning if we haven't appended anything yet
    if (this.lastOffset === -1 && program.declarations.length > 0) {
      // Find the first meaningful token (first declaration or using statement)
      let firstOffset = Number.MAX_SAFE_INTEGER;

      if (program.usingStatements && program.usingStatements.length > 0) {
        firstOffset = Math.min(firstOffset, program.usingStatements[0].usingOffset);
      }

      if (program.declarations.length > 0) {
        firstOffset = Math.min(firstOffset, this.getFirstTokenOffset(program.declarations[0]));
      }

      // Append everything before the first meaningful content (initial trivia)
      if (firstOffset > 0 && firstOffset < Number.MAX_SAFE_INTEGER) {
        for (let i = 0; i < firstOffset && i < this.tokens.length; i++) {
          if (this.isTrivia(this.tokens[i]) || this.tokens[i].type === TokenType.EOF) {
            this.appendToken(i);
          }
        }
      }
    }

    // Reconstruct using statements
    if (program.usingStatements) {
      for (let i = 0; i < program.usingStatements.length; i++) {
        const usingStmt = program.usingStatements[i];
        this.reconstructUsingStatement(usingStmt);

        // Append everything between using statements or until first declaration
        if (i < program.usingStatements.length - 1) {
          const nextOffset = program.usingStatements[i + 1].usingOffset;
          this.appendTokensUntil(nextOffset);
        } else if (program.declarations.length > 0) {
          const firstDeclOffset = this.getFirstTokenOffset(program.declarations[0]);
          this.appendTokensUntil(firstDeclOffset);
        }
      }
    }

    // Reconstruct declarations
    for (let i = 0; i < program.declarations.length; i++) {
      this.reconstructNode(program.declarations[i]);

      // Append everything between declarations
      if (i < program.declarations.length - 1) {
        const nextDeclOffset = this.getFirstTokenOffset(program.declarations[i + 1]);
        this.appendTokensUntil(nextDeclOffset);
      }
    }
  }

  /**
   * Reconstruct a using statement
   */
  private reconstructUsingStatement(usingStmt: any): void {
    // UsingStatement: { type: 'UsingStatement', path: string, usingOffset, openBraceOffset, closeBraceOffset, pathOffset }
    this.appendToken(usingStmt.usingOffset);
    this.appendToken(usingStmt.openBraceOffset);

    // Append everything between { and path
    this.appendTokensUntil(usingStmt.pathOffset);

    // The path might be multiple tokens (e.g., /Verse/org/Simulation)
    // We need to append tokens until we reach the closing brace
    let currentOffset = usingStmt.pathOffset;
    while (currentOffset < usingStmt.closeBraceOffset && currentOffset < this.tokens.length) {
      const token = this.tokens[currentOffset];
      if (!this.isTrivia(token)) {
        this.appendToken(currentOffset);
      }
      currentOffset++;
    }

    // Append everything until closing brace
    this.appendTokensUntil(usingStmt.closeBraceOffset);
    this.appendToken(usingStmt.closeBraceOffset);
  }

  private reconstructSet(node: AST.SetExpression): void {
    this.appendToken(node.setOffset);
    this.reconstructNode(node.target);
    this.appendToken(node.equalsOffset);
    this.reconstructNode(node.value);
  }

  private reconstructFor(node: AST.ForExpression): void {
    this.appendToken(node.forOffset);

    if (node.openParenOffset !== undefined) {
      this.appendToken(node.openParenOffset);
    }

    if (node.indexVariableOffset !== undefined) {
      this.appendToken(node.indexVariableOffset);
      if (node.arrowOffset !== undefined) {
        this.appendToken(node.arrowOffset);
      }
    }

    this.appendToken(node.variableOffset);
    this.appendToken(node.colonOffset);
    this.reconstructNode(node.iterable);

    if (node.closeParenOffset !== undefined) {
      this.appendToken(node.closeParenOffset);

      // Check if there's a colon after the closing paren (indented form)
      const nextTokenOffset = node.closeParenOffset + 1;
      if (nextTokenOffset < this.tokens.length) {
        const nextToken = this.tokens[nextTokenOffset];
        if (nextToken.type === TokenType.OPERATOR && nextToken.content === ':') {
          // This is an indented for loop, append the colon
          this.appendToken(nextTokenOffset);
        }
      }
    }

    if (node.doOffset !== undefined) {
      this.appendToken(node.doOffset);
    }

    this.reconstructNode(node.body);
  }

  private reconstructIf(node: AST.IfExpression): void {
    this.appendToken(node.ifOffset);
    this.reconstructNode(node.condition);

    // Handle then branch
    if (node.thenBranch) {
      if (node.thenOffset !== undefined) {
        // Has explicit 'then' keyword
        this.appendToken(node.thenOffset);

        // Check if there's a colon after 'then' (indented form)
        const nextTokenOffset = node.thenOffset + 1;
        if (nextTokenOffset < this.tokens.length) {
          const nextToken = this.tokens[nextTokenOffset];
          if (nextToken.type === TokenType.OPERATOR && nextToken.content === ':') {
            this.appendToken(nextTokenOffset);
          }
        }
      } else {
        // No 'then' keyword, check for colon after condition (Python-style if)
        // We need to find the colon that comes after the condition
        const conditionEndOffset = this.getLastTokenOffset(node.condition);
        let colonOffset = conditionEndOffset + 1;

        // Skip any trivia to find the colon
        while (colonOffset < this.tokens.length) {
          const token = this.tokens[colonOffset];
          if (token.type === TokenType.OPERATOR && token.content === ':') {
            this.appendToken(colonOffset);
            break;
          } else if (!this.isTrivia(token)) {
            // No colon found, might be brace syntax
            break;
          }
          colonOffset++;
        }
      }

      this.reconstructNode(node.thenBranch);
    }

    // Handle else branch
    if (node.elseOffset !== undefined && node.elseBranch) {
      this.appendToken(node.elseOffset);

      // Check if there's a colon after 'else' (indented form)
      const nextTokenOffset = node.elseOffset + 1;
      if (nextTokenOffset < this.tokens.length) {
        const nextToken = this.tokens[nextTokenOffset];
        if (nextToken.type === TokenType.OPERATOR && nextToken.content === ':') {
          this.appendToken(nextTokenOffset);
        }
      }

      this.reconstructNode(node.elseBranch);
    }
  }

  private reconstructLoop(node: AST.LoopExpression): void {
    this.appendToken(node.loopOffset);

    // For indented form, check if there's a colon after 'loop'
    const nextTokenOffset = node.loopOffset + 1;
    if (nextTokenOffset < this.tokens.length) {
      const nextToken = this.tokens[nextTokenOffset];
      if (nextToken.type === TokenType.OPERATOR && nextToken.content === ':') {
        this.appendToken(nextTokenOffset);
      }
    }

    this.reconstructNode(node.body);
  }

  private reconstructBlock(node: AST.BlockExpression): void {
    // BlockExpression is just a wrapper around IdentedCompoundExpression
    // The body already contains the block keyword and colon offsets,
    // so we just reconstruct the body directly
    this.reconstructNode(node.body);
  }

  private reconstructCase(node: AST.CaseExpression): void {
    this.appendToken(node.caseOffset);
    this.appendToken(node.openParenOffset);
    this.reconstructNode(node.scrutinee);
    this.appendToken(node.closeParenOffset);

    if (node.openBraceOffset !== undefined) {
      this.appendToken(node.openBraceOffset);

      // After opening brace, append everything until first branch
      if (node.branches.length > 0) {
        const firstBranch = node.branches[0];
        const firstBranchOffset = this.getCaseBranchFirstOffset(firstBranch);
        this.appendTokensUntil(firstBranchOffset);
      }

      for (let i = 0; i < node.branches.length; i++) {
        const branch = node.branches[i];

        // Reconstruct pattern
        if (typeof branch.pattern === 'string') {
          // Wildcard pattern - append the underscore token
          // We need to find it by looking for the underscore before the arrow
          let wildcardOffset = branch.arrowOffset - 1;
          while (wildcardOffset > 0 && this.tokens[wildcardOffset].content !== '_') {
            wildcardOffset--;
          }
          if (wildcardOffset > 0) {
            this.appendToken(wildcardOffset);
          }
        } else {
          this.reconstructNode(branch.pattern);
        }

        // Append everything between pattern and arrow
        this.appendTokensUntil(branch.arrowOffset);
        this.appendToken(branch.arrowOffset);

        // Reconstruct body
        this.reconstructNode(branch.body);

        // After each branch (except the last), append everything until next branch
        if (i < node.branches.length - 1) {
          const nextBranch = node.branches[i + 1];
          const nextBranchOffset = this.getCaseBranchFirstOffset(nextBranch);
          this.appendTokensUntil(nextBranchOffset);
        }
      }

      // After last branch, append everything until closing brace
      if (node.closeBraceOffset !== undefined) {
        this.appendTokensUntil(node.closeBraceOffset);
        this.appendToken(node.closeBraceOffset);
      }
    } else if (node.colonOffset !== undefined) {
      this.appendToken(node.colonOffset);

      // Similar handling for indented case
      if (node.branches.length > 0) {
        const firstBranch = node.branches[0];
        const firstBranchOffset = this.getCaseBranchFirstOffset(firstBranch);
        this.appendTokensUntil(firstBranchOffset);
      }

      for (let i = 0; i < node.branches.length; i++) {
        const branch = node.branches[i];

        if (typeof branch.pattern === 'string') {
          let wildcardOffset = branch.arrowOffset - 1;
          while (wildcardOffset > 0 && this.tokens[wildcardOffset].content !== '_') {
            wildcardOffset--;
          }
          if (wildcardOffset > 0) {
            this.appendToken(wildcardOffset);
          }
        } else {
          this.reconstructNode(branch.pattern);
        }

        this.appendTokensUntil(branch.arrowOffset);
        this.appendToken(branch.arrowOffset);
        this.reconstructNode(branch.body);

        if (i < node.branches.length - 1) {
          const nextBranch = node.branches[i + 1];
          const nextBranchOffset = this.getCaseBranchFirstOffset(nextBranch);
          this.appendTokensUntil(nextBranchOffset);
        }
      }
    }
  }

  /**
   * Get the first token offset of a case branch
   */
  private getCaseBranchFirstOffset(branch: AST.CaseBranch): number {
    if (typeof branch.pattern === 'string') {
      // Wildcard - need to find the underscore token
      let wildcardOffset = branch.arrowOffset - 1;
      while (wildcardOffset > 0 && this.tokens[wildcardOffset].content !== '_') {
        wildcardOffset--;
      }
      return wildcardOffset > 0 ? wildcardOffset : branch.arrowOffset;
    } else {
      return this.getFirstTokenOffset(branch.pattern);
    }
  }

  private reconstructIndentedCompound(node: AST.IdentedCompoundExpression): void {
    this.appendToken(node.keywordOffset);
    this.appendToken(node.colonOffset);

    for (let i = 0; i < node.expressions.length; i++) {
      this.reconstructNode(node.expressions[i]);
      if (i < node.separatorOffsets.length) {
        this.appendToken(node.separatorOffsets[i]);
      }
    }
  }

  private reconstructBreak(node: AST.BreakExpression): void {
    this.appendToken(node.tokenOffset);
  }

  private reconstructContinue(node: AST.ContinueExpression): void {
    this.appendToken(node.tokenOffset);
  }

  private reconstructReturn(node: AST.ReturnExpression): void {
    this.appendToken(node.tokenOffset);
    if (node.value) {
      this.reconstructNode(node.value);
    }
  }

  private reconstructConstant(node: AST.ConstantDeclaration): void {
    // Check for decorators
    const decoratorOffsets = (node as any).decoratorOffsets;
    if (decoratorOffsets) {
      for (const offset of decoratorOffsets) {
        this.appendToken(offset);
      }
    }

    this.appendToken(node.nameOffset);

    if (node.specifiers) {
      this.reconstructSpecifiers(node.specifiers);
    }

    if (node.colonOffset !== undefined) {
      this.appendToken(node.colonOffset);
      if (node.declaredType) {
        this.reconstructType(node.declaredType);
      }
    }

    if (node.equalsOffset !== undefined) {
      this.appendToken(node.equalsOffset);
    } else if (node.assignOffset !== undefined) {
      this.appendToken(node.assignOffset);
    }

    if (node.initializer) {
      this.reconstructNode(node.initializer);
    }
  }

  private reconstructVariable(node: AST.VariableDeclaration): void {
    // Check for decorators
    const decoratorOffsets = (node as any).decoratorOffsets;
    if (decoratorOffsets) {
      for (const offset of decoratorOffsets) {
        this.appendToken(offset);
      }
    }

    this.appendToken(node.varOffset);
    this.appendToken(node.nameOffset);

    if (node.specifiers) {
      this.reconstructSpecifiers(node.specifiers);
    }

    this.appendToken(node.colonOffset);
    this.reconstructType(node.declaredType);

    if (node.equalsOffset !== undefined && node.initializer) {
      this.appendToken(node.equalsOffset);
      this.reconstructNode(node.initializer);
    }
  }

  private reconstructFunction(node: AST.FunctionDeclaration): void {
    // Check for decorators
    const decoratorOffsets = (node as any).decoratorOffsets;
    if (decoratorOffsets) {
      for (const offset of decoratorOffsets) {
        this.appendToken(offset);
      }
    }

    this.appendToken(node.nameOffset);

    if (node.preSpecifiers) {
      this.reconstructSpecifiers(node.preSpecifiers);
    }

    this.appendToken(node.openParenOffset);

    for (let i = 0; i < node.parameters.length; i++) {
      this.reconstructParameter(node.parameters[i]);
      if (i < node.paramSeparatorOffsets.length) {
        this.appendToken(node.paramSeparatorOffsets[i]);
      }
    }

    this.appendToken(node.closeParenOffset);

    if (node.postSpecifiers) {
      this.reconstructSpecifiers(node.postSpecifiers);
    }

    if (node.returnColonOffset !== undefined && node.returnType) {
      this.appendToken(node.returnColonOffset);
      this.reconstructType(node.returnType);
    }

    if (node.assignOffset !== undefined) {
      this.appendToken(node.assignOffset);
    } else if (node.equalsOffset !== undefined) {
      this.appendToken(node.equalsOffset);
    }

    this.reconstructNode(node.body);
  }

  private reconstructDataStructure(node: AST.DataStructureDeclaration): void {
    // Check for decorators (added by parser but not in type definition)
    const decorators = (node as any).decorators;
    const decoratorOffsets = (node as any).decoratorOffsets;
    if (decorators && decoratorOffsets) {
      for (let i = 0; i < decoratorOffsets.length; i++) {
        this.appendToken(decoratorOffsets[i]);
      }
    }

    this.appendToken(node.nameOffset);

    if (node.nameSpecifiers) {
      this.reconstructSpecifiers(node.nameSpecifiers);
    }

    this.appendToken(node.assignOffset);
    this.appendToken(node.kindOffset);

    if (node.kindSpecifiers) {
      this.reconstructSpecifiers(node.kindSpecifiers);
    }

    if (node.openParenOffset !== undefined && node.argument) {
      this.appendToken(node.openParenOffset);
      this.reconstructNode(node.argument);
      if (node.closeParenOffset !== undefined) {
        this.appendToken(node.closeParenOffset);
      }
    }

    if (node.postSpecifiers) {
      this.reconstructSpecifiers(node.postSpecifiers);
    }

    if (node.openBraceOffset !== undefined) {
      this.appendToken(node.openBraceOffset);

      for (let i = 0; i < node.body.length; i++) {
        this.reconstructNode(node.body[i]);
        if (i < node.bodySeparatorOffsets.length) {
          this.appendToken(node.bodySeparatorOffsets[i]);
        }
      }

      if (node.closeBraceOffset !== undefined) {
        this.appendToken(node.closeBraceOffset);
      }
    } else if (node.colonOffset !== undefined) {
      this.appendToken(node.colonOffset);

      for (let i = 0; i < node.body.length; i++) {
        this.reconstructNode(node.body[i]);
        if (i < node.bodySeparatorOffsets.length) {
          this.appendToken(node.bodySeparatorOffsets[i]);
        }
      }
    }
  }

  private reconstructType(node: AST.TypeExpression): void {
    if (node.optionalOffset !== undefined) {
      this.appendToken(node.optionalOffset);
    }

    if (node.arrayOffsets) {
      for (const offset of node.arrayOffsets) {
        this.appendToken(offset);
        // Append the closing bracket that follows the opening bracket
        const closeBracketOffset = offset + 1;
        if (closeBracketOffset < this.tokens.length &&
            this.tokens[closeBracketOffset].type === TokenType.OPERATOR &&
            this.tokens[closeBracketOffset].content === ']') {
          this.appendToken(closeBracketOffset);
        }
      }
    }

    this.appendToken(node.typeNameOffset);
  }

  private reconstructSpecifiers(node: AST.SpecifierList): void {
    // Specifiers may be stored as a single token like "<public>"
    // or as separate tokens. Check the first specifier offset
    if (node.specifierOffsets.length > 0) {
      const firstToken = this.tokens[node.specifierOffsets[0]];
      if (firstToken && firstToken.type === TokenType.SPECIFIER) {
        // It's a single SPECIFIER token that includes the angle brackets
        // Just append the whole token
        for (let i = 0; i < node.specifierOffsets.length; i++) {
          this.appendToken(node.specifierOffsets[i]);
        }
        return;
      }
    }

    // Otherwise, reconstruct with separate angle brackets
    this.appendToken(node.openAngleOffset);

    for (let i = 0; i < node.specifiers.length; i++) {
      this.appendToken(node.specifierOffsets[i]);
      if (i < node.separatorOffsets.length) {
        this.appendToken(node.separatorOffsets[i]);
      }
    }

    this.appendToken(node.closeAngleOffset);
  }

  private reconstructParameter(node: AST.Parameter): void {
    this.appendToken(node.nameOffset);

    if (node.colonOffset !== undefined && node.paramType) {
      this.appendToken(node.colonOffset);
      this.reconstructType(node.paramType);
    }
  }

  private reconstructEnumMember(node: AST.EnumMember): void {
    this.appendToken(node.nameOffset);

    if (node.specifiers) {
      this.reconstructSpecifiers(node.specifiers);
    }

    if (node.equalsOffset !== undefined && node.value) {
      this.appendToken(node.equalsOffset);
      this.reconstructNode(node.value);
    }
  }
}

/**
 * Convenience function to reconstruct source from an AST node
 */
export function reconstructFromAST(
  source: string,
  node: AST.ASTNode,
  options?: ReconstructionOptions
): string {
  const reconstructor = new ASTReconstructor(source, options?.tokenStream);
  return reconstructor.reconstruct(node, options);
}

/**
 * Convenience function to reconstruct source from a program (array of nodes)
 */
export function reconstructProgramFromAST(
  source: string,
  nodes: AST.ASTNode[],
  options?: ReconstructionOptions
): string {
  const reconstructor = new ASTReconstructor(source, options?.tokenStream);
  return reconstructor.reconstructProgram(nodes, options);
}