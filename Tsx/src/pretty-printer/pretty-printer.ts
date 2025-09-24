/**
 * Pretty Printer for Verse AST
 *
 * This module reconstructs source code from AST nodes using token offsets.
 * It retrieves original tokens from the token stream and prints them
 * along with their trailing trivia to preserve formatting.
 *
 * Key features:
 * - Uses token offsets stored in AST nodes
 * - Preserves trailing trivia (whitespace, comments)
 * - Handles all expression types
 * - Reconstructs original formatting
 */

import { TokenStream } from '../lexer/tokenstream';
import { Token, TokenType } from '../lexer/token';
import * as AST from '../parser/ast';

/**
 * Pretty printer that reconstructs source from AST using token offsets.
 */
export class PrettyPrinter {
  private tokens: Token[];
  private printedUpTo: number;

  constructor(private tokenStream: TokenStream) {
    this.tokens = tokenStream.getAllTokens();
    this.printedUpTo = 0;
  }

  /**
   * Print an expression and reconstruct its source.
   */
  print(node: AST.Expression): string {
    this.printedUpTo = 0;
    return this.printExpression(node);
  }

  /**
   * Print a token at the given offset with its trailing trivia.
   */
  private printToken(offset: number): string {
    if (offset >= this.tokens.length) return '';

    const token = this.tokens[offset];
    let result = token.content;

    // Add quotes for strings if not present
    if (token.type === TokenType.STRING && !token.content.startsWith('"') && !token.content.startsWith("'")) {
      result = '"' + result + '"';
    }

    // Collect trailing trivia
    result += this.getTrailingTrivia(offset);

    return result;
  }

  /**
   * Get trailing trivia after a token.
   */
  private getTrailingTrivia(offset: number): string {
    let result = '';
    let pos = offset + 1;

    while (pos < this.tokens.length) {
      const token = this.tokens[pos];

      // Stop at non-trivia tokens
      if (!token.isTrivia() && !token.isWhitespace() && !token.isComment() &&
          token.type !== TokenType.NEWLINE) {
        break;
      }

      result += token.content;
      pos++;
    }

    // Update how far we've printed
    if (pos > this.printedUpTo) {
      this.printedUpTo = pos;
    }

    return result;
  }

  /**
   * Print tokens between two offsets (exclusive of end).
   */
  private printTokenRange(start: number, end: number): string {
    let result = '';
    for (let i = start; i < end && i < this.tokens.length; i++) {
      const token = this.tokens[i];
      if (token.type === TokenType.STRING && !token.content.startsWith('"') && !token.content.startsWith("'")) {
        result += '"' + token.content + '"';
      } else {
        result += token.content;
      }
    }
    return result;
  }

  /**
   * Print any skipped tokens up to the given offset.
   */
  private printSkippedTokens(upTo: number): string {
    if (this.printedUpTo >= upTo) return '';

    const result = this.printTokenRange(this.printedUpTo, upTo);
    this.printedUpTo = upTo;
    return result;
  }

  /**
   * Main expression printing dispatcher.
   */
  private printExpression(node: AST.Expression): string {
    switch (node.type) {
      case 'Literal':
        return this.printLiteral(node as AST.LiteralExpression);
      case 'Identifier':
        return this.printIdentifier(node as AST.IdentifierExpression);
      case 'BinaryExpression':
        return this.printBinaryExpression(node as AST.BinaryExpression);
      case 'UnaryExpression':
        return this.printUnaryExpression(node as AST.UnaryExpression);
      case 'AssignmentExpression':
        return this.printAssignmentExpression(node as AST.AssignmentExpression);
      case 'RangeExpression':
        return this.printRangeExpression(node as AST.RangeExpression);
      case 'MemberExpression':
        return this.printMemberExpression(node as AST.MemberExpression);
      case 'CallExpression':
        return this.printCallExpression(node as AST.CallExpression);
      case 'ArrayExpression':
        return this.printArrayExpression(node as AST.ArrayExpression);
      case 'LambdaExpression':
        return this.printLambdaExpression(node as AST.LambdaExpression);
      case 'ParenthesizedExpression':
        return this.printParenthesizedExpression(node as AST.ParenthesizedExpression);
      case 'CompoundExpression':
        return this.printCompoundExpression(node as AST.CompoundExpression);
      case 'IdentedCompoundExpression':
        return this.printIdentedCompoundExpression(node as AST.IdentedCompoundExpression);
      default:
        return '';
    }
  }

  private printLiteral(node: AST.LiteralExpression): string {
    let result = this.printSkippedTokens(node.tokenOffset);
    result += this.printToken(node.tokenOffset);
    return result;
  }

  private printIdentifier(node: AST.IdentifierExpression): string {
    let result = this.printSkippedTokens(node.tokenOffset);
    result += this.printToken(node.tokenOffset);
    return result;
  }

  private printBinaryExpression(node: AST.BinaryExpression): string {
    let result = this.printExpression(node.left);
    result += this.printSkippedTokens(node.operatorOffset);
    result += this.printToken(node.operatorOffset);
    result += this.printExpression(node.right);
    return result;
  }

  private printUnaryExpression(node: AST.UnaryExpression): string {
    let result = this.printSkippedTokens(node.operatorOffset);
    result += this.printToken(node.operatorOffset);
    result += this.printExpression(node.operand);
    return result;
  }

  private printAssignmentExpression(node: AST.AssignmentExpression): string {
    let result = this.printExpression(node.left);
    result += this.printSkippedTokens(node.operatorOffset);
    result += this.printToken(node.operatorOffset);
    result += this.printExpression(node.right);
    return result;
  }

  private printRangeExpression(node: AST.RangeExpression): string {
    let result = this.printExpression(node.start);
    result += this.printSkippedTokens(node.operatorOffset);
    result += this.printToken(node.operatorOffset);
    result += this.printExpression(node.end);
    return result;
  }

  private printMemberExpression(node: AST.MemberExpression): string {
    let result = this.printExpression(node.object);

    if (node.computed) {
      // Find the [ token
      let bracketOffset = this.findNextToken('[', this.printedUpTo);
      if (bracketOffset !== -1) {
        result += this.printSkippedTokens(bracketOffset);
        result += this.printToken(bracketOffset);
      }

      result += this.printExpression(node.property);

      // Find the ] token
      let closeBracketOffset = this.findNextToken(']', this.printedUpTo);
      if (closeBracketOffset !== -1) {
        result += this.printSkippedTokens(closeBracketOffset);
        result += this.printToken(closeBracketOffset);
      }
    } else {
      // Find the . token
      let dotOffset = this.findNextToken('.', this.printedUpTo);
      if (dotOffset !== -1) {
        result += this.printSkippedTokens(dotOffset);
        result += this.printToken(dotOffset);
      }
      result += this.printExpression(node.property);
    }

    return result;
  }

  private printCallExpression(node: AST.CallExpression): string {
    let result = this.printExpression(node.callee);

    // Find the ( token
    let openParenOffset = this.findNextToken('(', this.printedUpTo);
    if (openParenOffset !== -1) {
      result += this.printSkippedTokens(openParenOffset);
      result += this.printToken(openParenOffset);
    }

    // Print arguments
    for (let i = 0; i < node.arguments.length; i++) {
      result += this.printExpression(node.arguments[i]);

      if (i < node.arguments.length - 1) {
        // Find comma separator
        let commaOffset = this.findNextToken(',', this.printedUpTo);
        if (commaOffset !== -1) {
          result += this.printSkippedTokens(commaOffset);
          result += this.printToken(commaOffset);
        }
      }
    }

    // Find the ) token
    let closeParenOffset = this.findNextToken(')', this.printedUpTo);
    if (closeParenOffset !== -1) {
      result += this.printSkippedTokens(closeParenOffset);
      result += this.printToken(closeParenOffset);
    }

    return result;
  }

  private printArrayExpression(node: AST.ArrayExpression): string {
    let result = '';

    // Print array keyword
    if (node.arrayKeywordOffset !== undefined) {
      result += this.printSkippedTokens(node.arrayKeywordOffset);
      result += this.printToken(node.arrayKeywordOffset);
    }

    // Check if braced or indented syntax
    if (node.openBraceOffset !== undefined) {
      // array{...} syntax
      result += this.printSkippedTokens(node.openBraceOffset);
      result += this.printToken(node.openBraceOffset);

      // Print elements with separators
      for (let i = 0; i < node.elements.length; i++) {
        result += this.printExpression(node.elements[i]);

        if (i < node.separatorOffsets.length) {
          result += this.printSkippedTokens(node.separatorOffsets[i]);
          result += this.printToken(node.separatorOffsets[i]);
        }
      }

      if (node.closeBraceOffset !== undefined) {
        result += this.printSkippedTokens(node.closeBraceOffset);
        result += this.printToken(node.closeBraceOffset);
      }
    } else if (node.colonOffset !== undefined) {
      // array: syntax
      result += this.printSkippedTokens(node.colonOffset);
      result += this.printToken(node.colonOffset);

      // Print elements with separators
      for (let i = 0; i < node.elements.length; i++) {
        result += this.printExpression(node.elements[i]);

        // For indented arrays, check if separator wasn't already consumed
        if (i < node.separatorOffsets.length && node.separatorOffsets[i] >= this.printedUpTo) {
          result += this.printSkippedTokens(node.separatorOffsets[i]);

          // Only print if not already consumed
          if (node.separatorOffsets[i] >= this.printedUpTo) {
            result += this.printToken(node.separatorOffsets[i]);
          }
        }
      }
    }

    return result;
  }

  private printLambdaExpression(node: AST.LambdaExpression): string {
    let result = '';

    // Check if parameters are parenthesized
    if (node.parameters.length === 0 || node.parameters.length > 1) {
      // Find opening paren
      let openParenOffset = this.findNextToken('(', this.printedUpTo);
      if (openParenOffset !== -1) {
        result += this.printSkippedTokens(openParenOffset);
        result += this.printToken(openParenOffset);
      }
    }

    // Print parameters
    for (let i = 0; i < node.parameters.length; i++) {
      result += this.printExpression(node.parameters[i]);

      if (i < node.parameters.length - 1) {
        // Find comma separator
        let commaOffset = this.findNextToken(',', this.printedUpTo);
        if (commaOffset !== -1) {
          result += this.printSkippedTokens(commaOffset);
          result += this.printToken(commaOffset);
        }
      }
    }

    // Close parens if needed
    if (node.parameters.length === 0 || node.parameters.length > 1) {
      let closeParenOffset = this.findNextToken(')', this.printedUpTo);
      if (closeParenOffset !== -1) {
        result += this.printSkippedTokens(closeParenOffset);
        result += this.printToken(closeParenOffset);
      }
    }

    // Print arrow
    result += this.printSkippedTokens(node.arrowOffset);
    result += this.printToken(node.arrowOffset);

    // Print body
    result += this.printExpression(node.body);

    return result;
  }

  private printParenthesizedExpression(node: AST.ParenthesizedExpression): string {
    let result = '';

    // Find opening paren
    let openParenOffset = this.findNextToken('(', this.printedUpTo);
    if (openParenOffset !== -1) {
      result += this.printSkippedTokens(openParenOffset);
      result += this.printToken(openParenOffset);
    }

    result += this.printExpression(node.expression);

    // Find closing paren
    let closeParenOffset = this.findNextToken(')', this.printedUpTo);
    if (closeParenOffset !== -1) {
      result += this.printSkippedTokens(closeParenOffset);
      result += this.printToken(closeParenOffset);
    }

    return result;
  }

  private printCompoundExpression(node: AST.CompoundExpression): string {
    let result = '';

    // Print opening brace
    result += this.printSkippedTokens(node.openBraceOffset);
    result += this.printToken(node.openBraceOffset);

    // Print expressions with separators
    for (let i = 0; i < node.expressions.length; i++) {
      result += this.printExpression(node.expressions[i]);

      if (i < node.separatorOffsets.length) {
        result += this.printSkippedTokens(node.separatorOffsets[i]);
        result += this.printToken(node.separatorOffsets[i]);
      }
    }

    // Print closing brace
    result += this.printSkippedTokens(node.closeBraceOffset);
    result += this.printToken(node.closeBraceOffset);

    return result;
  }

  private printIdentedCompoundExpression(node: AST.IdentedCompoundExpression): string {
    let result = '';

    // Print keyword
    result += this.printSkippedTokens(node.keywordOffset);
    result += this.printToken(node.keywordOffset);

    // Print colon
    result += this.printSkippedTokens(node.colonOffset);
    result += this.printToken(node.colonOffset);

    // Print expressions with separators
    for (let i = 0; i < node.expressions.length; i++) {
      result += this.printExpression(node.expressions[i]);

      // For indented blocks, separators might already be printed as trailing trivia
      // Only print if we haven't already moved past this position
      if (i < node.separatorOffsets.length && node.separatorOffsets[i] >= this.printedUpTo) {
        result += this.printSkippedTokens(node.separatorOffsets[i]);

        // Only print the separator if it hasn't been consumed by trailing trivia
        if (node.separatorOffsets[i] >= this.printedUpTo) {
          result += this.printToken(node.separatorOffsets[i]);
        }
      }
    }

    return result;
  }

  /**
   * Find the next token with the given content starting from offset.
   */
  private findNextToken(content: string, fromOffset: number): number {
    for (let i = fromOffset; i < this.tokens.length; i++) {
      if (this.tokens[i].content === content) {
        return i;
      }
    }
    return -1;
  }
}

/**
 * Pretty print an AST node to source code.
 */
export function prettyPrint(node: AST.Expression, tokenStream: TokenStream): string {
  const printer = new PrettyPrinter(tokenStream);
  return printer.print(node);
}