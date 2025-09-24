/**
 * Proof of Concept: Source Range Based Reconstruction
 *
 * This demonstrates how perfect whitespace preservation could be achieved
 * by tracking source ranges during parsing.
 */

import * as AST from '../parser/ast';

/**
 * Source range information for any AST node
 */
export interface SourceRange {
  /** Starting byte offset in source (inclusive) */
  startOffset: number;

  /** Ending byte offset in source (exclusive) */
  endOffset: number;

  /** Optional: Start of meaningful content (after leading trivia) */
  contentStartOffset?: number;

  /** Optional: End of meaningful content (before trailing trivia) */
  contentEndOffset?: number;
}

/**
 * Enhanced AST node with source range information
 */
export interface RangedASTNode extends AST.ASTNode {
  sourceRange?: SourceRange;
}

/**
 * Source-range based reconstructor
 *
 * This reconstructor uses source ranges to perfectly preserve all whitespace
 * and formatting from the original source.
 */
export class SourceRangeReconstructor {
  private source: string;

  constructor(source: string) {
    this.source = source;
  }

  /**
   * Reconstruct an AST node using its source range
   */
  reconstruct(node: RangedASTNode): string {
    if (node.sourceRange) {
      // Perfect reconstruction - just extract the source
      return this.extractRange(node.sourceRange);
    } else {
      // Fallback for nodes without source ranges
      throw new Error(`Node type ${node.type} lacks source range information`);
    }
  }

  /**
   * Extract a substring using a source range
   */
  private extractRange(range: SourceRange): string {
    return this.source.substring(range.startOffset, range.endOffset);
  }

  /**
   * Reconstruct with transformation support
   * This allows modifying the AST while preserving formatting
   */
  reconstructWithTransform(
    node: RangedASTNode,
    transformer?: (content: string) => string
  ): string {
    if (!node.sourceRange) {
      throw new Error(`Node type ${node.type} lacks source range information`);
    }

    const range = node.sourceRange;

    if (!transformer) {
      // No transformation - exact reconstruction
      return this.extractRange(range);
    }

    // Extract parts
    const leadingTrivia = this.extractLeadingTrivia(range);
    const content = this.extractContent(range);
    const trailingTrivia = this.extractTrailingTrivia(range);

    // Transform only the content, preserve trivia
    const transformedContent = transformer(content);

    return leadingTrivia + transformedContent + trailingTrivia;
  }

  /**
   * Extract leading trivia (whitespace/comments before content)
   */
  private extractLeadingTrivia(range: SourceRange): string {
    if (range.contentStartOffset !== undefined) {
      return this.source.substring(range.startOffset, range.contentStartOffset);
    }
    return '';
  }

  /**
   * Extract actual content (excluding trivia)
   */
  private extractContent(range: SourceRange): string {
    const start = range.contentStartOffset ?? range.startOffset;
    const end = range.contentEndOffset ?? range.endOffset;
    return this.source.substring(start, end);
  }

  /**
   * Extract trailing trivia (whitespace/comments after content)
   */
  private extractTrailingTrivia(range: SourceRange): string {
    if (range.contentEndOffset !== undefined) {
      return this.source.substring(range.contentEndOffset, range.endOffset);
    }
    return '';
  }
}

/**
 * Example: Enhanced parser that tracks source ranges
 */
export class SourceRangeTrackingParser {
  private source: string;
  private position: number = 0;

  constructor(source: string) {
    this.source = source;
  }

  /**
   * Example: Parse an identifier with source range tracking
   */
  parseIdentifier(): RangedASTNode {
    const startOffset = this.position;

    // Skip leading whitespace
    while (this.position < this.source.length && /\s/.test(this.source[this.position])) {
      this.position++;
    }

    const contentStartOffset = this.position;

    // Parse identifier
    const identStart = this.position;
    while (this.position < this.source.length && /[a-zA-Z_]/.test(this.source[this.position])) {
      this.position++;
    }

    const name = this.source.substring(identStart, this.position);
    const contentEndOffset = this.position;

    // Skip trailing whitespace (up to newline)
    while (this.position < this.source.length &&
           this.source[this.position] === ' ') {
      this.position++;
    }

    const endOffset = this.position;

    return {
      type: 'Identifier',
      sourceRange: {
        startOffset,
        endOffset,
        contentStartOffset,
        contentEndOffset
      }
    } as RangedASTNode;
  }
}

/**
 * Example usage demonstrating perfect whitespace preservation
 */
export function demonstrateSourceRangeReconstruction(): void {
  const source = `  identifier   `; // Note the spaces

  // Parse with source range tracking
  const parser = new SourceRangeTrackingParser(source);
  const ast = parser.parseIdentifier();

  // Reconstruct - preserves all whitespace perfectly
  const reconstructor = new SourceRangeReconstructor(source);
  const reconstructed = reconstructor.reconstruct(ast);

  console.log('Original:', JSON.stringify(source));
  console.log('Reconstructed:', JSON.stringify(reconstructed));
  console.log('Match:', source === reconstructed);

  // Example with transformation
  const transformed = reconstructor.reconstructWithTransform(ast,
    content => content.toUpperCase()
  );
  console.log('Transformed:', JSON.stringify(transformed)); // "  IDENTIFIER   "
}

/**
 * Migration helper: Add source ranges to existing AST
 */
export function addSourceRangesToAST(
  node: AST.ASTNode,
  source: string,
  tokenOffsets: Map<AST.ASTNode, number>
): RangedASTNode {
  // This would be implemented to calculate source ranges
  // based on existing token offset information

  // For now, just return the node as-is
  return node as RangedASTNode;
}