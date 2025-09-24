/**
 * Example: Integrating Source Range Tracking into Existing Parser
 *
 * This shows how we could add source range tracking to the existing
 * parser with minimal disruption to the codebase.
 */

import { ParserState } from './parser-state';
import * as AST from './ast';

/**
 * Extended AST types with optional source range
 * This is backward compatible - existing code still works
 */
export interface RangedExpression extends AST.Expression {
  sourceRange?: {
    startOffset: number;
    endOffset: number;
  };
}

/**
 * Helper mixin for parser methods to track source ranges
 */
export class SourceRangeTracker {
  /**
   * Wrap a parser method to automatically track source ranges
   */
  static withRange<T extends AST.ASTNode>(
    parseMethod: (state: ParserState) => { node: T; state: ParserState } | null
  ): (state: ParserState) => { node: T & { sourceRange: any }; state: ParserState } | null {
    return (state: ParserState) => {
      // Record start position (in characters, not tokens)
      const startOffset = state.currentSourcePosition();

      // Call the original parser method
      const result = parseMethod(state);
      if (!result) return null;

      // Record end position
      const endOffset = result.state.currentSourcePosition();

      // Add source range to the node
      const nodeWithRange = {
        ...result.node,
        sourceRange: {
          startOffset,
          endOffset
        }
      };

      return {
        node: nodeWithRange,
        state: result.state
      };
    };
  }
}

/**
 * Extended ParserState with source position tracking
 */
export interface RangedParserState extends ParserState {
  /**
   * Get current position in source string (character offset)
   */
  currentSourcePosition(): number;
}

/**
 * Example: Modified parseIdentifier that tracks source ranges
 */
export function parseIdentifierWithRange(
  state: RangedParserState
): { node: RangedExpression; state: RangedParserState } | null {
  const startOffset = state.currentSourcePosition();

  // Skip trivia to find actual content start
  const triviaSkipped = state.skipTrivia();
  const contentStartOffset = triviaSkipped.currentSourcePosition();

  const token = triviaSkipped.current();
  if (!token || token.type !== 'IDENTIFIER') {
    return null;
  }

  const name = token.content;
  const tokenOffset = triviaSkipped.currentOffset();

  // Advance past the identifier
  const advanced = triviaSkipped.advance();
  const contentEndOffset = advanced.currentSourcePosition();

  // Skip trailing spaces (but not newlines)
  let endState = advanced;
  while (endState.current()?.type === 'SPACE') {
    endState = endState.advance();
  }
  const endOffset = endState.currentSourcePosition();

  const node: RangedExpression = {
    type: 'Identifier',
    name,
    tokenOffset,
    sourceRange: {
      startOffset,
      endOffset
    }
  };

  return { node, state: endState };
}

/**
 * Example: Retrofitting existing parser methods
 */
export class RangedParser {
  /**
   * Parse binary expression with automatic range tracking
   */
  parseBinaryExpression = SourceRangeTracker.withRange(
    (state: ParserState) => {
      // Existing binary expression parsing logic
      // ... (unchanged) ...
      return null; // Placeholder
    }
  );

  /**
   * Parse if expression with automatic range tracking
   */
  parseIfExpression = SourceRangeTracker.withRange(
    (state: ParserState) => {
      // Existing if expression parsing logic
      // ... (unchanged) ...
      return null; // Placeholder
    }
  );
}

/**
 * Hybrid reconstructor that uses source ranges when available
 */
export class HybridReconstructor {
  private source: string;
  private tokenBasedReconstructor: any; // Existing reconstructor

  constructor(source: string, tokenBasedReconstructor: any) {
    this.source = source;
    this.tokenBasedReconstructor = tokenBasedReconstructor;
  }

  reconstruct(node: any): string {
    // Use source range if available
    if (node.sourceRange) {
      return this.source.substring(
        node.sourceRange.startOffset,
        node.sourceRange.endOffset
      );
    }

    // Fallback to token-based reconstruction
    return this.tokenBasedReconstructor.reconstruct(node);
  }
}

/**
 * Migration example showing gradual adoption
 */
export function demonstrateMigration(): void {
  console.log(`
Migration Plan for Source Range Tracking
=========================================

Step 1: Extend ParserState (1 day)
  - Add currentSourcePosition() method
  - Track character offset alongside token offset

Step 2: Add sourceRange to AST types (1 day)
  - Make it optional for backward compatibility
  - TypeScript will ensure type safety

Step 3: Wrap parser methods (1 week)
  - Use SourceRangeTracker.withRange() wrapper
  - Start with high-value methods (expressions, statements)
  - Test each wrapped method

Step 4: Update reconstructor (2 days)
  - Create HybridReconstructor
  - Use source ranges when available
  - Fallback to token-based for unmigrated nodes

Step 5: Validate and refine (1 week)
  - Run full test suite
  - Fix any edge cases
  - Optimize performance

Benefits visible immediately:
- Methods with source ranges get perfect reconstruction
- Existing code continues to work
- Can migrate incrementally

Total effort: ~2 weeks for full migration
Result: 100% perfect reconstruction
  `);
}