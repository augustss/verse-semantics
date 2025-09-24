# Source Range Tracking Implementation Roadmap

## Executive Summary

Current reconstruction accuracy: **92%**
Target with source ranges: **100%**
Implementation effort: **2-3 weeks**
Risk level: **Low** (backward compatible)

## Problem Statement

The current token-based reconstruction system achieves 92% accuracy but struggles with:
- Complex whitespace (empty lines, mixed indentation)
- Indented forms without explicit delimiters
- Token ownership ambiguity in nested structures
- Edge cases requiring look-ahead/look-behind

## Proposed Solution

Implement **source range tracking** during parsing to enable perfect source reconstruction.

## Implementation Phases

### Phase 1: Infrastructure (Days 1-3)

#### 1.1 Extend ParserState
```typescript
interface RangedParserState extends ParserState {
  currentSourcePosition(): number;  // Character offset in source
}
```

#### 1.2 Extend AST Types
```typescript
interface SourceRange {
  startOffset: number;
  endOffset: number;
  contentStartOffset?: number;  // Optional: after leading trivia
  contentEndOffset?: number;     // Optional: before trailing trivia
}

interface RangedASTNode extends ASTNode {
  sourceRange?: SourceRange;  // Optional for backward compatibility
}
```

#### 1.3 Create SourceRangeTracker
- Wrapper utility to add range tracking to existing parser methods
- Minimal changes to existing code

### Phase 2: Parser Enhancement (Days 4-8)

#### 2.1 High-Priority Methods
Start with the most problematic constructs:
- `parseCompoundExpression` - Fix indented form issues
- `parseIfExpression` - Handle Python-style if
- `parseForExpression` - Fix indented loops
- `parseIdentifier` - Foundation for all expressions

#### 2.2 Progressive Migration
```typescript
// Before
parseIdentifier(state: ParserState): ParseResult<Identifier>

// After (with wrapper)
parseIdentifier = SourceRangeTracker.withRange(
  (state: ParserState) => { /* existing logic */ }
)
```

#### 2.3 Testing Each Method
- Verify range tracking is correct
- Ensure backward compatibility
- Add tests for edge cases

### Phase 3: Reconstructor Update (Days 9-10)

#### 3.1 Create HybridReconstructor
```typescript
class HybridReconstructor {
  reconstruct(node: RangedASTNode): string {
    if (node.sourceRange) {
      // Perfect reconstruction
      return source.substring(node.sourceRange.startOffset, node.sourceRange.endOffset);
    }
    // Fallback to token-based
    return tokenBasedReconstruct(node);
  }
}
```

#### 3.2 Update Build Pipeline
- Keep existing reconstructor as fallback
- Switch to hybrid reconstructor
- Monitor improvement metrics

### Phase 4: Validation (Days 11-15)

#### 4.1 Comprehensive Testing
- Run all parseset files
- Verify 100% reconstruction accuracy
- Performance benchmarks

#### 4.2 Edge Case Handling
- Complex whitespace scenarios
- Unicode characters
- Mixed line endings

#### 4.3 Documentation
- Update API documentation
- Migration guide for consumers
- Performance comparisons

## Technical Details

### Source Position Tracking

```typescript
class EnhancedTokenStream {
  private sourcePositions: number[] = [];  // Character offset for each token

  constructor(source: string) {
    // Build position map during tokenization
    let pos = 0;
    for (const token of this.tokens) {
      this.sourcePositions.push(pos);
      pos += token.content.length;
    }
  }

  getSourcePosition(tokenOffset: number): number {
    return this.sourcePositions[tokenOffset];
  }
}
```

### Trivia Handling

```typescript
enum TriviaAttachment {
  Leading,   // Attach to following token
  Trailing,  // Attach to previous token
  Floating   // Between statements (preserve as-is)
}
```

### Performance Optimization

Source range reconstruction is **faster** than token-based:
- Direct substring: O(1)
- Token iteration: O(n) where n = number of tokens

Memory overhead is minimal:
- 8 bytes per node (two integers)
- ~10KB for typical 1000-node AST

## Success Metrics

### Primary Goal
- **100% reconstruction accuracy** on all test files

### Secondary Goals
- **Performance**: 2x faster reconstruction
- **Code simplicity**: 50% less reconstruction code
- **Maintainability**: Eliminate edge case handling

## Risk Mitigation

### Backward Compatibility
- All changes are additive
- Existing API unchanged
- Gradual migration possible

### Testing Strategy
- Parallel testing with both systems
- A/B comparison on all test files
- Performance regression tests

### Rollback Plan
- Feature flag for reconstruction method
- Keep token-based system for 2 releases
- Monitor telemetry for issues

## Timeline

**Week 1**: Infrastructure and parser enhancement
**Week 2**: Reconstructor update and testing
**Week 3**: Edge cases and documentation

## Long-term Benefits

### Immediate (v1)
- 100% reconstruction accuracy
- Simpler codebase
- Better debugging

### Future (v2+)
- Foundation for code formatters
- Support for refactoring tools
- Source map generation
- Incremental parsing

## Conclusion

Source range tracking is a proven technique used by major parsers (TypeScript, Babel, Roslyn). This implementation would:

1. **Solve all current whitespace issues** permanently
2. **Simplify the codebase** significantly
3. **Improve performance** measurably
4. **Enable future tooling** capabilities

The investment of 2-3 weeks would yield immediate benefits and establish a solid foundation for future development.

## Next Steps

1. Review and approve this roadmap
2. Create feature branch `source-range-tracking`
3. Begin Phase 1 implementation
4. Daily progress updates
5. Phase gates for review

---

*Prepared by: AST Reconstruction Team*
*Date: 2024*
*Status: Ready for implementation*