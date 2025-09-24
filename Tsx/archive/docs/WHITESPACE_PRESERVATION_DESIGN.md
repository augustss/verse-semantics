# Comprehensive Whitespace Preservation Solution

## Current Issues

### 1. Token Ownership Ambiguity
- Whitespace tokens (SPACE, NEWLINE) exist between meaningful tokens
- Current system doesn't clearly define which construct "owns" which whitespace
- This leads to duplication or loss when reconstructing nested structures

### 2. Indented Form Detection
- Using `openBraceOffset === 0 && closeBraceOffset === 0` as a heuristic for indented forms
- This is fragile and can cause tokens at position 0 to be incorrectly appended

### 3. Out-of-Order Reconstruction
- Some constructs need to check for tokens that aren't tracked in the AST (e.g., colon after `then`)
- This requires looking ahead/behind, which can disrupt the linear token flow

## Proposed Solution: Source Range Tracking

### Core Concept
Every AST node should track its complete source range:
```typescript
interface SourceRange {
  startOffset: number;  // First character position in source
  endOffset: number;    // Last character position in source (exclusive)

  // Optional: For more precise tracking
  contentStartOffset?: number;  // Start of actual content (after leading trivia)
  contentEndOffset?: number;    // End of actual content (before trailing trivia)
}
```

### Implementation Strategy

#### Phase 1: Parser Enhancement
Modify the parser to track source ranges for every AST node:

```typescript
interface ASTNode {
  type: string;
  sourceRange: SourceRange;
  // ... existing fields
}
```

The parser would:
1. Record the start position before parsing each construct
2. Record the end position after parsing
3. Store these in the AST node

#### Phase 2: Trivia Attribution
Define clear rules for trivia ownership:

1. **Leading Trivia**: Whitespace/comments before a token belong to that token
2. **Trailing Trivia**: Whitespace/comments after a token up to (but not including) the next newline
3. **Separating Trivia**: Whitespace between elements in a list belongs to the separator

#### Phase 3: Smart Reconstruction
Two reconstruction modes:

##### A. Exact Mode (Default)
```typescript
reconstructExact(node: ASTNode): string {
  // Simply extract the exact source text
  return this.source.substring(node.sourceRange.startOffset, node.sourceRange.endOffset);
}
```

##### B. Transform Mode
For when we need to modify the AST but preserve formatting:
```typescript
reconstructTransform(node: ASTNode): string {
  // Preserve leading trivia
  const leadingTrivia = this.extractLeadingTrivia(node);

  // Reconstruct content (potentially modified)
  const content = this.reconstructContent(node);

  // Preserve trailing trivia
  const trailingTrivia = this.extractTrailingTrivia(node);

  return leadingTrivia + content + trailingTrivia;
}
```

### Specific Solutions

#### 1. Indented Forms
Instead of using offset === 0 as a heuristic, add explicit flags:
```typescript
interface CompoundExpression {
  type: 'CompoundExpression';
  style: 'braced' | 'indented';
  expressions: Expression[];
  sourceRange: SourceRange;
}
```

#### 2. Python-style If
Track the colon position explicitly:
```typescript
interface IfExpression {
  type: 'IfExpression';
  condition: Expression;
  colonOffset?: number;  // For if(x): style
  thenKeywordOffset?: number;  // For if(x) then style
  thenBranch?: Expression;
  sourceRange: SourceRange;
}
```

#### 3. Complex Whitespace
For empty lines and complex indentation:
```typescript
interface IndentationInfo {
  level: number;  // Indentation level (number of spaces/tabs)
  hasEmptyLinesBefore: boolean;
  emptyLineCount: number;
  exactWhitespace?: string;  // Optionally store exact whitespace
}
```

## Migration Path

### Step 1: Add Source Ranges (Non-Breaking)
- Add optional `sourceRange` field to all AST nodes
- Update parser to populate these fields
- Existing code continues to work

### Step 2: Dual Reconstruction
- Keep current token-based reconstruction as fallback
- Use source range reconstruction when available
- Gradually migrate all constructs

### Step 3: Simplification
- Once all nodes have source ranges, simplify reconstruction
- Remove complex token tracking logic
- Keep only for specific transformation needs

## Benefits

1. **Perfect Whitespace Preservation**: Exact source reconstruction by default
2. **Simpler Code**: No complex token tracking or heuristics
3. **Better Performance**: Direct string extraction instead of token iteration
4. **Transformation Support**: Can still modify AST while preserving formatting
5. **Debugging**: Clear source location for every AST node

## Example Implementation

```typescript
class EnhancedParser {
  parseExpression(state: ParserState): ParseResult<Expression> {
    const startOffset = state.sourcePosition();

    // ... existing parsing logic ...

    const endOffset = state.sourcePosition();

    return {
      node: {
        ...node,
        sourceRange: { startOffset, endOffset }
      },
      state
    };
  }
}

class EnhancedReconstructor {
  reconstruct(node: ASTNode): string {
    if (node.sourceRange) {
      // Perfect reconstruction
      return this.source.substring(
        node.sourceRange.startOffset,
        node.sourceRange.endOffset
      );
    } else {
      // Fallback to token-based reconstruction
      return this.reconstructTokenBased(node);
    }
  }
}
```

## Testing Strategy

1. **Roundtrip Tests**: Parse → Reconstruct → Compare with original
2. **Whitespace Tests**: Specific tests for complex whitespace scenarios
3. **Transform Tests**: Modify AST and ensure formatting preserved
4. **Performance Tests**: Ensure reconstruction remains fast

## Conclusion

This comprehensive solution would provide:
- 100% accurate source reconstruction
- Simpler, more maintainable code
- Better support for source transformations
- Clear path forward for tooling (formatters, refactoring tools)

The key insight is that trying to reconstruct from tokens alone is inherently complex. By tracking source ranges during parsing, we can achieve perfect reconstruction with much simpler code.