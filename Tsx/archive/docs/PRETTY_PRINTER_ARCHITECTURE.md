# Pretty Printer Architecture

## Overview

The pretty printer in the verse-parser project is designed to reconstruct source code from a token stream with perfect fidelity. It preserves all original formatting, including whitespace, comments, and the exact representation of literals.

## Core Design Principles

### 1. Token-Based Reconstruction
The pretty printer operates directly on tokens rather than AST nodes, which allows for:
- **Perfect source preservation**: Every character from the original source is maintained
- **Comment preservation**: All comments remain in their exact positions
- **Whitespace fidelity**: Original formatting and indentation are preserved
- **String literal accuracy**: Quotes and escape sequences remain unchanged

### 2. Offset-Based Approach
Each token stores its position in the original source:
```typescript
interface Token {
  type: TokenType;
  content: string;
  offset: number;  // Position in source
  line: number;
  column: number;
}
```

## Implementation Details

### Token Stream Processing

The pretty printer processes tokens sequentially:

```typescript
function prettyPrint(source: string, options?: PrettyPrintOptions): string {
  const stream = lex(source, { combineTrivia: options?.combineTrivia });
  const tokens = stream.getAllTokens();

  let result = '';
  for (const token of tokens) {
    result += token.content;
  }

  return result;
}
```

### Key Components

#### 1. Lexer Integration
The lexer provides tokens with exact content preservation:
- **String literals**: Keep original quotes (`"` or `'`)
- **Numbers**: Preserve original format (e.g., `1.0` vs `1`)
- **Comments**: Maintain exact comment syntax and content
- **Operators**: Store multi-character operators as single tokens

#### 2. TRIVIA Tokens
Special tokens that combine consecutive whitespace and comments:
```typescript
// Original source
x := 5  # comment
    + 3

// Without TRIVIA: [IDENTIFIER, SPACE, ASSIGN, SPACE, INTEGER, SPACE, SPACE, COMMENT, NEWLINE, SPACE, SPACE, SPACE, SPACE, PLUS, SPACE, INTEGER]
// With TRIVIA: [IDENTIFIER, TRIVIA(" "), ASSIGN, TRIVIA(" "), INTEGER, TRIVIA("  # comment\n    "), PLUS, TRIVIA(" "), INTEGER]
```

Benefits:
- Reduces token count for faster parsing
- Preserves formatting chunks together
- Simplifies whitespace handling

#### 3. Token Content Preservation

Each token stores its exact source representation:

```typescript
class Lexer {
  private captureToken(type: TokenType, startOffset: number): Token {
    const content = this.source.substring(startOffset, this.offset);
    return new Token(type, content, startOffset, this.line, this.column);
  }
}
```

### Special Cases

#### 1. String Literals
Strings maintain their original quotes and escapes:
```verse
"Hello\nWorld"   // Preserved exactly, including escape sequence
'Single quotes'  // Quote style preserved
```

#### 2. Negative Numbers
Context-sensitive handling:
```verse
x := -5          // INTEGER token with content "-5"
y := 5 - 3       // Separate INTEGER and MINUS tokens
```

#### 3. Multi-line Comments
Nested comment support with exact preservation:
```verse
<# Outer comment
   <# Inner nested comment #>
   Still in outer
#>
```

#### 4. Indented Blocks
Preserves exact indentation:
```verse
if (condition):
    # Original spacing maintained
    statement1
        nested_statement  # Exact indent levels preserved
```

## Usage Examples

### Basic Pretty Printing
```typescript
import { prettyPrint } from 'verse-parser';

const source = 'x:=5+3  # calculate sum';
const formatted = prettyPrint(source);
console.log(formatted); // "x:=5+3  # calculate sum" (exact copy)
```

### With TRIVIA Combination
```typescript
const formatted = prettyPrint(source, { combineTrivia: true });
// More efficient processing, same output
```

### Token Stream Manipulation
```typescript
const stream = lex(source);
const meaningfulTokens = stream.getMeaningfulTokens(); // Skip whitespace/comments
const reconstructed = stream.prettyPrintContents(); // Full reconstruction
```

## Advantages of This Approach

### 1. Lossless Transformation
- No information is lost during tokenization
- Source can be perfectly reconstructed
- Ideal for refactoring tools and formatters

### 2. Comment-Aware Processing
- Comments are first-class tokens
- Can analyze or transform code while preserving documentation
- Supports both single-line and nested multi-line comments

### 3. Formatting Preservation
- Maintains developer's formatting choices
- Useful for minimal-diff transformations
- Respects project-specific style guidelines

### 4. Efficient Processing
- TRIVIA tokens reduce parse overhead
- Single-pass tokenization
- No need for separate formatting pass

## Comparison with AST-Based Pretty Printing

### Token-Based (Current Approach)
**Pros:**
- Perfect source fidelity
- Simple implementation
- Fast reconstruction
- Preserves all formatting

**Cons:**
- Cannot reformat code
- Limited transformation capabilities
- Requires original source

### AST-Based (Alternative)
**Pros:**
- Can reformat to consistent style
- Supports code transformations
- Works without original source

**Cons:**
- Loses original formatting
- Comments require special handling
- More complex implementation

## Future Enhancements

### Potential Extensions

1. **Formatting Mode**: Add option to normalize formatting while preserving comments
2. **Partial Reconstruction**: Reconstruct specific token ranges
3. **Diff-Friendly Mode**: Minimize changes when modifying code
4. **Style Preservation**: Remember and reapply formatting patterns

### API Extensions
```typescript
interface PrettyPrintOptions {
  combineTrivia?: boolean;      // Current
  normalizeWhitespace?: boolean; // Future: Standardize spacing
  preserveComments?: boolean;    // Future: Option to strip comments
  indentStyle?: 'spaces' | 'tabs'; // Future: Indent normalization
  lineWidth?: number;            // Future: Line wrapping
}
```

## Testing Strategy

### Test Coverage
The pretty printer is validated through:

1. **Round-trip tests**: Source → Tokens → Pretty Print → Compare
2. **Parseset tests**: 1,490 test cases covering all syntax
3. **Edge cases**: Unicode, special characters, deeply nested structures
4. **Performance tests**: Large files with extensive formatting

### Validation Script
```typescript
// scripts/test-pretty-printer.js
function validatePrettyPrinter(source: string): boolean {
  const tokens = lex(source);
  const reconstructed = prettyPrint(source);
  return source === reconstructed;
}
```

## Performance Characteristics

### Time Complexity
- Tokenization: O(n) where n is source length
- Reconstruction: O(m) where m is token count
- Overall: O(n) linear in source size

### Space Complexity
- Token storage: O(m) tokens
- With TRIVIA: Reduced token count
- Memory efficient for large files

## Conclusion

The token-based pretty printer provides a robust solution for source code reconstruction in the verse-parser. Its design prioritizes perfect fidelity and simplicity, making it ideal for tools that need to preserve the exact source representation while performing analysis or limited transformations. The approach's strength lies in its straightforward implementation and guaranteed accuracy, ensuring that developers' formatting choices are always respected.