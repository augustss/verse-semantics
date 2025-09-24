# Verse Language Delimiters

## Delimiter Rules

Delimiters in Verse are context-dependent:

### 1. **Brace/Parenthesis Context**
In contexts with braces `{}` or parentheses `()`:
- **Delimiters**: `;` (semicolon) or `,` (comma)
- **Usage**: Separates expressions within the enclosing structure

Example:
```verse
{ x := 1; y := 2; z := 3 }        # Semicolon delimited
{ a := 1, b := 2, c := 3 }        # Comma delimited
func(arg1, arg2, arg3)            # Comma delimited arguments
```

### 2. **Indented Context**
In indented contexts (after block-forming keywords with `:`):
- **Delimiter**: EOL (End of Line / Newline)
- **Keywords**: `if:`, `then:`, `else:`, `for:`, `block:`
- **Multiple EOLs**: Multiple consecutive EOLs with only trivia (spaces, tabs, comments) between them are treated as a **single delimiter**

Example:
```verse
if:
    x := 1                        # EOL is delimiter
    y := 2                        # EOL is delimiter

    # Empty lines with comments   # Multiple EOLs = single delimiter

    z := 3
```

### 3. **TRIVIA Tokens**
TRIVIA tokens combine spaces, tabs, and comments but **NOT** newlines:
- Newlines remain as separate `NEWLINE` tokens
- This allows proper delimiter detection in indented contexts
- TRIVIA tokens preserve formatting and comments without interfering with parsing

## Implementation Notes

The parser must:
1. Track the current context (brace/paren vs indented)
2. Recognize appropriate delimiters for the context
3. Handle multiple consecutive EOLs as a single delimiter in indented contexts
4. Properly skip TRIVIA tokens when looking for delimiters