# Verse Parser

A comprehensive, lossless parser for the Verse programming language with HTML syntax highlighting support. This TypeScript implementation provides full AST generation, error reporting, and Pygments-style syntax highlighting for Verse code.

## Features

- **Lossless Parsing**: Preserves all source information including comments, whitespace, and exact positioning
- **Complete AST Generation**: Full Abstract Syntax Tree with source location tracking
- **HTML Syntax Highlighting**: Pygments-style syntax highlighting with dark/light theme support
- **Error Reporting**: Detailed error messages with line/column information and source context
- **TypeScript Implementation**: Modern TypeScript codebase with full type safety
- **Comprehensive Testing**: 99.5% test coverage with Jest test suite

## Supported Verse Features

- Function declarations with specifiers (`<override>`, `<suspends>`, etc.)
- Variable declarations with type annotations
- Control flow (if/else, loops, break/continue)
- Class definitions with inheritance
- Module imports with using statements
- Comments (line `#` and block `<# #>`)
- Operators and expressions
- Type system integration

## Installation

```bash
npm install verse-parser
```

## Quick Start

### Parsing Verse Code

```typescript
import { parseVersee } from 'verse-parser';

const verseCode = `
using { /Fortnite.com/Devices }

my_device := class(creative_device):
    var Health : int = 100

    OnBegin<override>()<suspends>:void=
        Print("Device started!")
`;

const result = parseVersee(verseCode);

if (result.success) {
    console.log('Parsed successfully!');
    console.log('AST:', JSON.stringify(result.value, null, 2));
} else {
    console.log('Parse error:', result.error);
}
```

### HTML Syntax Highlighting

```typescript
import { HtmlSyntaxHighlighter } from 'verse-parser';

const highlighter = new HtmlSyntaxHighlighter();
const html = highlighter.highlightToHtml(verseCode, {
    theme: 'dark',
    showLineNumbers: true,
    filename: 'my_device.verse'
});

console.log(html); // Returns complete HTML with CSS
```

### Error Reporting

```typescript
import { printNodeLine, generateErrorReport } from 'verse-parser';

// Get line number from any AST node
const lineNumber = printNodeLine(astNode);
console.log(lineNumber); // "Line 5, Column 12"

// Generate detailed error report
const errorReport = generateErrorReport(astNode, 'Unexpected token', {
    sourceText: verseCode,
    filename: 'example.verse'
});
console.log(errorReport);
```

## Command Line Usage

```bash
# Parse a single file
npx verse-parser my_file.verse

# Build the project
npm run build

# Run tests
npm test

# Type checking
npm run lint
```

## API Reference

### `parseVersee(source: string)`

Parses Verse source code and returns a result object.

**Returns:**
- `{ success: true, value: AST }` - On successful parsing
- `{ success: false, error: ParseError }` - On parsing failure

### `HtmlSyntaxHighlighter`

Provides HTML syntax highlighting for Verse code.

**Methods:**
- `highlightToHtml(code, options?)` - Generate complete HTML with CSS
- `highlightCode(code, options?)` - Generate highlighted code only

**Options:**
- `theme: 'dark' | 'light'` - Color theme (default: 'light')
- `showLineNumbers: boolean` - Show line numbers (default: false)
- `filename?: string` - Filename for context

### Error Reporting Functions

- `getLineNumber(node)` - Extract line number from AST node
- `getColumnNumber(node)` - Extract column number from AST node
- `printNodeLine(node)` - Format location as "Line X, Column Y"
- `generateErrorReport(node, message, context?)` - Generate detailed error report

## Project Structure

```
verse-parser/
├── src/
│   ├── ast/              # AST type definitions
│   ├── parser/           # Parser implementation
│   ├── error-reporting.ts # Error reporting utilities
│   ├── html-syntax-highlighter.ts # HTML highlighting
│   └── index.ts          # Main entry point
├── tests/                # Test files
│   ├── Verse/           # Sample Verse files
│   ├── parser.test.ts   # Parser tests
│   ├── golden.test.ts   # Golden reference tests
│   └── specifier.test.ts # Specifier tests
├── dist/                 # Compiled JavaScript
└── README.md
```

## Development

### Prerequisites

- Node.js 16+
- TypeScript 5+

### Setup

```bash
git clone https://github.com/username/verse-parser.git
cd verse-parser
npm install
npm run build
```

### Testing

```bash
# Run all tests
npm test

# Run with coverage
npm test -- --coverage

# Run specific test file
npm test parser.test.ts
```

### Building

```bash
# Compile TypeScript
npm run build

# Clean build artifacts
npm run clean

# Type checking only
npm run lint
```

## Test Results

Current test status: **205/206 tests passing (99.5% success rate)**

- ✅ Parser core functionality
- ✅ AST generation and location tracking
- ✅ HTML syntax highlighting
- ✅ Error reporting system
- ✅ Specifier parsing
- ⚠️ 1 known issue with curried function parsing

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## Verse Language

Verse is Epic Games' functional programming language used in Unreal Engine and Fortnite Creative. Learn more:

- [Verse Language Reference](https://dev.epicgames.com/documentation/en-us/uefn/verse-language-reference)
- [Unreal Editor for Fortnite](https://dev.epicgames.com/documentation/en-us/uefn)

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Related Projects

- [Official Verse Documentation](https://dev.epicgames.com/documentation/en-us/uefn/verse-language-reference)
- [Unreal Engine](https://www.unrealengine.com/)
- [Fortnite Creative](https://www.fortnite.com/creative)