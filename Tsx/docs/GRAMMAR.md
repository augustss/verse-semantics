# Verse Language Grammar

This grammar specification describes the Verse programming language syntax as implemented by the verse-parser.

## Implementation Status

### ✅ Fully Supported
- **Lexer**: Complete tokenization of all Verse syntax
  - All literals (strings with escapes, integers, floats)
  - All identifiers (regular and @-prefixed)
  - All operators (arithmetic, comparison, logical, assignment, special)
  - All keywords (block-forming, data structure, declaration, type, reserved)
  - All specifiers (`<public>`, `<private>`, `<scoped{...}>`, etc.)
  - Comments (single-line `#`, nested multi-line `<# #>`)
  - Significant whitespace and indentation tracking
  - TRIVIA token optimization for efficient parsing

- **Parser**: Full recursive descent parser with immutable state
  - All expressions (literals, identifiers, binary, unary, lambda, etc.)
  - All operators with proper precedence and associativity
  - Control flow: `if/then/else`, `for` (including arrow syntax), `loop`, `block`, `case`
  - Control statements: `break`, `continue`, `return` as expressions
  - Lambda expressions with single and multiple parameters
  - Compound expressions and array expressions
  - Assignment expressions with all operators
  - Member access and computed access
  - Function calls with parentheses and brackets
  - Object construction syntax
  - All data structure declarations
  - Function and variable declarations
  - Type annotations in declarations
  - Using statements for imports
  - Indented blocks with proper nesting
  - Set expressions for mutable reassignment

### ⚠️ Partially Supported
- Type system (parsed but not semantically validated)
- Generic type parameters (syntax recognized but not validated)
- Method signatures with complex type annotations
- Pattern matching in case expressions (limited to simple patterns)

### ❌ Not Yet Supported
- Array literals with square brackets `[1, 2, 3]` (design decision)
- C-style logical operators (`&&`, `||`, `!`) (use `and`, `or`, `not` instead)
- Complex arithmetic patterns in case expressions
- Semantic analysis and type checking
- Code generation and compilation

## Notation

- `::=` defines a production rule
- `|` separates alternatives
- `[ ]` denotes optional elements
- `{ }` denotes zero or more repetitions
- `( )` groups elements
- `'literal'` denotes literal strings/keywords
- `<rule>` references another rule
- `ε` denotes empty/epsilon production

## Lexical Grammar

### Tokens

```
<token>	::= <literal> | <identifier> | <operator> | <keyword> | <specifier> | <comment> | <whitespace>
```

### Literals

```
<literal>		::= <string-literal> | <integer-literal> | <float-literal>
<string-literal> 	::= '"' <string-content> '"'
<string-content> 	::= { <string-char> | <escape-sequence> }
<escape-sequence> 	::= '\' ( '"' | '\' | 'n' | 'r' | 't' | 'b' | 'f' )
<integer-literal> 	::= ['-'] <digits>
<float-literal> 	::= ['-'] ( <digits> '.' <digits> | '.' <digits> | <digits> '.' )
<digits> 		::= <digit> { <digit> }
<digit> 		::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

### Identifiers

```
<identifier>		::= <regular-identifier> | <at-identifier>
<regular-identifier> 	::= <identifier-start> { <identifier-part> }
<at-identifier> 	::= '@' <identifier-start> { <identifier-part> }
<identifier-start> 	::= <letter> | '_'
<identifier-part> 	::= <letter> | <digit> | '_'
<letter> :		:= 'a'..'z' | 'A'..'Z'
```

### Operators

```
<assignment-operator>	::= ':=' | '=' | '+=' | '-=' | '*=' | '/='
<equality-operator>	::= '==' | '!='
<comparison-operator> 	::= '<' | '<=' | '>' | '>='
<arithmetic-operator> 	::= '+' | '-' | '*' | '/' | '%'
<logical-operator> 	::= 'and' | 'or' | 'not'
<range-operator> 	::= '..'
<lambda-operator> 	::= '=>'
<arrow-operator> 	::= '->'
<member-operator> 	::= '.'
<delimiter> 		::= '(' | ')' | '{' | '}' | '[' | ']'
<separator> 		::= ',' | ';' | ':'
<other-operator> 	::= '?' | '!'
```

### Keywords

```
<block-forming-keyword> ::= 'if' | 'then' | 'else' | 'for' | 'block' | 'loop' | 'case' | 'array'
<data-structure-keyword>::= 'module' | 'class' | 'interface' | 'struct' | 'enum'
<decl-keyword> 		::= 'var' | 'set' | 'using'
<type-keyword> 		::= 'int' | 'float' | 'string' | 'logic' | 'char' | 'any' | 'void'
<reserved-word> 	::= 'do' | 'while' | 'break' | 'continue' | 'return' | 'yield' | 'spawn' | 'sync' | 'race'
```

**Keyword Categories:**
- **Block-forming keywords**: Control flow constructs that can form indented blocks with ':'
- **Data structure keywords**: Define types and modules (module, class, interface, struct, enum)
- **Declaration keywords**: Variable and import declarations (var, set, using)
- **Type keywords**: Built-in type names
- **Reserved words**: Reserved for future use, but currently usable as identifiers in expressions

### Specifiers

```
<specifier>		::= '<' <specifier-content> '>'
<specifier-content> 	::= <simple-specifier> | <scoped-specifier> | 'editable'
<simple-specifier> 	::= 'public' | 'private' | 'protected' | 'abstract' | 'final' | 'native' |
			      'override' | 'inline' | 'const' | 'computes' | 'decides' | 'suspends' |
                       	      'transacts' | 'internal' | 'reads' | 'writes' | 'allocates' | 'converges' |
                       	      'castable' | 'concrete' | 'unique' | 'open' | 'closed' | 'native_callable' |
                       	      'module_scoped_var_weak_map_key' | 'epic_internal'
<scoped-specifier> 	::= 'scoped' '{' <scoped-argument> '}'
```

### Comments and Whitespace

```
<comment>		::= <single-line-comment> | <multi-line-comment>
<single-line-comment> 	::= '#' { <any-char-except-newline> }
<multi-line-comment> 	::= '<#' <multi-line-content> '#>'
<multi-line-content> 	::= { <any-char> | <multi-line-comment> }  // Nested comments allowed
<whitespace> 		::= <space> | <tab> | <newline>
<space> 		::= ' '
<tab> 			::= '\t'
<newline> 		::= '\n' | '\r' | '\r\n'
```

## Syntactic Grammar

### Program Structure

```
<program>	  ::= { <top-level-decl> | <exp> }
<top-level-decl>  ::= <using-decl> | <decl>
<using-decl> 	::= 'using' '{' <path-list> '}'
<path-list> 		::= <path> { ',' <path> }
<path> 			::= <identifier> { '/' <identifier> } | '/' <identifier> { '/' <identifier> }
```

### Decls

```
<decl>		::= <constant-decl> | <variable-decl> | <function-decl>
			| <data-structure-decl> | <type-decl> | <specifier-decl>
<constant-decl> ::= <identifier> [<specifiers>] ':=' <exp>
                         | <identifier> [<specifiers>] ':' <type> '=' <exp>
<variable-decl> ::= 'var' <identifier> [<specifiers>] ':' <type> ['=' <exp>]
                         | 'var' <identifier> [<specifiers>] ':=' <exp>

<function-decl> ::= [<specifiers>] <identifier> '(' [<parameter-list>] ')' [<specifiers>] ':=' <exp>
                         | [<specifiers>] <identifier> '(' [<parameter-list>] ')'  [<specifiers>]  ':' <type> '=' <exp>
<parameter-list>       ::= <parameter> { ',' <parameter> }  // No trailing comma allowed
<parameter> 	       ::= <identifier> [':' <type>] ['=' <exp>]
<specifier-decl> ::= <specifiers> <data-structure>  // e.g., @editable class { ... }
<data-structure-decl> ::= <identifier> [<specifiers>] ':=' <data-structure>
<data-structure>       ::= <class-decl> | <module-decl> | <interface-decl>
                        |  <struct-decl> | <enum-decl>

<class-decl>    ::= 'class'  [<specifiers>] ['(' <inheritance-list> ')'] <class-body>
<module-decl>   ::= 'module'  [<specifiers>] <module-body>
<interface-decl> ::= 'interface'  [<specifiers>] <interface-body>
<struct-decl>   ::= 'struct'  [<specifiers>] <struct-body>
<enum-decl>     ::= 'enum'  [<specifiers>] <enum-body>  // enum-body cannot be empty
<inheritance-list>     ::= <type> { ',' <type> }
<class-body> 	       ::= '{' { <member-decl> } '}' | ':' <indented-members>
<module-body> 	       ::= '{' { <decl> } '}' | ':' <indented-decls>
<interface-body>       ::= '{' { <member-decl> } '}' | ':' <indented-members>
<struct-body> 	       ::= '{' { <field-decl> } '}' | ':' <indented-fields>
<enum-body> 	       ::= '{' <enum-values> '}'  // Must have at least one value

<member-decl>   ::= <field-decl> | <function-decl>
<field-decl>    ::= <identifier> ':' <type> ['=' <exp>]

<enum-values> 	       ::= <identifier> { ',' <identifier> }

<specifiers> 	       ::= '<' <specifier-list> '>'
<specifier-list>       ::= <specifier-content> { ',' <specifier-content> }

<type-decl>     ::= <identifier> [<specifiers>] ':' <type>
```

### Expressions

```
<exp>		   ::= <assignment-exp>
                    | 'set' <postfix-exp> '=' <exp>
<assignment-exp>   ::= <r-exp> [ (':=' | '=') <assignment-exp> ]  // Left side must be valid lvalue
<r-exp> 	   ::= <l-exp> [ '..' <l-exp> ]
<l-exp> 	   ::= <logical-or-exp>
                    | <identifier> '=>' <logical-or-exp>
                    | '(' <parameter-list> ')' '=>' <logical-or-exp>

<logical-or-exp>   ::= <logical-and-exp> { 'or' <logical-and-exp> }
<logical-and-exp>  ::= <equality-exp> { 'and' <equality-exp> }
<equality-exp> 	   ::= <comparison-exp> { <equality-operator> <comparison-exp> }
<comparison-exp>   ::= <additive-exp> { <comparison-operator> <additive-exp> }
<additive-exp> 	   ::= <multiplicative-exp> { ('+' | '-') <multiplicative-exp> }
<multiplicative-exp> ::= <unary-exp> { ('*' | '/' | '%') <unary-exp> }
<unary-exp> 	   ::= <postfix-exp> | ('-' | 'not') <unary-exp>
<postfix-exp> 	   ::= <primary-exp> { <postfix-operator> }
<postfix-operator> ::= '.' <identifier>
                    | '[' <exp> ']'
                    | '(' [<argument-list>] ')'
<argument-list>    ::= <exp> { ',' <exp> }
<primary-exp> 	   ::= <literal>
		    | <identifier> [':' <indented-exp-list>]  // Object construction
                    | '(' <exp> ')'
                    | <compound-exp>
                    | <array-exp>
                    | <if-exp>
                    | <for-exp>
                    | <block-exp>
		    | <loop-exp>
		    | <case-exp>
```

### Compound and Array Expressions

```
<compound-exp>	    ::= '{' [<compound-exp-list>] '}'
<compound-exp-list> ::= <exp> { (';' | <newline>) <exp> }  // Semicolon separator allowed
<array-exp> 	    ::= 'array' '{' [<array-elem-list>] '}'
                     | 'array' ':' <indented-exp-list>
<array-elem-list>   ::= <exp> { ',' <exp> }
<indented-exp-list> ::= <indent> <exp> { (<newline> | ';') <indent> <exp> }  // Consistent indentation required
		     | <newline>
```

### Control Flow Expressions

```
<if-exp>	  ::= 'if' '(' <exp> ')' ['then' (<exp> | ':' <indented-exp-list>)
                                     ['else' (<exp> | ':' <indented-exp-list>)]]
                   | 'if' '(' <exp> ')' ':' <indented-exp-list>  // then is implicit
                                        ['else' (<exp> | ':' <indented-exp-list>)]
                   | 'if' ':' <indented-exp-list> ['then' ':' <indented-exp-list>
                                                  ['else' ':' <indented-exp-list>]]

<for-exp>         ::= 'for' '(' <for-variables> ':' <exp> ')' <exp>
                   | 'for' ':' <indented-exp-list> ['do' ':' <indented-exp-list>]
                   | 'for' '(' <for-variables> ':' <exp> ')' ':' <indented-exp-list>

<for-variables>   ::= <identifier>                           // Traditional: for(x : items)
                   | <identifier> '->' <identifier>         // Arrow syntax: for(i -> x : items)

<loop-exp>       ::= 'loop' ':' <indented-exp-list>
                  |  'loop' <exp>

<block-exp>       ::= 'block' ':' <indented-exp-list>

<case-exp>       ::= 'case' '(' <exp> ')' '{' <case-branch-list> '}'  // At least one branch required
                  |  'case' '(' <exp> ')' ':' <newline> <indented-case-branches>  // No content on same line

<case-branch-list> ::= <case-branch> { ',' <case-branch> }  // Must have at least one
<case-branch>    ::= <pattern> '=>' <exp>
<pattern>        ::= <literal> | <identifier> | '_'

<indented-case-branches> ::= <indent> <case-branch> { <newline> <indent> <case-branch> }  // At least one required

```

### Types

```
<type>		::= <simple-type> | <compound-type> | <function-type> | <array-type> | <optional-type>
<simple-type> 	::= <type-keyword> | <identifier> | <qualified-type>
<qualified-type> ::= <identifier> { '.' <identifier> }
<compound-type> ::= '(' <type-list> ')'
<type-list> 	::= <type> { ',' <type> }
<function-type> ::= '(' [<type-list>] ')' '->' <type>
<array-type> 	::= '[' ']' <type>
<optional-type> ::= '?' <type>
```

## Indentation Rules

The Verse language uses significant whitespace for certain constructs:

1. **Indented Blocks**: After `:` in block-forming keywords, expressions must be indented on the next line
2. **Indentation Level**: All lines in an indented block must have exactly the same base indentation
3. **Consistency**: After empty lines, expressions must maintain the same indentation level
4. **Line Continuation**: Lines can be continued by increasing indentation
5. **Block Termination**: An indented block ends when a line with less indentation is encountered
6. **No Inline Content**: For indented forms with `:`, content cannot appear on the same line after the colon

## Operator Precedence and Associativity

From lowest to highest precedence:

1. **Assignment** (`:=`, `=`, `+=`, `-=`, `*=`, `/=`) - Right associative
   - `:=` is used for constant declarations
   - `=` is used for mutable assignments
2. **Range** (`..`) - Non-associative
3. **Lambda** (`=>`) - Right associative
4. **Logical OR** (`or`) - Left associative
5. **Logical AND** (`and`) - Left associative
6. **Equality** (`==`, `!=`) - Left associative
7. **Comparison** (`<`, `<=`, `>`, `>=`) - Non-associative
8. **Addition** (`+`, `-`) - Left associative
9. **Multiplication** (`*`, `/`, `%`) - Left associative
10. **Unary** (`-`, `not`) - Right associative
11. **Postfix** (`.`, `[]`, `()`) - Left associative
12. **Primary** (literals, identifiers, parentheses)

## Special Constructs

### Assignment Patterns

The language supports different assignment patterns:

1. **Constant Declaration**: `x := value` - Creates an immutable binding
2. **Variable Assignment**: `x = value` - Assigns to a mutable variable (lvalue must be valid)
3. **Variable Declaration**: `var x := value` or `var x : type = value` - Creates a mutable variable
4. **Set Expression**: `set x = value` - Reassigns an existing mutable variable
5. **Type-Annotated Constant**: `x : type = value` - Creates a constant with explicit type

Valid lvalues for assignment: identifiers and member expressions only.

### Object Construction

The pattern `identifier:` followed by indented content creates an object:

```
Person:
  name := "Alice"
  age := 30
```

This is distinguished from type annotations by the presence of a newline after the colon.

### Trivia Tokens
The lexer can combine consecutive whitespace and comments into single TRIVIA tokens for efficient parsing.

### Scoped Specifiers
The `<scoped{...}>` specifier accepts arguments that define scoping constraints.

### Nested Comments
Multi-line comments `<# ... #>` can be nested, with proper level tracking.

### Negative Numbers
Negative number literals are context-sensitive:
- Valid after operators and at expression start
- Invalid after numbers/identifiers (parsed as subtraction)

### String Literals
Strings must be properly closed with matching quotes. Unclosed strings result in UNKNOWN tokens and parse errors.

### String Escapes
Strings support standard escape sequences: `\"`, `\\`, `\n`, `\r`, `\t`, `\b`, `\f`

### For Loop Arrow Syntax
The arrow operator `->` enables enhanced for loop syntax with both index and value variables:

```verse
// Traditional syntax (value only)
for(x : items) { ... }         // x is the current item

// Arrow syntax (index and value)
for(i -> x : items) { ... }    // i is the index, x is the current item
```

In the arrow syntax:
- The first identifier (before `->`) represents the loop index/counter
- The second identifier (after `->`) represents the current item value
- Both forms support all for loop body types (single expression, compound block, or indented block)

### Case Expression Requirements

Case expressions enforce the following rules:
- Must have at least one branch
- Each branch must have a non-empty body expression after `=>`
- Empty branches (e.g., `_ =>` with nothing following) are rejected as errors
- In indented form, content cannot appear on the same line as the colon

## Keyword Usage and Context Sensitivity

The lexer tokenizes all keywords into their respective categories, but the parser allows flexible usage based on context:

### Contextual Keyword Usage

1. **Block-forming keywords** (`if`, `then`, `else`, `for`, `block`, `loop`, `array`, `case`):
   - Parsed as keywords when initiating control structures
   - `block` can be used as identifier in assignments: `block := { x }`
   - `array` initiates array expressions: `array{1, 2, 3}`

2. **Data structure keywords** (`module`, `class`, `interface`, `struct`, `enum`):
   - Reserved as keywords and cannot be used as regular identifiers
   - Used after `:=` in declarations: `MyClass := class { }`
   - Trigger data structure parsing in appropriate contexts

3. **Declaration keywords** (`var`, `set`, `using`):
   - `var` initiates variable declarations: `var x : int = 5`
   - `set` initiates mutable reassignment: `set x = 10`
   - `using` initiates import statements: `using { /Path/To/Module }`
   - These are reserved and tokenized as `DECL_KEYWORD`

4. **Type keywords** (`int`, `float`, `string`, `logic`, `char`, `any`, `void`):
   - Recognized in type annotation contexts: `x : int`
   - Can be used as identifiers in expression contexts: `int := 42` is valid
   - Parser accepts both `TYPE_KEYWORD` and `IDENTIFIER` tokens in type positions

5. **Reserved words** (`do`, `while`, `break`, `continue`, `return`, `yield`, `spawn`, `sync`, `race`):
   - Tokenized as `RESERVED_WORD` for future language features
   - Currently allowed as identifiers in expressions: `continue := true` is valid
   - Parser treats them as valid identifiers to maintain backward compatibility

### Implementation Details

- The lexer strictly categorizes keywords into their token types
- The parser's `parseIdentifier` accepts `IDENTIFIER`, `TYPE_KEYWORD`, and `RESERVED_WORD` tokens
- Type annotations accept both `IDENTIFIER` and `TYPE_KEYWORD` tokens
- This design allows for future language evolution while maintaining compatibility

## Grammar Notes

1. The grammar is designed for recursive descent parsing
2. Left recursion has been eliminated through iterative parsing
3. The parser maintains immutable state for backtracking
4. Object construction uses `identifier:` followed by indented fields
5. The parser distinguishes between type annotations (`x: int`) and object construction (`Person:`) based on context
6. Token offsets are stored in AST nodes for source reconstruction
7. Indentation context is tracked for block-structured elements
8. Empty data structures (like empty enums) are rejected during parsing
9. Trailing commas are not allowed in parameter lists
10. Specifiers can appear before expressions (e.g., `@editable class { }`)
11. Block expressions require the `block` keyword followed by `:` and indented content
12. Loop expressions support the `loop` keyword with either direct expression or indented form
13. Control flow statements (`break`, `continue`, `return`) are parsed as expressions
14. The `array` keyword can be used as an identifier when followed by assignment operators

## Test Coverage

### Current Status (100% Pass Rate)
- **Total tests**: 1,490
- **Pass rate**: 100% (1,490 passed out of 1,490)
- **Test categories**:
  - Error tests: 94 tests
  - Valid arrays: 41 tests
  - Control flow: 124 tests
  - Data structures: 31 tests
  - Declarations: 59 tests
  - Expressions: 750 tests
  - Literals: 12 tests
  - Operators: 77 tests
  - Top-level: 99 tests
  - Previously failing (now passing): 203 tests

### Parser Architecture

The parser has been refactored into a modular architecture:

```
parser/
├── parser.ts              # Main parser class and expression parsing
├── parser-state.ts        # Immutable parser state management
├── top-level-parser.ts    # Top-level declarations and program structure
├── ast.ts                 # AST node type definitions
└── parsers/               # Specialized parsers
    ├── literal-parser.ts  # Literals and basic tokens
    ├── operator-parser.ts # Binary and unary operators
    ├── lambda-parser.ts   # Lambda expressions
    ├── compound-parser.ts # Compound and array expressions
    └── declaration-parser.ts # Variable and function declarations
```

The lexer provides a robust foundation:

```
lexer/
├── lexer.ts              # Core tokenization logic
├── token.ts              # Token types and classes
├── tokenstream.ts        # Token navigation and filtering
└── index.ts              # Public API
```

### Key Achievements
- **Complete lexical analysis** of all Verse syntax
- **Full expression parsing** with proper precedence
- **Robust error handling** with position tracking
- **Immutable parser state** for backtracking
- **Modular architecture** for maintainability
- **100% test coverage** on all supported features