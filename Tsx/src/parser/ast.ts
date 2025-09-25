/**
 * Abstract Syntax Tree (AST) Node Definitions for Verse Language
 *
 * This module defines all AST node types that represent parsed Verse code structures.
 * The AST serves as the intermediate representation between lexical analysis (tokens)
 * and semantic analysis/code generation phases.
 *
 * DESIGN PRINCIPLES:
 * - **Immutability**: All properties are readonly to prevent accidental mutation
 * - **Memory Efficiency**: Store token offsets, not token references
 * - **Source Fidelity**: Preserve enough information to reconstruct original source
 * - **Type Safety**: Discriminated unions enable exhaustive pattern matching
 * - **Formatting Preservation**: Store positions of delimiters and separators
 *
 * TOKEN OFFSET STRATEGY:
 * Rather than storing token objects (which would keep entire token stream in memory),
 * we store numeric offsets that can be used to retrieve tokens on-demand:
 * - tokenStream.getToken(offset) retrieves the token
 * - tokenStream.getPosition(offset) gets line/column information
 * - Enables garbage collection of unused tokens
 * - Supports incremental parsing scenarios
 *
 * EXAMPLE USAGE:
 * ```typescript
 * const ast: AST.BinaryExpression = {
 *   type: 'BinaryExpression',
 *   left: { type: 'Identifier', name: 'x', tokenOffset: 0 },
 *   operator: '+',
 *   right: { type: 'Literal', value: 42, literalType: 'integer', tokenOffset: 4 },
 *   operatorOffset: 2  // Position of '+' token for error reporting
 * };
 * ```
 */

import { Token, Position } from '../lexer/token';

/**
 * Base interface for all AST nodes
 *
 * All nodes have a type discriminator for pattern matching.
 * Position information is not stored directly but can be
 * reconstructed from token offsets when needed.
 */
export interface ASTNode {
  readonly type: string;
}

/**
 * Base interface for expression nodes
 *
 * All expressions extend this interface.
 */
export interface Expression extends ASTNode { }

/**
 * Base interface for declaration nodes
 *
 * All declarations extend this interface.
 */
export interface Declaration extends ASTNode { }

/**
 * Literal expressions - constant values in source code
 *
 * Represents all literal values that appear directly in code:
 * - Numbers: 42, -17, 3.14159, -0.5
 * - Strings: "hello", "world", ""
 * - Booleans: true, false
 *
 * EXAMPLES:
 * ```verse
 * x := 42           // integer literal
 * pi := 3.14159     // float literal
 * name := "Alice"   // string literal
 * enabled := true   // boolean literal
 * ```
 *
 * TOKEN STORAGE:
 * - tokenOffset: Position of literal token for error reporting and source maps
 * - value: Parsed literal value (number for numeric, string for text, boolean for bool)
 * - literalType: Discriminator for type checking and code generation
 */
export interface LiteralExpression extends Expression {
  readonly type: 'Literal';
  readonly value: string | number | boolean;
  readonly literalType: 'string' | 'integer' | 'float' | 'boolean';
  readonly tokenOffset: number;  // Offset of the literal token in the token stream
}

/**
 * Identifier expressions (variable names, references)
 *
 * Stores:
 * - tokenOffset: Position of the identifier token in the stream
 */
export interface IdentifierExpression extends Expression {
  readonly type: 'Identifier';
  readonly name: string;
  readonly tokenOffset: number;  // Offset of the identifier token in the token stream
}

/**
 * Binary expressions (arithmetic, logical, comparison)
 *
 * Stores:
 * - operatorOffset: Position of the operator token
 */
export interface BinaryExpression extends Expression {
  readonly type: 'BinaryExpression';
  readonly left: Expression;
  readonly operator: string;
  readonly right: Expression;
  readonly operatorOffset: number;  // Offset of the operator token in the token stream
}

/**
 * Unary expressions (negation, logical not)
 *
 * Stores:
 * - operatorOffset: Position of the unary operator token
 */
export interface UnaryExpression extends Expression {
  readonly type: 'UnaryExpression';
  readonly operator: string;
  readonly operand: Expression;
  readonly operatorOffset: number;  // Offset of the operator token in the token stream
}

/**
 * Parenthesized expressions
 *
 * Stores:
 * - openParenOffset: Position of '(' token
 * - closeParenOffset: Position of ')' token
 */
export interface ParenthesizedExpression extends Expression {
  readonly type: 'ParenthesizedExpression';
  readonly expression: Expression;
  readonly openParenOffset: number;  // Offset of '(' token
  readonly closeParenOffset: number; // Offset of ')' token
}

/**
 * Assignment expressions (:= operator)
 *
 * Right-associative for chaining: a := b := c
 *
 * Stores:
 * - operatorOffset: Position of the := token
 */
export interface AssignmentExpression extends Expression {
  readonly type: 'AssignmentExpression';
  readonly left: Expression;
  readonly operator: string;
  readonly right: Expression;
  readonly operatorOffset: number;  // Offset of the operator token in the token stream
}

/**
 * Member access expressions (obj.prop)
 *
 * Stores:
 * - dotOffset: Position of '.' token (for dot access)
 * - openBracketOffset: Position of '[' token (for computed access)
 * - closeBracketOffset: Position of ']' token (for computed access)
 */
export interface MemberExpression extends Expression {
  readonly type: 'MemberExpression';
  readonly object: Expression;
  readonly property: Expression;
  readonly computed: boolean; // true for obj[prop], false for obj.prop
  readonly dotOffset?: number;  // Offset of '.' token (when computed = false)
  readonly openBracketOffset?: number;  // Offset of '[' token (when computed = true)
  readonly closeBracketOffset?: number; // Offset of ']' token (when computed = true)
}

/**
 * Qualified access expressions ((qualifier:)member)
 *
 * Used for accessing members with an explicit qualifier/namespace:
 * - (super:)method()
 * - (module:)function
 * - (namespace:)variable
 *
 * Stores:
 * - openParenOffset: Position of '(' token
 * - colonOffset: Position of ':' token
 * - closeParenOffset: Position of ')' token
 */
export interface QualifiedAccessExpression extends Expression {
  readonly type: 'QualifiedAccessExpression';
  readonly qualifier: string;  // The qualifier name (e.g., "super", "module")
  readonly member: Expression;  // The member being accessed (can be any expression)
  readonly openParenOffset: number;  // Offset of '(' token
  readonly colonOffset: number;      // Offset of ':' token
  readonly closeParenOffset: number; // Offset of ')' token
}

/**
 * Call expressions
 *
 * Stores:
 * - openParenOffset: Position of '(' token
 * - closeParenOffset: Position of ')' token
 * - argumentSeparatorOffsets: Positions of ',' tokens between arguments
 */
export interface CallExpression extends Expression {
  readonly type: 'CallExpression';
  readonly callee: Expression;
  readonly arguments: Expression[];
  readonly openParenOffset: number;  // Offset of '(' token
  readonly closeParenOffset: number; // Offset of ')' token
  readonly argumentSeparatorOffsets: number[]; // Offsets of ',' separators
}

/**
 * Object constructor expressions
 *
 * Constructs objects with named fields:
 * - Point{x:=1, y:=2}
 * - Player{name:="hero", level:=10}
 * - Empty{}
 * - Point{x:=1,} (trailing comma allowed)
 *
 * Stores:
 * - typeNameOffset: Position of the type name
 * - openBraceOffset/closeBraceOffset: Positions of { }
 * - fieldSeparatorOffsets: Positions of commas between fields
 */
export interface ObjectConstructorExpression extends Expression {
  readonly type: 'ObjectConstructorExpression';
  readonly typeName: string;  // The constructor name (e.g., "Point")
  readonly typeNameOffset: number;  // Offset of the type name token
  readonly fields: ObjectField[];  // Field assignments
  readonly openBraceOffset: number;  // Offset of '{' token
  readonly closeBraceOffset: number; // Offset of '}' token
  readonly fieldSeparatorOffsets: number[]; // Offsets of ',' separators
}

/**
 * Option expression for creating option types
 *
 * Represents expressions like: option{ x }, option{ value + 1 }
 *
 * Stores:
 * - optionOffset: Position of the 'option' keyword
 * - openBraceOffset: Position of the '{' token
 * - closeBraceOffset: Position of the '}' token
 * - value: The expression inside the braces
 */
export interface OptionExpression extends Expression {
  readonly type: 'OptionExpression';
  readonly optionOffset: number;  // Offset of 'option' keyword
  readonly value: Expression;  // The wrapped expression
  readonly openBraceOffset: number;  // Offset of '{' token
  readonly closeBraceOffset: number; // Offset of '}' token
}

/**
 * Object field in constructor expression
 *
 * Represents field assignments like x:=1, name:="value"
 *
 * Stores:
 * - nameOffset: Position of the field name
 * - assignOffset: Position of the := operator
 */
export interface ObjectField extends ASTNode {
  readonly type: 'ObjectField';
  readonly name: string;  // Field name
  readonly nameOffset: number;  // Offset of the field name token
  readonly assignOffset: number;  // Offset of ':=' operator
  readonly value: Expression;  // Field value expression
}

/**
 * Array expressions
 *
 * Two syntaxes:
 * - array{1, 2, 3} - Braced array
 * - array: - Indented array
 *     1
 *     2
 *
 * Stores:
 * - arrayKeywordOffset: Position of 'array' keyword
 * - openBraceOffset/closeBraceOffset: Positions of { } (braced syntax)
 * - colonOffset: Position of : (indented syntax)
 * - separatorOffsets: Positions of commas or newlines
 */
export interface ArrayExpression extends Expression {
  readonly type: 'ArrayExpression';
  readonly elements: Expression[];
  readonly arrayKeywordOffset?: number;  // Offset of 'array' keyword (for array{} or array:)
  readonly openBraceOffset?: number;     // Offset of '{' for array{...} syntax
  readonly closeBraceOffset?: number;    // Offset of '}' for array{...} syntax
  readonly colonOffset?: number;         // Offset of ':' for array: syntax
  readonly separatorOffsets: number[];   // Offsets of comma or EOL separators
}

/**
 * Range expressions (1..10)
 *
 * Used for creating ranges of values.
 *
 * Stores:
 * - operatorOffset: Position of the .. token
 */
export interface RangeExpression extends Expression {
  readonly type: 'RangeExpression';
  readonly start: Expression;
  readonly end: Expression;
  readonly operatorOffset: number;  // Offset of the '..' operator token in the token stream
}

/**
 * Tuple expressions ((1, 2, 3))
 *
 * Represents tuple literals with mixed-type elements.
 * Unlike arrays, tuples can contain different types and have fixed size known at compile time.
 *
 * Examples:
 * - (1, 2, 3) - simple tuple
 * - (1, 2.0, "three") - mixed types
 * - (1, (10, 20), "nested") - nested tuples
 *
 * Stores:
 * - openParenOffset: Position of opening '('
 * - closeParenOffset: Position of closing ')'
 * - separatorOffsets: Positions of commas between elements
 */
export interface TupleExpression extends Expression {
  readonly type: 'TupleExpression';
  readonly elements: Expression[];
  readonly openParenOffset: number;  // Offset of '(' token
  readonly closeParenOffset: number; // Offset of ')' token
  readonly separatorOffsets: number[]; // Offsets of ',' separators between elements
}

/**
 * Tuple element access (tuple(0), tuple(1))
 *
 * Non-failing indexed access to tuple elements by compile-time constant index.
 * Unlike array access, tuple access cannot fail since bounds are known at compile time.
 *
 * Examples:
 * - MyTuple(0) - first element
 * - MyTuple(2) - third element
 * - NestedTuple(1)(0) - chained access
 *
 * Stores:
 * - openParenOffset: Position of opening '('
 * - closeParenOffset: Position of closing ')'
 * - indexOffset: Position of the index expression
 */
export interface TupleAccessExpression extends Expression {
  readonly type: 'TupleAccessExpression';
  readonly tuple: Expression;  // The tuple being accessed
  readonly index: Expression;  // Index expression (must be compile-time constant)
  readonly openParenOffset: number;  // Offset of '(' token
  readonly closeParenOffset: number; // Offset of ')' token
}

/**
 * Tuple expansion expression (*tuple)
 *
 * Expands tuple elements as separate arguments in function calls.
 * The expansion operator (*) unpacks tuple elements for passing to functions
 * that expect multiple arguments.
 *
 * Examples:
 * - f(*myTuple) - expands tuple elements as function arguments
 * - process(*coords) - expands coordinate tuple
 * - combine(*first, *second) - multiple expansions in one call
 *
 * Stores:
 * - operatorOffset: Position of the '*' operator
 * - tuple: The tuple expression being expanded
 */
export interface TupleExpansionExpression extends Expression {
  readonly type: 'TupleExpansionExpression';
  readonly tuple: Expression;  // The tuple being expanded
  readonly operatorOffset: number;  // Offset of '*' token
}

/**
 * Lambda expressions (x => x + 1)
 *
 * Function literals with parameter lists and bodies.
 *
 * Stores:
 * - arrowOffset: Position of the => token
 * - parameterSeparatorOffsets: Positions of commas between parameters
 * - openParenOffset/closeParenOffset: Positions of ( ) if parameters are parenthesized
 */
export interface LambdaExpression extends Expression {
  readonly type: 'LambdaExpression';
  readonly parameters: IdentifierExpression[];
  readonly body: Expression;
  readonly arrowOffset: number;  // Offset of the '=>' token in the token stream
  readonly parameterSeparatorOffsets: number[]; // Offsets of commas between parameters
  readonly openParenOffset?: number; // Offset of '(' if parameters are parenthesized
  readonly closeParenOffset?: number; // Offset of ')' if parameters are parenthesized
}

/**
 * Compound expressions ({ a ; b ; c })
 *
 * Brace-delimited sequence of expressions.
 *
 * Stores:
 * - openBraceOffset/closeBraceOffset: Positions of { }
 * - separatorOffsets: Positions of semicolons or newlines
 */
export interface CompoundExpression extends Expression {
  readonly type: 'CompoundExpression';
  readonly expressions: Expression[];
  readonly openBraceOffset: number;   // Offset of '{' token in the token stream
  readonly closeBraceOffset: number;  // Offset of '}' token in the token stream
  readonly separatorOffsets: number[]; // Offsets of semicolon separators
}

/**
 * Set expression (mutable assignment)
 *
 * Form: set x = value
 * Used for reassigning mutable variables.
 *
 * Stores:
 * - setOffset: Position of 'set' keyword
 * - equalsOffset: Position of '=' operator
 */
export interface SetExpression extends Expression {
  readonly type: 'SetExpression';
  readonly target: Expression;  // Variable or member to set
  readonly value: Expression;   // Value to assign
  readonly setOffset: number;   // Offset of 'set' keyword
  readonly equalsOffset: number; // Offset of '=' operator
}

/**
 * For-loop expression
 *
 * Forms:
 * - for (x : v) { body }
 * - for (x : items) { process(x) }
 * - for: range do: body (indented)
 *
 * Stores:
 * - forOffset: Position of 'for' keyword
 * - openParenOffset/closeParenOffset: Optional parentheses
 * - colonOffset: Position of ':' in range
 * - doOffset: Optional 'do' keyword position
 */
export interface ForExpression extends Expression {
  readonly type: 'ForExpression';
  readonly variable: string;  // Loop variable name (value variable in i -> x syntax)
  readonly variableOffset: number;  // Offset of variable identifier
  readonly indexVariable?: string;  // Index variable name (in i -> x syntax)
  readonly indexVariableOffset?: number;  // Offset of index variable identifier
  readonly arrowOffset?: number;  // Offset of '->' operator if present
  readonly iterable: Expression;  // What to iterate over
  readonly body: Expression;  // Loop body
  readonly forOffset: number;  // Offset of 'for' keyword
  readonly openParenOffset?: number;  // Optional '('
  readonly closeParenOffset?: number; // Optional ')'
  readonly colonOffset: number;  // Offset of ':' in range
  readonly doOffset?: number;  // Offset of 'do' keyword if present
}

/**
 * If-expression (conditional)
 *
 * Forms:
 * - if expr then expr else expr
 * - if expr then expr
 * - if: expr then: expr else: expr (indented forms)
 * - if (condition). expression1; expression2 (dot format)
 * - if (condition). expression else. expression (dot format with else)
 *
 * Stores:
 * - ifOffset: Position of 'if' keyword
 * - thenOffset: Optional 'then' keyword position
 * - elseOffset: Optional 'else' keyword position
 * - dotOffset: Optional '.' position for dot format
 * - elseDotOffset: Optional '.' position after else in dot format
 */
export interface IfExpression extends Expression {
  readonly type: 'IfExpression';
  readonly condition: Expression;
  readonly ifOffset: number;  // Offset of 'if' keyword
  readonly thenBranch?: Expression;  // Optional then branch
  readonly thenOffset?: number;  // Offset of 'then' keyword if present
  readonly dotOffset?: number;  // Offset of '.' for dot format
  readonly elseBranch?: Expression;  // Optional else branch
  readonly elseOffset?: number;  // Offset of 'else' keyword if present
  readonly elseDotOffset?: number;  // Offset of '.' after else in dot format
}

/**
 * Loop expression
 *
 * Forms:
 * - loop <expression>
 * - loop: <indented-expression-list>
 *
 * Offset tracking:
 * - loopOffset: Position of 'loop' keyword
 * - colonOffset: Optional position of ':' for indented form
 */
export interface LoopExpression extends Expression {
  readonly type: 'LoopExpression';
  readonly body: Expression;  // Loop body
  readonly loopOffset: number;  // Offset of 'loop' keyword
  readonly colonOffset?: number;  // Optional ':' for indented form
}

/**
 * Block expression
 *
 * Forms:
 * - block: <indented-expression-list>
 *
 * Stores:
 * - blockOffset: Position of 'block' keyword
 * - colonOffset: Position of ':' token
 */
export interface BlockExpression extends Expression {
  readonly type: 'BlockExpression';
  readonly body: Expression;  // Block body (usually a compound expression)
  readonly blockOffset: number;  // Offset of 'block' keyword
  readonly colonOffset: number;  // Offset of ':' token
}

/**
 * Case expression (pattern matching)
 *
 * Forms:
 * - case(x) { 0 => a, 1 => b }
 * - case(x):
 *     0 => a
 *     1 => b
 *
 * Stores:
 * - caseOffset: Position of 'case' keyword
 * - openParenOffset: Position of '('
 * - closeParenOffset: Position of ')'
 * - openBraceOffset/closeBraceOffset: Positions of { } for brace form
 * - colonOffset: Position of ':' for indented form
 */
export interface CaseExpression extends Expression {
  readonly type: 'CaseExpression';
  readonly scrutinee: Expression;  // Expression being matched
  readonly branches: CaseBranch[]; // Pattern => expression pairs
  readonly caseOffset: number;     // Offset of 'case' keyword
  readonly openParenOffset: number;  // Offset of '(' token
  readonly closeParenOffset: number; // Offset of ')' token
  readonly openBraceOffset?: number;  // Offset of '{' token (for brace form)
  readonly closeBraceOffset?: number; // Offset of '}' token (for brace form)
  readonly colonOffset?: number;      // Offset of ':' token (for indented form)
}

/**
 * Case branch (pattern => expression)
 */
export interface CaseBranch extends ASTNode {
  readonly type: 'CaseBranch';
  readonly pattern: Expression | '_';  // Pattern to match (literal, identifier, or wildcard)
  readonly body: Expression;           // Expression to evaluate if pattern matches
  readonly arrowOffset: number;        // Offset of '=>' token
}

/**
 * Indented compound expressions (after block-forming keywords)
 *
 * Used after if:, then:, else:, for:, block:, array:
 *
 * Stores:
 * - keywordOffset: Position of the block-forming keyword
 * - colonOffset: Position of the : token
 * - separatorOffsets: Positions of newlines between expressions
 * - baseIndentation: Column number for the indented block
 */
export interface IdentedCompoundExpression extends Expression {
  readonly type: 'IdentedCompoundExpression';
  readonly expressions: Expression[];
  readonly keywordOffset: number; // Offset of the block-forming keyword token
  readonly colonOffset: number;   // Offset of the colon token after the keyword
  readonly separatorOffsets: number[]; // Offsets of EOL separators between expressions
  readonly baseIndentation: number; // The indentation level for the compound
}

/**
 * Where constraint for type parameters
 *
 * Represents constraints like "T:type", "T:comparable", etc.
 * Examples: T:type, U:comparable, V:numeric
 *
 * Offset tracking:
 * - parameterOffset: Position of constrained type parameter
 * - colonOffset: Position of ':' separator
 * - constraintOffset: Position of constraint type
 */
export interface WhereConstraint extends ASTNode {
  readonly type: 'WhereConstraint';
  readonly parameter: string;          // name of constrained type parameter (e.g., "T")
  readonly parameterOffset: number;    // offset of parameter name
  readonly constraint: string;         // constraint type (e.g., "type", "comparable")
  readonly constraintOffset: number;   // offset of constraint type name
  readonly colonOffset: number;        // offset of ':' separator
}

/**
 * Type expression
 *
 * Represents a type annotation (e.g., int, string, array{int}, ?int, []int, ?[][]int, T where T:type)
 */
export interface TypeExpression extends ASTNode {
  readonly type: 'TypeExpression';
  readonly typeName: string;
  readonly typeNameOffset: number;
  readonly isOptional?: boolean;          // true if prefixed with ?
  readonly arrayDimensions?: number;      // number of [] pairs (e.g., 2 for [][]int)
  readonly optionalOffset?: number;       // offset of ? token if present
  readonly arrayOffsets?: number[];       // offsets of [ tokens for each array dimension
  readonly typeParameters?: TypeExpression[];  // type parameters for generic types (e.g., weak_map(session, int))
  readonly typeParameterOffsets?: number[];    // offsets of comma separators between type parameters
  readonly mapKeyType?: TypeExpression;   // key type for map types [keytype]valuetype
  readonly mapBracketOffsets?: number[];  // offsets of [ and ] tokens for map type
  readonly typeExpression?: Expression;   // expression for type{expression} construct
  readonly typeExpressionOffsets?: number[];  // offsets of { and } tokens for type{expression}
  readonly whereConstraint?: WhereConstraint;  // where clause for type parameters
  readonly whereOffset?: number;  // offset of 'where' keyword
}

/**
 * Specifier list
 *
 * Represents specifiers like <public>, <private>, <decides>
 *
 * Stores:
 * - openAngleOffset: Position of '<'
 * - closeAngleOffset: Position of '>'
 * - specifiers: Array of specifier names
 * - specifierOffsets: Positions of each specifier token
 * - separatorOffsets: Positions of commas between specifiers
 */
export interface SpecifierList extends ASTNode {
  readonly type: 'SpecifierList';
  readonly specifiers: string[];
  readonly specifierOffsets: number[];
  readonly openAngleOffset: number;
  readonly closeAngleOffset: number;
  readonly separatorOffsets: number[]; // Offsets of commas between specifiers
}

/**
 * Parameter declaration for functions
 */
export interface Parameter extends ASTNode {
  readonly type: 'Parameter';
  readonly name: string;
  readonly nameOffset: number;
  readonly paramType?: TypeExpression;
  readonly colonOffset?: number;
}

/**
 * Constant declaration
 *
 * Forms:
 * 1. x:int - name followed by type
 * 2. x<public>:int - with specifiers
 * 3. x<private>:int = 2 - with initializer
 * 4. x:=2 - type inference with initializer
 */
export interface ConstantDeclaration extends Declaration {
  readonly type: 'ConstantDeclaration';
  readonly name: string;
  readonly nameOffset: number;
  readonly specifiers?: SpecifierList;
  readonly declaredType?: TypeExpression;
  readonly colonOffset?: number;
  readonly initializer?: Expression;
  readonly equalsOffset?: number;
  readonly assignOffset?: number; // For := operator
}

/**
 * Variable declaration (mutable)
 *
 * Forms:
 * 1. var x:int
 * 2. var x<public>:int
 * 3. var x:int = 2
 */
export interface VariableDeclaration extends Declaration {
  readonly type: 'VariableDeclaration';
  readonly varOffset: number;
  readonly name: string;
  readonly nameOffset: number;
  readonly specifiers?: SpecifierList;
  readonly declaredType: TypeExpression; // Required for variables
  readonly colonOffset: number;
  readonly initializer?: Expression;
  readonly equalsOffset?: number;
}

/**
 * Function declaration
 *
 * Forms:
 * 1. f(args) := body
 * 2. f<public>()<decides> := body
 * 3. f():int = body
 * 4. f(x:int, y:string):result_type = body
 *
 * Note: visibilitySpecifier (after name) must be unique and one of: public, protected, private, internal, scoped
 *       postSpecifiers (after parameters) can be multiple non-visibility specifiers like: decides, suspends, etc.
 */
export interface FunctionDeclaration extends Declaration {
  readonly type: 'FunctionDeclaration';
  readonly name: string;
  readonly nameOffset: number;
  readonly visibilitySpecifier?: SpecifierList;  // Visibility specifier after name (public, protected, private, internal, scoped)
  readonly parameters: Parameter[];
  readonly openParenOffset: number;
  readonly closeParenOffset: number;
  readonly paramSeparatorOffsets: number[];
  readonly postSpecifiers?: SpecifierList; // Non-visibility specifiers after ()
  readonly returnType?: TypeExpression;
  readonly returnColonOffset?: number;
  readonly assignOffset?: number;  // := operator
  readonly equalsOffset?: number;  // = operator
  readonly body: Expression;  // Can be any expression or IdentedCompoundExpression
  readonly isConstructor?: boolean;  // true if this is a constructor function
  readonly constructedType?: string;  // For constructors: the class name being constructed (e.g., "class1")
  readonly constructedTypeOffset?: number;  // Offset of the constructed type name
}

/**
 * Enum member declaration
 *
 * Represents a single member of an enum:
 * - Simple: Red, Green, Blue
 * - With value: Red = 1, Green = 2
 * - With specifiers: Red<public> = 1
 */
export interface EnumMember extends Declaration {
  readonly type: 'EnumMember';
  readonly name: string;
  readonly nameOffset: number;
  readonly specifiers?: SpecifierList;
  readonly value?: Expression;  // Optional explicit value
  readonly equalsOffset?: number;  // Offset of = if value is provided
}

/**
 * Data structure declaration
 *
 * Forms:
 * 1. name := module { body }
 * 2. name<public> := class<concrete>(parent)<singleton> { body }
 * 3. name := struct { ... }
 * 4. name := interface { ... }
 * 5. name := enum { Red, Green, Blue }
 * 6. name := module: (indented body)
 *
 * Kinds: module, interface, class, struct, enum
 * Note: For enums, body contains EnumMember nodes, not full declarations
 */
export interface DataStructureDeclaration extends Declaration {
  readonly type: 'DataStructureDeclaration';
  readonly name: string;
  readonly nameOffset: number;
  readonly nameSpecifiers?: SpecifierList;  // Specifiers after name
  readonly assignOffset: number;  // := operator
  readonly kind: 'module' | 'interface' | 'class' | 'struct' | 'enum';
  readonly kindOffset: number;
  readonly kindSpecifiers?: SpecifierList;  // Specifiers after kind
  readonly argument?: Expression;  // Argument in parentheses (e.g., parent class)
  readonly openParenOffset?: number;
  readonly closeParenOffset?: number;
  readonly postSpecifiers?: SpecifierList;  // Specifiers after parentheses
  readonly body: Declaration[];  // List of declarations (or EnumMember for enums)
  readonly openBraceOffset?: number;  // For braced body
  readonly closeBraceOffset?: number;  // For braced body
  readonly colonOffset?: number;  // For indented body
  readonly bodySeparatorOffsets: number[];  // Separators between body items
}

/**
 * Control flow statement expressions
 *
 * Break statement
 *
 * Stores:
 * - tokenOffset: Position of the 'break' keyword
 */
export interface BreakExpression extends Expression {
  readonly type: 'BreakExpression';
  readonly tokenOffset: number;  // Offset of the 'break' keyword
}

/**
 * Return statement
 *
 * Stores:
 * - tokenOffset: Position of the 'return' keyword
 * - value: Optional return value expression
 */
export interface ReturnExpression extends Expression {
  readonly type: 'ReturnExpression';
  readonly tokenOffset: number;  // Offset of the 'return' keyword
  readonly value?: Expression;  // Optional return value
}

/**
 * Spawn expression for async execution
 *
 * Forms:
 * - spawn{expression}
 * - spawn: indented-expression-list
 *
 * Stores:
 * - spawnOffset: Position of 'spawn' keyword
 * - openBraceOffset/closeBraceOffset: Positions of { } for brace form
 * - colonOffset: Position of ':' for indented form
 */
export interface SpawnExpression extends Expression {
  readonly type: 'SpawnExpression';
  readonly body: Expression;  // Expression(s) to spawn
  readonly spawnOffset: number;  // Offset of 'spawn' keyword
  readonly openBraceOffset?: number;  // Offset of '{' token (for brace form)
  readonly closeBraceOffset?: number; // Offset of '}' token (for brace form)
  readonly colonOffset?: number;      // Offset of ':' token (for indented form)
}

/**
 * Race expression - first to complete wins
 *
 * Forms:
 * - race: indented-expression-list
 * - race{expression1, expression2}
 *
 * Stores:
 * - raceOffset: Position of 'race' keyword
 * - colonOffset: Position of ':' for indented form
 */
export interface RaceExpression extends Expression {
  readonly type: 'RaceExpression';
  readonly branches: Expression[];  // Competing expressions
  readonly raceOffset: number;      // Offset of 'race' keyword
  readonly openBraceOffset?: number;  // Offset of '{' token (for brace form)
  readonly closeBraceOffset?: number; // Offset of '}' token (for brace form)
  readonly colonOffset?: number;      // Offset of ':' token (for indented form)
}

/**
 * Sync expression - synchronized execution
 *
 * Forms:
 * - sync: indented-expression-list
 * - sync{expression1, expression2}
 *
 * Stores:
 * - syncOffset: Position of 'sync' keyword
 * - colonOffset: Position of ':' for indented form
 */
export interface SyncExpression extends Expression {
  readonly type: 'SyncExpression';
  readonly operations: Expression[];  // Synchronized operations
  readonly syncOffset: number;       // Offset of 'sync' keyword
  readonly openBraceOffset?: number;  // Offset of '{' token (for brace form)
  readonly closeBraceOffset?: number; // Offset of '}' token (for brace form)
  readonly colonOffset?: number;      // Offset of ':' token (for indented form)
}

/**
 * Branch expression - branching control
 *
 * Forms:
 * - branch: indented-expression-list
 * - branch{expression1, expression2}
 *
 * Stores:
 * - branchOffset: Position of 'branch' keyword
 * - colonOffset: Position of ':' for indented form
 */
export interface BranchExpression extends Expression {
  readonly type: 'BranchExpression';
  readonly branches: Expression[];   // Branch paths
  readonly branchOffset: number;     // Offset of 'branch' keyword
  readonly openBraceOffset?: number;  // Offset of '{' token (for brace form)
  readonly closeBraceOffset?: number; // Offset of '}' token (for brace form)
  readonly colonOffset?: number;      // Offset of ':' token (for indented form)
}