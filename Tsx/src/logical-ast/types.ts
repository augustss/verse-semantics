/**
 * Logical AST Types
 *
 * Simplified AST representation that:
 * - Removes all token offset information
 * - Removes parentheses (precedence is implicit in tree structure)
 * - Simplifies compound expressions
 * - Focuses on semantic meaning rather than syntax
 */

/**
 * Base node type for all logical AST nodes
 */
export interface LogicalNode {
  type: string;
}

// ============================================================================
// EXPRESSIONS
// ============================================================================

/**
 * Literal value (number, string, boolean)
 */
export interface Literal extends LogicalNode {
  type: 'Literal';
  value: string | number | boolean;
  literalType: 'string' | 'integer' | 'float' | 'boolean';
}

/**
 * Variable reference
 */
export interface Identifier extends LogicalNode {
  type: 'Identifier';
  name: string;
}

/**
 * Binary operation (arithmetic, logical, comparison)
 */
export interface BinaryOp extends LogicalNode {
  type: 'BinaryOp';
  operator: string;
  left: Expression;
  right: Expression;
}

/**
 * Unary operation (negation, not)
 */
export interface UnaryOp extends LogicalNode {
  type: 'UnaryOp';
  operator: string;
  operand: Expression;
}

/**
 * Assignment (including :=, +=, -=, etc.)
 */
export interface Assignment extends LogicalNode {
  type: 'Assignment';
  operator: string;
  left: Expression;
  right: Expression;
}

/**
 * Member access (dot or bracket notation)
 */
export interface MemberAccess extends LogicalNode {
  type: 'MemberAccess';
  object: Expression;
  property: Expression;
  computed: boolean;
}

/**
 * Qualified access expression (e.g., (super:)method)
 */
export interface QualifiedAccess extends LogicalNode {
  type: 'QualifiedAccess';
  qualifier: string;
  member: Expression;
}

/**
 * Function call
 */
export interface Call extends LogicalNode {
  type: 'Call';
  callee: Expression;
  arguments: Expression[];
}

/**
 * Array literal
 */
export interface Array extends LogicalNode {
  type: 'Array';
  elements: Expression[];
}

/**
 * Object construction
 */
export interface ObjectConstruction extends LogicalNode {
  type: 'ObjectConstruction';
  typeName: string;
  fields: ObjectField[];
}

export interface ObjectField {
  name: string;
  value: Expression;
}

/**
 * Range expression (e.g., 1..10)
 */
export interface Range extends LogicalNode {
  type: 'Range';
  start: Expression;
  end: Expression;
}

/**
 * Lambda/arrow function
 */
export interface Lambda extends LogicalNode {
  type: 'Lambda';
  parameters: Parameter[];
  body: Expression;
}

export interface Parameter {
  name: string;
  paramType?: Type;
}

/**
 * Block of expressions
 */
export interface Block extends LogicalNode {
  type: 'Block';
  expressions: Expression[];
}

/**
 * Set expression for mutation
 */
export interface Set extends LogicalNode {
  type: 'Set';
  target: Expression;
  value: Expression;
}

// ============================================================================
// CONTROL FLOW
// ============================================================================

/**
 * If-then-else expression
 */
export interface If extends LogicalNode {
  type: 'If';
  condition: Expression;
  thenBranch?: Expression;
  elseBranch?: Expression;
}

/**
 * For loop
 */
export interface For extends LogicalNode {
  type: 'For';
  variable: string;
  indexVariable?: string;
  iterable: Expression;
  body: Expression;
}

/**
 * Infinite loop
 */
export interface Loop extends LogicalNode {
  type: 'Loop';
  body: Expression;
}

/**
 * Pattern matching
 */
export interface Case extends LogicalNode {
  type: 'Case';
  scrutinee: Expression;
  branches: CaseBranch[];
}

export interface CaseBranch {
  pattern: Expression | '_';
  body: Expression;
}

/**
 * Break statement
 */
export interface Break extends LogicalNode {
  type: 'Break';
}

/**
 * Return statement
 */
export interface Return extends LogicalNode {
  type: 'Return';
  value?: Expression;
}

// ============================================================================
// CONCURRENT CONSTRUCTS
// ============================================================================

/**
 * Spawn expression for async execution
 */
export interface Spawn extends LogicalNode {
  type: 'Spawn';
  body: Expression;
}

/**
 * Race expression - first to complete wins
 */
export interface Race extends LogicalNode {
  type: 'Race';
  branches: Expression[];
}

/**
 * Sync expression - synchronized execution
 */
export interface Sync extends LogicalNode {
  type: 'Sync';
  operations: Expression[];
}

/**
 * Branch expression - branching control
 */
export interface Branch extends LogicalNode {
  type: 'Branch';
  branches: Expression[];
}

// ============================================================================
// DECLARATIONS
// ============================================================================

/**
 * Constant declaration
 */
export interface ConstDecl extends LogicalNode {
  type: 'ConstDecl';
  name: string;
  declaredType?: Type;
  initializer?: Expression;
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
}

/**
 * Variable declaration
 */
export interface VarDecl extends LogicalNode {
  type: 'VarDecl';
  name: string;
  declaredType: Type;
  initializer?: Expression;
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
}

/**
 * Function declaration
 *
 * Separates visibility specifier (public, private, protected, internal, scoped)
 * from other behavioral specifiers (decides, suspends, transacts, override, etc.)
 *
 * The visibility specifier appears after the function name: myFunc<public>()
 * Other specifiers can appear before or after parameters.
 */
export interface FunctionDecl extends LogicalNode {
  type: 'FunctionDecl';
  name: string;
  parameters: Parameter[];
  returnType?: Type;
  body: Expression;
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
}

/**
 * Class declaration
 */
export interface ClassDecl extends LogicalNode {
  type: 'ClassDecl';
  name: string;
  members: Declaration[];
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
  parents?: Expression[];
}

/**
 * Struct declaration
 */
export interface StructDecl extends LogicalNode {
  type: 'StructDecl';
  name: string;
  members: Declaration[];
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
}

/**
 * Interface declaration
 */
export interface InterfaceDecl extends LogicalNode {
  type: 'InterfaceDecl';
  name: string;
  members: Declaration[];
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
}

/**
 * Enum declaration
 */
export interface EnumDecl extends LogicalNode {
  type: 'EnumDecl';
  name: string;
  members: EnumMember[];
  visibility?: 'public' | 'private' | 'protected' | 'internal' | 'scoped';  // Single visibility specifier
  specifiers?: string[];  // Other behavioral/modifier specifiers
}

export interface EnumMember {
  name: string;
  value?: Expression;
}

// ============================================================================
// TYPES
// ============================================================================

/**
 * Type expression
 */
export interface WhereConstraint extends LogicalNode {
  type: 'WhereConstraint';
  parameter: string;   // constrained type parameter name
  constraint: string;  // constraint type (e.g., 'type', 'comparable')
}

export interface Type extends LogicalNode {
  type: 'Type';
  name: string;
  isOptional?: boolean;
  isArray?: boolean;
  arrayDimensions?: number;
  whereConstraint?: WhereConstraint;
}

// ============================================================================
// PROGRAM
// ============================================================================

/**
 * Top-level program
 */
export interface Program extends LogicalNode {
  type: 'Program';
  usingPaths?: string[];
  declarations: Declaration[];
}

// ============================================================================
// UNION TYPES
// ============================================================================

export type Expression =
  | Literal
  | Identifier
  | BinaryOp
  | UnaryOp
  | Assignment
  | MemberAccess
  | QualifiedAccess
  | Call
  | Array
  | ObjectConstruction
  | Range
  | Lambda
  | Block
  | Set
  | If
  | For
  | Loop
  | Case
  | Break
  | Return
  | Spawn
  | Race
  | Sync
  | Branch;

export type Declaration =
  | ConstDecl
  | VarDecl
  | FunctionDecl
  | ClassDecl
  | StructDecl
  | InterfaceDecl
  | EnumDecl;

export type Statement = Expression | Declaration;
export type Node = Expression | Declaration | Type | Program;