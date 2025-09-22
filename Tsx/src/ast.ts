export interface Span {
  start: number;
  end: number;
}

export interface Trivia {
  leading: string;  // Contains both whitespace and comments
  trailing: string; // Contains both whitespace and comments
}

export interface Token<T = string> {
  text: string;
  value: T;
  trivia: Trivia;
  span: Span;
}

export type Expr =
  | IntegerLiteral
  | BooleanLiteral
  | FloatLiteral
  | StringLiteral
  | Variable
  | BinaryOp
  | UnaryOp
  | Parenthesized
  | FunctionCall
  | Application
  | MemberAccess
  | IndexAccess
  | LambdaExpression
  | MethodCall
  | ObjectConstruction
  | ArrayConstruction
  | ForExpression
  | IfExpression
  | CaseExpression
  | RangeExpression
  | Block
  | Specifier
  | VariableDeclaration
  | ConstDeclaration
  | FunctionDeclaration
  | Assignment
  | ClassExpression
  | EmptyExpression
  | BreakExpression
  | ContinueExpression
  | SetStatement
  | LoopStatement;

export interface IntegerLiteral {
  type: 'IntegerLiteral';
  token: Token<number>;
  span: Span;
}

export interface BooleanLiteral {
  type: 'BooleanLiteral';
  token: Token<boolean>;
  span: Span;
}

export interface FloatLiteral {
  type: 'FloatLiteral';
  token: Token<number>;
  span: Span;
}

export interface StringLiteral {
  type: 'StringLiteral';
  token: Token<string>;
  span: Span;
}

export interface Variable {
  type: 'Variable';
  token: Token<string>;
  span: Span;
}

export interface BinaryOp {
  type: 'BinaryOp';
  left: Expr;
  operator: Token<'+' | '-' | '*' | '/' | '%' | '>' | '<' | '>=' | '<=' | '==' | '!=' | 'and' | 'or'>;
  right: Expr;
  span: Span;
}

export interface UnaryOp {
  type: 'UnaryOp';
  operator: Token<'not' | '-'>;
  operand: Expr;
  span: Span;
}

export interface Parenthesized {
  type: 'Parenthesized';
  leftParen: Token<'('>;
  expr: Expr;
  rightParen: Token<')'>;
  span: Span;
}

export interface FunctionCall {
  type: 'FunctionCall';
  name: Token<string>;
  leftParen: Token<'(' | '['>;
  args: Expr[];
  commas: Token<','>[];
  rightParen: Token<')' | ']'>;
  span: Span;
}

export interface Application {
  type: 'Application';
  func: Expr;
  leftParen: Token<'(' | '['>;
  args: Expr[];
  commas: Token<','>[];
  rightParen: Token<')' | ']'>;
  span: Span;
}

export interface MemberAccess {
  type: 'MemberAccess';
  object: Expr;
  dot: Token<'.'>;
  member: Token<string>;
  span: Span;
}

export interface IndexAccess {
  type: 'IndexAccess';
  object: Expr;
  leftBracket: Token<'['>;
  index: Expr;
  rightBracket: Token<']'>;
  span: Span;
}

export interface LambdaExpression {
  type: 'LambdaExpression';
  parameter: Token<string>;
  arrow: Token<'=>'>;
  body: Expr;
  span: Span;
}

export interface MethodCall {
  type: 'MethodCall';
  object: Expr;
  dot: Token<'.'>;
  method: Token<string>;
  leftParen: Token<'(' | '['>;
  args: Expr[];
  commas: Token<','>[];
  rightParen: Token<')' | ']'>;
  span: Span;
}

export interface FieldAssignment {
  name: Token<string>;
  colon?: Token<':'>;          // For field declarations: name:type = value
  typeAnnotation?: Token<string>; // Type in field declarations
  assignOp: Token<':=' | '='>;  // := for assignments, = for declarations
  value: Expr;
  comma?: Token<','>;  // Optional for last field or indentation style
  span: Span;
}

export interface ObjectConstruction {
  type: 'ObjectConstruction';
  typeExpr: Variable | Application;  // Can be simple variable or generic type
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;  // Only for brace style
  colon?: Token<':'>;      // Only for indentation style
  fields: FieldAssignment[];
  rightBrace?: Token<'}'>;  // Only for brace style
  span: Span;
}

export interface ArrayConstruction {
  type: 'ArrayConstruction';
  arrayKeyword?: Token<'array'>;  // Optional for bare brace syntax like { 1, 2, 3 }
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;     // Only for brace style
  colon?: Token<':'>;          // Only for indentation style
  elements: Expr[];            // Array elements
  commas: Token<','>[];        // Commas between elements
  rightBrace?: Token<'}'>;     // Only for brace style
  span: Span;
}

// For loop iterator binding: i -> x or just x
export interface ForIterator {
  index?: Token<string>;        // Optional index variable (i in "i -> x")
  arrow?: Token<'->'>;           // Arrow token if index is present
  variable: Token<string>;       // Iterator variable (x)
  span: Span;
}

// Range expression: 1..20
export interface RangeExpression {
  type: 'RangeExpression';
  start: Expr;
  dotDot: Token<'..'>;
  end: Expr;
  span: Span;
}

// For expression: for (i -> x : A) { body } or for (x : 1..20) { body }
export interface ForExpression {
  type: 'ForExpression';
  forKeyword: Token<'for'>;
  leftParen: Token<'('>;
  iterator: ForIterator;
  colon: Token<':'>;
  iterable: Expr;              // Can be a variable, array, or RangeExpression
  rightParen: Token<')'>;
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;      // Only for brace style
  bodyColon?: Token<':'>;      // Only for indentation style
  body: Expr;                   // The loop body
  rightBrace?: Token<'}'>;     // Only for brace style
  span: Span;
}

export interface IfExpression {
  type: 'IfExpression';
  ifKeyword: Token<'if'>;
  leftParen?: Token<'('>;      // Optional parentheses around condition
  condition: Expr;
  rightParen?: Token<')'>;     // Optional parentheses around condition
  thenKeyword: Token<'then'>;  // Required then keyword
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;      // Only for brace style
  bodyColon?: Token<':'>;      // Only for indentation style
  thenBody: Expr;
  rightBrace?: Token<'}'>;     // Only for brace style
  elseClause?: ElseClause;     // Optional else or else if
  span: Span;
}

export interface ElseClause {
  elseKeyword: Token<'else'>;
  elseIf?: IfExpression;       // For else if chains
  style?: 'braces' | 'indentation';  // Only for final else
  leftBrace?: Token<'{'>;      // Only for brace style else
  bodyColon?: Token<':'>;      // Only for indentation style else
  elseBody?: Expr;             // Only for final else
  rightBrace?: Token<'}'>;     // Only for brace style else
  span: Span;
}

// Case branch: 0 => expr or _ => expr
export interface CaseBranch {
  pattern: Expr | Token<'_'>;  // Pattern to match or _ for default
  arrow: Token<'=>'>;
  body?: Expr;                  // Body expression (optional for _ =>)
  span: Span;
}

// Case expression: case (e) { 0 => exp, _ => }
export interface CaseExpression {
  type: 'CaseExpression';
  caseKeyword: Token<'case'>;
  leftParen: Token<'('>;
  expr: Expr;
  rightParen: Token<')'>;
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;      // Only for brace style
  colon?: Token<':'>;          // Only for indentation style
  branches: CaseBranch[];
  commas: Token<','>[];        // Commas between branches
  rightBrace?: Token<'}'>;     // Only for brace style
  span: Span;
}

export interface Statement {
  expr?: Expr;  // Optional to support empty statements (just semicolon)
  semicolon?: Token<';'>;  // Optional for last statement
  span: Span;
}

export interface Block {
  type: 'Block';
  style: 'braces' | 'indentation' | 'parentheses';
  keyword?: Token<'block'>;  // Only for indentation style
  leftBrace?: Token<'{'>;    // Only for brace style
  colon?: Token<':'>;        // Only for indentation style
  statements: Statement[];
  rightBrace?: Token<'}'>;   // Only for brace style
  span: Span;
}

export interface Specifier {
  type: 'Specifier';
  leftAngle: Token<'<'>;
  name: Token<string>;
  leftParen?: Token<'('>;     // Only if has argument
  argument?: Token<string>;    // The argument value
  rightParen?: Token<')'>;    // Only if has argument
  rightAngle: Token<'>'>;
  span: Span;
}

export interface VariableDeclaration {
  type: 'VariableDeclaration';
  varKeyword: Token<'var'>;
  name: Token<string>;
  specifier?: Specifier;
  colon: Token<':'>;
  typeName: Token<string>;
  equals: Token<'='>;
  value: Expr;
  span: Span;
}

export interface ConstDeclaration {
  type: 'ConstDeclaration';
  name: Token<string>;
  colon?: Token<':'>;
  typeName?: Token<string>;
  assignOp: Token<':=' | '='>;
  value: Expr;
  span: Span;
}

export interface FunctionParam {
  name: Token<string>;
  colon?: Token<':'>;     // Optional type annotation
  type?: Token<string>;   // Optional type
  comma?: Token<','>;
  span: Span;
}

export interface FunctionDeclaration {
  type: 'FunctionDeclaration';
  name: Token<string>;
  specifiers: Specifier[];
  postParenSpecifiers?: Specifier[]; // New field for specifiers after parentheses
  leftParen: Token<'('>;
  params: FunctionParam[];
  rightParen: Token<')'>;
  colon?: Token<':'>;
  returnType?: Token<string>;
  assignOp: Token<':=' | '='>;
  body: Expr;
  span: Span;
}

export interface Assignment {
  type: 'Assignment';
  target: Expr;  // Can be Variable, IndexAccess, MemberAccess, or nested combinations
  assignOp: Token<':=' | '='>;
  value: Expr;
  span: Span;
}

export interface ClassExpression {
  type: 'ClassExpression';
  decorators?: Decorator[];    // Optional decorators
  keyword: Token<'class'>;
  specifiers?: Token<string>[]; // Class-level annotations like <computes>
  leftParenInherit?: Token<'('>;  // For class inheritance
  parentClass?: Token<string>;     // Parent class name
  rightParenInherit?: Token<')'>;  // For class inheritance
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;     // Only for brace style
  colon?: Token<':'>;          // Only for indentation style
  body: ClassMember[];         // Class members
  rightBrace?: Token<'}'>;     // Only for brace style
  span: Span;
}

export interface EmptyExpression {
  type: 'EmptyExpression';
  span: Span;
}

export interface BreakExpression {
  type: 'BreakExpression';
  keyword: Token<'break'>;
  span: Span;
}

export interface ContinueExpression {
  type: 'ContinueExpression';
  keyword: Token<'continue'>;
  span: Span;
}

export interface SetStatement {
  type: 'SetStatement';
  setKeyword: Token<'set'>;
  variable: Token<string>;
  equals: Token<'='>;
  value: Expr;
  span: Span;
}

export interface LoopStatement {
  type: 'LoopStatement';
  loopKeyword: Token<'loop'>;
  style: 'braces' | 'indentation';
  body: Expr;
  span: Span;
  leftBrace?: Token<'{'>;
  colon?: Token<':'>;
  rightBrace?: Token<'}'>;
}


// Helper constructors
export const token = <T>(
  text: string,
  value: T,
  trivia: Trivia,
  span: Span
): Token<T> => ({
  text,
  value,
  trivia,
  span,
});

export const integerLiteral = (
  token: Token<number>,
  span: Span
): IntegerLiteral => ({
  type: 'IntegerLiteral',
  token,
  span,
});

export const booleanLiteral = (
  token: Token<boolean>,
  span: Span
): BooleanLiteral => ({
  type: 'BooleanLiteral',
  token,
  span,
});

export const floatLiteral = (
  token: Token<number>,
  span: Span
): FloatLiteral => ({
  type: 'FloatLiteral',
  token,
  span,
});

export const stringLiteral = (
  token: Token<string>,
  span: Span
): StringLiteral => ({
  type: 'StringLiteral',
  token,
  span,
});

export const variable = (
  token: Token<string>,
  span: Span
): Variable => ({
  type: 'Variable',
  token,
  span,
});

export const binaryOp = (
  left: Expr,
  operator: Token<'+' | '-' | '*' | '/' | '%' | '>' | '<' | '>=' | '<=' | '==' | '!=' | 'and' | 'or'>,
  right: Expr,
  span: Span
): BinaryOp => ({
  type: 'BinaryOp',
  left,
  operator,
  right,
  span,
});

export const unaryOp = (
  operator: Token<'not' | '-'>,
  operand: Expr,
  span: Span
): UnaryOp => ({
  type: 'UnaryOp',
  operator,
  operand,
  span,
});

export const parenthesized = (
  leftParen: Token<'('>,
  expr: Expr,
  rightParen: Token<')'>,
  span: Span
): Parenthesized => ({
  type: 'Parenthesized',
  leftParen,
  expr,
  rightParen,
  span,
});

export const functionCall = (
  name: Token<string>,
  leftParen: Token<'(' | '['>,
  args: Expr[],
  commas: Token<','>[],
  rightParen: Token<')' | ']'>,
  span: Span
): FunctionCall => ({
  type: 'FunctionCall',
  name,
  leftParen,
  args,
  commas,
  rightParen,
  span,
});

export const application = (
  func: Expr,
  leftParen: Token<'(' | '['>,
  args: Expr[],
  commas: Token<','>[],
  rightParen: Token<')' | ']'>,
  span: Span
): Application => ({
  type: 'Application',
  func,
  leftParen,
  args,
  commas,
  rightParen,
  span,
});

export const memberAccess = (
  object: Expr,
  dot: Token<'.'>,
  member: Token<string>,
  span: Span
): MemberAccess => ({
  type: 'MemberAccess',
  object,
  dot,
  member,
  span,
});

export const indexAccess = (
  object: Expr,
  leftBracket: Token<'['>,
  index: Expr,
  rightBracket: Token<']'>,
  span: Span
): IndexAccess => ({
  type: 'IndexAccess',
  object,
  leftBracket,
  index,
  rightBracket,
  span,
});

export const lambdaExpression = (
  parameter: Token<string>,
  arrow: Token<'=>'>,
  body: Expr,
  span: Span
): LambdaExpression => ({
  type: 'LambdaExpression',
  parameter,
  arrow,
  body,
  span,
});

export const methodCall = (
  object: Expr,
  dot: Token<'.'>,
  method: Token<string>,
  leftParen: Token<'(' | '['>,
  args: Expr[],
  commas: Token<','>[],
  rightParen: Token<')' | ']'>,
  span: Span
): MethodCall => ({
  type: 'MethodCall',
  object,
  dot,
  method,
  leftParen,
  args,
  commas,
  rightParen,
  span,
});

export const fieldAssignment = (
  name: Token<string>,
  assignOp: Token<':=' | '='>,
  value: Expr,
  comma: Token<','> | undefined,
  span: Span,
  colon?: Token<':'>,
  typeAnnotation?: Token<string>
): FieldAssignment => ({
  name,
  colon,
  typeAnnotation,
  assignOp,
  value,
  comma,
  span,
});

export const objectConstruction = (
  typeExpr: Variable | Application,
  style: 'braces' | 'indentation' | 'parentheses',
  fields: FieldAssignment[],
  leftBrace: Token<'{'> | undefined,
  rightBrace: Token<'}'> | undefined,
  colon: Token<':'> | undefined,
  span: Span
): ObjectConstruction => ({
  type: 'ObjectConstruction',
  typeExpr,
  style,
  fields,
  leftBrace,
  rightBrace,
  colon,
  span,
});

export const arrayConstruction = (
  arrayKeyword: Token<'array'> | undefined,
  style: 'braces' | 'indentation' | 'parentheses',
  elements: Expr[],
  commas: Token<','>[],
  span: Span,
  leftBrace?: Token<'{'>,
  colon?: Token<':'>,
  rightBrace?: Token<'}'>
): ArrayConstruction => ({
  type: 'ArrayConstruction',
  arrayKeyword,
  style,
  elements,
  commas,
  leftBrace,
  colon,
  rightBrace,
  span,
});

export const forIterator = (
  variable: Token<string>,
  span: Span,
  index?: Token<string>,
  arrow?: Token<'->'>
): ForIterator => ({
  index,
  arrow,
  variable,
  span,
});

export const rangeExpression = (
  start: Expr,
  dotDot: Token<'..'>,
  end: Expr,
  span: Span
): RangeExpression => ({
  type: 'RangeExpression',
  start,
  dotDot,
  end,
  span,
});

export const breakStatement = (
  keyword: Token<'break'>,
  span: Span
): BreakExpression => ({
  type: 'BreakExpression',
  keyword,
  span,
});

export const continueStatement = (
  keyword: Token<'continue'>,
  span: Span
): ContinueExpression => ({
  type: 'ContinueExpression',
  keyword,
  span,
});

export const setStatement = (
  setKeyword: Token<'set'>,
  variable: Token<string>,
  equals: Token<'='>,
  value: Expr,
  span: Span
): SetStatement => ({
  type: 'SetStatement',
  setKeyword,
  variable,
  equals,
  value,
  span,
});

export const loopStatement = (
  loopKeyword: Token<'loop'>,
  style: 'braces' | 'indentation',
  body: Expr,
  span: Span,
  leftBrace?: Token<'{'>,
  colon?: Token<':'>,
  rightBrace?: Token<'}'>
): LoopStatement => ({
  type: 'LoopStatement',
  loopKeyword,
  style,
  body,
  span,
  leftBrace,
  colon,
  rightBrace,
});

export const caseBranch = (
  pattern: Expr | Token<'_'>,
  arrow: Token<'=>'>,
  span: Span,
  body?: Expr
): CaseBranch => ({
  pattern,
  arrow,
  body,
  span,
});

export const forExpression = (
  forKeyword: Token<'for'>,
  leftParen: Token<'('>,
  iterator: ForIterator,
  colon: Token<':'>,
  iterable: Expr,
  rightParen: Token<')'>,
  style: 'braces' | 'indentation' | 'parentheses',
  body: Expr,
  span: Span,
  leftBrace?: Token<'{'>,
  bodyColon?: Token<':'>,
  rightBrace?: Token<'}'>
): ForExpression => ({
  type: 'ForExpression',
  forKeyword,
  leftParen,
  iterator,
  colon,
  iterable,
  rightParen,
  style,
  leftBrace,
  bodyColon,
  body,
  rightBrace,
  span,
});

export const ifExpression = (
  ifKeyword: Token<'if'>,
  leftParen: Token<'('> | undefined,
  condition: Expr,
  rightParen: Token<')'> | undefined,
  thenKeyword: Token<'then'>,
  style: 'braces' | 'indentation' | 'parentheses',
  thenBody: Expr,
  span: Span,
  leftBrace?: Token<'{'>,
  bodyColon?: Token<':'>,
  rightBrace?: Token<'}'>,
  elseClause?: ElseClause
): IfExpression => ({
  type: 'IfExpression',
  ifKeyword,
  leftParen,
  condition,
  rightParen,
  thenKeyword,
  style,
  leftBrace,
  bodyColon,
  thenBody,
  rightBrace,
  elseClause,
  span,
});

export const elseClause = (
  elseKeyword: Token<'else'>,
  span: Span,
  elseIf?: IfExpression,
  style?: 'braces' | 'indentation',
  leftBrace?: Token<'{'>,
  bodyColon?: Token<':'>,
  elseBody?: Expr,
  rightBrace?: Token<'}'>
): ElseClause => ({
  elseKeyword,
  elseIf,
  style,
  leftBrace,
  bodyColon,
  elseBody,
  rightBrace,
  span,
});

export const caseExpression = (
  caseKeyword: Token<'case'>,
  leftParen: Token<'('>,
  expr: Expr,
  rightParen: Token<')'>,
  style: 'braces' | 'indentation' | 'parentheses',
  branches: CaseBranch[],
  commas: Token<','>[],
  span: Span,
  leftBrace?: Token<'{'>,
  colon?: Token<':'>,
  rightBrace?: Token<'}'>
): CaseExpression => ({
  type: 'CaseExpression',
  caseKeyword,
  leftParen,
  expr,
  rightParen,
  style,
  leftBrace,
  colon,
  branches,
  commas,
  rightBrace,
  span,
});

export const statement = (
  expr: Expr | undefined,
  semicolon: Token<';'> | undefined,
  span: Span
): Statement => ({
  expr,
  semicolon,
  span,
});

export const block = (
  style: 'braces' | 'indentation' | 'parentheses',
  statements: Statement[],
  keyword: Token<'block'> | undefined,
  leftBrace: Token<'{'> | undefined,
  rightBrace: Token<'}'> | undefined,
  colon: Token<':'> | undefined,
  span: Span
): Block => ({
  type: 'Block',
  style,
  statements,
  keyword,
  leftBrace,
  rightBrace,
  colon,
  span,
});

export const specifier = (
  leftAngle: Token<'<'>,
  name: Token<string>,
  leftParen: Token<'('> | undefined,
  argument: Token<string> | undefined,
  rightParen: Token<')'> | undefined,
  rightAngle: Token<'>'>,
  span: Span
): Specifier => ({
  type: 'Specifier',
  leftAngle,
  name,
  leftParen,
  argument,
  rightParen,
  rightAngle,
  span,
});

export const variableDeclaration = (
  varKeyword: Token<'var'>,
  name: Token<string>,
  specifier: Specifier | undefined,
  colon: Token<':'>,
  typeName: Token<string>,
  equals: Token<'='>,
  value: Expr,
  span: Span
): VariableDeclaration => ({
  type: 'VariableDeclaration',
  varKeyword,
  name,
  specifier,
  colon,
  typeName,
  equals,
  value,
  span,
});

export const constDeclaration = (
  name: Token<string>,
  colon: Token<':'> | undefined,
  typeName: Token<string> | undefined,
  assignOp: Token<':=' | '='>,
  value: Expr,
  span: Span
): ConstDeclaration => ({
  type: 'ConstDeclaration',
  name,
  colon,
  typeName,
  assignOp,
  value,
  span,
});

export const functionParam = (
  name: Token<string>,
  colon: Token<':'> | undefined,
  typeAnnotation: Expr | Token<string> | undefined,
  span: Span
): FunctionParam => ({
  name,
  colon,
  type: typeAnnotation && typeof typeAnnotation === 'object' && 'type' in typeAnnotation && typeAnnotation.type === 'Variable'
    ? typeAnnotation.token
    : typeAnnotation && 'text' in typeAnnotation
    ? typeAnnotation as Token<string>
    : undefined,
  comma: undefined,
  span,
});

export const functionDeclaration = (
  name: Token<string>,
  specifiers: Specifier[],
  postParenSpecifiers: Specifier[] | undefined,
  leftParen: Token<'('>,
  params: FunctionParam[],
  rightParen: Token<')'>,
  colon: Token<':'> | undefined,
  returnType: Token<string> | undefined,
  assignOp: Token<':=' | '='>,
  body: Expr,
  span: Span
): FunctionDeclaration => ({
  type: 'FunctionDeclaration',
  name,
  specifiers,
  postParenSpecifiers,
  leftParen,
  params,
  rightParen,
  colon,
  returnType,
  assignOp,
  body,
  span,
});

export const assignment = (
  target: Expr,
  assignOp: Token<':=' | '='>,
  value: Expr,
  span: Span
): Assignment => ({
  type: 'Assignment',
  target,
  assignOp,
  value,
  span,
});

export const emptyExpression = (
  span: Span
): EmptyExpression => ({
  type: 'EmptyExpression',
  span,
});

export const breakExpression = (
  keyword: Token<'break'>,
  span: Span
): BreakExpression => ({
  type: 'BreakExpression',
  keyword,
  span,
});

export const continueExpression = (
  keyword: Token<'continue'>,
  span: Span
): ContinueExpression => ({
  type: 'ContinueExpression',
  keyword,
  span,
});


const prettyPrintField = (field: FieldAssignment): string => {
  let result = printToken(field.name);
  if (field.colon) {
    result += printToken(field.colon);
  }
  if (field.typeAnnotation) {
    result += printToken(field.typeAnnotation);
  }
  result += printToken(field.assignOp);
  result += prettyPrintExpression(field.value);
  if (field.comma) {
    result += printToken(field.comma);
  }
  return result;
};

// Helper function for field declarations
export const fieldDeclaration = (
  name: Token<string>,
  colon: Token<':'> | undefined,
  typeAnnotation: Expr | undefined,
  assignOp: Token<'=' | ':='> | undefined,
  initializer: Expr | undefined,
  span: Span,
  decorators?: Decorator[],
  varKeyword?: Token<'var'>
): FieldDeclaration => ({
  type: 'FieldDeclaration',
  decorators: decorators && decorators.length > 0 ? decorators : undefined,
  varKeyword: varKeyword,
  name,
  colon,
  typeAnnotation,
  assignOp,
  initializer,
  semicolon: undefined,
  span
});

// Helper function for method declarations
export const methodDeclaration = (
  name: Token<string>,
  leftParen: Token<'('>,
  params: FunctionParam[],
  commas: Token<','>[], // Added commas parameter
  rightParen: Token<')'>,
  colon: Token<':'> | undefined,
  returnType: Expr | undefined,
  assignOp: Token<'=' | ':='>,
  body: Expr | Block,
  span: Span
): MethodDeclaration => ({
  type: 'MethodDeclaration',
  decorators: undefined,
  name,
  preSpecifiers: undefined,
  leftParen,
  params,
  rightParen,
  postSpecifiers: undefined,
  colon,
  returnType,
  assignOp,
  body,
  semicolon: undefined,
  span
});

// Helper function for enum members
export const enumMember = (
  name: Token<string>,
  span: Span
): EnumMember => ({
  type: 'EnumMember',
  name,
  span
});

// Helper function to create decorator nodes
export const decorator = (
  at: Token<'@'>,
  name: Token<string>,
  span: Span,
  leftParen?: Token<'('>,
  args?: Expr[],
  commas?: Token<','>[],
  rightParen?: Token<')'>,
): Decorator => ({
  type: 'Decorator',
  at,
  name,
  leftParen,
  args,
  commas,
  rightParen,
  span,
});

// Helper function to create class expression nodes
export const classExpression = (
  keyword: Token<'class'>,
  specifiers: Token<string>[] | undefined,
  leftParenInherit: Token<'('> | undefined,
  parentClass: Token<string> | undefined,
  rightParenInherit: Token<')'> | undefined,
  style: 'braces' | 'indentation' | 'parentheses',
  leftBrace: Token<'{'> | undefined,
  colon: Token<':'> | undefined,
  body: ClassMember[],
  rightBrace: Token<'}'> | undefined,
  span: Span,
  decorators?: Decorator[]
): ClassExpression => ({
  type: 'ClassExpression',
  decorators,
  keyword,
  specifiers,
  leftParenInherit,
  parentClass,
  rightParenInherit,
  style,
  leftBrace,
  colon,
  body,
  rightBrace,
  span,
});

const prettyPrintFieldDeclaration = (field: FieldDeclaration, isIndentationStyle: boolean = false): string => {
  let result = '';

  // Print decorators
  if (field.decorators) {
    for (const dec of field.decorators) {
      result += prettyPrintDecorator(dec, isIndentationStyle);
    }
  }

  // Print var keyword if present
  if (field.varKeyword) {
    result += printToken(field.varKeyword);
  }

  // Print field name
  result += printToken(field.name);

  // Print colon and type (optional for := syntax)
  if (field.colon && field.typeAnnotation) {
    result += printToken(field.colon);
    result += prettyPrintExpression(field.typeAnnotation);
  }

  // Print optional initializer
  if (field.assignOp && field.initializer) {
    result += printToken(field.assignOp);
    result += prettyPrintExpression(field.initializer);
  }

  // Print optional semicolon
  if (field.semicolon) {
    result += printToken(field.semicolon);
  }

  return result;
};

const prettyPrintMethodDeclaration = (method: MethodDeclaration, isIndentationStyle: boolean = false): string => {
  let result = '';

  // Print decorators
  if (method.decorators) {
    for (const dec of method.decorators) {
      result += prettyPrintDecorator(dec, isIndentationStyle);
    }
  }

  // Print method name
  result += printToken(method.name);

  // Print pre-specifiers
  if (method.preSpecifiers) {
    for (const spec of method.preSpecifiers) {
      result += prettyPrintExpression(spec as any); // Specifier prettyPrint exists
    }
  }

  // Print parameters
  result += printToken(method.leftParen);
  for (let i = 0; i < method.params.length; i++) {
    const param = method.params[i];

    result += printToken(param.name);
    if (param.colon && param.type) {
      result += printToken(param.colon);
      result += printToken(param.type);
    }
    // Print comma if present (all parameters except the last should have one)
    if (param.comma) {
      result += printToken(param.comma);
    }
  }
  result += printToken(method.rightParen);

  // Print post-specifiers
  if (method.postSpecifiers) {
    for (const spec of method.postSpecifiers) {
      result += prettyPrintExpression(spec as any);
    }
  }

  // Print return type
  if (method.colon && method.returnType) {
    result += printToken(method.colon);
    result += prettyPrintExpression(method.returnType);
  }

  // Print assignment and body (skip for method signatures)
  const isMethodSignature = method.body.type === 'EmptyExpression' &&
                            method.assignOp.span.start === method.assignOp.span.end;

  if (!isMethodSignature) {
    result += printToken(method.assignOp);
    if (method.body.type === 'Block') {
      result += prettyPrintExpression(method.body);
    } else {
      result += prettyPrintExpression(method.body);
    }
  }

  // Print optional semicolon
  if (method.semicolon) {
    result += printToken(method.semicolon);
  }

  return result;
};

const prettyPrintEnumMember = (member: EnumMember): string => {
  return printToken(member.name);
};

const prettyPrintClassMember = (member: ClassMember, isIndentationStyle: boolean = false): string => {
  switch (member.type) {
    case 'FieldDeclaration':
      return prettyPrintFieldDeclaration(member, isIndentationStyle);
    case 'MethodDeclaration':
      return prettyPrintMethodDeclaration(member, isIndentationStyle);
    default:
      throw new Error(`Unknown class member type`);
  }
};

const prettyPrintMember = (member: ClassMember | EnumMember, isIndentationStyle: boolean = false): string => {
  switch (member.type) {
    case 'EnumMember':
      return prettyPrintEnumMember(member);
    case 'FieldDeclaration':
      return prettyPrintFieldDeclaration(member, isIndentationStyle);
    case 'MethodDeclaration':
      return prettyPrintMethodDeclaration(member, isIndentationStyle);
    default:
      throw new Error(`Unknown member type`);
  }
};

const prettyPrintStatement = (stmt: Statement): string => {
  let result = '';
  if (stmt.expr) {
    result += prettyPrintExpression(stmt.expr);
  }
  if (stmt.semicolon) {
    result += `${stmt.semicolon.trivia.leading}${stmt.semicolon.text}${stmt.semicolon.trivia.trailing}`;
  }
  return result;
};

// Pretty print for expressions
// Utility functions for reconstruction
function printToken<T>(token: Token<T>): string {
  return `${token.trivia.leading}${token.text}${token.trivia.trailing}`;
}

function printTokens<T>(tokens: Token<T>[], separator?: (i: number) => string): string {
  return tokens.map((token, i) => {
    const tokenStr = printToken(token);
    const sep = separator && i < tokens.length - 1 ? separator(i) : '';
    return tokenStr + sep;
  }).join('');
}

function printOptionalToken<T>(token: Token<T> | undefined): string {
  return token ? printToken(token) : '';
}

export function prettyPrint(expr: Expr): string;
// Pretty print for programs
export function prettyPrint(program: Program): string;
// Pretty print for using statements
export function prettyPrint(usingStmt: UsingStatement): string;
// Pretty print for top-level declarations
export function prettyPrint(decl: TopLevelDeclaration): string;
// Implementation
export function prettyPrint(node: Expr | Program | UsingStatement | TopLevelDeclaration): string {
  if (typeof node === 'object' && node !== null && 'type' in node) {
    switch (node.type) {
      case 'Program':
        return prettyPrintProgram(node);
      case 'UsingStatement':
        return prettyPrintUsingStatement(node);
      case 'TopLevelDeclaration':
        return prettyPrintTopLevelDeclaration(node);
      default:
        return prettyPrintExpression(node as Expr);
    }
  }
  throw new Error('Invalid node passed to prettyPrint');
}

const prettyPrintExpression = (expr: Expr): string => {
  switch (expr.type) {
    case 'IntegerLiteral':
      return printToken(expr.token);
    case 'BooleanLiteral':
      return printToken(expr.token);
    case 'FloatLiteral':
      return printToken(expr.token);
    case 'StringLiteral':
      return printToken(expr.token);
    case 'Variable':
      return printToken(expr.token);
    case 'BinaryOp':
      return `${prettyPrintExpression(expr.left)}${printToken(expr.operator)}${prettyPrintExpression(expr.right)}`;
    case 'UnaryOp':
      return `${printToken(expr.operator)}${prettyPrintExpression(expr.operand)}`;
    case 'Parenthesized':
      return `${printToken(expr.leftParen)}${prettyPrintExpression(expr.expr)}${printToken(expr.rightParen)}`;
    case 'FunctionCall': {
      let result = printToken(expr.name);
      result += printToken(expr.leftParen);
      for (let i = 0; i < expr.args.length; i++) {
        result += prettyPrintExpression(expr.args[i]);
        // Print comma if it exists (including trailing comma)
        if (i < expr.commas.length) {
          result += printToken(expr.commas[i]);
        }
      }
      result += printToken(expr.rightParen);
      return result;
    }
    case 'Application': {
      let result = prettyPrintExpression(expr.func);
      result += printToken(expr.leftParen);
      for (let i = 0; i < expr.args.length; i++) {
        result += prettyPrintExpression(expr.args[i]);
        // Print comma if it exists (including trailing comma)
        if (i < expr.commas.length) {
          result += printToken(expr.commas[i]);
        }
      }
      result += printToken(expr.rightParen);
      return result;
    }
    case 'MemberAccess': {
      let result = prettyPrintExpression(expr.object);
      result += printToken(expr.dot);
      result += printToken(expr.member);
      return result;
    }
    case 'IndexAccess': {
      let result = prettyPrintExpression(expr.object);
      result += `${expr.leftBracket.trivia.leading}${expr.leftBracket.text}${expr.leftBracket.trivia.trailing}`;
      result += prettyPrintExpression(expr.index);
      result += `${expr.rightBracket.trivia.leading}${expr.rightBracket.text}${expr.rightBracket.trivia.trailing}`;
      return result;
    }
    case 'LambdaExpression': {
      let result = `${expr.parameter.trivia.leading}${expr.parameter.text}${expr.parameter.trivia.trailing}`;
      result += `${expr.arrow.trivia.leading}${expr.arrow.text}${expr.arrow.trivia.trailing}`;
      result += prettyPrintExpression(expr.body);
      return result;
    }
    case 'MethodCall': {
      let result = prettyPrintExpression(expr.object);
      result += printToken(expr.dot);
      result += printToken(expr.method);
      result += printToken(expr.leftParen);
      for (let i = 0; i < expr.args.length; i++) {
        result += prettyPrintExpression(expr.args[i]);
        // Print comma if it exists (including trailing comma)
        if (i < expr.commas.length) {
          result += printToken(expr.commas[i]);
        }
      }
      result += printToken(expr.rightParen);
      return result;
    }
    case 'ObjectConstruction': {
      let result = prettyPrintExpression(expr.typeExpr);
      if (expr.style === 'braces') {
        if (expr.leftBrace) {
          result += printToken(expr.leftBrace);
        }
        for (const field of expr.fields) {
          result += prettyPrintField(field);
        }
        if (expr.rightBrace) {
          result += printToken(expr.rightBrace);
        }
      } else {
        // indentation style
        if (expr.colon) {
          result += printToken(expr.colon);
        }
        for (const field of expr.fields) {
          result += prettyPrintField(field);
        }
      }
      return result;
    }
    case 'ForExpression': {
      let result = `${expr.forKeyword.trivia.leading}${expr.forKeyword.text}${expr.forKeyword.trivia.trailing}`;
      result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;

      if (expr.iterator.index && expr.iterator.arrow) {
        result += `${expr.iterator.index.trivia.leading}${expr.iterator.index.text}${expr.iterator.index.trivia.trailing}`;
        result += `${expr.iterator.arrow.trivia.leading}${expr.iterator.arrow.text}${expr.iterator.arrow.trivia.trailing}`;
      }

      result += `${expr.iterator.variable.trivia.leading}${expr.iterator.variable.text}${expr.iterator.variable.trivia.trailing}`;
      result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
      result += prettyPrintExpression(expr.iterable);
      result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;

      if (expr.style === 'braces') {
        if (expr.leftBrace) {
          result += `${expr.leftBrace.trivia.leading}${expr.leftBrace.text}${expr.leftBrace.trivia.trailing}`;
        }
        // If body is a block that shares the same braces as the for loop,
        // print only its statements (since the for loop provides the braces)
        if (expr.body.type === 'Block' && expr.body.style === 'braces' &&
            expr.leftBrace && expr.rightBrace &&
            expr.body.leftBrace && expr.body.rightBrace &&
            expr.body.leftBrace.span.start === expr.leftBrace.span.start &&
            expr.body.rightBrace.span.end === expr.rightBrace.span.end) {
          // Block represents the for loop's own braces - don't double-print
          for (const stmt of expr.body.statements) {
            result += prettyPrintStatement(stmt);
          }
        } else {
          // Either not a block, or it's a nested block with its own braces
          result += prettyPrintExpression(expr.body);
        }
        if (expr.rightBrace) {
          result += `${expr.rightBrace.trivia.leading}${expr.rightBrace.text}${expr.rightBrace.trivia.trailing}`;
        }
      } else {
        // indentation style
        if (expr.bodyColon) {
          result += `${expr.bodyColon.trivia.leading}${expr.bodyColon.text}${expr.bodyColon.trivia.trailing}`;
        }
        // If body is a block that shares the same braces as the for loop,
        // print only its statements (since the for loop provides the braces)
        if (expr.body.type === 'Block' && expr.body.style === 'braces' &&
            expr.leftBrace && expr.rightBrace &&
            expr.body.leftBrace && expr.body.rightBrace &&
            expr.body.leftBrace.span.start === expr.leftBrace.span.start &&
            expr.body.rightBrace.span.end === expr.rightBrace.span.end) {
          // Block represents the for loop's own braces - don't double-print
          for (const stmt of expr.body.statements) {
            result += prettyPrintStatement(stmt);
          }
        } else {
          // Either not a block, or it's a nested block with its own braces
          result += prettyPrintExpression(expr.body);
        }
      }
      return result;
    }
    case 'ArrayConstruction': {
      let result = '';
      if (expr.arrayKeyword) {
        result += `${expr.arrayKeyword.trivia.leading}${expr.arrayKeyword.text}${expr.arrayKeyword.trivia.trailing}`;
      }
      if (expr.style === 'braces') {
        if (expr.leftBrace) {
          result += `${expr.leftBrace.trivia.leading}${expr.leftBrace.text}${expr.leftBrace.trivia.trailing}`;
        }
        for (let i = 0; i < expr.elements.length; i++) {
          result += prettyPrintExpression(expr.elements[i]);
          if (i < expr.commas.length) {
            const comma = expr.commas[i];
            result += `${comma.trivia.leading}${comma.text}${comma.trivia.trailing}`;
          }
        }
        if (expr.rightBrace) {
          result += `${expr.rightBrace.trivia.leading}${expr.rightBrace.text}${expr.rightBrace.trivia.trailing}`;
        }
      } else {
        // indentation style
        if (expr.colon) {
          result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
        }
        for (let i = 0; i < expr.elements.length; i++) {
          result += prettyPrintExpression(expr.elements[i]);
          if (i < expr.commas.length) {
            const comma = expr.commas[i];
            result += `${comma.trivia.leading}${comma.text}${comma.trivia.trailing}`;
          }
        }
      }
      return result;
    }
    case 'Block': {
      let result = '';
      if (expr.style === 'braces') {
        // Include keyword if present (for 'block { }' syntax)
        if (expr.keyword) {
          result += printToken(expr.keyword);
        }
        if (expr.leftBrace) {
          result += printToken(expr.leftBrace);
        }
        for (const stmt of expr.statements) {
          result += prettyPrintStatement(stmt);
        }
        if (expr.rightBrace) {
          result += printToken(expr.rightBrace);
        }
      } else {
        // indentation style
        if (expr.keyword) {
          result += printToken(expr.keyword);
        }
        if (expr.colon) {
          result += printToken(expr.colon);
        }
        for (const stmt of expr.statements) {
          result += prettyPrintStatement(stmt);
        }
      }
      return result;
    }
    case 'Specifier': {
      let result = `${expr.leftAngle.trivia.leading}${expr.leftAngle.text}${expr.leftAngle.trivia.trailing}`;
      result += `${expr.name.trivia.leading}${expr.name.text}${expr.name.trivia.trailing}`;
      if (expr.leftParen && expr.argument && expr.rightParen) {
        result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;
        result += `${expr.argument.trivia.leading}${expr.argument.text}${expr.argument.trivia.trailing}`;
        result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;
      }
      result += `${expr.rightAngle.trivia.leading}${expr.rightAngle.text}${expr.rightAngle.trivia.trailing}`;
      return result;
    }
    case 'VariableDeclaration': {
      let result = printToken(expr.varKeyword);
      result += printToken(expr.name);
      if (expr.specifier) {
        result += prettyPrint(expr.specifier);
      }
      result += printToken(expr.colon);
      result += printToken(expr.typeName);
      result += printToken(expr.equals);
      result += prettyPrintExpression(expr.value);
      return result;
    }
    case 'ConstDeclaration': {
      let result = printToken(expr.name);
      if (expr.colon && expr.typeName) {
        result += printToken(expr.colon);
        result += printToken(expr.typeName);
      }
      result += printToken(expr.assignOp);
      result += prettyPrintExpression(expr.value);
      return result;
    }
    case 'Assignment': {
      let result = prettyPrintExpression(expr.target);
      result += `${expr.assignOp.trivia.leading}${expr.assignOp.text}${expr.assignOp.trivia.trailing}`;
      result += prettyPrintExpression(expr.value);
      return result;
    }
    case 'ClassExpression': {
      let result = '';

      // Include decorators if present
      if (expr.decorators && expr.decorators.length > 0) {
        result += expr.decorators.map((dec) => prettyPrintDecorator(dec)).join('');
      }

      result += `${expr.keyword.trivia.leading}${expr.keyword.text}${expr.keyword.trivia.trailing}`;

      // Include class-level specifiers if present
      if (expr.specifiers && expr.specifiers.length > 0) {
        result += expr.specifiers.map(spec => `${spec.trivia.leading}${spec.text}${spec.trivia.trailing}`).join('');
      }

      // Handle parentheses (for inheritance or empty parentheses)
      if (expr.leftParenInherit && expr.rightParenInherit) {
        result += `${expr.leftParenInherit.trivia.leading}${expr.leftParenInherit.text}${expr.leftParenInherit.trivia.trailing}`;
        if (expr.parentClass) {
          result += `${expr.parentClass.trivia.leading}${expr.parentClass.text}${expr.parentClass.trivia.trailing}`;
        }
        result += `${expr.rightParenInherit.trivia.leading}${expr.rightParenInherit.text}${expr.rightParenInherit.trivia.trailing}`;
      }

      if (expr.style === 'braces' && expr.leftBrace && expr.rightBrace) {
        result += `${expr.leftBrace.trivia.leading}${expr.leftBrace.text}${expr.leftBrace.trivia.trailing}`;
        result += expr.body.map(member => prettyPrintMember(member, false)).join('');
        result += `${expr.rightBrace.trivia.leading}${expr.rightBrace.text}${expr.rightBrace.trivia.trailing}`;
      } else if (expr.style === 'indentation' && expr.colon) {
        result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
        result += expr.body.map(member => prettyPrintMember(member, true)).join('');
      } else if (expr.style === 'parentheses') {
        // Empty class with just parentheses - body is empty
        // Nothing more to add
      }

      return result;
    }
    case 'EmptyExpression': {
      return '';
    }
    case 'BreakExpression': {
      return `${expr.keyword.trivia.leading}${expr.keyword.text}${expr.keyword.trivia.trailing}`;
    }
    case 'ContinueExpression': {
      return `${expr.keyword.trivia.leading}${expr.keyword.text}${expr.keyword.trivia.trailing}`;
    }
    case 'SetStatement': {
      return `${expr.setKeyword.trivia.leading}${expr.setKeyword.text}${expr.setKeyword.trivia.trailing}` +
             `${expr.variable.trivia.leading}${expr.variable.text}${expr.variable.trivia.trailing}` +
             `${expr.equals.trivia.leading}${expr.equals.text}${expr.equals.trivia.trailing}` +
             prettyPrint(expr.value);
    }
    case 'LoopStatement': {
      let result = `${expr.loopKeyword.trivia.leading}${expr.loopKeyword.text}${expr.loopKeyword.trivia.trailing}`;
      if (expr.style === 'braces') {
        if (expr.leftBrace) {
          result += `${expr.leftBrace.trivia.leading}${expr.leftBrace.text}${expr.leftBrace.trivia.trailing}`;
        }
        result += prettyPrint(expr.body);
        if (expr.rightBrace) {
          result += `${expr.rightBrace.trivia.leading}${expr.rightBrace.text}${expr.rightBrace.trivia.trailing}`;
        }
      } else {
        if (expr.colon) {
          result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
        }
        result += prettyPrint(expr.body);
      }
      return result;
    }
    case 'FunctionDeclaration': {
      let result = `${expr.name.trivia.leading}${expr.name.text}${expr.name.trivia.trailing}`;
      // Add pre-parenthesis specifiers
      for (const spec of expr.specifiers) {
        result += prettyPrint(spec);
      }
      // Add params
      result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;
      for (let i = 0; i < expr.params.length; i++) {
        const param = expr.params[i];
        result += `${param.name.trivia.leading}${param.name.text}${param.name.trivia.trailing}`;
        // Add colon and type if present
        if (param.colon) {
          result += `${param.colon.trivia.leading}${param.colon.text}${param.colon.trivia.trailing}`;
        }
        if (param.type) {
          result += `${param.type.trivia.leading}${param.type.text}${param.type.trivia.trailing}`;
        }
        if (param.comma) {
          result += `${param.comma.trivia.leading}${param.comma.text}${param.comma.trivia.trailing}`;
        }
      }
      result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;
      // Add post-parenthesis specifiers
      if (expr.postParenSpecifiers) {
        for (const spec of expr.postParenSpecifiers) {
          result += prettyPrint(spec);
        }
      }
      // Add return type if present
      if (expr.colon && expr.returnType) {
        result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
        result += `${expr.returnType.trivia.leading}${expr.returnType.text}${expr.returnType.trivia.trailing}`;
      }
      // Add assignment operator
      result += `${expr.assignOp.trivia.leading}${expr.assignOp.text}${expr.assignOp.trivia.trailing}`;
      // Add body
      result += prettyPrint(expr.body);
      return result;
    }
    case 'IfExpression': {
      let result = `${expr.ifKeyword.trivia.leading}${expr.ifKeyword.text}${expr.ifKeyword.trivia.trailing}`;
      if (expr.leftParen) {
        result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;
      }
      result += prettyPrintExpression(expr.condition);
      if (expr.rightParen) {
        result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;
      }
      // Print then keyword for both styles, but only if it has actual content (non-zero span)
      if (expr.thenKeyword && expr.thenKeyword.text && expr.thenKeyword.span.start < expr.thenKeyword.span.end) {
        result += `${expr.thenKeyword.trivia.leading}${expr.thenKeyword.text}${expr.thenKeyword.trivia.trailing}`;
      }

      if (expr.style === 'braces') {
        // The braces are handled by the nested Block expression, not the IF expression
        result += prettyPrintExpression(expr.thenBody);
      } else {
        // indentation style
        if (expr.bodyColon) {
          result += `${expr.bodyColon.trivia.leading}${expr.bodyColon.text}${expr.bodyColon.trivia.trailing}`;
        }
        result += prettyPrintExpression(expr.thenBody);
      }

      if (expr.elseClause) {
        result += `${expr.elseClause.elseKeyword.trivia.leading}${expr.elseClause.elseKeyword.text}${expr.elseClause.elseKeyword.trivia.trailing}`;

        if (expr.elseClause.elseIf) {
          // else if case - recursively print the if expression
          result += prettyPrintExpression(expr.elseClause.elseIf);
        } else if (expr.elseClause.elseBody) {
          // final else case
          if (expr.elseClause.style === 'braces') {
            if (expr.elseClause.leftBrace) {
              result += `${expr.elseClause.leftBrace.trivia.leading}${expr.elseClause.leftBrace.text}${expr.elseClause.leftBrace.trivia.trailing}`;
            }
            result += prettyPrintExpression(expr.elseClause.elseBody);
            if (expr.elseClause.rightBrace) {
              result += `${expr.elseClause.rightBrace.trivia.leading}${expr.elseClause.rightBrace.text}${expr.elseClause.rightBrace.trivia.trailing}`;
            }
          } else {
            // indentation style else
            if (expr.elseClause.bodyColon) {
              result += `${expr.elseClause.bodyColon.trivia.leading}${expr.elseClause.bodyColon.text}${expr.elseClause.bodyColon.trivia.trailing}`;
            }
            result += prettyPrintExpression(expr.elseClause.elseBody);
          }
        }
      }

      return result;
    }
    case 'CaseExpression': {
      let result = `${expr.caseKeyword.trivia.leading}${expr.caseKeyword.text}${expr.caseKeyword.trivia.trailing}`;
      result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;
      result += prettyPrint(expr.expr);
      result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;

      if (expr.style === 'braces') {
        if (expr.leftBrace) {
          result += `${expr.leftBrace.trivia.leading}${expr.leftBrace.text}${expr.leftBrace.trivia.trailing}`;
        }
        for (let i = 0; i < expr.branches.length; i++) {
          const branch = expr.branches[i];
          if ('value' in branch.pattern && branch.pattern.value === '_') {
            result += `${branch.pattern.trivia.leading}${branch.pattern.text}${branch.pattern.trivia.trailing}`;
          } else {
            result += prettyPrint(branch.pattern as Expr);
          }
          result += `${branch.arrow.trivia.leading}${branch.arrow.text}${branch.arrow.trivia.trailing}`;
          if (branch.body) {
            result += prettyPrint(branch.body);
          }
          if (i < expr.commas.length) {
            const comma = expr.commas[i];
            result += `${comma.trivia.leading}${comma.text}${comma.trivia.trailing}`;
          }
        }
        if (expr.rightBrace) {
          result += `${expr.rightBrace.trivia.leading}${expr.rightBrace.text}${expr.rightBrace.trivia.trailing}`;
        }
      } else {
        if (expr.colon) {
          result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
        }
        for (let i = 0; i < expr.branches.length; i++) {
          const branch = expr.branches[i];
          if ('value' in branch.pattern && branch.pattern.value === '_') {
            result += `${branch.pattern.trivia.leading}${branch.pattern.text}${branch.pattern.trivia.trailing}`;
          } else {
            result += prettyPrint(branch.pattern as Expr);
          }
          result += `${branch.arrow.trivia.leading}${branch.arrow.text}${branch.arrow.trivia.trailing}`;
          if (branch.body) {
            result += prettyPrint(branch.body);
          }
          if (i < expr.commas.length) {
            const comma = expr.commas[i];
            result += `${comma.trivia.leading}${comma.text}${comma.trivia.trailing}`;
          }
        }
      }
      return result;
    }
    case 'RangeExpression': {
      let result = prettyPrint(expr.start);
      result += `${expr.dotDot.trivia.leading}${expr.dotDot.text}${expr.dotDot.trivia.trailing}`;
      result += prettyPrint(expr.end);
      return result;
    }
    case 'ForExpression': {
      let result = `${expr.forKeyword.trivia.leading}${expr.forKeyword.text}${expr.forKeyword.trivia.trailing}`;
      result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;

      // Iterator
      result += `${expr.iterator.variable.trivia.leading}${expr.iterator.variable.text}${expr.iterator.variable.trivia.trailing}`;

      result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
      result += prettyPrint(expr.iterable);
      result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;

      // Body
      if (expr.style === 'braces') {
        if (expr.leftBrace) {
          result += `${expr.leftBrace.trivia.leading}${expr.leftBrace.text}${expr.leftBrace.trivia.trailing}`;
        }
        result += prettyPrint(expr.body);
        if (expr.rightBrace) {
          result += `${expr.rightBrace.trivia.leading}${expr.rightBrace.text}${expr.rightBrace.trivia.trailing}`;
        }
      } else if (expr.style === 'indentation') {
        if (expr.bodyColon) {
          result += `${expr.bodyColon.trivia.leading}${expr.bodyColon.text}${expr.bodyColon.trivia.trailing}`;
        }
        result += prettyPrint(expr.body);
      }

      return result;
    }
    case 'BreakExpression': {
      return `${expr.keyword.trivia.leading}${expr.keyword.text}${expr.keyword.trivia.trailing}`;
    }
    case 'ContinueExpression': {
      return `${expr.keyword.trivia.leading}${expr.keyword.text}${expr.keyword.trivia.trailing}`;
    }
    case 'SetStatement': {
      return `${expr.setKeyword.trivia.leading}${expr.setKeyword.text}${expr.setKeyword.trivia.trailing}` +
             `${expr.variable.trivia.leading}${expr.variable.text}${expr.variable.trivia.trailing}` +
             `${expr.equals.trivia.leading}${expr.equals.text}${expr.equals.trivia.trailing}` +
             prettyPrint(expr.value);
    }
    case 'EmptyExpression': {
      return '';
    }
    default: {
      // This should never be reached if the switch is exhaustive
      const _exhaustive: never = expr;
      throw new Error(`Unhandled expression type in prettyPrint: ${(expr as any).type}`);
    }
  }
};

const toCompactStringEnumMember = (member: EnumMember): string => {
  return member.name.value;
};

const toCompactStringClassMember = (member: ClassMember): string => {
  switch (member.type) {
    case 'FieldDeclaration':
      // Format: [var] name:type[=value]
      let fieldStr = '';
      if (member.varKeyword) {
        fieldStr += member.varKeyword.value + ' ';
      }
      fieldStr += member.name.value;
      if (member.typeAnnotation) {
        fieldStr += ':' + toCompactString(member.typeAnnotation);
      }
      if (member.assignOp && member.initializer) {
        fieldStr += member.assignOp.value + ' ' + toCompactString(member.initializer);
      }
      return fieldStr;

    case 'MethodDeclaration':
      // Format: name(params)[:type]=body
      let methodStr = member.name.value;

      // Add pre-specifiers
      if (member.preSpecifiers) {
        for (const spec of member.preSpecifiers) {
          methodStr = member.name.value + '<' + spec.name.value + '>';
        }
      }

      methodStr += '(';
      if (member.params.length > 0) {
        methodStr += member.params.map(p => {
          let paramStr = p.name.value;
          if (p.type) {
            paramStr += ':' + p.type.value;
          }
          return paramStr;
        }).join(', ');
      }
      methodStr += ')';

      // Add post-specifiers
      if (member.postSpecifiers) {
        for (const spec of member.postSpecifiers) {
          methodStr += '<' + spec.name.value + '>';
        }
      }

      // Add return type
      if (member.returnType) {
        methodStr += ':' + toCompactString(member.returnType);
      }

      // Add body
      methodStr += member.assignOp.value + ' ' + toCompactString(member.body);
      return methodStr;

    default:
      return '';
  }
};

const toCompactStringMember = (member: ClassMember | EnumMember): string => {
  switch (member.type) {
    case 'EnumMember':
      return toCompactStringEnumMember(member);
    case 'FieldDeclaration':
    case 'MethodDeclaration':
      return toCompactStringClassMember(member);
    default:
      return '';
  }
};

export const toCompactString = (expr: Expr): string => {
  switch (expr.type) {
    case 'IntegerLiteral':
      return expr.token.value.toString();
    case 'BooleanLiteral':
      return expr.token.value.toString();
    case 'FloatLiteral':
      return expr.token.value.toString();
    case 'StringLiteral':
      // For compact representation, just show the string content with quotes
      return `"${expr.token.value}"`;
    case 'Variable':
      return expr.token.value;
    case 'BinaryOp':
      return `${toCompactString(expr.left)} ${expr.operator.value} ${toCompactString(expr.right)}`;
    case 'UnaryOp':
      return `${expr.operator.value} ${toCompactString(expr.operand)}`;
    case 'Parenthesized':
      return `(${toCompactString(expr.expr)})`;
    case 'FunctionCall': {
      const isSquareBracket = expr.leftParen.value === '[';
      const leftBracket = isSquareBracket ? '[' : '(';
      const rightBracket = isSquareBracket ? ']' : ')';
      return `${expr.name.value}${leftBracket}${expr.args.map(toCompactString).join(', ')}${rightBracket}`;
    }
    case 'Application': {
      const isSquareBracket = expr.leftParen.value === '[';
      const leftBracket = isSquareBracket ? '[' : '(';
      const rightBracket = isSquareBracket ? ']' : ')';
      return `${toCompactString(expr.func)}${leftBracket}${expr.args.map(toCompactString).join(', ')}${rightBracket}`;
    }
    case 'MemberAccess': {
      return `${toCompactString(expr.object)}.${expr.member.value}`;
    }
    case 'IndexAccess': {
      return `${toCompactString(expr.object)}[${toCompactString(expr.index)}]`;
    }
    case 'LambdaExpression': {
      return `${expr.parameter.value} => ${toCompactString(expr.body)}`;
    }
    case 'MethodCall': {
      const isSquareBracket = expr.leftParen.value === '[';
      const leftBracket = isSquareBracket ? '[' : '(';
      const rightBracket = isSquareBracket ? ']' : ')';
      return `${toCompactString(expr.object)}.${expr.method.value}${leftBracket}${expr.args.map(toCompactString).join(', ')}${rightBracket}`;
    }
    case 'ObjectConstruction': {
      const fields = expr.fields.map(f => `${f.name.value} := ${toCompactString(f.value)}`).join(', ');
      if (expr.style === 'braces') {
        return `${toCompactString(expr.typeExpr)}{${fields}}`;
      } else {
        // For indentation style, we still show compact with braces but note the style
        return `${toCompactString(expr.typeExpr)}:{${fields}}`;
      }
    }
    case 'ForExpression': {
      const index = expr.iterator.index && expr.iterator.arrow ? `${expr.iterator.index.value} -> ` : '';
      const body = toCompactString(expr.body);
      if (expr.style === 'braces') {
        return `for(${index}${expr.iterator.variable.value} : ${toCompactString(expr.iterable)}){${body}}`;
      } else {
        return `for(${index}${expr.iterator.variable.value} : ${toCompactString(expr.iterable)}):{${body}}`;
      }
    }
    case 'IfExpression': {
      const condition = toCompactString(expr.condition);
      const thenBody = toCompactString(expr.thenBody);
      let result = '';

      if (expr.style === 'braces') {
        result = `if(${condition})then{${thenBody}}`;
      } else {
        result = `if(${condition})then:{${thenBody}}`;
      }

      if (expr.elseClause) {
        if (expr.elseClause.elseIf) {
          // else if case
          result += `else ${toCompactString(expr.elseClause.elseIf)}`;
        } else if (expr.elseClause.elseBody) {
          // final else case
          const elseBody = toCompactString(expr.elseClause.elseBody);
          if (expr.elseClause.style === 'braces') {
            result += `else{${elseBody}}`;
          } else {
            result += `else:{${elseBody}}`;
          }
        }
      }

      return result;
    }
    case 'ArrayConstruction': {
      const elements = expr.elements.map(toCompactString).join(', ');
      if (expr.style === 'braces') {
        return `array{${elements}}`;
      } else {
        // For indentation style
        return `array:{${elements}}`;
      }
    }
    case 'Block': {
      const stmts = expr.statements.map(s => s.expr ? toCompactString(s.expr) : '').join('; ');
      if (expr.style === 'braces') {
        return `{${stmts}}`;
      } else {
        // For indentation style blocks
        return `block:{${stmts}}`;
      }
    }
    case 'Specifier': {
      const arg = expr.argument ? `(${expr.argument.value})` : '';
      return `<${expr.name.value}${arg}>`;
    }
    case 'VariableDeclaration': {
      const spec = expr.specifier ? ` ${toCompactString(expr.specifier)}` : '';
      return `var ${expr.name.value}${spec} : ${expr.typeName.value} = ${toCompactString(expr.value)}`;
    }
    case 'ConstDeclaration': {
      if (expr.typeName) {
        return `${expr.name.value} : ${expr.typeName.value} = ${toCompactString(expr.value)}`;
      } else {
        return `${expr.name.value} := ${toCompactString(expr.value)}`;
      }
    }
    case 'EmptyExpression': {
      return '';
    }
    case 'BreakExpression': {
      return 'break';
    }
    case 'ContinueExpression': {
      return 'continue';
    }
    case 'SetStatement': {
      return `set ${expr.variable.value} = ${toCompactString(expr.value)}`;
    }
    case 'LoopStatement': {
      return `loop { ${toCompactString(expr.body)} }`;
    }
    case 'FunctionDeclaration': {
      let result = expr.name.value;
      // Add specifiers
      for (const spec of expr.specifiers) {
        result += ' ' + toCompactString(spec);
      }
      // Add params
      const paramNames = expr.params.map(p => p.name.value).join(', ');
      result += `(${paramNames})`;
      // Add return type if present
      if (expr.returnType) {
        result += ` : ${expr.returnType.value}`;
      }
      // Add body
      result += ` ${expr.assignOp.value} ${toCompactString(expr.body)}`;
      return result;
    }
    case 'CaseExpression': {
      const branches = expr.branches.map(branch => {
        const pattern = (branch.pattern as any).type === 'Token' ? (branch.pattern as any).value : toCompactString(branch.pattern as any);
        const body = branch.body ? ` ${toCompactString(branch.body)}` : '';
        return `${pattern} =>${body}`;
      }).join(', ');
      if (expr.style === 'braces') {
        return `case(${toCompactString(expr.expr)}) {${branches}}`;
      } else {
        return `case(${toCompactString(expr.expr)}): ${branches}`;
      }
    }
    case 'RangeExpression': {
      return `${toCompactString(expr.start)}..${toCompactString(expr.end)}`;
    }
    case 'Assignment': {
      return `${toCompactString(expr.target)} ${expr.assignOp.text} ${toCompactString(expr.value)}`;
    }
    case 'ClassExpression': {
      let result = 'class';
      if (expr.specifiers && expr.specifiers.length > 0) {
        result += expr.specifiers.map(spec => spec.text).join('');
      }
      if (expr.parentClass) {
        result += `(${expr.parentClass.text})`;
      }
      result += ' { ';
      result += expr.body.map(m => toCompactStringClassMember(m)).join('; ');
      result += ' }';
      return result;
    }
    case 'EmptyExpression': {
      return '';
    }
    default: {
      // This should never be reached if the switch is exhaustive
      const _exhaustive: never = expr;
      throw new Error(`Unhandled expression type in toCompactString: ${(expr as any).type}`);
    }
  }
};

// Using statement for imports
export interface UsingStatement {
  type: 'UsingStatement';
  usingKeyword: Token<'using'>;
  leftBrace: Token<'{'>;
  path: Token<string>;
  rightBrace: Token<'}'>;
  span: Span;
}

// Top-level declaration kinds
export type DeclarationKind = 'module' | 'class' | 'interface' | 'enum' | 'struct' | 'function';

// Decorator interface
export interface Decorator {
  type: 'Decorator';
  at: Token<'@'>;
  name: Token<string>;
  leftParen?: Token<'('>;
  args?: Expr[];
  commas?: Token<','>[];
  rightParen?: Token<')'>;
  span: Span;
}

// Class member types
export type ClassMember = FieldDeclaration | MethodDeclaration;

// Enum member (simple identifier value)
export interface EnumMember {
  type: 'EnumMember';
  name: Token<string>;
  span: Span;
}

// Field declaration in a class
export interface FieldDeclaration {
  type: 'FieldDeclaration';
  decorators?: Decorator[];
  varKeyword?: Token<'var'>;
  name: Token<string>;
  colon?: Token<':'>;  // Optional for := syntax
  typeAnnotation?: Expr;  // Type can be complex expression like event(player)
  assignOp?: Token<'=' | ':='>;  // Can be = or :=
  initializer?: Expr;
  semicolon?: Token<';'>;  // Optional semicolon after field
  span: Span;
}

// Method declaration in a class
export interface MethodDeclaration {
  type: 'MethodDeclaration';
  decorators?: Decorator[];
  name: Token<string>;
  preSpecifiers?: Specifier[];    // Specifiers before parentheses
  leftParen: Token<'('>;
  params: FunctionParam[];
  rightParen: Token<')'>;
  postSpecifiers?: Specifier[];   // Specifiers after parentheses
  colon?: Token<':'>;
  returnType?: Expr;
  assignOp: Token<'=' | ':='>;
  body: Expr | Block;             // Method body can be expression or block
  semicolon?: Token<';'>;         // Optional semicolon after method
  span: Span;
}

// Top-level declaration (module, class, interface, enum, struct)
export interface TopLevelDeclaration {
  type: 'TopLevelDeclaration';
  decorators?: Decorator[];    // Optional decorators
  kind: DeclarationKind;
  name: Token<string>;
  assignOp: Token<':='>;
  keyword: Token<DeclarationKind>;
  leftParenInherit?: Token<'('>;  // For class inheritance
  parentClass?: Token<string>;     // Parent class name
  rightParenInherit?: Token<')'>;  // For class inheritance
  style: 'braces' | 'indentation' | 'parentheses';
  leftBrace?: Token<'{'>;     // Only for brace style
  colon?: Token<':'>;          // Only for indentation style
  body: (ClassMember | EnumMember)[];  // Supports both class members and enum members
  rightBrace?: Token<'}'>;     // Only for brace style
  span: Span;
}

// Program represents the entire file
export interface Program {
  type: 'Program';
  leadingTrivia: string;
  usingStatements: UsingStatement[];
  declarations: (TopLevelDeclaration | FunctionDeclaration | ConstDeclaration | Expr)[];
  trailingTrivia: string;
  span: Span;
}

// Helper functions for new AST nodes
export const usingStatement = (
  usingKeyword: Token<'using'>,
  leftBrace: Token<'{'>,
  path: Token<string>,
  rightBrace: Token<'}'>,
  span: Span
): UsingStatement => ({
  type: 'UsingStatement',
  usingKeyword,
  leftBrace,
  path,
  rightBrace,
  span
});

export const topLevelDeclaration = (
  kind: DeclarationKind,
  name: Token<string>,
  assignOp: Token<':='> | undefined,
  keyword: Token<string>,
  colon: Token<':'> | undefined,
  leftParenInherit: Token<'('> | undefined,
  parentClass: Token<string> | undefined,
  rightParenInherit: Token<')'> | undefined,
  leftBrace: Token<'{'> | undefined,
  rightBrace: Token<'}'> | undefined,
  body: (ClassMember | EnumMember)[],
  span: Span,
  decorators?: Decorator[]
): TopLevelDeclaration => {
  let style: 'braces' | 'indentation' | 'parentheses' = 'braces';
  if (colon) style = 'indentation';
  else if (leftParenInherit && !leftBrace) style = 'parentheses';

  return {
    type: 'TopLevelDeclaration',
    decorators: decorators && decorators.length > 0 ? decorators : undefined,
    kind,
    name,
    assignOp: assignOp as Token<':='>,
    keyword: keyword as Token<DeclarationKind>,
    leftParenInherit,
    parentClass,
    rightParenInherit,
    style,
    body,
    span,
    leftBrace,
    colon,
    rightBrace
  };
};

export const program = (
  usingStatements: UsingStatement[],
  declarations: (TopLevelDeclaration | FunctionDeclaration | ConstDeclaration | Expr)[],
  span: Span,
  leadingTrivia: string = '',
  trailingTrivia: string = ''
): Program => ({
  type: 'Program',
  leadingTrivia,
  usingStatements,
  declarations,
  trailingTrivia,
  span
});

// Pretty print implementations for top-level constructs
const prettyPrintProgram = (program: Program): string => {
  let result = '';

  // Print leading trivia (comments and whitespace at start of file)
  result += program.leadingTrivia;

  // Print using statements
  for (const usingStmt of program.usingStatements) {
    result += prettyPrintUsingStatement(usingStmt);
  }

  // Print declarations
  for (const decl of program.declarations) {
    if (decl.type === 'TopLevelDeclaration') {
      result += prettyPrintTopLevelDeclaration(decl);
    } else if (decl.type === 'FunctionDeclaration') {
      result += prettyPrintFunctionDeclaration(decl);
    } else if (decl.type === 'ConstDeclaration') {
      result += prettyPrintExpression(decl);
    } else {
      // Handle any other expression types (e.g., ForExpression, IfExpression, etc.)
      result += prettyPrintExpression(decl as Expr);
    }
  }

  // Print trailing trivia (comments and whitespace at end of file)
  result += program.trailingTrivia;

  return result;
};

const prettyPrintUsingStatement = (usingStmt: UsingStatement): string => {
  let result = printToken(usingStmt.usingKeyword);
  result += printToken(usingStmt.leftBrace);
  result += printToken(usingStmt.path);
  result += printToken(usingStmt.rightBrace);
  return result;
};

const prettyPrintFunctionDeclaration = (expr: FunctionDeclaration): string => {
  let result = printToken(expr.name);
  // Add pre-parenthesis specifiers
  for (const spec of expr.specifiers) {
    result += prettyPrint(spec);
  }
  // Add params
  result += `${expr.leftParen.trivia.leading}${expr.leftParen.text}${expr.leftParen.trivia.trailing}`;
  for (let i = 0; i < expr.params.length; i++) {
    const param = expr.params[i];
    result += `${param.name.trivia.leading}${param.name.text}${param.name.trivia.trailing}`;
    if (param.comma) {
      result += `${param.comma.trivia.leading}${param.comma.text}${param.comma.trivia.trailing}`;
    }
  }
  result += `${expr.rightParen.trivia.leading}${expr.rightParen.text}${expr.rightParen.trivia.trailing}`;
  // Add post-parenthesis specifiers
  if (expr.postParenSpecifiers) {
    for (const spec of expr.postParenSpecifiers) {
      result += prettyPrint(spec);
    }
  }
  // Add return type if present
  if (expr.colon && expr.returnType) {
    result += `${expr.colon.trivia.leading}${expr.colon.text}${expr.colon.trivia.trailing}`;
    result += `${expr.returnType.trivia.leading}${expr.returnType.text}${expr.returnType.trivia.trailing}`;
  }
  // Add assignment operator
  result += `${expr.assignOp.trivia.leading}${expr.assignOp.text}${expr.assignOp.trivia.trailing}`;
  // Add body
  result += prettyPrint(expr.body);
  return result;
};

const prettyPrintDecorator = (dec: Decorator, isIndentationStyle: boolean = false): string => {
  let result = `${dec.at.trivia.leading}${dec.at.text}${dec.at.trivia.trailing}`;

  // Include the full trailing trivia - the indentation after newlines is part of the source formatting
  result += `${dec.name.trivia.leading}${dec.name.text}${dec.name.trivia.trailing}`;

  if (dec.leftParen && dec.rightParen) {
    result += `${dec.leftParen.trivia.leading}${dec.leftParen.text}${dec.leftParen.trivia.trailing}`;
    if (dec.args) {
      for (let i = 0; i < dec.args.length; i++) {
        result += prettyPrintExpression(dec.args[i]);
        if (dec.commas && i < dec.commas.length) {
          result += `${dec.commas[i].trivia.leading}${dec.commas[i].text}${dec.commas[i].trivia.trailing}`;
        }
      }
    }
    result += `${dec.rightParen.trivia.leading}${dec.rightParen.text}${dec.rightParen.trivia.trailing}`;
  }

  return result;
};

const prettyPrintTopLevelDeclaration = (decl: TopLevelDeclaration): string => {
  let result = '';

  // Print decorators first
  if (decl.decorators) {
    for (const dec of decl.decorators) {
      result += prettyPrintDecorator(dec);
    }
  }

  // Check if this is the new syntax (interface Name { }) vs old syntax (Name := interface { })
  // The new syntax has a zero-width assignOp token as a placeholder
  const isNewSyntax = (decl.kind === 'class' || decl.kind === 'interface' || decl.kind === 'struct' || decl.kind === 'enum') &&
                      decl.assignOp.span.start === decl.assignOp.span.end;

  if (isNewSyntax) {
    // New syntax: interface Name { ... }
    result += printToken(decl.keyword);
    // Add space between keyword and name if not already in keyword's trailing trivia
    if (!decl.keyword.trivia.trailing) {
      result += ` `;
    }
    result += printToken(decl.name);
  } else {
    // Old syntax: Name := class { ... }
    result += printToken(decl.name);
    result += printToken(decl.assignOp);
    result += printToken(decl.keyword);
  }

  // Print inheritance parentheses if present
  if (decl.leftParenInherit && decl.rightParenInherit) {
    result += printToken(decl.leftParenInherit);
    if (decl.parentClass) {
      result += printToken(decl.parentClass);
    }
    result += printToken(decl.rightParenInherit);
  }

  if (decl.style === 'braces') {
    if (decl.leftBrace) {
      result += printToken(decl.leftBrace);
    }
    for (const member of decl.body) {
      result += prettyPrintMember(member, false);
    }
    if (decl.rightBrace) {
      result += printToken(decl.rightBrace);
    }
  } else {
    // indentation style
    if (decl.colon) {
      result += printToken(decl.colon);
    }
    for (const member of decl.body) {
      result += prettyPrintMember(member, true);
    }
  }

  return result;
};