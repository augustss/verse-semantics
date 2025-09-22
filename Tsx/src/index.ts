import { parse, parseTopLevel, parseProgram } from './parser';
import { prettyPrint, toCompactString } from './ast';
import * as AST from './ast';

export { parse, prettyPrint, toCompactString, parseTopLevel, parseProgram };
export type {
  Expr, Span, Trivia, Token, IntegerLiteral, Variable, BinaryOp, Parenthesized,
  FunctionCall, Application, MemberAccess, IndexAccess, LambdaExpression, ObjectConstruction, FieldAssignment, Statement, Block, Specifier,
  VariableDeclaration, ConstDeclaration, FunctionDeclaration, FunctionParam,
  UsingStatement, TopLevelDeclaration, DeclarationKind, Program
} from './ast';
export * from './parser-combinators';