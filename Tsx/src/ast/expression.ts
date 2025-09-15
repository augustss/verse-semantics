import { L } from './location';
import { Pat } from './pattern';
import { IdentExp } from './identifier';

export type SimpleName = string;

export type Specifier =
  | 'decides'
  | 'succeeds'
  | 'fails'
  | 'transacts'
  | 'computes'
  | 'ambiguates'
  | 'reads'
  | 'writes'
  | 'allocates'
  | 'suspends'
  | 'closed';

// Function parameter type
export interface FuncParam {
  name?: SimpleName;
  pattern?: L<Pat>;
  type?: L<Exp>;
  defaultValue?: L<Exp>;
}

// Function declaration type
export type FuncDecl = {
  name: SimpleName;
  params: FuncParam[];
  returnType?: L<Exp>;
  specifiers: Specifier[];
  body: L<Exp>;
  isDefinition: boolean; // true for :=, false for =
};

export type Exp =
  // Function declarations
  | { kind: 'FuncDecl'; decl: FuncDecl }

  // Binary operators
  | { kind: 'Assign'; left: L<Exp>; right: L<Exp> }
  | { kind: 'NotEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Or'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Dot'; left: L<Exp>; right: L<IdentExp> }
  | { kind: 'Range'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Less'; left: L<Exp>; right: L<Exp> }
  | { kind: 'LessEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Greater'; left: L<Exp>; right: L<Exp> }
  | { kind: 'GreaterEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Choice'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Multiply'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Add'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Subtract'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Arrow'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Divide'; left: L<Exp>; right: L<Exp> }

  // Unary operators
  | { kind: 'All'; expr: L<Exp> }
  | { kind: 'And'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Array'; elements: L<Exp>[] }
  | { kind: 'Block'; expr: L<Exp> }
  | { kind: 'Brace'; expr: L<Exp> }
  | { kind: 'BracketInvoke'; func: L<Exp>; arg: L<Exp> }
  | { kind: 'Break' }
  | { kind: 'Continue' }
  | { kind: 'Catch'; try: L<Exp>; handler: L<Exp> }
  | { kind: 'Char'; value: string }
  | { kind: 'Char32'; value: string }
  | { kind: 'Class'; parent: L<Exp> | null; body: L<Exp> }
  | { kind: 'Continue' }
  | { kind: 'Do'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'Enum'; specs: L<Exp>[]; names: [L<Exp>[], L<SimpleName>][] }
  | { kind: 'Exists'; name: L<SimpleName> }
  | { kind: 'ExpInfixColon'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Fail' }
  | { kind: 'Fails'; expr: L<Exp> }
  | { kind: 'False' }
  | { kind: 'Float'; value: number }
  | { kind: 'Units'; expr: L<Exp>; unit: L<SimpleName> }
  | { kind: 'For'; expr: L<Exp> }
  | { kind: 'ForDo'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'Forall'; name: L<SimpleName> }
  | { kind: 'Lam'; param: L<Exp>; body: L<Exp> }
  | { kind: 'If'; cond: L<Exp> }
  | { kind: 'IfElse'; cond: L<Exp>; else: L<Exp> }
  | { kind: 'IfThen'; cond: L<Exp>; then: L<Exp> }
  | { kind: 'IfThenElse'; cond: L<Exp>; then: L<Exp>; else: L<Exp> }
  | { kind: 'InfixColonEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'InfixDivideEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'InfixMinusEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'InfixMultiplyEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'InfixPlusEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'Inst'; func: L<Exp>; arg: L<Exp> }
  | { kind: 'Int'; value: bigint }
  | { kind: 'List'; elements: L<Exp>[] }
  | { kind: 'Module'; body: L<Exp> }
  | { kind: 'Not'; expr: L<Exp> }
  | { kind: 'One'; expr: L<Exp> }
  | { kind: 'Option'; expr: L<Exp> }
  | { kind: 'Paren'; expr: L<Exp> }
  | { kind: 'ParenInvoke'; func: L<Exp>; arg: L<Exp> }
  | { kind: 'Pat'; pattern: Pat }
  | { kind: 'PostfixCaret'; expr: L<Exp> }
  | { kind: 'PostfixQuery'; expr: L<Exp> }
  | { kind: 'PostfixIncrement'; expr: L<Exp> }
  | { kind: 'PostfixDecrement'; expr: L<Exp> }
  | { kind: 'PrefixBracket'; specs: L<Exp>[]; expr: L<Exp> }
  | { kind: 'PrefixCaret'; expr: L<Exp> }
  | { kind: 'PrefixMinus'; expr: L<Exp> }
  | { kind: 'PrefixMultiply'; expr: L<Exp> }
  | { kind: 'PrefixPlus'; expr: L<Exp> }
  | { kind: 'PrefixQuery'; expr: L<Exp> }
  | { kind: 'PrefixAmpersand'; expr: L<Exp> }
  | { kind: 'PrefixDotDot'; expr: L<Exp> }
  | { kind: 'Return'; value: L<Exp> | null }
  | { kind: 'ExpVar'; expr: L<Exp> }
  | { kind: 'ExpSet'; expr: L<Exp> }
  | { kind: 'ExpRef'; expr: L<Exp> }
  | { kind: 'ExpAlias'; expr: L<Exp> }
  | { kind: 'Set'; target: L<Exp>; value: L<Exp> }
  | { kind: 'SetInfixDivideEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'SetInfixMinusEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'SetInfixMultiplyEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'SetInfixPlusEqual'; left: L<Exp>; right: L<Exp> }
  | { kind: 'ExpSpecs'; expr: L<Exp>; specs: L<Exp>[] }
  | { kind: 'AtSpec'; spec: L<Exp>; expr: L<Exp> }
  | { kind: 'SpecAt'; expr: L<Exp>; spec: L<Exp> }
  | { kind: 'String'; text: string; interpolations: [L<Exp>, L<string>][] }
  | { kind: 'Struct'; body: L<Exp> }
  | { kind: 'Interface'; body: L<Exp> }
  | { kind: 'Enum'; body: L<Exp> }
  | { kind: 'Class'; body: L<Exp> }
  | { kind: 'True' }
  | { kind: 'Truth'; expr: L<Exp> }
  | { kind: 'Tuple'; elements: L<Exp>[] }
  | { kind: 'Until'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'Yield' }
  | { kind: 'Next'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'Over'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'When'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'While'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'Where'; expr: L<Exp>; decls: L<Exp> }
  | { kind: 'Is'; expr: L<Exp>; body: L<Exp> }
  | { kind: 'Specifier'; spec: Specifier }
  | { kind: 'Comment'; text: string };

// Helper functions for creating expressions
export function createAssign(left: L<Exp>, right: L<Exp>): Exp {
  return { kind: 'Assign', left, right };
}

export function createInt(value: bigint): Exp {
  return { kind: 'Int', value };
}

export function createFloat(value: number): Exp {
  return { kind: 'Float', value };
}

export function createString(text: string, interpolations: [L<Exp>, L<string>][] = []): Exp {
  return { kind: 'String', text, interpolations };
}

export function createList(elements: L<Exp>[]): Exp {
  return { kind: 'List', elements };
}

export function createTrue(): Exp {
  return { kind: 'True' };
}

export function createFalse(): Exp {
  return { kind: 'False' };
}

export function createSpecifier(spec: Specifier): Exp {
  return { kind: 'Specifier', spec };
}

export function createFuncDecl(
  name: SimpleName,
  params: FuncParam[],
  returnType: L<Exp> | undefined,
  specifiers: Specifier[],
  body: L<Exp>,
  isDefinition: boolean
): Exp {
  return {
    kind: 'FuncDecl',
    decl: { name, params, returnType, specifiers, body, isDefinition }
  };
}