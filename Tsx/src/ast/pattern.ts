import { L } from './location';
import { Exp } from './expression';
import { IdentExp } from './identifier';

export type Pat =
  | { kind: 'Name'; ident: IdentExp }
  | { kind: 'Var'; beforeSpecs: L<Exp>[]; ident: L<IdentExp>; afterSpecs: L<Exp>[] }
  | { kind: 'PrefixColon'; expr: L<Exp> }
  | { kind: 'InfixColon'; pattern: L<Pat>; expr: L<Exp> }
  | { kind: 'InfixArrow'; left: L<Pat>; right: L<Pat> }
  | { kind: 'Invoke'; pattern: L<Pat>; arg: L<Exp> }
  | { kind: 'Specs'; pattern: L<Pat>; specs: L<Exp>[] }
  | { kind: 'Extension'; expr: L<Exp>; pattern: L<Pat> };

// Helper functions for creating patterns
export function createNamePattern(ident: IdentExp): Pat {
  return { kind: 'Name', ident };
}

export function createVarPattern(
  beforeSpecs: L<Exp>[],
  ident: L<IdentExp>,
  afterSpecs: L<Exp>[]
): Pat {
  return { kind: 'Var', beforeSpecs, ident, afterSpecs };
}

export function createPrefixColonPattern(expr: L<Exp>): Pat {
  return { kind: 'PrefixColon', expr };
}

export function createInfixColonPattern(pattern: L<Pat>, expr: L<Exp>): Pat {
  return { kind: 'InfixColon', pattern, expr };
}

export function createInfixArrowPattern(left: L<Pat>, right: L<Pat>): Pat {
  return { kind: 'InfixArrow', left, right };
}

export function createInvokePattern(pattern: L<Pat>, arg: L<Exp>): Pat {
  return { kind: 'Invoke', pattern, arg };
}

export function createSpecsPattern(pattern: L<Pat>, specs: L<Exp>[]): Pat {
  return { kind: 'Specs', pattern, specs };
}

export function createExtensionPattern(expr: L<Exp>, pattern: L<Pat>): Pat {
  return { kind: 'Extension', expr, pattern };
}

// Function to convert expression to pattern (if possible)
export function expToPat(expr: L<Exp>): L<Pat> | null {
  const value = expr.value;

  switch (value.kind) {
    case 'Pat':
      return { loc: expr.loc, value: value.pattern };

    case 'Paren':
      const innerPat = expToPat(value.expr);
      return innerPat ? { loc: expr.loc, value: innerPat.value } : null;

    case 'List':
      if (value.elements.length === 1) {
        const innerPat = expToPat(value.elements[0]);
        return innerPat ? { loc: expr.loc, value: innerPat.value } : null;
      }
      return null;

    case 'ParenInvoke':
      const funcPat = expToPat(value.func);
      if (funcPat) {
        return {
          loc: expr.loc,
          value: createInvokePattern(funcPat, value.arg)
        };
      }
      return null;

    case 'ExpInfixColon':
      const leftPat = expToPat(value.left);
      if (leftPat) {
        return {
          loc: expr.loc,
          value: createInfixColonPattern(leftPat, value.right)
        };
      }
      return null;

    case 'ExpVar':
      const varInner = expToPat(value.expr);
      if (varInner && varInner.value.kind === 'Name') {
        const identExp = varInner.value.ident;
        return {
          loc: expr.loc,
          value: createVarPattern([], { loc: varInner.loc, value: identExp }, [])
        };
      }
      if (varInner && varInner.value.kind === 'Specs') {
        const specsPat = varInner.value;
        if (specsPat.pattern.value.kind === 'Name') {
          const identExp = specsPat.pattern.value.ident;
          return {
            loc: expr.loc,
            value: createVarPattern([], { loc: specsPat.pattern.loc, value: identExp }, specsPat.specs)
          };
        }
      }
      return null;

    case 'ExpSpecs':
      const basePat = expToPat(value.expr);
      if (basePat) {
        return {
          loc: expr.loc,
          value: createSpecsPattern(basePat, value.specs)
        };
      }
      return null;

    case 'Arrow':
      const leftArrowPat = expToPat(value.left);
      const rightArrowPat = expToPat(value.right);
      if (leftArrowPat && rightArrowPat) {
        return {
          loc: expr.loc,
          value: createInfixArrowPattern(leftArrowPat, rightArrowPat)
        };
      }
      return null;

    default:
      return null;
  }
}