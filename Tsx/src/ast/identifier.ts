import { L } from './location';
import { Exp, SimpleName } from './expression';

export type IdentExp =
  | { kind: 'IdentName'; name: SimpleName }
  | { kind: 'IdentQualName'; qualifiers: L<Exp>[]; name: L<SimpleName> }
  | { kind: 'IdentPath'; path: Path };

export interface PathSegment {
  path: Path | null;
  label: L<SimpleName>;
}

export interface Path {
  label: L<SimpleName>;
  segments: PathSegment[];
}

// Helper functions for creating identifiers
export function createIdentName(name: SimpleName): IdentExp {
  return { kind: 'IdentName', name };
}

export function createIdentQualName(qualifiers: L<Exp>[], name: L<SimpleName>): IdentExp {
  return { kind: 'IdentQualName', qualifiers, name };
}

export function createIdentPath(path: Path): IdentExp {
  return { kind: 'IdentPath', path };
}

export function createPath(label: L<SimpleName>, segments: PathSegment[] = []): Path {
  return { label, segments };
}

export function createPathSegment(path: Path | null, label: L<SimpleName>): PathSegment {
  return { path, label };
}