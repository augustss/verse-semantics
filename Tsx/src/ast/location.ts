export interface Pos {
  line: number;
  column: number;
  offset: number;
}

export interface Loc {
  start: Pos;
  end: Pos;
}

export interface L<T> {
  loc: Loc;
  value: T;
  leadingTrivia?: import('./trivia').TriviaList;
  trailingTrivia?: import('./trivia').TriviaList;
}

// For backwards compatibility, we'll alias Position to Pos
export type Position = Pos;

export function createPos(line: number, column: number, offset: number): Pos {
  return { line, column, offset };
}

export function createLoc(start: Pos, end: Pos): Loc {
  return { start, end };
}

export function withLoc<T>(loc: Loc, value: T): L<T>;
export function withLoc<T>(
  loc: Loc,
  value: T,
  leadingTrivia: import('./trivia').TriviaList,
  trailingTrivia: import('./trivia').TriviaList
): L<T>;
export function withLoc<T>(
  loc: Loc,
  value: T,
  leadingTrivia?: import('./trivia').TriviaList,
  trailingTrivia?: import('./trivia').TriviaList
): L<T> {
  if (leadingTrivia !== undefined || trailingTrivia !== undefined) {
    return { loc, value, leadingTrivia, trailingTrivia };
  }
  return { loc, value };
}

export function extractValue<T>(l: L<T>): T {
  return l.value;
}

export function extractLoc<T>(l: L<T>): Loc {
  return l.loc;
}

export function minPos(): Pos {
  return { line: 1, column: 1, offset: 0 };
}

export function minLoc(): Loc {
  return { start: minPos(), end: minPos() };
}