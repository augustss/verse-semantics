// Trivia system for preserving whitespace and comments in the AST
// This enables lossless parsing and accurate source reconstruction

import { Position } from './location';

export type TriviaKind =
  | 'whitespace'
  | 'newline'
  | 'line_comment'
  | 'block_comment';

export interface Trivia {
  kind: TriviaKind;
  text: string;
  start: Position;
  end: Position;
}

export interface TriviaList {
  trivia: Trivia[];
}

// Helper functions for creating trivia
export function createWhitespace(text: string, start: Position, end: Position): Trivia {
  return {
    kind: 'whitespace',
    text,
    start,
    end
  };
}

export function createNewline(text: string, start: Position, end: Position): Trivia {
  return {
    kind: 'newline',
    text,
    start,
    end
  };
}

export function createLineComment(text: string, start: Position, end: Position): Trivia {
  return {
    kind: 'line_comment',
    text,
    start,
    end
  };
}

export function createBlockComment(text: string, start: Position, end: Position): Trivia {
  return {
    kind: 'block_comment',
    text,
    start,
    end
  };
}

export function createTriviaList(trivia: Trivia[] = []): TriviaList {
  return { trivia };
}

// Utility functions for working with trivia
export function isEmpty(triviaList: TriviaList): boolean {
  return triviaList.trivia.length === 0;
}

export function combine(first: TriviaList, second: TriviaList): TriviaList {
  return {
    trivia: [...first.trivia, ...second.trivia]
  };
}

export function toString(triviaList: TriviaList): string {
  return triviaList.trivia.map(t => t.text).join('');
}