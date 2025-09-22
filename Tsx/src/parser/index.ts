/**
 * Modular Expression Parser for Verse Language
 *
 * This is a clean, modularized version of the expression parser that splits
 * the monolithic expression-parser.ts into logical, maintainable modules.
 */

// Re-export foundation utilities
export * from './foundation/helpers';
export * from './foundation/trivia';
export * from './foundation/tokens';

// Re-export literal parsers
export * from './literals/numbers';
export * from './literals/booleans';
export * from './literals/strings';
export * from './literals/identifiers';

// Re-export operator parsers
export * from './operators/arithmetic';
export * from './operators/comparison';
export * from './operators/logical';
export * from './operators/punctuation';

// Re-export expression parsers
export * from './expressions/core';

// Re-export top-level parsers
export * from './top-level/core';

// Export parser combinator types for convenience
export type { Parser, ParserState, ParserResult } from '../parser-combinators';