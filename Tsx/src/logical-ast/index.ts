/**
 * Logical AST Module
 *
 * Exports for the simplified logical AST system
 */

export * from './types';
export { simplify, simplifyProgram } from './simplifier';
export { printLogicalAST } from './printer';