// Error reporting utilities for AST nodes
// Provides line numbers, column positions, and contextual error information

import { L, Loc, Pos } from './ast/location';
import { Exp } from './ast/expression';
import { Pat } from './ast/pattern';
import { IdentExp } from './ast/identifier';

// Type for any located AST node
export type AnyLocatedNode = L<Exp> | L<Pat> | L<IdentExp> | L<any>;

// Enhanced error information
export interface ErrorInfo {
  line: number;
  column: number;
  offset: number;
  endLine: number;
  endColumn: number;
  endOffset: number;
  nodeKind: string;
  hasLocation: boolean;
}

// Basic error context for reporting
export interface ErrorContext {
  sourceText?: string;
  filename?: string;
  contextLines?: number; // Number of lines before/after to show
}

/**
 * Extract line number from any AST node
 * @param node - Any located AST node (L<T>)
 * @returns Line number (1-based) or null if no location info
 */
export function getLineNumber(node: any): number | null {
  if (!node) return null;

  // Check if it's a located node with 'loc' property
  if (node.loc && node.loc.start && typeof node.loc.start.line === 'number') {
    return node.loc.start.line;
  }

  // Check if it's a raw location object
  if (node.start && typeof node.start.line === 'number') {
    return node.start.line;
  }

  // Check if it's just a position object
  if (typeof node.line === 'number') {
    return node.line;
  }

  return null;
}

/**
 * Extract column number from any AST node
 * @param node - Any located AST node (L<T>)
 * @returns Column number (1-based) or null if no location info
 */
export function getColumnNumber(node: any): number | null {
  if (!node) return null;

  if (node.loc && node.loc.start && typeof node.loc.start.column === 'number') {
    return node.loc.start.column;
  }

  if (node.start && typeof node.start.column === 'number') {
    return node.start.column;
  }

  if (typeof node.column === 'number') {
    return node.column;
  }

  return null;
}

/**
 * Extract comprehensive error information from an AST node
 * @param node - Any located AST node
 * @returns Complete error information including position and node type
 */
export function getErrorInfo(node: any): ErrorInfo {
  const line = getLineNumber(node) || 0;
  const column = getColumnNumber(node) || 0;

  let endLine = line;
  let endColumn = column;
  let offset = 0;
  let endOffset = 0;
  let nodeKind = 'unknown';
  let hasLocation = false;

  if (node) {
    // Extract location information
    if (node.loc) {
      hasLocation = true;
      if (node.loc.start) {
        offset = node.loc.start.offset || 0;
      }
      if (node.loc.end) {
        endLine = node.loc.end.line || line;
        endColumn = node.loc.end.column || column;
        endOffset = node.loc.end.offset || offset;
      }
    }

    // Extract node kind/type information
    if (node.value && node.value.kind) {
      nodeKind = node.value.kind;
    } else if (node.kind) {
      nodeKind = node.kind;
    } else if (node.value) {
      nodeKind = typeof node.value === 'object' ?
        (node.value.constructor?.name || 'object') : typeof node.value;
    }
  }

  return {
    line,
    column,
    offset,
    endLine,
    endColumn,
    endOffset,
    nodeKind,
    hasLocation
  };
}

/**
 * Print basic line information for an AST node
 * @param node - Any located AST node
 * @returns Human-readable line information string
 */
export function printNodeLine(node: any): string {
  const line = getLineNumber(node);
  const column = getColumnNumber(node);

  if (line === null) {
    return 'No location information available';
  }

  if (column === null) {
    return `Line ${line}`;
  }

  return `Line ${line}, Column ${column}`;
}

/**
 * Generate a comprehensive error report for an AST node
 * @param node - Any located AST node
 * @param message - Error message
 * @param context - Optional context for enhanced error reporting
 * @returns Formatted error report
 */
export function generateErrorReport(
  node: any,
  message: string,
  context?: ErrorContext
): string {
  const info = getErrorInfo(node);
  const location = info.hasLocation ?
    `${info.line}:${info.column}` : 'unknown';

  let report = `Error at ${location}: ${message}\n`;

  if (info.hasLocation) {
    report += `  Node: ${info.nodeKind}\n`;
    report += `  Position: Line ${info.line}, Column ${info.column}`;

    if (info.endLine !== info.line || info.endColumn !== info.column) {
      report += ` to Line ${info.endLine}, Column ${info.endColumn}`;
    }
    report += '\n';

    if (info.offset > 0) {
      report += `  Offset: ${info.offset}`;
      if (info.endOffset !== info.offset) {
        report += ` to ${info.endOffset}`;
      }
      report += '\n';
    }
  }

  // Add source context if available
  if (context?.sourceText && info.hasLocation) {
    const contextLines = context.contextLines || 2;
    const sourceContext = extractSourceContext(
      context.sourceText,
      info.line,
      contextLines
    );
    if (sourceContext) {
      report += '\n' + sourceContext;
    }
  }

  if (context?.filename) {
    report += `\nFile: ${context.filename}`;
  }

  return report;
}

/**
 * Extract source code context around an error line
 * @param sourceText - The complete source text
 * @param errorLine - Line number where error occurred (1-based)
 * @param contextLines - Number of lines to show before and after
 * @returns Formatted source context with line numbers
 */
export function extractSourceContext(
  sourceText: string,
  errorLine: number,
  contextLines: number = 2
): string {
  const lines = sourceText.split('\n');
  const startLine = Math.max(0, errorLine - contextLines - 1);
  const endLine = Math.min(lines.length - 1, errorLine + contextLines - 1);

  let context = '';
  for (let i = startLine; i <= endLine; i++) {
    const lineNum = i + 1;
    const isErrorLine = lineNum === errorLine;
    const prefix = isErrorLine ? '>>> ' : '    ';
    const paddedLineNum = lineNum.toString().padStart(3, ' ');

    context += `${prefix}${paddedLineNum} | ${lines[i] || ''}\n`;
  }

  return context.trim();
}

/**
 * Quick utility to just print the line number of a node
 * @param node - Any located AST node
 * @returns Line number as string or "unknown"
 */
export function nodeLineNumber(node: any): string {
  const line = getLineNumber(node);
  return line !== null ? line.toString() : 'unknown';
}

/**
 * Create a simple error message with location
 * @param node - Any located AST node
 * @param message - Error message
 * @returns Simple formatted error string
 */
export function simpleError(node: any, message: string): string {
  const location = printNodeLine(node);
  return `${message} (${location})`;
}

/**
 * Check if a node has valid location information
 * @param node - Any potential AST node
 * @returns True if the node has location information
 */
export function hasLocationInfo(node: any): boolean {
  return getLineNumber(node) !== null;
}

/**
 * Compare two nodes by their position in source code
 * @param nodeA - First AST node
 * @param nodeB - Second AST node
 * @returns -1 if A comes before B, 1 if A comes after B, 0 if same position
 */
export function compareNodePositions(nodeA: any, nodeB: any): number {
  const lineA = getLineNumber(nodeA);
  const lineB = getLineNumber(nodeB);

  if (lineA === null || lineB === null) return 0;

  if (lineA !== lineB) {
    return lineA < lineB ? -1 : 1;
  }

  const colA = getColumnNumber(nodeA) || 0;
  const colB = getColumnNumber(nodeB) || 0;

  if (colA !== colB) {
    return colA < colB ? -1 : 1;
  }

  return 0;
}

// Export type guards for convenience
export function isLocatedNode(node: any): node is L<any> {
  return node && typeof node === 'object' && 'loc' in node && 'value' in node;
}

export function isPosition(pos: any): pos is Pos {
  return pos && typeof pos.line === 'number' && typeof pos.column === 'number';
}

export function isLocation(loc: any): loc is Loc {
  return loc && isPosition(loc.start) && isPosition(loc.end);
}