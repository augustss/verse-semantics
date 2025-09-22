/**
 * Core helper functions for parsing
 */

// Helper functions for common character checks
export const isNewline = (char: string): boolean => {
  return char === '\n' || char === '\r';
};

export const isWhitespace = (char: string): boolean => {
  return char === ' ' || char === '\t';
};

export const isAlphaNumeric = (char: string): boolean => {
  return /[a-zA-Z0-9_]/.test(char);
};

export const isAtNewline = (state: { input: string; position: number }): boolean => {
  return state.position < state.input.length && isNewline(state.input[state.position]);
};

export const isAtWhitespace = (state: { input: string; position: number }): boolean => {
  return state.position < state.input.length && isWhitespace(state.input[state.position]);
};

// Helper function to format parse errors with line context and caret
export const formatParseError = (input: string, position: number, error: string): string => {
  const lines = input.split('\n');
  let currentPos = 0;
  let lineNumber = 1;
  let columnNumber = 1;

  // Find which line and column the error occurred on
  for (const line of lines) {
    if (currentPos + line.length >= position) {
      columnNumber = position - currentPos + 1;
      break;
    }
    currentPos += line.length + 1; // +1 for newline
    lineNumber++;
  }

  const errorLine = lines[lineNumber - 1] || '';
  const caret = ' '.repeat(Math.max(0, columnNumber - 1)) + '^';

  return `Parse error at line ${lineNumber}, column ${columnNumber}: ${error}\n${errorLine}\n${caret}`;
};