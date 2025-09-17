// Utility to detect and normalize line endings for lossless parsing

export type LineEndingStyle = 'LF' | 'CRLF' | 'CR';

export interface LineEndingInfo {
  style: LineEndingStyle;
  sequence: string;
}

/**
 * Detects the line ending style used in the input text by examining the first newline
 */
export function detectLineEnding(text: string): LineEndingInfo {
  // Look for the first line ending in the text
  for (let i = 0; i < text.length; i++) {
    const char = text[i];

    if (char === '\r') {
      // Check if it's CRLF or just CR
      if (i + 1 < text.length && text[i + 1] === '\n') {
        return { style: 'CRLF', sequence: '\r\n' };
      } else {
        return { style: 'CR', sequence: '\r' };
      }
    } else if (char === '\n') {
      return { style: 'LF', sequence: '\n' };
    }
  }

  // Default to LF if no line endings found
  return { style: 'LF', sequence: '\n' };
}

/**
 * Normalizes all line endings in text to the specified style
 */
export function normalizeLineEndings(text: string, targetStyle: LineEndingInfo): string {
  // First normalize everything to LF
  const normalizedToLF = text.replace(/\r\n/g, '\n').replace(/\r/g, '\n');

  // Then convert to target style
  switch (targetStyle.style) {
    case 'CRLF':
      return normalizedToLF.replace(/\n/g, '\r\n');
    case 'CR':
      return normalizedToLF.replace(/\n/g, '\r');
    case 'LF':
    default:
      return normalizedToLF;
  }
}