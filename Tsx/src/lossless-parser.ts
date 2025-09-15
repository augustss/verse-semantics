// Lossless parser that preserves all source information for round-trip parsing
import { parseVersee as originalParseVersee } from './parser/parser';
import { L, Pos } from './ast/location';
import { Exp } from './ast/expression';
import { printAST } from './printer/pretty-printer';

export interface LosslessParseOptions {
  preserveTrivia: boolean;
}

export interface LosslessParseResult {
  success: boolean;
  ast?: L<Exp>;
  error?: {
    message: string;
    position: Pos;
  };
  // Additional metadata for lossless parsing
  sourceText?: string;
  roundTripText?: string;
  isRoundTripExact?: boolean;
}

export const defaultLosslessOptions: LosslessParseOptions = {
  preserveTrivia: true
};

// Main lossless parsing function
export function parseLossless(
  input: string,
  options: LosslessParseOptions = defaultLosslessOptions
): LosslessParseResult {
  // For now, use the existing parser but wrap it with trivia information
  const result = originalParseVersee(input);

  if (!result.success) {
    return {
      success: false,
      error: result.error
    };
  }

  const ast = result.value;

  // If trivia preservation is enabled, attempt round-trip
  if (options.preserveTrivia) {
    try {
      const roundTripText = printAST(ast, { preserveOriginalFormatting: true });
      const isExact = input === roundTripText;

      return {
        success: true,
        ast,
        sourceText: input,
        roundTripText,
        isRoundTripExact: isExact
      };
    } catch (error) {
      // If pretty printing fails, fall back to regular parsing
      return {
        success: true,
        ast,
        sourceText: input,
        isRoundTripExact: false
      };
    }
  }

  return {
    success: true,
    ast
  };
}

// Convenience function to check if round-trip parsing works
export function testRoundTrip(input: string): {
  success: boolean;
  isExact: boolean;
  originalLength: number;
  roundTripLength: number;
  differences?: string[];
} {
  const result = parseLossless(input, { preserveTrivia: true });

  if (!result.success) {
    return {
      success: false,
      isExact: false,
      originalLength: input.length,
      roundTripLength: 0
    };
  }

  const isExact = result.isRoundTripExact || false;
  const roundTripLength = result.roundTripText?.length || 0;

  let differences: string[] = [];
  if (!isExact && result.roundTripText) {
    // Simple difference detection (could be enhanced with proper diff algorithm)
    if (input.length !== roundTripLength) {
      differences.push(`Length mismatch: ${input.length} vs ${roundTripLength}`);
    }

    // Find first difference position
    const minLength = Math.min(input.length, roundTripLength);
    for (let i = 0; i < minLength; i++) {
      if (input[i] !== result.roundTripText[i]) {
        differences.push(`First difference at position ${i}: '${input[i]}' vs '${result.roundTripText[i]}'`);
        break;
      }
    }
  }

  return {
    success: true,
    isExact,
    originalLength: input.length,
    roundTripLength,
    differences: differences.length > 0 ? differences : undefined
  };
}

// Export the original parser for backward compatibility
export { parseVersee } from './parser/parser';
export { printAST } from './printer/pretty-printer';

// Re-export types
export * from './ast/expression';
export * from './ast/pattern';
export * from './ast/identifier';
export * from './ast/location';
export * from './ast/trivia';