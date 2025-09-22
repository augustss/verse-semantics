/**
 * Token creation utilities
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { trivia } from './trivia';

export type TokenWithTrivia<T> = {
  value: T;
  leading: string;
  text: string;
  trailing: string;
  span: AST.Span;
};

// Parse a token with trivia (generic version)
export const withTrivia = <T>(
  parser: PC.Parser<T>,
  text: string
): PC.Parser<AST.Token<T>> => {
  return (state: PC.ParserState): PC.ParserResult<AST.Token<T>> => {
    const startPos = state.position;

    // Parse leading trivia
    const leadingTriviaResult = trivia(state);
    if (!leadingTriviaResult.success) return leadingTriviaResult;

    // Parse the token
    const tokenResult = parser(leadingTriviaResult.state);
    if (!tokenResult.success) return tokenResult;

    // Parse trailing trivia
    const trailingTriviaResult = trivia(tokenResult.state);
    if (!trailingTriviaResult.success) return trailingTriviaResult;

    return {
      success: true,
      value: AST.token(
        text,
        tokenResult.value,
        {
          leading: leadingTriviaResult.value,
          trailing: trailingTriviaResult.value
        },
        { start: startPos, end: trailingTriviaResult.state.position }
      ),
      state: trailingTriviaResult.state
    };
  };
};

// Parse a literal token with trivia (for specific literal types)
export const withTriviaLiteral = <T extends string>(
  literalValue: T,
  parser: PC.Parser<string>
): PC.Parser<AST.Token<T>> => {
  return (state: PC.ParserState): PC.ParserResult<AST.Token<T>> => {
    const startPos = state.position;

    // Parse leading trivia
    const leadingTriviaResult = trivia(state);
    if (!leadingTriviaResult.success) return leadingTriviaResult;

    // Parse the literal
    const tokenResult = parser(leadingTriviaResult.state);
    if (!tokenResult.success) return tokenResult;

    // Parse trailing trivia
    const trailingTriviaResult = trivia(tokenResult.state);
    if (!trailingTriviaResult.success) return trailingTriviaResult;

    return {
      success: true,
      value: AST.token(
        tokenResult.value,
        literalValue,
        {
          leading: leadingTriviaResult.value,
          trailing: trailingTriviaResult.value
        },
        { start: startPos, end: trailingTriviaResult.state.position }
      ),
      state: trailingTriviaResult.state
    };
  };
};

// Parse a specific keyword with trivia
export const keyword = (kw: string): PC.Parser<AST.Token<string>> => {
  return (state: PC.ParserState): PC.ParserResult<AST.Token<string>> => {
    const startPos = state.position;

    // Parse leading trivia
    const leadingTriviaResult = trivia(state);
    const leadingTrivia = leadingTriviaResult.success ? leadingTriviaResult.value : '';
    const afterLeading = leadingTriviaResult.success ? leadingTriviaResult.state : state;

    // Check if the keyword matches
    if (!afterLeading.input.startsWith(kw, afterLeading.position)) {
      return {
        success: false,
        error: `Expected keyword '${kw}'`,
        state
      };
    }

    // Make sure it's not part of a larger identifier
    const nextPos = afterLeading.position + kw.length;
    if (nextPos < afterLeading.input.length) {
      const nextChar = afterLeading.input[nextPos];
      if (/[a-zA-Z0-9_]/.test(nextChar)) {
        return {
          success: false,
          error: `Expected keyword '${kw}', but found identifier`,
          state
        };
      }
    }

    const afterKeyword = { ...afterLeading, position: nextPos };

    // Parse trailing trivia
    const trailingTriviaResult = trivia(afterKeyword);
    const trailingTrivia = trailingTriviaResult.success ? trailingTriviaResult.value : '';
    const finalState = trailingTriviaResult.success ? trailingTriviaResult.state : afterKeyword;

    return {
      success: true,
      value: AST.token(
        kw,
        kw,
        { leading: leadingTrivia, trailing: trailingTrivia },
        { start: startPos, end: finalState.position }
      ),
      state: finalState
    };
  };
};