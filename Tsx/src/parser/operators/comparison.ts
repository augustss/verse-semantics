/**
 * Comparison operator parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';

// Comparison operators (>, <, >=, <=, ==, !=)
export const compareOp: PC.Parser<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>> = (state) => {
  // Try two-character operators first
  const geResult = withTriviaLiteral('>=', PC.string('>='))(state);
  if (geResult.success) {
    return geResult as PC.ParserResult<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>>;
  }

  const leResult = withTriviaLiteral('<=', PC.string('<='))(state);
  if (leResult.success) {
    return leResult as PC.ParserResult<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>>;
  }

  const eqResult = withTriviaLiteral('==', PC.string('=='))(state);
  if (eqResult.success) {
    return eqResult as PC.ParserResult<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>>;
  }

  const neResult = withTriviaLiteral('!=', PC.string('!='))(state);
  if (neResult.success) {
    return neResult as PC.ParserResult<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>>;
  }

  // Try single-character operators
  const gtResult = withTriviaLiteral('>', PC.char('>'))(state);
  if (gtResult.success) {
    return gtResult as PC.ParserResult<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>>;
  }

  const ltResult = withTriviaLiteral('<', PC.char('<'))(state);
  if (ltResult.success) {
    return ltResult as PC.ParserResult<AST.Token<'>' | '<' | '>=' | '<=' | '==' | '!='>>;
  }

  return { success: false, error: 'Expected comparison operator', state };
};