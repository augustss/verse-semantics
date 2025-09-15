export enum StringDelimiter {
  Quote = 'Quote',
  Brace = 'Brace'
}

export type Token =
  | { kind: 'All' }
  | { kind: 'Ampersand' }
  | { kind: 'And' }
  | { kind: 'Array' }
  | { kind: 'At' }
  | { kind: 'AtSign' }
  | { kind: 'Block' }
  | { kind: 'Caret' }
  | { kind: 'Catch' }
  | { kind: 'Char'; value: string }
  | { kind: 'Class' }
  | { kind: 'Colon' }
  | { kind: 'ColonEOL' }
  | { kind: 'ColonEqual' }
  | { kind: 'ColonRightParen' }
  | { kind: 'Comma' }
  | { kind: 'Dedent' }
  | { kind: 'Divide' }
  | { kind: 'DivideEqual' }
  | { kind: 'Do' }
  | { kind: 'Dot' }
  | { kind: 'DotDot' }
  | { kind: 'DotSpace' }
  | { kind: 'EOF' }
  | { kind: 'Else' }
  | { kind: 'Enum' }
  | { kind: 'Equal' }
  | { kind: 'Exists' }
  | { kind: 'Fail' }
  | { kind: 'Fails' }
  | { kind: 'False' }
  | { kind: 'FatArrow' }
  | { kind: 'Float'; value: number }
  | { kind: 'For' }
  | { kind: 'Forall' }
  | { kind: 'Function' }
  | { kind: 'Greater' }
  | { kind: 'GreaterEqual' }
  | { kind: 'If' }
  | { kind: 'Indent' }
  | { kind: 'Int'; value: bigint }
  | { kind: 'LeftBrace' }
  | { kind: 'LeftBracket' }
  | { kind: 'LeftParen' }
  | { kind: 'Less' }
  | { kind: 'LessEqual' }
  | { kind: 'Minus' }
  | { kind: 'MinusEqual' }
  | { kind: 'Module' }
  | { kind: 'Multiply' }
  | { kind: 'MultiplyEqual' }
  | { kind: 'Name'; value: string }
  | { kind: 'Path'; value: string }
  | { kind: 'Prefix'; value: string }
  | { kind: 'Newline' }
  | { kind: 'Not' }
  | { kind: 'NotEqual' }
  | { kind: 'Of' }
  | { kind: 'One' }
  | { kind: 'Option' }
  | { kind: 'Or' }
  | { kind: 'Pipe' }
  | { kind: 'Plus' }
  | { kind: 'PlusEqual' }
  | { kind: 'QuestionMark' }
  | { kind: 'Return' }
  | { kind: 'RightBrace' }
  | { kind: 'RightBracket' }
  | { kind: 'RightParen' }
  | { kind: 'Semi' }
  | { kind: 'SemiPrime' } // Semi' in Haskell
  | { kind: 'Set' }
  | { kind: 'String'; begin: StringDelimiter; value: string; end: StringDelimiter }
  | { kind: 'Struct' }
  | { kind: 'Sync' }
  | { kind: 'Then' }
  | { kind: 'ThinArrow' }
  | { kind: 'Tilde' }
  | { kind: 'True' }
  | { kind: 'Truth' }
  | { kind: 'Until' }
  | { kind: 'Var' }
  | { kind: 'Where' }
  | { kind: 'Break' }
  | { kind: 'Continue' }
  | { kind: 'Yield' }
  | { kind: 'Next' }
  | { kind: 'Over' }
  | { kind: 'While' }
  | { kind: 'When' }
  // Specifiers
  | { kind: 'Decides' }
  | { kind: 'Succeeds' }
  | { kind: 'Fails' }
  | { kind: 'Transacts' }
  | { kind: 'Computes' }
  | { kind: 'Ambiguates' }
  | { kind: 'Reads' }
  | { kind: 'Writes' }
  | { kind: 'Allocates' }
  | { kind: 'Suspends' };

// Helper functions for creating tokens
export function createIntToken(value: bigint): Token {
  return { kind: 'Int', value };
}

export function createFloatToken(value: number): Token {
  return { kind: 'Float', value };
}

export function createCharToken(value: string): Token {
  return { kind: 'Char', value };
}

export function createNameToken(value: string): Token {
  return { kind: 'Name', value };
}

export function createStringToken(begin: StringDelimiter, value: string, end: StringDelimiter): Token {
  return { kind: 'String', begin, value, end };
}

export function createPathToken(value: string): Token {
  return { kind: 'Path', value };
}

export function createPrefixToken(value: string): Token {
  return { kind: 'Prefix', value };
}