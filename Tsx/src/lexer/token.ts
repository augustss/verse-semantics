export enum TokenType {
  STRING = 'STRING',
  INVALID_STRING = 'INVALID_STRING',  // String with invalid escape sequences
  INTEGER = 'INTEGER',
  FLOAT = 'FLOAT',
  IDENTIFIER = 'IDENTIFIER',
  SPECIFIER = 'SPECIFIER',
  OPERATOR = 'OPERATOR',
  BLOCK_FORMING_KEYWORD = 'BLOCK_FORMING_KEYWORD',  // if, then, else, for, block, loop, array, case
  DATA_STRUCTURE_KEYWORD = 'DATA_STRUCTURE_KEYWORD',  // module, interface, class, struct, enum
  DECL_KEYWORD = 'DECL_KEYWORD',  // var, set, using
  TYPE_KEYWORD = 'TYPE_KEYWORD',  // int, float, string, logic, char, any, void
  RESERVED_WORD = 'RESERVED_WORD',  // do, while, break, continue, return, yield, spawn, sync, race
  COMMENT = 'COMMENT',
  MULTILINE_COMMENT = 'MULTILINE_COMMENT',
  SPACE = 'SPACE',
  TAB = 'TAB',
  NEWLINE = 'NEWLINE',
  TRIVIA = 'TRIVIA',
  EOF = 'EOF',
  UNKNOWN = 'UNKNOWN'
}

export interface Position {
  line: number;
  column: number;
  offset: number;
}

export class Token {
  public readonly content: string;
  public readonly type: TokenType;
  public readonly position: Position;
  public readonly endPosition: Position;
  public readonly length: number;
  public readonly indentation: number;
  public readonly scopedContent?: string; // For <scoped(...)> specifiers
  private static tabWidth: number = 4;

  constructor(
    content: string,
    type: TokenType,
    position: Position,
    endPosition?: Position,
    indentation?: number,
    scopedContent?: string
  ) {
    this.content = content;
    this.type = type;
    this.position = position;
    this.endPosition = endPosition || {
      line: position.line,
      column: position.column + content.length,
      offset: position.offset + content.length
    };
    this.indentation = indentation ?? 0;
    this.scopedContent = scopedContent;
    // Calculate length after setting indentation (needed for TRIVIA tokens)
    this.length = this.calculateLength();
  }

  private calculateLength(): number {
    // Comments have zero visual length
    if (this.isComment()) {
      return 0;
    }

    // TRIVIA tokens have their length set via indentation parameter
    // (which represents the calculated non-comment, non-EOL characters)
    if (this.isTrivia()) {
      return this.indentation;
    }

    // Tabs have configurable visual length
    if (this.isTab()) {
      // Each tab character counts as tabWidth spaces
      return this.content.length * Token.tabWidth;
    }

    // All other tokens have their content length as visual length
    return this.content.length;
  }

  static setDefaultTabWidth(width: number): void {
    Token.tabWidth = width;
  }

  isString(): boolean {
    return this.type === TokenType.STRING;
  }

  isInteger(): boolean {
    return this.type === TokenType.INTEGER;
  }

  isFloat(): boolean {
    return this.type === TokenType.FLOAT;
  }

  isIdentifier(): boolean {
    return this.type === TokenType.IDENTIFIER;
  }

  isSpecifier(): boolean {
    return this.type === TokenType.SPECIFIER;
  }

  isOperator(): boolean {
    return this.type === TokenType.OPERATOR;
  }

  isBlockFormingKeyword(): boolean {
    return this.type === TokenType.BLOCK_FORMING_KEYWORD;
  }

  isDataStructureKeyword(): boolean {
    return this.type === TokenType.DATA_STRUCTURE_KEYWORD;
  }

  isDeclKeyword(): boolean {
    return this.type === TokenType.DECL_KEYWORD;
  }

  isTypeKeyword(): boolean {
    return this.type === TokenType.TYPE_KEYWORD;
  }

  isReservedWord(): boolean {
    return this.type === TokenType.RESERVED_WORD;
  }

  isKeyword(): boolean {
    return this.isBlockFormingKeyword() || this.isDataStructureKeyword() ||
           this.isDeclKeyword() || this.isTypeKeyword() || this.isReservedWord();
  }

  isComment(): boolean {
    return this.type === TokenType.COMMENT || this.type === TokenType.MULTILINE_COMMENT;
  }

  isSpace(): boolean {
    return this.type === TokenType.SPACE;
  }

  isTab(): boolean {
    return this.type === TokenType.TAB;
  }

  isNewline(): boolean {
    return this.type === TokenType.NEWLINE;
  }

  isWhitespace(): boolean {
    return this.isSpace() || this.isTab();
  }

  isEOF(): boolean {
    return this.type === TokenType.EOF;
  }

  isUnknown(): boolean {
    return this.type === TokenType.UNKNOWN;
  }

  isTrivia(): boolean {
    return this.type === TokenType.TRIVIA;
  }

  toString(): string {
    return `Token(${this.type}, "${this.content}", ${this.position.line}:${this.position.column})`;
  }
}