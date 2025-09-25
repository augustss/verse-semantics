/**
 * Top-Level Parser for Verse
 *
 * Handles parsing of complete Verse files including:
 * - Initial trivia (comments, whitespace)
 * - Using statements (imports)
 * - Top-level declarations (constants, variables, functions, classes, modules)
 *
 * Structure of a Verse file:
 * ```
 * # Optional comments and whitespace
 * using { /Fortnite.com/Devices }
 * using { /Verse.org/Simulation }
 *
 * # Declarations
 * MyModule := module { ... }
 * x := 42
 * var y : int = 10
 * f() := x + y
 * ```
 */

import { TokenStream } from '../lexer/tokenstream';
import { Token, TokenType } from '../lexer/token';
import * as AST from './ast';
import { ParserState, ParseResult, ParseError } from './parser-state';
import { Parser, createParser, createParserState } from './parser';

/**
 * Represents a using statement in Verse
 * e.g., using { /Fortnite.com/Devices }
 */
export interface UsingStatement extends AST.ASTNode {
  readonly type: 'UsingStatement';
  readonly path: string;
  readonly usingOffset: number;     // Offset of 'using' keyword
  readonly openBraceOffset: number; // Offset of '{'
  readonly closeBraceOffset: number; // Offset of '}'
  readonly pathOffset: number;      // Offset of the path string
}

/**
 * Represents a complete Verse file/program
 */
export interface Program extends AST.ASTNode {
  readonly type: 'Program';
  readonly initialTrivia: Token[];      // Leading comments/whitespace
  readonly usingStatements: UsingStatement[];
  readonly declarations: AST.Declaration[];
}

/**
 * Top-level parser for complete Verse files
 */
export class TopLevelParser {
  private parser: Parser;

  constructor() {
    this.parser = createParser();
  }

  /**
   * Parse a complete Verse file
   */
  parseProgram(state: ParserState): ParseResult<Program> {
    // 1. Collect initial trivia
    const initialTrivia = this.collectInitialTrivia(state);
    state = this.skipAllTrivia(state);

    // 2. Parse using statements
    const usingStatements: UsingStatement[] = [];
    while (!state.isAtEnd()) {
      const usingResult = this.tryParseUsingStatement(state);
      if (!usingResult) break;

      usingStatements.push(usingResult.node);
      state = this.skipAllTrivia(usingResult.state);
    }

    // 3. Parse declarations
    const declarations: AST.Declaration[] = [];
    while (!state.isAtEnd()) {
      try {
        const declResult = this.parser.parseDeclaration(state);
        declarations.push(declResult.node);
        state = this.skipAllTrivia(declResult.state);
      } catch (error: any) {
        // Re-throw type{} validation errors
        if (error instanceof ParseError && error.message && error.message.includes('type{')) {
          throw error;
        }
        // If we can't parse as a declaration, log details and break
        const currentToken = state.current();
        let tempState = state;
        for (let i = 0; i < 5 && !tempState.isAtEnd(); i++) {
          const tok = tempState.current();
          tempState = tempState.advance();
        }
        break;
      }
    }

    const program: Program = {
      type: 'Program',
      initialTrivia,
      usingStatements,
      declarations
    };

    return { node: program, state };
  }

  /**
   * Skip all trivia including newlines (for top-level parsing)
   */
  private skipAllTrivia(state: ParserState): ParserState {
    while (!state.isAtEnd()) {
      const token = state.current();
      if (!token) break;

      // Skip trivia, whitespace, comments, and newlines
      if (token.type === TokenType.COMMENT ||
          token.type === TokenType.MULTILINE_COMMENT ||
          token.type === TokenType.SPACE ||
          token.type === TokenType.TAB ||
          token.type === TokenType.NEWLINE ||
          token.type === TokenType.TRIVIA) {
        state = state.advance();
      } else {
        break;
      }
    }
    return state;
  }

  /**
   * Collect initial trivia tokens (comments, whitespace) before any code
   */
  private collectInitialTrivia(state: ParserState): Token[] {
    const trivia: Token[] = [];

    while (!state.isAtEnd()) {
      const token = state.current();
      if (!token) break;

      // Collect trivia tokens
      if (token.type === TokenType.COMMENT ||
          token.type === TokenType.MULTILINE_COMMENT ||
          token.type === TokenType.SPACE ||
          token.type === TokenType.TAB ||
          token.type === TokenType.NEWLINE ||
          token.type === TokenType.TRIVIA) {
        trivia.push(token);
        state = state.advance();
      } else {
        // Stop at first non-trivia token
        break;
      }
    }

    return trivia;
  }

  /**
   * Try to parse a using statement, return null if not a using statement
   */
  private tryParseUsingStatement(state: ParserState): ParseResult<UsingStatement> | null {
    const token = state.current();
    if (!token || token.type !== TokenType.DECL_KEYWORD || token.content !== 'using') {
      return null;
    }

    try {
      return this.parseUsingStatement(state);
    } catch {
      return null;
    }
  }

  /**
   * Parse a using statement
   * Grammar: using { path }
   */
  private parseUsingStatement(state: ParserState): ParseResult<UsingStatement> {
    state = state.skipTrivia();

    // Parse 'using' keyword
    const usingOffset = state.currentOffset();
    const usingToken = state.current();
    if (!usingToken || usingToken.type !== TokenType.DECL_KEYWORD || usingToken.content !== 'using') {
      throw new ParseError('Expected using', state.position, usingToken || undefined);
    }
    state = state.advance().skipTrivia();

    // Parse opening brace
    const openBraceOffset = state.currentOffset();
    const openBrace = state.current();
    if (!openBrace || openBrace.type !== TokenType.OPERATOR || openBrace.content !== '{') {
      throw new ParseError('Expected {', state.position, openBrace || undefined);
    }
    state = state.advance().skipTrivia();

    // Parse path (could be a string literal, identifier, or a path-like identifier)
    const pathOffset = state.currentOffset();
    const pathToken = state.current();
    let path: string;

    if (!pathToken) {
      throw new ParseError('Expected path', state.position);
    }

    if (pathToken.type === TokenType.STRING) {
      // String literal path
      path = pathToken.content;
      state = state.advance();
    } else if (pathToken.type === TokenType.IDENTIFIER) {
      // Simple identifier like 'std' or identifier path
      path = pathToken.content;
      state = state.advance();

      // Check if it's a path with dots or slashes
      while (!state.isAtEnd()) {
        const current = state.current();
        if (!current) break;

        if (current.type === TokenType.OPERATOR && (current.content === '.' || current.content === '/')) {
          path += current.content;
          state = state.advance();

          // Expect identifier after dot/slash
          const next = state.current();
          if (next && next.type === TokenType.IDENTIFIER) {
            path += next.content;
            state = state.advance();
          } else {
            break;
          }
        } else {
          break;
        }
      }
    } else if (pathToken.type === TokenType.OPERATOR && pathToken.content === '/') {
      // Path starting with /
      let pathParts: string[] = ['/'];
      state = state.advance();

      // Collect path segments
      while (!state.isAtEnd()) {
        const current = state.current();
        if (!current) break;

        if (current.type === TokenType.IDENTIFIER) {
          pathParts.push(current.content);
          state = state.advance();
        } else if (current.type === TokenType.OPERATOR && (current.content === '.' || current.content === '/')) {
          pathParts.push(current.content);
          state = state.advance();
        } else {
          break;
        }
      }

      path = pathParts.join('');
    } else {
      throw new ParseError('Expected path string, identifier, or /path', state.position, pathToken);
    }

    state = state.skipTrivia();

    // Parse closing brace
    const closeBraceOffset = state.currentOffset();
    const closeBrace = state.current();
    if (!closeBrace || closeBrace.type !== TokenType.OPERATOR || closeBrace.content !== '}') {
      throw new ParseError('Expected }', state.position, closeBrace || undefined);
    }
    state = state.advance();

    const usingStatement: UsingStatement = {
      type: 'UsingStatement',
      path,
      usingOffset,
      openBraceOffset,
      closeBraceOffset,
      pathOffset
    };

    return { node: usingStatement, state };
  }
}

/**
 * Parse a complete Verse program from source code
 */
export function parseProgram(source: string): Program {
  const tokens = TokenStream.fromString(source);
  tokens.combineTrivia();
  const parser = new TopLevelParser();
  const state = createParserState(tokens);
  const result = parser.parseProgram(state);
  return result.node;
}


// parseTopLevel has been removed - use parseProgram instead.
// For single declarations, parseProgram will return a Program with one declaration.