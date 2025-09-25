/**
 * Declaration Parser - Handles all forms of declarations in Verse
 *
 * This module parses the various declaration forms that can appear at the top level
 * of a program or within data structure bodies (classes, modules, interfaces).
 *
 * DECLARATION TYPES SUPPORTED:
 *
 * 1. CONSTANT DECLARATIONS:
 *    - Simple: x := 42
 *    - With type: x : int = 42
 *    - With specifiers: x<public> := 42
 *    - Combined: x<public> : int = 42
 *
 * 2. VARIABLE DECLARATIONS:
 *    - Basic: var x : int = 42
 *    - Inferred: var x := 42
 *    - Uninitialized: var x : int
 *
 * 3. FUNCTION DECLARATIONS:
 *    - Simple: f() := body
 *    - With params: f(x: int, y: string) := body
 *    - With return type: f() : int = body
 *    - With specifiers: f<public>() := body
 *    - Method signatures (interface): f() : int (no body)
 *
 * 4. DATA STRUCTURE DECLARATIONS:
 *    - Classes: MyClass := class { field := value }
 *    - Interfaces: IContract := interface { Method() : int }
 *    - Structs: Point := struct { x : float, y : float }
 *    - Enums: Color := enum { Red, Green, Blue }
 *    - Modules: Utils := module { Helper() := ... }
 *
 * 5. SPECIFIER HANDLING:
 *    - Access: <public>, <private>, <protected>
 *    - Behavior: <override>, <abstract>, <final>
 *    - Effects: <suspends>, <transacts>, <decides>
 *    - Scoped: <scoped{path}>
 *    - Custom: <editable>, <replicated>
 *
 * KEY DESIGN DECISIONS:
 * - Decorators (@decorator) are collected before parsing declarations
 * - Specifiers can appear before identifiers or after types
 * - Function bodies are required unless in interface context
 * - Data structures support both braced {...} and indented : forms
 * - Trailing commas are not allowed in parameter lists
 */

import { Token, TokenType } from '../../lexer/token';
import { ParserState, ParseResult, ParseError } from '../parser-state';
import * as AST from '../ast';

/**
 * Parser for declarations.
 *
 * This parser handles all forms of declarations in Verse,
 * including constants, variables, and functions with their
 * various syntactic forms.
 */
export class DeclarationParser {
  // Visibility specifiers that can appear after function name (only one allowed)
  private static readonly VISIBILITY_SPECIFIERS = new Set([
    'public',
    'protected',
    'private',
    'internal',
    'scoped',
    'constructor'
  ]);

  private parseExpression: (state: ParserState) => ParseResult<AST.Expression>;
  private parseIdentedCompound: (state: ParserState) => ParseResult<AST.IdentedCompoundExpression>;

  constructor(
    parseExpression: (state: ParserState) => ParseResult<AST.Expression>,
    parseIdentedCompound: (state: ParserState) => ParseResult<AST.IdentedCompoundExpression>
  ) {
    this.parseExpression = parseExpression;
    this.parseIdentedCompound = parseIdentedCompound;
  }

  /**
   * Parse any declaration.
   *
   * This is the main entry point that determines the type of declaration
   * and delegates to the appropriate parser.
   */
  parseDeclaration(state: ParserState, context?: { kind?: string }): ParseResult<AST.Declaration> {
    state = state.skipTrivia();

    // Check for decorators (e.g., @editable, @replicated)
    const decorators: string[] = [];
    const decoratorOffsets: number[] = [];
    while (state.current()?.type === TokenType.IDENTIFIER && state.current()?.content.startsWith('@')) {
      decoratorOffsets.push(state.currentOffset());
      decorators.push(state.current()!.content);
      state = state.advance().skipTrivia();
      // Skip newlines after decorators
      while (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance().skipTrivia();
      }
    }

    const token = state.current();

    if (!token) {
      throw new ParseError('Expected declaration', state.position);
    }

    // Check for 'var' keyword for variable declarations
    if (token.type === TokenType.DECL_KEYWORD && token.content === 'var') {
      const result = this.parseVariableDeclaration(state);
      if (decorators.length > 0) {
        (result.node as any).decorators = decorators;
        (result.node as any).decoratorOffsets = decoratorOffsets;
      }
      return result;
    }


    // Handle declarations starting with specifier (e.g., <decides> MyFunction())
    if (token.type === TokenType.SPECIFIER) {
      // Parse the specifier and continue
      const specifiers: string[] = [];
      const specifierOffsets: number[] = [];

      // Collect all consecutive specifiers
      while (state.current()?.type === TokenType.SPECIFIER) {
        specifierOffsets.push(state.currentOffset());
        const specToken = state.current()!;
        // Extract content between < and >
        const content = specToken.content.slice(1, -1);
        specifiers.push(content);
        state = state.advance().skipTrivia();
      }

      // After specifiers, we should have an identifier
      if (state.current()?.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected identifier after specifiers', state.position, state.current() || undefined);
      }

      // Create a specifier list node
      const specifierList: AST.SpecifierList = {
        type: 'SpecifierList',
        specifiers,
        specifierOffsets,
        openAngleOffset: specifierOffsets[0],
        closeAngleOffset: specifierOffsets[specifierOffsets.length - 1],
        separatorOffsets: []
      };

      // Now determine what kind of declaration follows
      const lookahead = this.lookAheadForDeclarationType(state);

      if (lookahead === 'function') {
        // Parse as function with pre-specifiers already collected
        const result = this.parseFunctionDeclarationWithSpecifiers(state, specifierList);
        if (decorators.length > 0) {
          (result.node as any).decorators = decorators;
          (result.node as any).decoratorOffsets = decoratorOffsets;
        }
        return result;
      } else if (lookahead === 'datastructure') {
        // For now, data structures with pre-specifiers are not common
        const result = this.parseDataStructureDeclaration(state);
        if (decorators.length > 0) {
          (result.node as any).decorators = decorators;
          (result.node as any).decoratorOffsets = decoratorOffsets;
        }
        return result;
      } else {
        // Parse as constant declaration with pre-specifiers
        const result = this.parseConstantDeclarationWithSpecifiers(state, specifierList);
        if (decorators.length > 0) {
          (result.node as any).decorators = decorators;
          (result.node as any).decoratorOffsets = decoratorOffsets;
        }
        return result;
      }
    }

    // Otherwise, it could be a constant, function, or data structure declaration
    // We need to look ahead to determine which
    if (token.type === TokenType.IDENTIFIER) {
      const lookahead = this.lookAheadForDeclarationType(state);

      if (lookahead === 'function') {
        const allowSignatureOnly = context?.kind === 'interface';
        const result = this.parseFunctionDeclaration(state, allowSignatureOnly);
        if (decorators.length > 0) {
          (result.node as any).decorators = decorators;
          (result.node as any).decoratorOffsets = decoratorOffsets;
        }
        return result;
      } else if (lookahead === 'datastructure') {
        try {
          const result = this.parseDataStructureDeclaration(state);
          if (decorators.length > 0) {
            (result.node as any).decorators = decorators;
            (result.node as any).decoratorOffsets = decoratorOffsets;
          }
          return result;
        } catch (err: any) {
          throw err;
        }
      } else {
        const result = this.parseConstantDeclaration(state);
        if (decorators.length > 0) {
          (result.node as any).decorators = decorators;
          (result.node as any).decoratorOffsets = decoratorOffsets;
        }
        return result;
      }
    }

    throw new ParseError('Expected declaration', state.position, token);
  }

  /**
   * Look ahead to determine what type of declaration we're parsing.
   *
   * This method analyzes upcoming tokens to distinguish between:
   *
   * FUNCTION DECLARATIONS:
   * - Pattern: identifier(...) := body
   * - Pattern: identifier(...) : type = body
   * - Examples: f() := 42, calculate(x: int) : float = x * 2.0
   *
   * DATA STRUCTURE DECLARATIONS:
   * - Pattern: identifier := (class|struct|interface|enum|module)
   * - Examples: MyClass := class { }, Point := struct { }
   *
   * CONSTANT DECLARATIONS:
   * - Pattern: identifier := expression
   * - Pattern: identifier : type = expression
   * - Examples: x := 42, PI : float = 3.14159
   *
   * The lookahead is necessary because all three start with an identifier,
   * and we need to see what follows to determine the parsing strategy.
   */
  private lookAheadForDeclarationType(state: ParserState): 'function' | 'constant' | 'datastructure' {
    let pos = 1;
    let token = state.peek(pos);

    // Skip trivia
    while (token && (token.type === TokenType.TRIVIA || token.type === TokenType.SPACE ||
                     token.type === TokenType.TAB || token.type === TokenType.NEWLINE)) {
      pos++;
      token = state.peek(pos);
    }

    // Skip specifiers if present (can be multiple consecutive <spec> blocks or SPECIFIER tokens)
    while (token && (token.type === TokenType.SPECIFIER ||
                     (token.type === TokenType.OPERATOR && token.content === '<'))) {
      if (token.type === TokenType.SPECIFIER) {
        // Simple SPECIFIER token, just skip it
        pos++;
        token = state.peek(pos);
      } else {
        // Manual <...> style specifier
        // Skip until we find >
        while (token && !(token.type === TokenType.OPERATOR && token.content === '>')) {
          pos++;
          token = state.peek(pos);
        }
        if (token) {
          pos++;
          token = state.peek(pos);
        }
      }
      // Skip trivia after this specifier
      while (token && (token.type === TokenType.TRIVIA || token.type === TokenType.SPACE ||
                       token.type === TokenType.TAB || token.type === TokenType.NEWLINE)) {
        pos++;
        token = state.peek(pos);
      }
    }

    // Check for opening paren (function)
    if (token && token.type === TokenType.OPERATOR && token.content === '(') {
      return 'function';
    }

    // Check for := followed by a data structure kind
    if (token && token.type === TokenType.OPERATOR && token.content === ':=') {
      pos++;
      token = state.peek(pos);

      // Skip trivia after :=
      while (token && (token.type === TokenType.TRIVIA || token.type === TokenType.SPACE ||
                       token.type === TokenType.TAB || token.type === TokenType.NEWLINE)) {
        pos++;
        token = state.peek(pos);
      }

      if (token && token.type === TokenType.DATA_STRUCTURE_KEYWORD) {
        return 'datastructure';
      }
    }

    // Otherwise it's a constant
    return 'constant';
  }

  /**
   * Parse a constant declaration.
   *
   * Grammar:
   *   const_decl = identifier specifiers? ":" type ("=" expression)?
   *              | identifier specifiers? ":=" expression
   */
  parseConstantDeclaration(state: ParserState): ParseResult<AST.ConstantDeclaration> {
    state = state.skipTrivia();

    // Parse name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected identifier', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance();

    // Parse optional specifiers
    let specifiers: AST.SpecifierList | undefined;
    const specResult = this.parseSpecifiers(state);
    if (specResult) {
      specifiers = specResult.node;
      state = specResult.state;
    }

    state = state.skipTrivia();

    // Check for : or :=
    const colonOffset = state.currentOffset();
    const colonToken = state.current();

    if (colonToken && colonToken.type === TokenType.OPERATOR && colonToken.content === ':=') {
      // Type inference with initializer
      state = state.advance();

      // Try parsing as type first only for patterns that look like type aliases
      // (e.g., starts with [], contains type keywords, etc.)
      // Otherwise, parse as expression first to avoid object constructor conflicts
      let initResult: ParseResult<AST.Expression> | undefined;
      let typeAliasResult: ParseResult<AST.TypeExpression> | undefined;
      const stateBeforeParsing = state;

      if (this.looksLikeTypeAlias(state)) {
        try {
          typeAliasResult = this.parseType(state);
        } catch {
          // Type parsing failed, try expression parsing
          initResult = this.parseExpression(stateBeforeParsing);
        }
      } else {
        // Parse as expression first for most cases
        try {
          initResult = this.parseExpression(state);
        } catch {
          // Expression parsing failed, try type parsing as fallback
          typeAliasResult = this.parseType(stateBeforeParsing);
        }
      }

      let finalState: ParserState;
      let node: AST.ConstantDeclaration;

      if (typeAliasResult) {
        // This is a type alias (like "numbers := []float")
        node = {
          type: 'ConstantDeclaration',
          name,
          nameOffset,
          specifiers,
          assignOffset: colonOffset,
          declaredType: typeAliasResult.node
        };
        finalState = typeAliasResult.state;
      } else {
        // This is a regular constant with an expression initializer
        node = {
          type: 'ConstantDeclaration',
          name,
          nameOffset,
          specifiers,
          assignOffset: colonOffset,
          initializer: initResult!.node
        };
        finalState = initResult!.state;
      }

      return { node, state: finalState };
    } else if (colonToken && colonToken.type === TokenType.OPERATOR && colonToken.content === ':') {
      // Explicit type
      state = state.advance();

      // Parse type
      const typeResult = this.parseType(state);
      state = typeResult.state;

      // Check for optional initializer
      state = state.skipTrivia();
      let initializer: AST.Expression | undefined;
      let equalsOffset: number | undefined;

      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '=') {
        equalsOffset = state.currentOffset();
        state = state.advance();

        const initResult = this.parseExpression(state);
        initializer = initResult.node;
        state = initResult.state;
      }

      const node: AST.ConstantDeclaration = {
        type: 'ConstantDeclaration',
        name,
        nameOffset,
        specifiers,
        declaredType: typeResult.node,
        colonOffset,
        initializer,
        equalsOffset
      };

      return { node, state };
    }

    throw new ParseError('Expected : or := after identifier', state.position, colonToken || undefined);
  }

  /**
   * Parse a variable declaration.
   *
   * Grammar:
   *   var_decl = "var" identifier specifiers? ":" type ("=" expression)?
   */
  parseVariableDeclaration(state: ParserState): ParseResult<AST.VariableDeclaration> {
    state = state.skipTrivia();

    // Parse 'var' keyword
    const varOffset = state.currentOffset();
    const varToken = state.current();
    if (!varToken || varToken.type !== TokenType.DECL_KEYWORD || varToken.content !== 'var') {
      throw new ParseError('Expected var keyword', state.position, varToken || undefined);
    }
    state = state.advance().skipTrivia();

    // Parse name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected identifier after var', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance();

    // Parse optional specifiers
    let specifiers: AST.SpecifierList | undefined;
    const specResult = this.parseSpecifiers(state);
    if (specResult) {
      specifiers = specResult.node;
      state = specResult.state;
    }

    state = state.skipTrivia();

    // Require : for type
    const colonOffset = state.currentOffset();
    const colonToken = state.current();
    if (!colonToken || colonToken.type !== TokenType.OPERATOR || colonToken.content !== ':') {
      throw new ParseError('Expected : after variable name', state.position, colonToken || undefined);
    }
    state = state.advance();

    // Parse type (required for variables)
    const typeResult = this.parseType(state);
    state = typeResult.state;

    // Check for optional initializer
    state = state.skipTrivia();
    let initializer: AST.Expression | undefined;
    let equalsOffset: number | undefined;

    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '=') {
      equalsOffset = state.currentOffset();
      state = state.advance();

      const initResult = this.parseExpression(state);
      initializer = initResult.node;
      state = initResult.state;
    }

    const node: AST.VariableDeclaration = {
      type: 'VariableDeclaration',
      varOffset,
      name,
      nameOffset,
      specifiers,
      declaredType: typeResult.node,
      colonOffset,
      initializer,
      equalsOffset
    };

    return { node, state };
  }

  /**
   * Parse a function declaration with pre-collected specifiers.
   */
  private parseFunctionDeclarationWithSpecifiers(state: ParserState, preSpecifiers: AST.SpecifierList): ParseResult<AST.FunctionDeclaration> {
    state = state.skipTrivia();

    // Separate visibility specifiers from other specifiers for logical AST
    const allSpecifiers = preSpecifiers.specifiers;
    const visibilitySpecs = allSpecifiers.filter(s => DeclarationParser.VISIBILITY_SPECIFIERS.has(s));

    let visibilitySpecifier: AST.SpecifierList | undefined;
    if (visibilitySpecs.length > 0) {
      // Create a new SpecifierList with only visibility specifiers
      const visibilityOffsets = preSpecifiers.specifierOffsets.filter((_, i) =>
        DeclarationParser.VISIBILITY_SPECIFIERS.has(allSpecifiers[i]));

      visibilitySpecifier = {
        type: 'SpecifierList',
        specifiers: visibilitySpecs,
        specifierOffsets: visibilityOffsets,
        openAngleOffset: preSpecifiers.openAngleOffset,
        closeAngleOffset: preSpecifiers.closeAngleOffset,
        separatorOffsets: preSpecifiers.separatorOffsets
      };
    }

    // Parse name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected function name', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance().skipTrivia();

    // Parse parameter list
    const openParenOffset = state.currentOffset();
    const openParen = state.current();
    if (!openParen || openParen.type !== TokenType.OPERATOR || openParen.content !== '(') {
      throw new ParseError('Expected ( after function name', state.position, openParen || undefined);
    }
    state = state.advance();

    // Parse parameters
    const { parameters, separatorOffsets, state: afterParams } = this.parseParameterList(state);
    state = afterParams;

    // Parse closing paren
    state = state.skipTrivia();
    const closeParenOffset = state.currentOffset();
    const closeParen = state.current();
    if (!closeParen || closeParen.type !== TokenType.OPERATOR || closeParen.content !== ')') {
      throw new ParseError('Expected )', state.position, closeParen || undefined);
    }
    state = state.advance();

    // Parse optional post-specifiers
    let postSpecifiers: AST.SpecifierList | undefined;
    const postSpecResult = this.parseSpecifiers(state);
    if (postSpecResult) {
      postSpecifiers = postSpecResult.node;
      state = postSpecResult.state;
    }

    state = state.skipTrivia();

    // Parse optional return type
    let returnType: AST.TypeExpression | undefined;
    let returnColonOffset: number | undefined;

    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Check if it's : followed by type (not :=)
      if (state.peek(1)?.type !== TokenType.OPERATOR || state.peek(1)?.content !== '=') {
        returnColonOffset = state.currentOffset();
        state = state.advance();

        const typeResult = this.parseType(state);
        returnType = typeResult.node;
        state = typeResult.state.skipTrivia();
      }
    }

    // Parse := or =
    let assignOffset: number | undefined;
    let equalsOffset: number | undefined;

    if (state.current()?.type === TokenType.OPERATOR) {
      if (state.current()?.content === ':=' || state.current()?.content === '⩴') {
        assignOffset = state.currentOffset();
        state = state.advance();
      } else if (state.current()?.content === '=') {
        equalsOffset = state.currentOffset();
        state = state.advance();
      } else {
        throw new ParseError('Expected := or = after function signature', state.position, state.current() || undefined);
      }
    } else {
      throw new ParseError('Expected := or = after function signature', state.position, state.current() || undefined);
    }

    // Check if this is a constructor function
    let isConstructor = false;
    if (visibilitySpecifier) {
      isConstructor = visibilitySpecifier.specifiers.includes('constructor');
    }
    if (postSpecifiers && !isConstructor) {
      isConstructor = postSpecifiers.specifiers.includes('constructor');
    }

    // Parse body (different logic for constructors vs regular functions)
    state = state.skipTrivia();
    let body: AST.Expression;
    let constructedType: string | undefined;
    let constructedTypeOffset: number | undefined;

    if (isConstructor) {
      // For constructors: parse class name followed by : and constructor body
      const classNameToken = state.current();
      if (!classNameToken || classNameToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected class name after := in constructor', state.position, classNameToken || undefined);
      }

      constructedType = classNameToken.content;
      constructedTypeOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Expect : after class name
      const colonToken = state.current();
      if (!colonToken || colonToken.type !== TokenType.OPERATOR || colonToken.content !== ':') {
        throw new ParseError('Expected : after class name in constructor', state.position, colonToken || undefined);
      }
      state = state.advance().skipTrivia();

      // Parse constructor body
      const bodyResult = this.parseExpression(state);
      body = bodyResult.node;
      state = bodyResult.state;
    } else {
      // Regular function: parse body as expression
      const bodyResult = this.parseExpression(state);
      body = bodyResult.node;
      state = bodyResult.state;
    }

    const node: AST.FunctionDeclaration = {
      type: 'FunctionDeclaration',
      name,
      nameOffset,
      parameters,
      openParenOffset,
      closeParenOffset,
      paramSeparatorOffsets: separatorOffsets,
      visibilitySpecifier, // separated visibility specifiers
      postSpecifiers,
      returnType,
      returnColonOffset,
      assignOffset,
      equalsOffset,
      body,
      isConstructor: isConstructor || undefined,
      constructedType,
      constructedTypeOffset
    };

    return { node, state };
  }

  /**
   * Parse a constant declaration with pre-collected specifiers.
   */
  private parseConstantDeclarationWithSpecifiers(state: ParserState, specifiers: AST.SpecifierList): ParseResult<AST.ConstantDeclaration> {
    state = state.skipTrivia();

    // Parse name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected identifier', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance().skipTrivia();

    // Check for := or :
    if (state.current()?.type === TokenType.OPERATOR) {
      if (state.current()?.content === ':=' || state.current()?.content === '⩴') {
        // Type-inferred constant with initializer
        const colonOffset = state.currentOffset();
        state = state.advance();
        const initResult = this.parseExpression(state);

        const node: AST.ConstantDeclaration = {
          type: 'ConstantDeclaration',
          name,
          nameOffset,
          specifiers,
          assignOffset: colonOffset,
          initializer: initResult.node
        };
        return { node, state: initResult.state };
      } else if (state.current()?.content === ':') {
        // Type-annotated constant
        const colonOffset = state.currentOffset();
        state = state.advance();

        // Parse type
        const typeResult = this.parseType(state);
        state = typeResult.state.skipTrivia();

        // Check for = initializer
        let initializer: AST.Expression | undefined;
        let equalsOffset: number | undefined;
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '=') {
          equalsOffset = state.currentOffset();
          state = state.advance();
          const initResult = this.parseExpression(state);
          initializer = initResult.node;
          state = initResult.state;
        }

        const node: AST.ConstantDeclaration = {
          type: 'ConstantDeclaration',
          name,
          nameOffset,
          specifiers,
          declaredType: typeResult.node,
          colonOffset,
          initializer,
          equalsOffset
        };
        return { node, state };
      }
    }

    throw new ParseError('Expected : or := after identifier', state.position, state.current() || undefined);
  }

  /**
   * Parse a function declaration.
   *
   * Grammar:
   *   func_decl = identifier specifiers? "(" params ")" specifiers? (":" type)? (":=" | "=") body
   *   params = (param ("," param)*)?
   *   param = identifier (":" type)?
   *   body = expression | indented_compound
   */
  parseFunctionDeclaration(state: ParserState, allowSignatureOnly: boolean = false): ParseResult<AST.FunctionDeclaration> {
    state = state.skipTrivia();

    // Parse name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected function name', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance();

    // Parse optional visibility specifier (separate visibility from other specifiers)
    let visibilitySpecifier: AST.SpecifierList | undefined;
    const visSpecResult = this.parseSpecifiers(state);
    if (visSpecResult) {
      // Separate visibility specifiers from other specifiers for logical AST
      const allSpecifiers = visSpecResult.node.specifiers;
      const visibilitySpecs = allSpecifiers.filter(s => DeclarationParser.VISIBILITY_SPECIFIERS.has(s));

      // If we have visibility specifiers, create a separate node for them
      if (visibilitySpecs.length > 0) {
        // Create a new SpecifierList with only visibility specifiers
        const visibilityOffsets = visSpecResult.node.specifierOffsets.filter((_, i) =>
          DeclarationParser.VISIBILITY_SPECIFIERS.has(allSpecifiers[i]));

        visibilitySpecifier = {
          type: 'SpecifierList',
          specifiers: visibilitySpecs,
          specifierOffsets: visibilityOffsets,
          openAngleOffset: visSpecResult.node.openAngleOffset,
          closeAngleOffset: visSpecResult.node.closeAngleOffset,
          separatorOffsets: visSpecResult.node.separatorOffsets
        };
      }

      state = visSpecResult.state;
    }

    state = state.skipTrivia();

    // Parse parameter list
    const openParenOffset = state.currentOffset();
    const openParen = state.current();
    if (!openParen || openParen.type !== TokenType.OPERATOR || openParen.content !== '(') {
      throw new ParseError('Expected ( after function name', state.position, openParen || undefined);
    }
    state = state.advance();

    // Parse parameters
    const { parameters, separatorOffsets, state: afterParams } = this.parseParameterList(state);
    state = afterParams;

    // Parse closing paren
    state = state.skipTrivia();
    const closeParenOffset = state.currentOffset();
    const closeParen = state.current();
    if (!closeParen || closeParen.type !== TokenType.OPERATOR || closeParen.content !== ')') {
      throw new ParseError('Expected )', state.position, closeParen || undefined);
    }
    state = state.advance();

    // Parse optional post-specifiers
    let postSpecifiers: AST.SpecifierList | undefined;
    const postSpecResult = this.parseSpecifiers(state);
    if (postSpecResult) {
      postSpecifiers = postSpecResult.node;
      state = postSpecResult.state;
    }

    state = state.skipTrivia();

    // Parse optional return type
    let returnType: AST.TypeExpression | undefined;
    let returnColonOffset: number | undefined;

    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      // Check if it's : followed by type (not :=)
      if (state.peek(1)?.type !== TokenType.OPERATOR || state.peek(1)?.content !== '=') {
        returnColonOffset = state.currentOffset();
        state = state.advance();

        const typeResult = this.parseType(state);
        returnType = typeResult.node;
        state = typeResult.state.skipTrivia();
      }
    }

    // Check if this is a constructor function
    let isConstructor = false;
    if (visibilitySpecifier) {
      isConstructor = visibilitySpecifier.specifiers.includes('constructor');
    }
    if (postSpecifiers && !isConstructor) {
      isConstructor = postSpecifiers.specifiers.includes('constructor');
    }

    // Parse := or =
    let assignOffset: number | undefined;
    let equalsOffset: number | undefined;

    const assignToken = state.current();
    if (assignToken && assignToken.type === TokenType.OPERATOR && assignToken.content === ':=') {
      assignOffset = state.currentOffset();
      state = state.advance();
    } else if (assignToken && assignToken.type === TokenType.OPERATOR && assignToken.content === '=') {
      equalsOffset = state.currentOffset();
      state = state.advance();
    } else if (allowSignatureOnly) {
      // For interface method signatures, body is optional
      const node: AST.FunctionDeclaration = {
        type: 'FunctionDeclaration',
        name,
        nameOffset,
        visibilitySpecifier,
        parameters,
        openParenOffset,
        closeParenOffset,
        paramSeparatorOffsets: separatorOffsets,
        postSpecifiers,
        returnType,
        returnColonOffset,
        assignOffset,
        equalsOffset,
        body: undefined as any // Interface methods have no body
      };
      return { node, state };
    } else {
      throw new ParseError('Expected := or = for function body', state.position, assignToken || undefined);
    }

    // Parse body (different logic for constructors vs regular functions)
    state = state.skipTrivia();
    let body: AST.Expression;
    let constructedType: string | undefined;
    let constructedTypeOffset: number | undefined;

    if (isConstructor) {
      // For constructors: parse class name followed by : and constructor body
      const classNameToken = state.current();
      if (!classNameToken || classNameToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected class name after := in constructor', state.position, classNameToken || undefined);
      }

      constructedType = classNameToken.content;
      constructedTypeOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Expect : after class name
      const colonToken = state.current();
      if (!colonToken || colonToken.type !== TokenType.OPERATOR || colonToken.content !== ':') {
        throw new ParseError('Expected : after class name in constructor', state.position, colonToken || undefined);
      }
      state = state.advance().skipTrivia();

      // Parse constructor body (can be compound expression with field initialization)
      const bodyResult = this.parseExpression(state);
      body = bodyResult.node;
      state = bodyResult.state;
    } else {
      // Regular function: parse body as normal
      const nextToken = state.current();
      if (nextToken && nextToken.type === TokenType.BLOCK_FORMING_KEYWORD) {
        const compoundResult = this.parseIdentedCompound(state);
        body = compoundResult.node;
        state = compoundResult.state;
      } else {
        // Regular expression body
        const bodyResult = this.parseExpression(state);
        body = bodyResult.node;
        state = bodyResult.state;
      }
    }

    const node: AST.FunctionDeclaration = {
      type: 'FunctionDeclaration',
      name,
      nameOffset,
      visibilitySpecifier,
      parameters,
      openParenOffset,
      closeParenOffset,
      paramSeparatorOffsets: separatorOffsets,
      postSpecifiers,
      returnType,
      returnColonOffset,
      assignOffset,
      equalsOffset,
      body,
      isConstructor: isConstructor || undefined,
      constructedType,
      constructedTypeOffset
    };

    return { node, state };
  }

  /**
   * Parse a parameter list.
   */
  private parseParameterList(state: ParserState): {
    parameters: AST.Parameter[],
    separatorOffsets: number[],
    state: ParserState
  } {
    const parameters: AST.Parameter[] = [];
    const separatorOffsets: number[] = [];

    state = state.skipTrivia();

    // Empty parameter list
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')') {
      return { parameters, separatorOffsets, state };
    }

    // Parse parameters
    while (true) {
      // Parse parameter name
      const nameOffset = state.currentOffset();
      const nameToken = state.current();
      if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected parameter name', state.position, nameToken || undefined);
      }

      state = state.advance().skipTrivia();

      // Check for optional type
      let paramType: AST.TypeExpression | undefined;
      let colonOffset: number | undefined;

      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
        colonOffset = state.currentOffset();
        state = state.advance();

        const typeResult = this.parseType(state);
        paramType = typeResult.node;
        state = typeResult.state;
      }

      const param: AST.Parameter = {
        type: 'Parameter',
        name: nameToken.content,
        nameOffset,
        paramType,
        colonOffset
      };
      parameters.push(param);

      state = state.skipTrivia();

      // Check for comma (more parameters)
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance().skipTrivia();

        // Check for trailing comma (comma followed by closing paren)
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')') {
          throw new ParseError('Trailing comma not allowed in parameter list', state.position, state.current() || undefined);
        }
      } else {
        // No more parameters
        break;
      }
    }

    return { parameters, separatorOffsets, state };
  }

  /**
   * Parse a specifier list.
   *
   * Grammar:
   *   specifiers = "<" identifier ">" ("<" identifier ">")*
   *
   * Examples:
   *   <public>
   *   <public><final>
   *   <public><override><final>
   */
  private parseSpecifierList(state: ParserState): ParseResult<AST.SpecifierList> {
    state = state.skipTrivia();

    const specifiers: string[] = [];
    const specifierOffsets: number[] = [];
    const separatorOffsets: number[] = []; // Not used for this format but kept for compatibility

    // Record the first < position
    const firstOpenAngleOffset = state.currentOffset();
    let lastCloseAngleOffset = firstOpenAngleOffset;

    // Parse one or more <specifier> groups
    while (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '<') {
      const openAngleOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse specifier identifier
      const specOffset = state.currentOffset();
      const specToken = state.current();
      if (!specToken || specToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected specifier', state.position, specToken || undefined);
      }

      specifiers.push(specToken.content);
      specifierOffsets.push(specOffset);
      state = state.advance().skipTrivia();

      // Parse closing >
      lastCloseAngleOffset = state.currentOffset();
      const closeAngle = state.current();
      if (!closeAngle || closeAngle.type !== TokenType.OPERATOR || closeAngle.content !== '>') {
        throw new ParseError('Expected >', state.position, closeAngle || undefined);
      }
      state = state.advance();

      // Skip trivia (including spaces) between specifiers
      state = state.skipTrivia();
    }

    const node: AST.SpecifierList = {
      type: 'SpecifierList',
      specifiers,
      specifierOffsets,
      openAngleOffset: firstOpenAngleOffset,
      closeAngleOffset: lastCloseAngleOffset,
      separatorOffsets
    };

    return { node, state };
  }

  /**
   * Parse a type expression.
   *
   * Supports:
   *   - Simple types: int, string
   *   - Optional types: ?int, ?string
   *   - Array types: []int, [][]string
   *   - Combined: ?[]int, ?[][]string
   */
  private parseType(state: ParserState): ParseResult<AST.TypeExpression> {
    return this.parseTypeWithModifiers(state);
  }

  private parseTypeWithModifiers(state: ParserState): ParseResult<AST.TypeExpression> {
    state = state.skipTrivia();

    // Check for optional modifier (?)
    let isOptional = false;
    let optionalOffset: number | undefined;
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '?') {
      isOptional = true;
      optionalOffset = state.currentOffset();
      state = state.advance().skipTrivia();
    }

    // Parse the base type (which might be a map or array type)
    const baseTypeResult = this.parseBaseType(state);
    let baseType = baseTypeResult.node;
    state = baseTypeResult.state;

    // Apply optional modifier if present
    if (isOptional) {
      baseType = {
        ...baseType,
        isOptional,
        optionalOffset
      };
    }

    return { node: baseType, state };
  }

  private parseBaseType(state: ParserState): ParseResult<AST.TypeExpression> {
    state = state.skipTrivia();

    // Check for array modifiers ([]) or map types ([keytype])
    let arrayDimensions = 0;
    const arrayOffsets: number[] = [];
    let mapKeyType: AST.TypeExpression | undefined;
    const mapBracketOffsets: number[] = [];

    // Look ahead to distinguish between array [] and map [keytype]
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '[') {
      const openBracketOffset = state.currentOffset();
      let lookaheadState = state.advance().skipTrivia();

      // Check if it's an empty bracket (array type) or has content (map type)
      if (lookaheadState.current()?.type === TokenType.OPERATOR && lookaheadState.current()?.content === ']') {
        // This is an array type: []
        arrayOffsets.push(openBracketOffset);
        state = lookaheadState.advance().skipTrivia();
        arrayDimensions++;

        // Continue checking for more array dimensions (but stop if we encounter a map type)
        while (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '[') {
          // Look ahead to see if this is another array dimension [] or a map type [keytype]
          const tempOpenOffset = state.currentOffset();
          let tempState = state.advance().skipTrivia();

          if (tempState.current()?.type === TokenType.OPERATOR && tempState.current()?.content === ']') {
            // This is another array dimension
            arrayOffsets.push(tempOpenOffset);
            state = tempState.advance().skipTrivia();
            arrayDimensions++;
          } else {
            // This is a map type, break out of the array dimension loop
            break;
          }
        }

        // After parsing array dimensions, check if there's a map type
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '[') {
          const mapOpenBracketOffset = state.currentOffset();
          let mapLookaheadState = state.advance().skipTrivia();

          // Check if this is a map type (not an empty array dimension)
          if (!(mapLookaheadState.current()?.type === TokenType.OPERATOR && mapLookaheadState.current()?.content === ']')) {
            // This is a map type: [keytype]
            mapBracketOffsets.push(mapOpenBracketOffset);
            state = mapLookaheadState;

            // Parse the key type
            const keyTypeResult = this.parseTypeWithModifiers(state);
            mapKeyType = keyTypeResult.node;
            state = keyTypeResult.state.skipTrivia();

            // Expect closing ]
            const mapCloseBracketOffset = state.currentOffset();
            if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== ']') {
              throw new ParseError('Expected ] after map key type', state.position, state.current() || undefined);
            }
            mapBracketOffsets.push(mapCloseBracketOffset);
            state = state.advance().skipTrivia();
          }
        }
      } else {
        // This is a map type: [keytype]
        mapBracketOffsets.push(openBracketOffset);
        state = lookaheadState;

        // Parse the key type
        const keyTypeResult = this.parseTypeWithModifiers(state);
        mapKeyType = keyTypeResult.node;
        state = keyTypeResult.state.skipTrivia();

        // Expect closing ]
        const closeBracketOffset = state.currentOffset();
        if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== ']') {
          throw new ParseError('Expected ] after map key type', state.position, state.current() || undefined);
        }
        mapBracketOffsets.push(closeBracketOffset);
        state = state.advance().skipTrivia();
      }
    }

    // For map types, we need to parse the value type recursively
    if (mapKeyType) {
      // Parse the value type (which could itself be a complex type with optional, arrays, etc.)
      const valueTypeResult = this.parseTypeWithModifiers(state);
      const valueType = valueTypeResult.node;
      state = valueTypeResult.state;

      // Create map type node by combining the map key with the value type
      // If we also have array dimensions, they apply to the whole map type
      const node: AST.TypeExpression = {
        type: 'TypeExpression',
        typeName: valueType.typeName,
        typeNameOffset: valueType.typeNameOffset,
        mapKeyType,
        mapBracketOffsets,
        ...(arrayDimensions > 0 && { arrayDimensions, arrayOffsets }),
        ...(valueType.isOptional && { isOptional: valueType.isOptional, optionalOffset: valueType.optionalOffset }),
        ...(valueType.arrayDimensions && {
          arrayDimensions: (arrayDimensions || 0) + valueType.arrayDimensions,
          arrayOffsets: [...(arrayOffsets || []), ...(valueType.arrayOffsets || [])]
        }),
        ...(valueType.typeParameters && { typeParameters: valueType.typeParameters, typeParameterOffsets: valueType.typeParameterOffsets })
      };

      return { node, state };
    }

    // Parse the base type name
    const typeNameOffset = state.currentOffset();
    const typeToken = state.current();
    // Accept IDENTIFIER or TYPE_KEYWORD as valid type names
    if (!typeToken || (typeToken.type !== TokenType.IDENTIFIER &&
                       typeToken.type !== TokenType.TYPE_KEYWORD)) {
      throw new ParseError('Expected type name', state.position, typeToken || undefined);
    }

    const typeName = typeToken.content;
    state = state.advance().skipTrivia();

    // Check for type{expression} construct
    let typeExpression: AST.Expression | undefined;
    let typeExpressionOffsets: number[] | undefined;

    if (typeName === 'type' && state.current()?.type === TokenType.OPERATOR && state.current()?.content === '{') {
      const openBraceOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse the function type expression inside type{...}
      // This could be a function signature like "_(:int)<transacts><decides> : void"
      const exprResult = this.parseFunctionTypeExpression(state);
      typeExpression = exprResult.node;
      state = exprResult.state.skipTrivia();

      // Expect closing }
      const closeBraceOffset = state.currentOffset();
      if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== '}') {
        throw new ParseError('Expected } after type expression', state.position, state.current() || undefined);
      }
      typeExpressionOffsets = [openBraceOffset, closeBraceOffset];
      state = state.advance().skipTrivia();
    }

    // Check for type parameters (e.g., weak_map(session, int) or option<int>)
    let typeParameters: AST.TypeExpression[] | undefined;
    let typeParameterOffsets: number[] | undefined;

    // Handle parenthesized type parameters like weak_map(session, int)
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '(') {
      state = state.advance().skipTrivia();
      typeParameters = [];
      typeParameterOffsets = [];

      while (state.current() && !(state.current()!.type === TokenType.OPERATOR && state.current()!.content === ')')) {
        // Parse each type parameter
        const paramResult = this.parseTypeWithModifiers(state);
        typeParameters.push(paramResult.node);
        state = paramResult.state.skipTrivia();

        // Check for comma separator
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
          typeParameterOffsets.push(state.currentOffset());
          state = state.advance().skipTrivia();
        }
      }

      // Expect closing )
      if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== ')') {
        throw new ParseError('Expected ) after type parameters', state.position, state.current() || undefined);
      }
      state = state.advance();
    }
    // Handle angle bracket type parameters like option<int>
    else if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '<') {
      state = state.advance().skipTrivia();
      typeParameters = [];
      typeParameterOffsets = [];

      while (state.current() && !(state.current()!.type === TokenType.OPERATOR && state.current()!.content === '>')) {
        // Parse each type parameter
        const paramResult = this.parseTypeWithModifiers(state);
        typeParameters.push(paramResult.node);
        state = paramResult.state.skipTrivia();

        // Check for comma separator
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
          typeParameterOffsets.push(state.currentOffset());
          state = state.advance().skipTrivia();
        }
      }

      // Expect closing >
      if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== '>') {
        throw new ParseError('Expected > after type parameters', state.position, state.current() || undefined);
      }
      state = state.advance();
    }

    // Parse optional where clause (e.g., "T where T:type")
    let whereConstraint: AST.WhereConstraint | undefined;
    let whereOffset: number | undefined;

    state = state.skipTrivia();
    if (state.current()?.type === TokenType.IDENTIFIER && state.current()?.content === 'where') {
      whereOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse constraint: parameter:constraint_type
      const parameterOffset = state.currentOffset();
      const parameterToken = state.current();
      if (!parameterToken || parameterToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected type parameter after where', state.position, parameterToken || undefined);
      }
      const parameter = parameterToken.content;
      state = state.advance().skipTrivia();

      // Expect colon
      const colonOffset = state.currentOffset();
      const colonToken = state.current();
      if (!colonToken || colonToken.type !== TokenType.OPERATOR || colonToken.content !== ':') {
        throw new ParseError('Expected : after type parameter in where clause', state.position, colonToken || undefined);
      }
      state = state.advance().skipTrivia();

      // Parse constraint type
      const constraintOffset = state.currentOffset();
      const constraintToken = state.current();
      if (!constraintToken || constraintToken.type !== TokenType.IDENTIFIER) {
        throw new ParseError('Expected constraint type after :', state.position, constraintToken || undefined);
      }
      const constraint = constraintToken.content;
      state = state.advance().skipTrivia();

      whereConstraint = {
        type: 'WhereConstraint',
        parameter,
        parameterOffset,
        constraint,
        constraintOffset,
        colonOffset
      };
    }

    const node: AST.TypeExpression = {
      type: 'TypeExpression',
      typeName,
      typeNameOffset,
      ...(arrayDimensions > 0 && { arrayDimensions, arrayOffsets }),
      ...(typeParameters && typeParameters.length > 0 && { typeParameters, typeParameterOffsets }),
      ...(typeExpression && { typeExpression, typeExpressionOffsets }),
      ...(whereConstraint && { whereConstraint, whereOffset })
    };

    return { node, state };
  }

  /**
   * Parse a direct data structure declaration (class Name: or class Name { }).
   *
   * Grammar:
   *   direct_ds_decl = (struct|class|interface|module|enum) name generics? (superclass)? (":" indented_body | "{" body "}")
   */

  /**
   * Parse a data structure declaration.
   *
   * Grammar:
   *   data_struct = identifier specifiers? ":=" kind specifiers? ("(" argument ")")? specifiers? ("{" body "}" | ":" indented_body)
   *   kind = "module" | "interface" | "class" | "struct" | "enum"
   *   body = declaration*
   */
  parseDataStructureDeclaration(state: ParserState): ParseResult<AST.DataStructureDeclaration> {
    state = state.skipTrivia();

    // Parse name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected identifier', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance();

    // Parse optional name specifiers
    let nameSpecifiers: AST.SpecifierList | undefined;
    const nameSpecResult = this.parseSpecifiers(state);
    if (nameSpecResult) {
      nameSpecifiers = nameSpecResult.node;
      state = nameSpecResult.state;
    }

    state = state.skipTrivia();

    // Parse :=
    const assignOffset = state.currentOffset();
    const assignToken = state.current();
    if (!assignToken || assignToken.type !== TokenType.OPERATOR || assignToken.content !== ':=') {
      throw new ParseError('Expected := after data structure name', state.position, assignToken || undefined);
    }
    state = state.advance().skipTrivia();

    // Parse kind
    const kindOffset = state.currentOffset();
    const kindToken = state.current();
    if (!kindToken || kindToken.type !== TokenType.DATA_STRUCTURE_KEYWORD) {
      throw new ParseError('Expected module, interface, class, struct, or enum', state.position, kindToken || undefined);
    }
    const kind = kindToken.content as 'module' | 'interface' | 'class' | 'struct' | 'enum';
    state = state.advance();

    // Parse optional kind specifiers
    let kindSpecifiers: AST.SpecifierList | undefined;
    const kindSpecResult = this.parseSpecifiers(state);
    if (kindSpecResult) {
      kindSpecifiers = kindSpecResult.node;
      state = kindSpecResult.state;
    }

    // Parse optional argument (e.g., parent class)
    let argument: AST.Expression | undefined;
    let openParenOffset: number | undefined;
    let closeParenOffset: number | undefined;

    state = state.skipTrivia();
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '(') {
      openParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Check if we have an argument or empty parentheses
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ')') {
        // Empty parentheses - no argument
        closeParenOffset = state.currentOffset();
        state = state.advance();
      } else {
        // Parse the argument expression
        const argResult = this.parseExpression(state);
        argument = argResult.node;
        state = argResult.state.skipTrivia();

        closeParenOffset = state.currentOffset();
        const closeParen = state.current();
        if (!closeParen || closeParen.type !== TokenType.OPERATOR || closeParen.content !== ')') {
          throw new ParseError('Expected )', state.position, closeParen || undefined);
        }
        state = state.advance();
      }
    }

    // Parse optional post specifiers
    let postSpecifiers: AST.SpecifierList | undefined;
    const postSpecResult = this.parseSpecifiers(state);
    if (postSpecResult) {
      postSpecifiers = postSpecResult.node;
      state = postSpecResult.state;
    }

    state = state.skipTrivia();

    // Parse body (braced or indented)
    let body: AST.Declaration[] = [];
    let bodySeparatorOffsets: number[] = [];
    let openBraceOffset: number | undefined;
    let closeBraceOffset: number | undefined;
    let colonOffset: number | undefined;

    const bodyToken = state.current();

    if (bodyToken && bodyToken.type === TokenType.OPERATOR && bodyToken.content === '{') {
      // Braced body
      openBraceOffset = state.currentOffset();
      state = state.advance();

      // Special handling for enum bodies (simple member list)
      if (kind === 'enum') {
        const enumResult = this.parseEnumMemberList(state);
        body = enumResult.members;
        bodySeparatorOffsets = enumResult.separatorOffsets;
        state = enumResult.state.skipTrivia();
      } else {
        const bodyResult = this.parseDeclarationList(state, '}', kind);
        body = bodyResult.declarations;
        bodySeparatorOffsets = bodyResult.separatorOffsets;
        state = bodyResult.state.skipTrivia();
      }

      closeBraceOffset = state.currentOffset();
      const closeBrace = state.current();
      if (!closeBrace || closeBrace.type !== TokenType.OPERATOR || closeBrace.content !== '}') {
        throw new ParseError('Expected }', state.position, closeBrace || undefined);
      }
      state = state.advance();

    } else if (bodyToken && bodyToken.type === TokenType.OPERATOR && bodyToken.content === ':') {
      // Indented body
      colonOffset = state.currentOffset();
      state = state.advance();

      // Look ahead to find indentation
      const nextLineIndent = state.getNextLineIndentation();
      if (nextLineIndent !== null) {
        state = state.enterIndentationContext(nextLineIndent);

        // Skip to first declaration
        state = state.skipTrivia();
        while (state.current()?.type === TokenType.NEWLINE) {
          state = state.advance().skipTrivia();
        }

        // Special handling for enum bodies (simple member list)
        if (kind === 'enum') {
          const enumResult = this.parseIndentedEnumMemberList(state, nextLineIndent);
          body = enumResult.members;
          bodySeparatorOffsets = enumResult.separatorOffsets;
          state = enumResult.state;
        } else {
          const bodyResult = this.parseIndentedDeclarationList(state, nextLineIndent, kind);
          body = bodyResult.declarations;
          bodySeparatorOffsets = bodyResult.separatorOffsets;
          state = bodyResult.state;
        }

        state = state.exitIndentationContext();
      }
    } else {
      throw new ParseError('Expected { or : for data structure body', state.position, bodyToken || undefined);
    }

    const node: AST.DataStructureDeclaration = {
      type: 'DataStructureDeclaration',
      name,
      nameOffset,
      nameSpecifiers,
      assignOffset,
      kind,
      kindOffset,
      kindSpecifiers,
      argument,
      openParenOffset,
      closeParenOffset,
      postSpecifiers,
      body,
      openBraceOffset,
      closeBraceOffset,
      colonOffset,
      bodySeparatorOffsets
    };

    return { node, state };
  }

  /**
   * Parse a list of declarations within braces.
   */
  private parseDeclarationList(state: ParserState, endToken: string, kind?: string): {
    declarations: AST.Declaration[],
    separatorOffsets: number[],
    state: ParserState
  } {
    const declarations: AST.Declaration[] = [];
    const separatorOffsets: number[] = [];

    while (!state.isAtEnd()) {
      state = state.skipTrivia();

      // Check for end token
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === endToken) {
        break;
      }

      // Parse declaration
      const declResult = this.parseDeclaration(state, kind ? { kind } : undefined);
      declarations.push(declResult.node);
      state = declResult.state;

      // Skip trailing trivia
      state = state.skipTrivia();

      // Check for separator (newline, semicolon, or comma)
      if (state.current()?.type === TokenType.NEWLINE) {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      } else if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ';') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      } else if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      }
    }

    return { declarations, separatorOffsets, state };
  }

  /**
   * Parse an indented list of declarations.
   */
  private parseIndentedDeclarationList(state: ParserState, indentLevel: number, kind?: string): {
    declarations: AST.Declaration[],
    separatorOffsets: number[],
    state: ParserState
  } {
    const declarations: AST.Declaration[] = [];
    const separatorOffsets: number[] = [];


    // Skip initial newline to get to first declaration line
    if (state.current()?.type === TokenType.NEWLINE) {
      state = state.advance();
    }

    // Skip trivia (spaces/tabs) which acts as indentation
    while (state.current()?.type === TokenType.TRIVIA ||
           state.current()?.type === TokenType.SPACE ||
           state.current()?.type === TokenType.TAB) {
      state = state.advance();
    }

    while (!state.isAtEnd()) {
      // Check if still indented
      const currentToken = state.current();
      if (!currentToken || currentToken.position.column < indentLevel) {
        break;
      }

      // Parse declaration
      const declResult = this.parseDeclaration(state, kind ? { kind } : undefined);
      declarations.push(declResult.node);
      state = declResult.state;

      // Skip trailing trivia
      state = state.skipTrivia();

      // Check for comma separator (multiple declarations on same line)
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance().skipTrivia();
        // Continue parsing next declaration on same line
        continue;
      }

      // Check for newline separator
      if (state.current()?.type === TokenType.NEWLINE) {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();

        // Skip additional newlines and whitespace (blank lines)
        while (state.current()?.type === TokenType.NEWLINE ||
               state.current()?.type === TokenType.SPACE ||
               state.current()?.type === TokenType.TAB ||
               state.current()?.type === TokenType.TRIVIA ||
               state.current()?.type === TokenType.COMMENT ||
               state.current()?.type === TokenType.MULTILINE_COMMENT) {
          state = state.advance();
        }
      } else {
        // No separator, we're done
        break;
      }
    }

    return { declarations, separatorOffsets, state };
  }

  /**
   * Parse specifiers in any format (SPECIFIER tokens or <> syntax)
   */
  private parseSpecifiers(state: ParserState): ParseResult<AST.SpecifierList> | null {
    // Skip trivia to handle spaces like "f <public> ()"
    state = state.skipTrivia();

    if (state.current()?.type === TokenType.SPECIFIER) {
      // Parse one or more consecutive SPECIFIER tokens
      const specifiers: string[] = [];
      const specifierOffsets: number[] = [];

      // Record the first specifier position
      const firstSpecifierOffset = state.currentOffset();
      let lastSpecifierOffset = firstSpecifierOffset;

      // Parse all consecutive SPECIFIER tokens
      while (state.current()?.type === TokenType.SPECIFIER) {
        const specifierOffset = state.currentOffset();
        const specifierToken = state.current()!;

        // Extract specifier content from <specifier> format
        const content = specifierToken.content;
        const specifierName = content.slice(1, -1); // Remove < and >

        specifiers.push(specifierName);
        specifierOffsets.push(specifierOffset);
        lastSpecifierOffset = specifierOffset;

        state = state.advance();
        // Skip trivia between specifiers to allow spaces
        state = state.skipTrivia();
      }

      const node: AST.SpecifierList = {
        type: 'SpecifierList',
        specifiers,
        specifierOffsets,
        openAngleOffset: firstSpecifierOffset,
        closeAngleOffset: lastSpecifierOffset,
        separatorOffsets: []
      };

      return { node, state };
    } else if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '<') {
      return this.parseSpecifierList(state);
    }
    return null;
  }

  /**
   * Parse a list of enum members within braces.
   *
   * Grammar:
   *   enum_members = member ("," member)* ","?
   *   member = identifier specifiers? ("=" expression)?
   */
  private parseEnumMemberList(state: ParserState): {
    members: AST.EnumMember[],
    separatorOffsets: number[],
    state: ParserState
  } {
    const members: AST.EnumMember[] = [];
    const separatorOffsets: number[] = [];

    while (!state.isAtEnd()) {
      state = state.skipTrivia();

      // Check for closing brace
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}') {
        break;
      }

      // Parse enum member
      const memberResult = this.parseEnumMember(state);
      members.push(memberResult.node);
      state = memberResult.state.skipTrivia();

      // Check for comma
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance().skipTrivia();

        // Allow trailing comma
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '}') {
          break;
        }
      } else if (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== '}') {
        // If no comma and not closing brace, error
        throw new ParseError('Expected , or } in enum body', state.position, state.current() || undefined);
      }
    }

    // Enums must have at least one member
    if (members.length === 0) {
      throw new ParseError('Enum must have at least one member', state.position);
    }

    return { members, separatorOffsets, state };
  }

  /**
   * Parse indented enum members.
   */
  private parseIndentedEnumMemberList(state: ParserState, indentLevel: number): {
    members: AST.EnumMember[],
    separatorOffsets: number[],
    state: ParserState
  } {
    const members: AST.EnumMember[] = [];
    const separatorOffsets: number[] = [];

    while (!state.isAtEnd()) {
      // Check if we've dedented
      const currentIndent = state.currentIndentationLevel;
      if (currentIndent !== null && currentIndent < indentLevel) {
        break;
      }

      state = state.skipTrivia();

      // Skip empty lines
      if (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance();
        continue;
      }

      // Parse enum member
      const memberResult = this.parseEnumMember(state);
      members.push(memberResult.node);
      state = memberResult.state;

      // Check for comma or newline
      state = state.skipTrivia();
      if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
        separatorOffsets.push(state.currentOffset());
        state = state.advance();
      }

      // Skip to next line
      state = state.skipTrivia();
      if (state.current()?.type === TokenType.NEWLINE) {
        state = state.advance();
      }
    }

    // Enums must have at least one member
    if (members.length === 0) {
      throw new ParseError('Enum must have at least one member', state.position);
    }

    return { members, separatorOffsets, state };
  }

  /**
   * Parse a single enum member.
   *
   * Grammar:
   *   enum_member = identifier specifiers? ("=" expression)?
   */
  private parseEnumMember(state: ParserState): ParseResult<AST.EnumMember> {
    state = state.skipTrivia();

    // Parse member name
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER) {
      throw new ParseError('Expected enum member name', state.position, nameToken || undefined);
    }
    const name = nameToken.content;
    state = state.advance();

    // Parse optional specifiers
    let specifiers: AST.SpecifierList | undefined;
    const specResult = this.parseSpecifiers(state);
    if (specResult) {
      specifiers = specResult.node;
      state = specResult.state;
    }

    // Parse optional value
    let value: AST.Expression | undefined;
    let equalsOffset: number | undefined;

    state = state.skipTrivia();
    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '=') {
      equalsOffset = state.currentOffset();
      state = state.advance();

      const valueResult = this.parseExpression(state);
      value = valueResult.node;
      state = valueResult.state;
    }

    const node: AST.EnumMember = {
      type: 'EnumMember',
      name,
      nameOffset,
      specifiers,
      value,
      equalsOffset
    };

    return { node, state };
  }

  /**
   * Parse function type expressions inside type{} constructs.
   *
   * Function type expressions have the syntax:
   *   _(...params) <specifiers> : returnType
   *
   * For example:
   *   _(:int)<transacts><decides> : void
   *   _() : string
   *   __(:t, :t)<computes><decides>:void
   */
  private parseFunctionTypeExpression(state: ParserState): ParseResult<AST.Expression> {
    state = state.skipTrivia();

    // Check if this looks like a function type signature (starts with _ or __)
    const firstToken = state.current();
    if (firstToken && firstToken.type === TokenType.IDENTIFIER &&
        (firstToken.content === '_' || firstToken.content === '__')) {

      try {
        return this.parseFunctionTypeSignature(state);
      } catch (error) {
        // Fall back to regular expression parsing
        const resetState = state;
        try {
          return this.parseExpression(resetState);
        } catch {
          // Create a placeholder if both approaches fail
          return this.createFunctionTypePlaceholder(state);
        }
      }
    }

    // Try parsing as a regular expression first for non-function patterns
    try {
      return this.parseExpression(state);
    } catch (error) {
      return this.createFunctionTypePlaceholder(state);
    }
  }

  /**
   * Parse a function type signature like _(:int, :string)<decides> : void
   */
  private parseFunctionTypeSignature(state: ParserState): ParseResult<AST.Expression> {
    state = state.skipTrivia();

    // Parse function name (_ or __)
    const nameOffset = state.currentOffset();
    const nameToken = state.current();
    if (!nameToken || nameToken.type !== TokenType.IDENTIFIER ||
        (nameToken.content !== '_' && nameToken.content !== '__')) {
      throw new ParseError('Expected _ or __ for function type', state.position, nameToken || undefined);
    }
    const functionName = nameToken.content;
    state = state.advance().skipTrivia();

    // Parse parameter list
    let parameters: AST.Parameter[] = [];
    let openParenOffset: number | undefined;
    let closeParenOffset: number | undefined;
    let paramSeparatorOffsets: number[] = [];

    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === '(') {
      openParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      // Parse parameters
      while (state.current() &&
             !(state.current()!.type === TokenType.OPERATOR && state.current()!.content === ')')) {

        // Parse parameter - expecting :type format
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
          const colonOffset = state.currentOffset();
          state = state.advance().skipTrivia();

          // Parse parameter type
          const paramTypeResult = this.parseType(state);

          // Create parameter with anonymous name
          const param: AST.Parameter = {
            type: 'Parameter',
            name: '_', // Anonymous parameter
            nameOffset: colonOffset,
            paramType: paramTypeResult.node,
            colonOffset
          };

          parameters.push(param);
          state = paramTypeResult.state.skipTrivia();
        } else {
          // Handle regular parameter names if present
          const paramNameToken = state.current();
          if (paramNameToken && paramNameToken.type === TokenType.IDENTIFIER) {
            const paramNameOffset = state.currentOffset();
            state = state.advance().skipTrivia();

            let colonOffset: number | undefined;
            let paramType: AST.TypeExpression | undefined;

            if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
              colonOffset = state.currentOffset();
              state = state.advance().skipTrivia();

              const paramTypeResult = this.parseType(state);
              paramType = paramTypeResult.node;
              state = paramTypeResult.state.skipTrivia();
            }

            const param: AST.Parameter = {
              type: 'Parameter',
              name: paramNameToken.content,
              nameOffset: paramNameOffset,
              paramType,
              colonOffset
            };

            parameters.push(param);
          } else {
            throw new ParseError('Expected parameter name or :type', state.position, paramNameToken || undefined);
          }
        }

        // Check for comma separator
        if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ',') {
          paramSeparatorOffsets.push(state.currentOffset());
          state = state.advance().skipTrivia();
        } else {
          break;
        }
      }

      // Expect closing )
      if (!state.current() || state.current()!.type !== TokenType.OPERATOR || state.current()!.content !== ')') {
        throw new ParseError('Expected ) after parameters', state.position, state.current() || undefined);
      }
      closeParenOffset = state.currentOffset();
      state = state.advance().skipTrivia();
    }

    // Parse specifiers (effects like <decides>, <transacts>, etc.)
    let postSpecifiers: AST.SpecifierList | undefined;
    if (state.current()?.type === TokenType.SPECIFIER) {
      const specifiersResult = this.parseSpecifierList(state);
      postSpecifiers = specifiersResult.node;
      state = specifiersResult.state.skipTrivia();
    }

    // Parse return type
    let returnType: AST.TypeExpression | undefined;
    let returnColonOffset: number | undefined;

    if (state.current()?.type === TokenType.OPERATOR && state.current()?.content === ':') {
      returnColonOffset = state.currentOffset();
      state = state.advance().skipTrivia();

      const returnTypeResult = this.parseType(state);
      returnType = returnTypeResult.node;
      state = returnTypeResult.state;
    }

    // Create a function declaration node to represent the function type
    const functionTypeNode: AST.FunctionDeclaration = {
      type: 'FunctionDeclaration',
      name: functionName,
      nameOffset,
      parameters,
      openParenOffset: openParenOffset ?? nameOffset, // Use nameOffset as fallback
      closeParenOffset: closeParenOffset ?? nameOffset, // Use nameOffset as fallback
      paramSeparatorOffsets,
      ...(postSpecifiers && { postSpecifiers }),
      ...(returnType && { returnType }),
      ...(returnColonOffset !== undefined && { returnColonOffset }),
      body: undefined as any, // Function types don't have bodies
      assignOffset: undefined,
      equalsOffset: undefined
    };

    return { node: functionTypeNode as any, state };
  }

  /**
   * Create a placeholder for unparseable function types
   */
  private createFunctionTypePlaceholder(state: ParserState): ParseResult<AST.Expression> {
    const placeholderExpr: AST.Expression = {
      type: 'Identifier',
      name: 'UnparsedFunctionType',
      nameOffset: state.currentOffset()
    } as any;

    // Consume tokens until we find the closing }
    while (!state.isAtEnd() &&
           (state.current()?.type !== TokenType.OPERATOR || state.current()?.content !== '}')) {
      state = state.advance();
    }

    return { node: placeholderExpr, state };
  }

  /**
   * Check if the current state looks like a type alias pattern.
   *
   * Type aliases typically start with:
   * - Array syntax: [], [][], [key]
   * - Tuple syntax: tuple(...)
   * - Optional syntax: ?type
   * - Type keyword: type{...}
   * - Known type keywords: int, float, string, void, etc.
   *
   * This helps avoid parsing object constructors (Point{...}) as types.
   */
  private looksLikeTypeAlias(state: ParserState): boolean {
    state = state.skipTrivia();
    const token = state.current();

    if (!token) return false;

    // Starts with array syntax []
    if (token.type === TokenType.OPERATOR && token.content === '[') {
      return true;
    }

    // Starts with optional syntax ?
    if (token.type === TokenType.OPERATOR && token.content === '?') {
      return true;
    }

    // Starts with known type keywords
    if (token.type === TokenType.TYPE_KEYWORD) {
      return true;
    }

    // Check for specific type identifiers
    if (token.type === TokenType.IDENTIFIER) {
      const typeKeywords = ['tuple', 'type', 'int', 'float', 'string', 'void', 'logic', 'comparable'];
      if (typeKeywords.includes(token.content)) {
        return true;
      }

      // Look ahead to see if it's followed by parentheses (like tuple(...))
      let lookahead = state.advance().skipTrivia();
      if (lookahead.current()?.type === TokenType.OPERATOR && lookahead.current()?.content === '(') {
        return true;
      }
    }

    // Default to false - parse as expression first
    return false;
  }
}