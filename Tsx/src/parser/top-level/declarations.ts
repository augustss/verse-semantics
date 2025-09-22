/**
 * Top-level declaration parsers (using, module, class)
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { keyword } from '../foundation/tokens';
import { trivia } from '../foundation/trivia';
import { variable, identifier } from '../literals/identifiers';
import { stringLiteral } from '../literals/strings';
import { leftBrace, rightBrace, leftParen, rightParen, assignOp, colon, comma, dot, semicolon } from '../operators/punctuation';
import { modularExpr } from '../expressions/core';
import { decorators as parseDecorators } from '../decorators/decorators';
import { parseIndentedStatements, statementsToBody } from '../statements/shared-indented';

/**
 * Check if we have indented content after assignment operator for method bodies
 */
const hasIndentedContent = (state: PC.ParserState): boolean => {
  let pos = state.position;

  // Skip spaces and tabs (but not newlines)
  while (pos < state.input.length && (state.input[pos] === ' ' || state.input[pos] === '\t')) {
    pos++;
  }

  // Check if we hit a newline (indicating indented content follows)
  return pos < state.input.length && (state.input[pos] === '\n' || state.input[pos] === '\r');
};

/**
 * Parse method body, handling both single expressions and indented multi-statement bodies
 */
const parseMethodBody = (state: PC.ParserState): PC.ParserResult<AST.Expr> => {
  // Check if we have indented content
  if (hasIndentedContent(state)) {
    // Use indented statement parsing like block: expressions
    const indentedResult = parseIndentedStatements(state, () => modularExpr);
    if (indentedResult.success) {
      // Convert statements to appropriate body
      const body = statementsToBody(
        indentedResult.value,
        state.position,
        indentedResult.state.position
      );
      return {
        success: true,
        value: body,
        state: indentedResult.state
      };
    }
  }

  // Fall back to single expression parsing
  return modularExpr(state);
};

/**
 * Recursively check if an expression contains a class expression (nested class)
 */
const containsClassExpression = (expr: AST.Expr): boolean => {
  if (expr.type === 'ClassExpression') {
    return true;
  }

  // Check common expression types that can contain other expressions
  switch (expr.type) {
    case 'Assignment':
      return containsClassExpression(expr.value);
    case 'Block':
      const blockExpr = expr as AST.Block;
      return blockExpr.statements.some((stmt: any) => containsClassExpression(stmt));
    case 'BinaryOp':
      return containsClassExpression(expr.left) || containsClassExpression(expr.right);
    case 'UnaryOp':
      return containsClassExpression(expr.operand);
    case 'Parenthesized':
      return containsClassExpression(expr.expr);
    case 'FunctionCall':
      return expr.args.some(arg => containsClassExpression(arg));
    case 'MemberAccess':
      return containsClassExpression(expr.object);
    case 'IfExpression':
      const ifExpr = expr as AST.IfExpression;
      return containsClassExpression(ifExpr.condition) ||
             containsClassExpression(ifExpr.thenBody) ||
             (ifExpr.elseClause?.elseBody ? containsClassExpression(ifExpr.elseClause.elseBody) : false);
    case 'ForExpression':
      const forExpr = expr as AST.ForExpression;
      return containsClassExpression(forExpr.iterable) || containsClassExpression(forExpr.body);
    case 'CaseExpression':
      const caseExpr = expr as AST.CaseExpression;
      return containsClassExpression(caseExpr.expr) ||
             caseExpr.branches.some((branch: any) => branch.body ? containsClassExpression(branch.body) : false);
    case 'ConstDeclaration':
      return containsClassExpression(expr.value);
    case 'FunctionDeclaration':
      return containsClassExpression(expr.body);
    default:
      return false;
  }
};

/**
 * Parse a module path like /Fortnite.com/Devices, /Verse.org/Simulation/Tags, std, or math/complex
 */
const modulePath: PC.Parser<AST.Token<string>> = (state) => {
  const startPos = state.position;

  // Parse leading trivia
  const triviaResult = trivia(state);
  let currentState = triviaResult.state;
  const leadingTrivia = triviaResult.success ? triviaResult.value : '';

  if (currentState.position >= currentState.input.length) {
    return { success: false, error: 'Expected module path', state };
  }

  let pathPattern: RegExp;
  if (currentState.input[currentState.position] === '/') {
    // Absolute path - starts with '/' like /Domain.com/Module/Submodule
    pathPattern = /^\/[a-zA-Z0-9._/-]+/;
  } else {
    // Relative path - like std, math/complex, or io/file
    pathPattern = /^[a-zA-Z][a-zA-Z0-9._/-]*/;
  }

  const remaining = currentState.input.slice(currentState.position);
  const match = remaining.match(pathPattern);

  if (!match) {
    return { success: false, error: 'Invalid module path format', state };
  }

  const pathText = match[0];
  const endPos = currentState.position + pathText.length;

  // Don't parse trailing trivia here - let the next token handle it
  const finalState = { ...currentState, position: endPos };

  const token: AST.Token<string> = {
    text: leadingTrivia + pathText,
    value: pathText,
    trivia: { leading: leadingTrivia, trailing: '' },
    span: { start: startPos, end: endPos }
  };

  return {
    success: true,
    value: token,
    state: finalState
  };
};

/**
 * Parse a using statement: using { /path/to/module }
 */
export const usingStatement: PC.Parser<AST.UsingStatement> = (state) => {
  const startPos = state.position;

  // Parse 'using' keyword
  const usingResult = keyword('using')(state);
  if (!usingResult.success) return usingResult;

  // Parse opening brace
  const lbraceResult = leftBrace(usingResult.state);
  if (!lbraceResult.success) return lbraceResult;

  // Parse module path
  const pathResult = modulePath(lbraceResult.state);
  if (!pathResult.success) return pathResult;

  // Parse closing brace
  const rbraceResult = rightBrace(pathResult.state);
  if (!rbraceResult.success) return rbraceResult;

  return {
    success: true,
    value: AST.usingStatement(
      usingResult.value as AST.Token<'using'>,
      lbraceResult.value,
      pathResult.value,
      rbraceResult.value,
      { start: startPos, end: rbraceResult.state.position }
    ),
    state: rbraceResult.state
  };
};

/**
 * Parse a field declaration: [@decorator] fieldName : type = value
 */
export const fieldDeclaration: PC.Parser<AST.FieldDeclaration> = (state) => {
  const startPos = state.position;

  // Parse optional decorators first
  const decoratorsResult = parseDecorators(state);
  let currentState = decoratorsResult.success ? decoratorsResult.state : state;
  const fieldDecorators = decoratorsResult.success ? decoratorsResult.value : [];

  // Parse optional var keyword
  let varKeyword: AST.Token<'var'> | undefined;
  const varResult = keyword('var')(currentState);
  if (varResult.success) {
    varKeyword = varResult.value as AST.Token<'var'>;
    currentState = varResult.state;
  }

  // Parse field name
  const nameResult = variable(currentState);
  if (!nameResult.success) return nameResult;

  // Parse colon
  const colonResult = colon(nameResult.state);
  if (!colonResult.success) return colonResult;

  // Parse type (can be simple identifier or complex type like event(player))
  const typeResult = modularExpr(colonResult.state);
  if (!typeResult.success) return typeResult;

  // Check for assignment
  const assignResult = assignOp(typeResult.state);
  if (!assignResult.success) {
    // No initializer
    return {
      success: true,
      value: AST.fieldDeclaration(
        nameResult.value.token,
        colonResult.value,
        typeResult.value,
        undefined,
        undefined,
        { start: startPos, end: typeResult.state.position },
        fieldDecorators,
        varKeyword
      ),
      state: typeResult.state
    };
  }

  // Parse initializer
  const initResult = modularExpr(assignResult.state);
  if (!initResult.success) return initResult;

  return {
    success: true,
    value: AST.fieldDeclaration(
      nameResult.value.token,
      colonResult.value,
      typeResult.value,
      assignResult.value,
      initResult.value,
      { start: startPos, end: initResult.state.position },
      fieldDecorators,
      varKeyword
    ),
    state: initResult.state
  };
};

/**
 * Parse a method signature (for interfaces): MethodName(params):return_type
 */
export const methodSignature: PC.Parser<AST.MethodDeclaration> = (state) => {
  const startPos = state.position;

  // Parse method name
  const nameResult = variable(state);
  if (!nameResult.success) return nameResult;

  // Parse parameters
  const lparenResult = leftParen(nameResult.state);
  if (!lparenResult.success) return lparenResult;

  // Parse parameter list (simplified - just parse expressions for now)
  const params: AST.FunctionParam[] = [];
  const commas: AST.Token<','>[] = [];
  let currentState = lparenResult.state;

  // Check for empty parameter list
  const rparenCheck = rightParen(currentState);
  if (!rparenCheck.success) {
    // Parse parameters
    while (true) {
      // Try to parse a parameter (name : type)
      const paramNameResult = variable(currentState);
      if (!paramNameResult.success) break;

      // Check for colon and type
      const colonResult = colon(paramNameResult.state);
      let paramType: AST.Expr | undefined;
      let afterParam = paramNameResult.state;

      if (colonResult.success) {
        const typeResult = modularExpr(colonResult.state);
        if (typeResult.success) {
          paramType = typeResult.value;
          afterParam = typeResult.state;
        }
      }

      params.push(AST.functionParam(
        paramNameResult.value.token,
        colonResult.success ? colonResult.value : undefined,
        paramType,
        { start: paramNameResult.value.span.start, end: afterParam.position }
      ));

      currentState = afterParam;

      // Check for comma
      const commaResult = comma(currentState);
      if (commaResult.success) {
        commas.push(commaResult.value);
        currentState = commaResult.state;
      } else {
        break;
      }
    }
  }

  const rparenResult = rightParen(currentState);
  if (!rparenResult.success) return rparenResult;

  // Parse return type
  const colonResult = colon(rparenResult.state);
  let returnType: AST.Expr | undefined;
  let afterReturn = rparenResult.state;

  if (colonResult.success) {
    const typeResult = modularExpr(colonResult.state);
    if (typeResult.success) {
      returnType = typeResult.value;
      afterReturn = typeResult.state;
    }
  }

  // For method signatures, we create a placeholder assignment and empty body
  const placeholderAssign = AST.token('=' as const, '=', { leading: '', trailing: '' }, { start: afterReturn.position, end: afterReturn.position }) as AST.Token<'=' | ':='>;
  const emptyBody = AST.emptyExpression({ start: afterReturn.position, end: afterReturn.position });

  return {
    success: true,
    value: AST.methodDeclaration(
      nameResult.value.token,
      lparenResult.value,
      params,
      commas,
      rparenResult.value,
      colonResult.success ? colonResult.value : undefined,
      returnType,
      placeholderAssign,
      emptyBody,
      { start: startPos, end: afterReturn.position }
    ),
    state: afterReturn
  };
};

/**
 * Parse a method declaration: MethodName(params):return_type = body
 */
export const methodDeclaration: PC.Parser<AST.MethodDeclaration> = (state) => {
  const startPos = state.position;

  // For now, let's try to parse as a function declaration first using modularExpr
  // This should handle specifiers correctly if they're implemented in the expression parser
  const exprResult = modularExpr(state);
  if (exprResult.success && exprResult.value.type === 'FunctionDeclaration') {
    const func = exprResult.value as AST.FunctionDeclaration;
    // Convert FunctionDeclaration to MethodDeclaration
    const method: AST.MethodDeclaration = {
      type: 'MethodDeclaration',
      decorators: undefined,
      name: func.name,
      preSpecifiers: func.specifiers,
      leftParen: func.leftParen,
      params: func.params,
      rightParen: func.rightParen,
      postSpecifiers: func.postParenSpecifiers,
      colon: func.colon,
      returnType: func.returnType ? AST.variable(func.returnType, func.returnType.span) : undefined,
      assignOp: func.assignOp,
      body: func.body,
      semicolon: undefined,
      span: func.span
    };
    return {
      success: true,
      value: method,
      state: exprResult.state
    };
  }

  // Fallback to manual parsing if modularExpr doesn't work
  // Parse method name
  const nameResult = variable(state);
  if (!nameResult.success) return nameResult;

  // Parse parameters
  const lparenResult = leftParen(nameResult.state);
  if (!lparenResult.success) return lparenResult;

  // Parse parameter list (simplified - just parse expressions for now)
  const params: AST.FunctionParam[] = [];
  const commas: AST.Token<','>[] = [];
  let currentState = lparenResult.state;

  // Check for empty parameter list
  const rparenCheck = rightParen(currentState);
  if (!rparenCheck.success) {
    // Parse parameters
    while (true) {
      // Try to parse a parameter (name : type)
      const paramNameResult = variable(currentState);
      if (!paramNameResult.success) break;

      // Check for colon and type
      const colonResult = colon(paramNameResult.state);
      let paramType: AST.Expr | undefined;
      let afterParam = paramNameResult.state;

      if (colonResult.success) {
        const typeResult = modularExpr(colonResult.state);
        if (typeResult.success) {
          paramType = typeResult.value;
          afterParam = typeResult.state;
        }
      }

      params.push(AST.functionParam(
        paramNameResult.value.token,
        colonResult.success ? colonResult.value : undefined,
        paramType,
        { start: paramNameResult.value.span.start, end: afterParam.position }
      ));

      currentState = afterParam;

      // Check for comma
      const commaResult = comma(currentState);
      if (commaResult.success) {
        commas.push(commaResult.value);
        currentState = commaResult.state;
      } else {
        break;
      }
    }
  }

  const rparenResult = rightParen(currentState);
  if (!rparenResult.success) return rparenResult;

  // Parse return type
  const colonResult = colon(rparenResult.state);
  let returnType: AST.Expr | undefined;
  let afterReturn = rparenResult.state;

  if (colonResult.success) {
    const typeResult = modularExpr(colonResult.state);
    if (typeResult.success) {
      returnType = typeResult.value;
      afterReturn = typeResult.state;
    }
  }

  // Parse assignment operator
  const assignResult = assignOp(afterReturn);
  if (!assignResult.success) return assignResult;

  // Parse method body (handles both single expressions and indented multi-statement bodies)
  const bodyResult = parseMethodBody(assignResult.state);
  if (!bodyResult.success) return bodyResult;

  return {
    success: true,
    value: AST.methodDeclaration(
      nameResult.value.token,
      lparenResult.value,
      params,
      commas,
      rparenResult.value,
      colonResult.success ? colonResult.value : undefined,
      returnType,
      assignResult.value,
      bodyResult.value,
      { start: startPos, end: bodyResult.state.position }
    ),
    state: bodyResult.state
  };
};

/**
 * Parse enum members (simple identifiers)
 */
const parseEnumMembers = (state: PC.ParserState, isIndentationStyle: boolean = false): PC.ParserResult<AST.EnumMember[]> => {
  const members: AST.EnumMember[] = [];
  let currentState = state;
  let consecutiveFailures = 0;

  while (currentState.position < currentState.input.length) {
    const startPos = currentState.position;

    // Capture leading trivia
    const triviaResult = trivia(currentState);
    let leadingTrivia = '';
    if (triviaResult.success) {
      leadingTrivia = triviaResult.value;
      currentState = triviaResult.state;
    }

    // Check for closing brace (end of body) - only for brace-style enums
    if (!isIndentationStyle) {
      const rbraceCheck = rightBrace(currentState);
      if (rbraceCheck.success) {
        break;
      }
    }

    // For indentation style, check for dedented content to end the enum
    if (isIndentationStyle) {
      // Simple heuristic: if we hit a line that doesn't start with whitespace, stop
      const currentLine = currentState.input.slice(currentState.position);
      const newlineIndex = currentLine.indexOf('\n');
      if (newlineIndex !== -1) {
        const nextLine = currentState.input.slice(currentState.position + newlineIndex + 1);
        if (nextLine.length > 0 && !nextLine.match(/^\s/)) {
          // Next line doesn't start with whitespace, end of enum
          break;
        }
      }
    }

    // Try to parse an identifier (enum value)
    const identifierResult = identifier(currentState);
    if (identifierResult.success) {
      const memberToken = AST.token(
        identifierResult.value,
        identifierResult.value,
        { leading: leadingTrivia, trailing: '' },
        { start: startPos, end: identifierResult.state.position }
      );
      const member = AST.enumMember(
        memberToken,
        { start: startPos, end: identifierResult.state.position }
      );

      members.push(member);
      currentState = identifierResult.state;
      consecutiveFailures = 0;
      continue;
    }

    // If we can't parse anything and haven't made progress, increment failure counter
    if (currentState.position === startPos) {
      consecutiveFailures++;
      if (consecutiveFailures >= 10) {
        // Too many consecutive failures, stop parsing
        break;
      }
      // Try to advance one character to avoid infinite loop
      currentState = { ...currentState, position: currentState.position + 1 };
    } else {
      consecutiveFailures = 0;
    }
  }

  return {
    success: true,
    value: members,
    state: currentState
  };
};

/**
 * Parse class/module body members
 */
const parseMembers = (state: PC.ParserState, isIndentationStyle: boolean = false, isInterface: boolean = false): PC.ParserResult<AST.ClassMember[]> => {
  const members: AST.ClassMember[] = [];
  let currentState = state;
  let consecutiveFailures = 0;


  while (currentState.position < currentState.input.length) {
    const startPos = currentState.position;

    // Skip trivia
    const triviaResult = trivia(currentState);
    if (triviaResult.success) {
      currentState = triviaResult.state;
    }

    // Check for closing brace (end of body) - only for brace-style classes
    if (!isIndentationStyle) {
      const rbraceCheck = rightBrace(currentState);
      if (rbraceCheck.success) {
        break;
      }
    }

    // For indentation style, we'll just try to parse members directly
    // TODO: Add proper indentation detection later

    // Skip nested declarations for now (they're not ClassMembers)
    // TODO: Handle nested classes/modules properly

    // Try to parse a method declaration (or signature for interfaces)
    let methodResult: PC.ParserResult<AST.MethodDeclaration>;
    if (isInterface) {
      // For interfaces, try signature first, then full method declaration
      methodResult = methodSignature(currentState);
      if (!methodResult.success) {
        methodResult = methodDeclaration(currentState);
      }
    } else {
      // For classes, try full method declaration first
      methodResult = methodDeclaration(currentState);
    }

    if (methodResult.success) {
      let member = methodResult.value;
      let afterMember = methodResult.state;

      // Check for optional semicolon in brace style
      if (!isIndentationStyle) {
        const semicolonResult = semicolon(afterMember);
        if (semicolonResult.success) {
          member = { ...member, semicolon: semicolonResult.value };
          afterMember = semicolonResult.state;
        }
      }

      members.push(member);
      currentState = afterMember;
      consecutiveFailures = 0;
      continue;
    }

    // Try to parse a field declaration
    const fieldResult = fieldDeclaration(currentState);
    if (fieldResult.success) {
      let member = fieldResult.value;
      let afterMember = fieldResult.state;

      // Check for optional semicolon in brace style
      if (!isIndentationStyle) {
        const semicolonResult = semicolon(afterMember);
        if (semicolonResult.success) {
          member = { ...member, semicolon: semicolonResult.value };
          afterMember = semicolonResult.state;
        }
      }

      members.push(member);
      currentState = afterMember;
      consecutiveFailures = 0;
      continue;
    }

    // Try to parse general expressions (const declarations, assignments, etc.)
    const exprResult = modularExpr(currentState);
    if (exprResult.success) {
      const expr = exprResult.value;

      // Reject expressions containing nested class declarations - classes cannot be nested inside other classes
      if (containsClassExpression(expr)) {
        // This expression contains a nested class, which is not allowed
        break;
      }

      if (expr.type === 'ConstDeclaration') {
        // Convert ConstDeclaration to FieldDeclaration
        const constDecl = expr as AST.ConstDeclaration;
        let field: AST.FieldDeclaration = {
          type: 'FieldDeclaration',
          decorators: undefined,
          varKeyword: undefined,
          name: constDecl.name,
          colon: constDecl.colon,
          typeAnnotation: constDecl.typeName ? AST.variable(constDecl.typeName, constDecl.typeName.span) : undefined,
          assignOp: constDecl.assignOp,
          initializer: constDecl.value,
          semicolon: undefined,
          span: constDecl.span
        };
        let afterMember = exprResult.state;

        // Check for optional semicolon in brace style
        if (!isIndentationStyle) {
          const semicolonResult = semicolon(afterMember);
          if (semicolonResult.success) {
            field = { ...field, semicolon: semicolonResult.value };
            afterMember = semicolonResult.state;
          }
        }

        members.push(field);
        currentState = afterMember;
        consecutiveFailures = 0;
        continue;
      } else if (expr.type === 'FunctionDeclaration') {
        // Convert FunctionDeclaration to MethodDeclaration
        const func = expr as AST.FunctionDeclaration;
        let method: AST.MethodDeclaration = {
          type: 'MethodDeclaration',
          decorators: undefined,
          name: func.name,
          preSpecifiers: undefined,
          leftParen: func.leftParen,
          params: func.params,
          rightParen: func.rightParen,
          postSpecifiers: undefined,
          colon: func.colon,
          returnType: func.returnType ? AST.variable(func.returnType, { start: func.returnType.span.start, end: func.returnType.span.end }) : undefined,
          assignOp: func.assignOp,
          body: func.body,
          semicolon: undefined,
          span: func.span
        };
        let afterMember = exprResult.state;

        // Check for optional semicolon in brace style
        if (!isIndentationStyle) {
          const semicolonResult = semicolon(afterMember);
          if (semicolonResult.success) {
            method = { ...method, semicolon: semicolonResult.value };
            afterMember = semicolonResult.state;
          }
        }

        members.push(method);
        currentState = afterMember;
        consecutiveFailures = 0;
        continue;
      } else if (expr.type === 'Assignment') {
        // Convert Assignment to FieldDeclaration
        const assignment = expr as AST.Assignment;

        if (assignment.target.type === 'Variable') {
          const variable = assignment.target as AST.Variable;
          let field: AST.FieldDeclaration = {
            type: 'FieldDeclaration',
            decorators: undefined,
            varKeyword: undefined,
            name: variable.token,
            colon: undefined,
            typeAnnotation: undefined,
            assignOp: assignment.assignOp,
            initializer: assignment.value,
            semicolon: undefined,
            span: assignment.span
          };
          let afterMember = exprResult.state;

          // Check for optional semicolon in brace style
          if (!isIndentationStyle) {
            const semicolonResult = semicolon(afterMember);
            if (semicolonResult.success) {
              field = { ...field, semicolon: semicolonResult.value };
              afterMember = semicolonResult.state;
            }
          }

          members.push(field);
          currentState = afterMember;
          consecutiveFailures = 0;
          continue;
        }
      }
      // Error: expression is not a valid class member
      return { success: false, error: `Invalid class member: ${expr.type}`, state: currentState };
    }

    // Nothing matched
    if (currentState.position === startPos) {
      // We didn't advance at all
      consecutiveFailures++;
      if (consecutiveFailures > 3) {
        // Too many failures, give up
        break;
      }
      // Skip one character and try again
      currentState = { ...currentState, position: currentState.position + 1 };
    } else {
      // We consumed trivia but didn't match a member
      // In indentation style, this might just be blank lines, continue
      if (isIndentationStyle) {
        continue;
      } else {
        break;
      }
    }
  }

  return {
    success: true,
    value: members,
    state: currentState
  };
};

/**
 * Unified parser for class, interface, and struct declarations
 */
const parseTypeDeclaration = (declarationType: 'class' | 'interface' | 'struct'): PC.Parser<AST.TopLevelDeclaration> => (state) => {
  const startPos = state.position;
  const isInterface = declarationType === 'interface';

  // Parse optional decorators first
  const decoratorsResult = parseDecorators(state);
  let currentState = decoratorsResult.success ? decoratorsResult.state : state;
  const typeDecorators = decoratorsResult.success ? decoratorsResult.value : [];

  // Check for new syntax: type Name { ... } (only valid for class and struct, not interface)
  if (declarationType !== 'interface') {
    const typeKeywordResult = keyword(declarationType)(currentState);
    if (typeKeywordResult.success) {
      // New syntax
      const nameResult = variable(typeKeywordResult.state);
      if (nameResult.success) {
        // Check for inheritance
        let leftParenInherit: AST.Token<'('> | undefined;
        let parentClass: AST.Token<string> | undefined;
        let rightParenInherit: AST.Token<')'> | undefined;
        let bodyState = nameResult.state;

        const lparenResult = leftParen(bodyState);
        if (lparenResult.success) {
          leftParenInherit = lparenResult.value;
          const parentResult = variable(lparenResult.state);
          if (parentResult.success) {
            parentClass = parentResult.value.token;
            const rparenResult = rightParen(parentResult.state);
            if (rparenResult.success) {
              rightParenInherit = rparenResult.value;
              bodyState = rparenResult.state;
            }
          }
        }

        // Parse opening brace
        const lbraceResult = leftBrace(bodyState);
        if (lbraceResult.success) {
          // Parse members
          const membersResult = parseMembers(lbraceResult.state, false, isInterface);
          if (membersResult.success) {
            const rbraceResult = rightBrace(membersResult.state);
            if (rbraceResult.success) {
            return {
              success: true,
              value: AST.topLevelDeclaration(
                declarationType,
                nameResult.value.token,
                { text: '', value: ':=', trivia: { leading: '', trailing: '' }, span: { start: 0, end: 0 } } as AST.Token<':='>,
                typeKeywordResult.value as AST.Token<string>,
                undefined, // colon
                leftParenInherit,
                parentClass,
                rightParenInherit,
                lbraceResult.value,
                rbraceResult.value,
                membersResult.value as unknown as AST.ClassMember[],
                { start: startPos, end: rbraceResult.state.position },
                typeDecorators
              ),
              state: rbraceResult.state
            };
            }
          }
        }
      }
    }
  }

  // Old syntax: Name := type...
  const nameResult = variable(currentState);
  if (!nameResult.success) return { success: false, error: `Expected ${declarationType} declaration`, state };

  const assignResult = assignOp(nameResult.state);
  if (!assignResult.success) return { success: false, error: `Expected := after ${declarationType} name`, state };

  // Parse keyword with optional annotation (e.g., interface<abstract>)
  const typeResult = keyword(declarationType)(assignResult.state);
  if (!typeResult.success) return { success: false, error: `Expected ${declarationType} keyword`, state };

  let annotationState = typeResult.state;
  let keywordWithAnnotation = typeResult.value;

  // Check for annotations after the type keyword (e.g., interface<abstract>)
  const leftAngleResult = PC.char('<')(annotationState);
  if (leftAngleResult.success) {
    // Parse the annotation content until >
    const annotationContentResult = PC.regex(/^[^>]+/)(leftAngleResult.state);
    if (annotationContentResult.success) {
      const rightAngleResult = PC.char('>')(annotationContentResult.state);
      if (rightAngleResult.success) {
        // Include the annotation in the keyword token's text
        keywordWithAnnotation = {
          ...typeResult.value,
          text: typeResult.value.text + '<' + annotationContentResult.value + '>',
          value: typeResult.value.value + '<' + annotationContentResult.value + '>'
        };
        annotationState = rightAngleResult.state;
      }
    }
  }

  // Check for inheritance
  let leftParenInherit: AST.Token<'('> | undefined;
  let parentClass: AST.Token<string> | undefined;
  let rightParenInherit: AST.Token<')'> | undefined;
  let inheritanceState = annotationState;

  const lparenResult = leftParen(inheritanceState);
  if (lparenResult.success) {
    leftParenInherit = lparenResult.value;

    // Try to parse parent type
    const parentResult = variable(lparenResult.state);
    if (parentResult.success) {
      parentClass = parentResult.value.token;
      inheritanceState = parentResult.state;
    } else {
      // No parent type, could be empty parens
      inheritanceState = lparenResult.state;
    }

    // Parse closing paren
    const rparenResult = rightParen(inheritanceState);
    if (rparenResult.success) {
      rightParenInherit = rparenResult.value;
      inheritanceState = rparenResult.state;
    }
  }

  // Check for colon (indentation style) or brace
  const colonResult = colon(inheritanceState);
  if (colonResult.success) {
    // Indentation style - parse members after the colon
    const membersResult = parseMembers(colonResult.state, true, isInterface);
    const members = membersResult.success ? membersResult.value : [];
    const finalState = membersResult.success ? membersResult.state : colonResult.state;

    return {
      success: true,
      value: AST.topLevelDeclaration(
        declarationType,
        nameResult.value.token,
        assignResult.value as AST.Token<':='>,
        keywordWithAnnotation as AST.Token<string>,
        colonResult.value,
        leftParenInherit,
        parentClass,
        rightParenInherit,
        undefined,
        undefined,
        members,
        { start: startPos, end: finalState.position },
        typeDecorators
      ),
      state: finalState
    };
  }

  // Brace style
  const lbraceResult = leftBrace(inheritanceState);
  if (lbraceResult.success) {
    const membersResult = parseMembers(lbraceResult.state, false, isInterface);
    if (membersResult.success) {
      const rbraceResult = rightBrace(membersResult.state);
      if (rbraceResult.success) {
      return {
        success: true,
        value: AST.topLevelDeclaration(
          declarationType,
          nameResult.value.token,
          assignResult.value as AST.Token<':='>,
          keywordWithAnnotation as AST.Token<string>,
          undefined,
          leftParenInherit,
          parentClass,
          rightParenInherit,
          lbraceResult.value,
          rbraceResult.value,
          membersResult.value as AST.ClassMember[],
          { start: startPos, end: rbraceResult.state.position },
          typeDecorators
        ),
        state: rbraceResult.state
      };
      }
    }
  }

  // Fallback case for interfaces and structs: with parentheses but no body (empty type)
  if ((declarationType === 'interface' || declarationType === 'struct') && leftParenInherit && rightParenInherit) {
    return {
      success: true,
      value: AST.topLevelDeclaration(
        declarationType,
        nameResult.value.token,
        assignResult.value as AST.Token<':='>,
        keywordWithAnnotation as AST.Token<string>,
        undefined, // colon
        leftParenInherit,
        parentClass,
        rightParenInherit,
        undefined, // leftBrace
        undefined, // rightBrace
        [],
        { start: startPos, end: inheritanceState.position },
        typeDecorators
      ),
      state: inheritanceState
    };
  }

  // Error: type declaration requires a body (either : or { })
  return { success: false, error: `Expected : or { after ${declarationType} declaration`, state: inheritanceState };
};

/**
 * Parse a class declaration
 */
export const classDeclaration: PC.Parser<AST.TopLevelDeclaration> = parseTypeDeclaration('class');

/**
 * Parse a module declaration
 */
export const moduleDeclaration: PC.Parser<AST.TopLevelDeclaration> = (state) => {
  const startPos = state.position;

  // Parse name
  const nameResult = variable(state);
  if (!nameResult.success) return { success: false, error: 'Expected module name', state };

  // Parse :=
  const assignResult = assignOp(nameResult.state);
  if (!assignResult.success) return { success: false, error: 'Expected := after module name', state };

  // Parse 'module' keyword
  const moduleResult = keyword('module')(assignResult.state);
  if (!moduleResult.success) return { success: false, error: 'Expected module keyword', state };

  // Check for colon (indentation style) or brace
  const colonResult = colon(moduleResult.state);
  if (colonResult.success) {
    // Indentation style - parse as empty for now
    return {
      success: true,
      value: AST.topLevelDeclaration(
        'module',
        nameResult.value.token,
        assignResult.value as AST.Token<':='>,
        moduleResult.value as AST.Token<string>,
        colonResult.value,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        [],
        { start: startPos, end: colonResult.state.position }
      ),
      state: colonResult.state
    };
  }

  // Brace style
  const lbraceResult = leftBrace(moduleResult.state);
  if (lbraceResult.success) {
    const membersResult = parseMembers(lbraceResult.state);
    if (membersResult.success) {
      const rbraceResult = rightBrace(membersResult.state);
      if (rbraceResult.success) {
        return {
          success: true,
          value: AST.topLevelDeclaration(
            'module',
            nameResult.value.token,
            assignResult.value as AST.Token<':='>,
            moduleResult.value as AST.Token<string>,
            undefined, // colon
            undefined, // leftParenInherit
            undefined, // parentClass
            undefined, // rightParenInherit
            lbraceResult.value,
            rbraceResult.value,
            membersResult.value as unknown as AST.ClassMember[],
            { start: startPos, end: rbraceResult.state.position }
          ),
          state: rbraceResult.state
        };
      }
    }
  }

  return { success: false, error: 'Expected : or { after module', state: moduleResult.state };
};

/**
 * Parse an interface declaration
 */
export const interfaceDeclaration: PC.Parser<AST.TopLevelDeclaration> = parseTypeDeclaration('interface');

/**
 * Parse a struct declaration
 */
export const structDeclaration: PC.Parser<AST.TopLevelDeclaration> = parseTypeDeclaration('struct');

/**
 * Parse an enum declaration (similar to class but with 'enum' keyword)
 */
export const enumDeclaration: PC.Parser<AST.TopLevelDeclaration> = (state) => {
  const startPos = state.position;

  // Check for new syntax: enum Name { ... }
  const enumKeywordResult = keyword('enum')(state);
  if (enumKeywordResult.success) {
    // New syntax
    const nameResult = variable(enumKeywordResult.state);
    if (nameResult.success) {
      // Enums don't have inheritance, but might have empty parens
      let leftParenInherit: AST.Token<'('> | undefined;
      let rightParenInherit: AST.Token<')'> | undefined;
      let currentState = nameResult.state;

      const lparenResult = leftParen(currentState);
      if (lparenResult.success) {
        leftParenInherit = lparenResult.value;
        // Parse closing paren (enums don't inherit)
        const rparenResult = rightParen(lparenResult.state);
        if (rparenResult.success) {
          rightParenInherit = rparenResult.value;
          currentState = rparenResult.state;
        }
      }

      // Parse opening brace
      const lbraceResult = leftBrace(currentState);
      if (lbraceResult.success) {
        // Parse members (enum values)
        const membersResult = parseEnumMembers(lbraceResult.state);
        if (membersResult.success) {
          const rbraceResult = rightBrace(membersResult.state);
          if (rbraceResult.success) {
            return {
              success: true,
              value: AST.topLevelDeclaration(
                'enum',
                nameResult.value.token,
                { text: '', value: ':=', trivia: { leading: '', trailing: '' }, span: { start: 0, end: 0 } } as AST.Token<':='>,
                enumKeywordResult.value as AST.Token<string>,
                undefined, // colon
                leftParenInherit,
                undefined, // parentClass
                rightParenInherit,
                lbraceResult.value,
                rbraceResult.value,
                membersResult.value as (AST.ClassMember | AST.EnumMember)[],
                { start: startPos, end: rbraceResult.state.position }
              ),
              state: rbraceResult.state
            };
          }
        }
      }
    }
  }

  // Old syntax: Name := enum...
  const nameResult = variable(state);
  if (!nameResult.success) return { success: false, error: 'Expected enum declaration', state };

  const assignResult = assignOp(nameResult.state);
  if (!assignResult.success) return { success: false, error: 'Expected := after enum name', state };

  const enumResult = keyword('enum')(assignResult.state);
  if (!enumResult.success) return { success: false, error: 'Expected enum keyword', state };

  // Enums don't have inheritance, but might have empty parens
  let leftParenInherit: AST.Token<'('> | undefined;
  let rightParenInherit: AST.Token<')'> | undefined;
  let currentState = enumResult.state;

  const lparenResult = leftParen(currentState);
  if (lparenResult.success) {
    leftParenInherit = lparenResult.value;
    // Parse closing paren (enums don't inherit)
    const rparenResult = rightParen(lparenResult.state);
    if (rparenResult.success) {
      rightParenInherit = rparenResult.value;
      currentState = rparenResult.state;
    }
  }

  // Check for colon (indentation style) or brace
  const colonResult = colon(currentState);
  if (colonResult.success) {
    // Indentation style - parse members after the colon
    const membersResult = parseEnumMembers(colonResult.state, true);
    const members = membersResult.success ? membersResult.value : [];
    const finalState = membersResult.success ? membersResult.state : colonResult.state;

    return {
      success: true,
      value: AST.topLevelDeclaration(
        'enum',
        nameResult.value.token,
        assignResult.value as AST.Token<':='>,
        enumResult.value as AST.Token<string>,
        colonResult.value,
        leftParenInherit,
        undefined, // parentClass
        rightParenInherit,
        undefined, // leftBrace
        undefined, // rightBrace
        members as (AST.ClassMember | AST.EnumMember)[],
        { start: startPos, end: finalState.position }
      ),
      state: finalState
    };
  }

  // Brace style
  const lbraceResult = leftBrace(currentState);
  if (lbraceResult.success) {
    const membersResult = parseEnumMembers(lbraceResult.state);
    if (membersResult.success) {
      const rbraceResult = rightBrace(membersResult.state);
      if (rbraceResult.success) {
        return {
          success: true,
          value: AST.topLevelDeclaration(
            'enum',
            nameResult.value.token,
            assignResult.value as AST.Token<':='>,
            enumResult.value as AST.Token<string>,
            undefined, // colon
            leftParenInherit,
            undefined, // parentClass
            rightParenInherit,
            lbraceResult.value,
            rbraceResult.value,
            membersResult.value as (AST.ClassMember | AST.EnumMember)[],
            { start: startPos, end: rbraceResult.state.position }
          ),
          state: rbraceResult.state
        };
      }
    }
  }

  // Fallback case: enum with parentheses but no body (empty enum)
  if (leftParenInherit && rightParenInherit) {
    return {
      success: true,
      value: AST.topLevelDeclaration(
        'enum',
        nameResult.value.token,
        assignResult.value as AST.Token<':='>,
        enumResult.value as AST.Token<string>,
        undefined, // colon
        leftParenInherit,
        undefined, // parentClass
        rightParenInherit,
        undefined, // leftBrace
        undefined, // rightBrace
        [],
        { start: startPos, end: currentState.position }
      ),
      state: currentState
    };
  }

  return { success: false, error: 'Expected : or { after enum', state: currentState };
};

/**
 * Parse a constant declaration: name := value
 */
export const constDeclaration: PC.Parser<AST.ConstDeclaration> = (state) => {
  // Try to parse as an assignment and convert to const declaration
  const exprResult = modularExpr(state);
  if (exprResult.success && exprResult.value.type === 'Assignment') {
    const assignment = exprResult.value as AST.Assignment;
    // Only convert if target is a simple variable
    if (assignment.target.type === 'Variable') {
      const variable = assignment.target as AST.Variable;
      const constDecl: AST.ConstDeclaration = {
        type: 'ConstDeclaration',
        name: variable.token,
        assignOp: assignment.assignOp,
        value: assignment.value,
        span: assignment.span
      };
      return {
        success: true,
        value: constDecl,
        state: exprResult.state
      };
    }
  }
  return { success: false, error: 'Expected constant declaration', state };
};

/**
 * Parse a function declaration: name(params) := body
 */
export const functionDeclaration: PC.Parser<AST.FunctionDeclaration> = (state) => {
  // Try to parse as a function declaration using the existing expression parser
  const exprResult = modularExpr(state);
  if (exprResult.success && exprResult.value.type === 'FunctionDeclaration') {
    return {
      success: true,
      value: exprResult.value,
      state: exprResult.state
    };
  }
  return { success: false, error: 'Expected function declaration', state };
};

/**
 * Parse control flow expressions that can be top-level (for loops, if statements, etc.)
 */
const controlFlowExpression: PC.Parser<AST.Expr> = (state) => {
  // Try to parse using the modular expression parser and check if it's a control flow expression
  const exprResult = modularExpr(state);
  if (exprResult.success) {
    const expr = exprResult.value;
    // Only accept specific control flow expression types at top-level
    if (expr.type === 'ForExpression' ||
        expr.type === 'IfExpression' ||
        expr.type === 'Block' ||
        expr.type === 'CaseExpression') {
      return exprResult;
    }
  }
  return { success: false, error: 'Not a control flow expression', state };
};

/**
 * Parse any top-level declaration
 */
export const topLevelDeclaration: PC.Parser<AST.TopLevelDeclaration | AST.FunctionDeclaration | AST.ConstDeclaration | AST.Expr> = (state) => {
  // Try structured declarations first
  const structuredResult = PC.choice(
    classDeclaration,
    moduleDeclaration,
    interfaceDeclaration,
    structDeclaration,
    enumDeclaration
  )(state);

  if (structuredResult.success) {
    return structuredResult;
  }

  // Try function declaration
  const funcResult = functionDeclaration(state);
  if (funcResult.success) {
    return funcResult;
  }

  // Try control flow expressions (for loops, if statements) before constant declarations
  const controlFlowResult = controlFlowExpression(state);
  if (controlFlowResult.success) {
    return controlFlowResult;
  }

  // Try constant declaration
  const constResult = constDeclaration(state);
  if (constResult.success) {
    return constResult;
  }

  return { success: false, error: 'Expected top-level declaration', state };
};