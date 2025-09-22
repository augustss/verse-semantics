/**
 * Class expression parsing
 */

import * as PC from '../../parser-combinators';
import * as AST from '../../ast';
import { withTriviaLiteral } from '../foundation/tokens';
import { variable } from '../literals/identifiers';
import { leftParen, rightParen, leftBrace, rightBrace, colon, assignOp, semicolon } from '../operators/punctuation';
import { decorators as parseDecorators, setExprParser as setDecoratorExprParser } from '../decorators/decorators';
import { fieldDeclaration } from '../top-level/declarations';

// Helper function to recursively check for nested class expressions in an AST node
const containsNestedClass = (expr: AST.Expr): boolean => {
  // console.log('DEBUG: Checking for nested class in:', expr.type);
  switch (expr.type) {
    case 'ClassExpression':
      return true;
    case 'Block':
      return expr.statements?.some(stmt => stmt.expr ? containsNestedClass(stmt.expr) : false) || false;
    case 'Assignment':
      return containsNestedClass(expr.value) || (expr.target.type !== 'Variable' && containsNestedClass(expr.target as AST.Expr));
    case 'BinaryOp':
      return containsNestedClass(expr.left) || containsNestedClass(expr.right);
    case 'UnaryOp':
      return containsNestedClass(expr.operand);
    case 'FunctionCall':
      return expr.args?.some(arg => containsNestedClass(arg)) || false;
    case 'Application':
      return containsNestedClass(expr.func) || expr.args?.some(arg => containsNestedClass(arg)) || false;
    case 'IfExpression':
      return containsNestedClass(expr.condition) ||
             containsNestedClass(expr.thenBody) ||
             (expr.elseClause?.elseBody ? containsNestedClass(expr.elseClause.elseBody) : false) ||
             (expr.elseClause?.elseIf ? containsNestedClass(expr.elseClause.elseIf) : false);
    case 'ForExpression':
      return containsNestedClass(expr.iterable) || containsNestedClass(expr.body);
    case 'CaseExpression':
      return containsNestedClass(expr.expr) || expr.branches?.some(branch => branch.body ? containsNestedClass(branch.body) : false) || false;
    case 'ArrayConstruction':
      return expr.elements?.some(elem => containsNestedClass(elem)) || false;
    case 'ObjectConstruction':
      return expr.fields?.some(field => containsNestedClass(field.value)) || false;
    case 'Parenthesized':
      return containsNestedClass(expr.expr);
    default:
      return false;
  }
};

// We'll get the expression parser passed in via a getter to avoid circular dependencies
let getExpr: () => PC.Parser<AST.Expr>;

export const setExprParser = (exprParser: () => PC.Parser<AST.Expr>) => {
  getExpr = exprParser;
  setDecoratorExprParser(exprParser);
};

// Parse class members (fields and methods)
const parseClassMembers = (state: PC.ParserState): PC.ParserResult<AST.ClassMember[]> => {
  const members: AST.ClassMember[] = [];
  let currentState = state;

  while (true) {
    // Skip whitespace
    const wsResult = PC.optional(PC.regex(/^[\s\n\r]*/m))(currentState);
    if (wsResult.success) {
      currentState = wsResult.state;
    }

    // Check for closing brace
    if (currentState.position < currentState.input.length &&
        currentState.input[currentState.position] === '}') {
      break;
    }

    // Try to parse decorators first
    const decoratorsResult = parseDecorators(currentState);
    let memberDecorators: AST.Decorator[] = [];
    let stateAfterDecorators = currentState;
    if (decoratorsResult.success) {
      memberDecorators = decoratorsResult.value;
      stateAfterDecorators = decoratorsResult.state;

      // Skip whitespace after decorators
      const wsResult2 = PC.optional(PC.regex(/^[\s\n\r]*/m))(stateAfterDecorators);
      if (wsResult2.success) {
        stateAfterDecorators = wsResult2.state;
      }
    }

    // Try to parse a field declaration first
    const fieldResult = fieldDeclaration(stateAfterDecorators);
    if (fieldResult.success) {
      const field = fieldResult.value;
      // Add decorators to the field if we have any
      if (memberDecorators.length > 0) {
        field.decorators = memberDecorators;
      }
      members.push(field);
      currentState = fieldResult.state;
      continue;
    }

    // Try to parse a member (field assignment or method) using general expression parser
    if (!getExpr) {
      // If we parsed decorators but can't parse a member, that's an error
      if (memberDecorators.length > 0) {
        return { success: false, error: 'Expected member declaration after decorators', state: stateAfterDecorators };
      }
      break;
    }

    const memberResult = getExpr()(stateAfterDecorators);
    if (!memberResult.success) {
      // If we parsed decorators but can't parse a member, that's an error
      if (memberDecorators.length > 0) {
        return { success: false, error: 'Expected member declaration after decorators', state: stateAfterDecorators };
      }
      break;
    }

    currentState = memberResult.state;

    // Check for optional semicolon first
    let semicolonToken: AST.Token<';'> | undefined;
    const semicolonResult = semicolon(currentState);
    if (semicolonResult.success) {
      semicolonToken = semicolonResult.value;
      currentState = semicolonResult.state;
    }

    // Convert expression to ClassMember if possible
    const expr = memberResult.value;

    // Reject nested class expressions (including deeply nested ones)
    if (containsNestedClass(expr)) {
      return { success: false, error: 'Nested class declarations are not allowed', state: currentState };
    }

    if ((expr as any).type === 'FieldDeclaration') {
      const field = expr as any as AST.FieldDeclaration;
      members.push({ ...field, semicolon: semicolonToken });
    } else if ((expr as any).type === 'MethodDeclaration') {
      const method = expr as any as AST.MethodDeclaration;
      members.push({ ...method, semicolon: semicolonToken });
    } else if (expr.type === 'ConstDeclaration') {
      // Convert ConstDeclaration to FieldDeclaration
      const constDecl = expr as AST.ConstDeclaration;
      const field: AST.FieldDeclaration = {
        type: 'FieldDeclaration',
        decorators: memberDecorators.length > 0 ? memberDecorators : undefined,
        varKeyword: undefined,
        name: constDecl.name,
        colon: constDecl.colon,
        typeAnnotation: constDecl.typeName ? AST.variable(constDecl.typeName, constDecl.typeName.span) : undefined,
        assignOp: constDecl.assignOp,
        initializer: constDecl.value,
        semicolon: semicolonToken,
        span: constDecl.span
      };
      members.push(field);
    } else if (expr.type === 'FunctionDeclaration') {
      // Convert FunctionDeclaration to MethodDeclaration
      const func = expr as AST.FunctionDeclaration;
      const method: AST.MethodDeclaration = {
        type: 'MethodDeclaration',
        decorators: memberDecorators.length > 0 ? memberDecorators : undefined,
        name: func.name,
        preSpecifiers: func.specifiers && func.specifiers.length > 0 ? func.specifiers : undefined,
        leftParen: func.leftParen,
        params: func.params,
        rightParen: func.rightParen,
        postSpecifiers: func.postParenSpecifiers && func.postParenSpecifiers.length > 0 ? func.postParenSpecifiers : undefined,
        colon: func.colon,
        returnType: func.returnType ? AST.variable(func.returnType, { start: func.returnType.span.start, end: func.returnType.span.end }) : undefined,
        assignOp: func.assignOp,
        body: func.body,
        semicolon: semicolonToken,
        span: func.span
      };
      members.push(method);
    } else if (expr.type === 'Assignment') {
      // Convert Assignment to FieldDeclaration
      const assignment = expr as AST.Assignment;

      if (assignment.target.type === 'Variable') {
        const field: AST.FieldDeclaration = {
          type: 'FieldDeclaration',
          decorators: memberDecorators.length > 0 ? memberDecorators : undefined,
          varKeyword: undefined,
          name: assignment.target.token,
          colon: undefined,
          typeAnnotation: undefined,
          assignOp: assignment.assignOp,
          initializer: assignment.value,
          semicolon: semicolonToken,
          span: assignment.span
        };
        members.push(field);
      }
    } else if (expr.type === 'Variable') {
      // Handle Variables which might be methods with unsupported syntax
      // Create a placeholder method for now
      const variable = expr as AST.Variable;
      const placeholderMethod: AST.MethodDeclaration = {
        type: 'MethodDeclaration',
        decorators: undefined,
        name: variable.token,
        preSpecifiers: undefined,
        leftParen: AST.token('(' as const, '(', { leading: '', trailing: '' }, { start: variable.span.end, end: variable.span.end }),
        params: [],
        rightParen: AST.token(')' as const, ')', { leading: '', trailing: '' }, { start: variable.span.end, end: variable.span.end }),
        postSpecifiers: undefined,
        colon: undefined,
        returnType: undefined,
        assignOp: AST.token('=' as const, '=', { leading: '', trailing: '' }, { start: variable.span.end, end: variable.span.end }),
        body: AST.emptyExpression({ start: variable.span.end, end: variable.span.end }),
        semicolon: semicolonToken,
        span: variable.span
      };
      members.push(placeholderMethod);
    } else {
      // Error: expression is not a valid class member
      return { success: false, error: `Invalid class member: ${expr.type}`, state: currentState };
    }
  }

  return { success: true, value: members, state: currentState };
};

// Class expression
export const classExpression: PC.Parser<AST.Expr> = (state) => {
  const startPos = state.position;

  // Parse optional decorators first
  const decoratorsResult = parseDecorators(state);
  let currentState = decoratorsResult.success ? decoratorsResult.state : state;
  const classDecorators = decoratorsResult.success ? decoratorsResult.value : [];

  // Try 'class' keyword with boundary check
  const classCheck = PC.string('class')(currentState);
  if (!classCheck.success) return { success: false, error: 'Not a class expression', state };

  // Check it's not part of a longer identifier
  const nextPos = classCheck.state.position;
  if (nextPos < classCheck.state.input.length && /[a-zA-Z0-9_]/.test(classCheck.state.input[nextPos])) {
    return { success: false, error: 'class is part of identifier', state };
  }

  // Parse 'class' with trivia
  const classResult = withTriviaLiteral('class', PC.string('class'))(currentState);
  if (!classResult.success) return classResult;

  let bodyState = classResult.state;

  // Parse optional class-level annotations like <computes>
  let classSpecifiers: AST.Token<string>[] = [];
  while (true) {
    const ltResult = PC.char('<')(bodyState);
    if (!ltResult.success) break;

    const specifierResult = variable(ltResult.state);
    if (!specifierResult.success) break;

    const gtResult = PC.char('>')(specifierResult.state);
    if (!gtResult.success) break;

    // Create a compound token for the annotation
    const annotationToken: AST.Token<string> = {
      text: `<${specifierResult.value.token.text}>`,
      value: `<${specifierResult.value.token.value}>`,
      trivia: { leading: '', trailing: '' },
      span: { start: bodyState.position, end: gtResult.state.position }
    };

    classSpecifiers.push(annotationToken);
    bodyState = gtResult.state;
  }

  // Optional: inheritance with (ParentClass) or empty ()
  let parentClass: AST.Token<string> | undefined;
  let leftParenInherit: AST.Token<'('> | undefined;
  let rightParenInherit: AST.Token<')'> | undefined;
  const lparenResult = leftParen(bodyState);
  if (lparenResult.success) {
    // Check for empty parentheses first
    const rparenResult = rightParen(lparenResult.state);
    if (rparenResult.success) {
      // Empty parentheses - class with no parent
      leftParenInherit = lparenResult.value;
      rightParenInherit = rparenResult.value;
      bodyState = rparenResult.state;
    } else {
      // Try to parse parent class
      const parentResult = variable(lparenResult.state);
      if (parentResult.success) {
        const rparenResult2 = rightParen(parentResult.state);
        if (rparenResult2.success) {
          leftParenInherit = lparenResult.value;
          parentClass = (parentResult.value as AST.Variable).token;
          rightParenInherit = rparenResult2.value;
          bodyState = rparenResult2.state;
        }
      }
    }
  }

  // Now expect either { } or :
  const lbraceResult = leftBrace(bodyState);
  if (lbraceResult.success) {
    // Brace style
    const membersResult = parseClassMembers(lbraceResult.state);
    if (!membersResult.success) {
      return membersResult;
    }
    const rbraceResult = rightBrace(membersResult.state);

    if (rbraceResult.success) {
      // Create proper ClassExpression
      return {
        success: true,
        value: AST.classExpression(
          classResult.value as AST.Token<'class'>,
          classSpecifiers.length > 0 ? classSpecifiers : undefined,
          leftParenInherit,
          parentClass,
          rightParenInherit,
          'braces',
          lbraceResult.value,
          undefined, // colon
          membersResult.success ? membersResult.value : [],
          rbraceResult.value,
          { start: startPos, end: rbraceResult.state.position },
          classDecorators.length > 0 ? classDecorators : undefined
        ),
        state: rbraceResult.state
      };
    }
  }

  // Try colon/indentation style
  const colonResult = colon(bodyState);
  if (colonResult.success) {
    // For now, just parse one member after colon
    if (!getExpr) {
      return { success: false, error: 'Expression parser not initialized', state };
    }

    // For indentation style, parse members after colon
    const membersResult = parseClassMembers(colonResult.state);
    if (membersResult.success) {
      // Create proper ClassExpression
      return {
        success: true,
        value: AST.classExpression(
          classResult.value as AST.Token<'class'>,
          classSpecifiers.length > 0 ? classSpecifiers : undefined,
          leftParenInherit,
          parentClass,
          rightParenInherit,
          'indentation',
          undefined, // leftBrace
          colonResult.value,
          membersResult.success ? membersResult.value : [],
          undefined, // rightBrace
          { start: startPos, end: membersResult.state.position },
          classDecorators.length > 0 ? classDecorators : undefined
        ),
        state: membersResult.state
      };
    }
  }

  // If we have parentheses but no body (e.g., "class()" or "class(Base)"), create an empty class
  if (leftParenInherit && rightParenInherit) {
    return {
      success: true,
      value: AST.classExpression(
        classResult.value as AST.Token<'class'>,
        classSpecifiers.length > 0 ? classSpecifiers : undefined,
        leftParenInherit,
        parentClass,
        rightParenInherit,
        'braces', // Default to brace style for empty class
        undefined, // No actual left brace
        undefined, // No colon
        [], // Empty members
        undefined, // No actual right brace
        { start: startPos, end: bodyState.position },
        classDecorators.length > 0 ? classDecorators : undefined
      ),
      state: bodyState
    };
  }

  return { success: false, error: 'Expected { or : after class', state };
};