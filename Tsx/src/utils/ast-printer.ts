/**
 * Compact AST Pretty Printer
 *
 * Provides a readable, compact representation of AST nodes
 * with key information on separate lines.
 */

import * as AST from '../parser/ast';

/**
 * Options for AST printing
 */
export interface AstPrintOptions {
  /** Indentation string (default: "  ") */
  indent?: string;
  /** Whether to show token offsets (default: false) */
  showOffsets?: boolean;
  /** Whether to use colors (default: true) */
  useColors?: boolean;
  /** Maximum string length before truncation (default: 40) */
  maxStringLength?: number;
}

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  dim: '\x1b[2m',
  bold: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
};

/**
 * Pretty print an AST node in a compact, readable format
 */
export function prettyPrintAST(
  node: AST.ASTNode | AST.ASTNode[] | null | undefined,
  options: AstPrintOptions = {}
): string {
  const opts = {
    indent: options.indent ?? '  ',
    showOffsets: options.showOffsets ?? false,
    useColors: options.useColors ?? true,
    maxStringLength: options.maxStringLength ?? 40,
  };

  const printer = new AstPrinter(opts);
  return printer.print(node);
}

class AstPrinter {
  private options: Required<AstPrintOptions>;
  private indentLevel: number = 0;

  constructor(options: Required<AstPrintOptions>) {
    this.options = options;
  }

  print(node: AST.ASTNode | AST.ASTNode[] | null | undefined): string {
    if (node === null || node === undefined) {
      return this.color('null', 'dim');
    }

    if (Array.isArray(node)) {
      if (node.length === 0) {
        return this.color('[]', 'dim');
      }
      return node.map(n => this.print(n)).join('\n');
    }

    return this.printNode(node);
  }

  private printNode(node: AST.ASTNode): string {
    const indent = this.getIndent();
    const offset = this.options.showOffsets ? this.getOffsetInfo(node) : '';

    switch (node.type) {
      case 'Literal':
        return this.printLiteral(node as AST.LiteralExpression, indent, offset);
      case 'Identifier':
        return this.printIdentifier(node as AST.IdentifierExpression, indent, offset);
      case 'BinaryExpression':
        return this.printBinary(node as AST.BinaryExpression, indent, offset);
      case 'UnaryExpression':
        return this.printUnary(node as AST.UnaryExpression, indent, offset);
      case 'AssignmentExpression':
        return this.printAssignment(node as AST.AssignmentExpression, indent, offset);
      case 'CallExpression':
        return this.printCall(node as AST.CallExpression, indent, offset);
      case 'MemberExpression':
        return this.printMember(node as AST.MemberExpression, indent, offset);
      case 'ArrayExpression':
        return this.printArray(node as AST.ArrayExpression, indent, offset);
      case 'ObjectConstructorExpression':
        return this.printObjectConstructor(node as AST.ObjectConstructorExpression, indent, offset);
      case 'ParenthesizedExpression':
        return this.printParenthesized(node as AST.ParenthesizedExpression, indent, offset);
      case 'CompoundExpression':
        return this.printCompound(node as AST.CompoundExpression, indent, offset);
      case 'LambdaExpression':
        return this.printLambda(node as AST.LambdaExpression, indent, offset);
      case 'IfExpression':
        return this.printIf(node as AST.IfExpression, indent, offset);
      case 'ForExpression':
        return this.printFor(node as AST.ForExpression, indent, offset);
      case 'BlockExpression':
        return this.printBlock(node as AST.BlockExpression, indent, offset);
      case 'SetExpression':
        return this.printSet(node as AST.SetExpression, indent, offset);
      case 'RangeExpression':
        return this.printRange(node as AST.RangeExpression, indent, offset);
      case 'BreakExpression':
        return `${indent}${this.color('break', 'red')}${offset}`;
      case 'ReturnExpression':
        return this.printReturn(node as AST.ReturnExpression, indent, offset);
      case 'ConstantDeclaration':
        return this.printConstant(node as AST.ConstantDeclaration, indent, offset);
      case 'VariableDeclaration':
        return this.printVariable(node as AST.VariableDeclaration, indent, offset);
      case 'FunctionDeclaration':
        return this.printFunction(node as AST.FunctionDeclaration, indent, offset);
      case 'DataStructureDeclaration':
        return this.printDataStructure(node as AST.DataStructureDeclaration, indent, offset);
      case 'Program':
        return this.printProgram(node as any, indent, offset);
      default:
        return `${indent}${this.color(node.type, 'yellow')}${offset}`;
    }
  }

  private printLiteral(node: AST.LiteralExpression, indent: string, offset: string): string {
    const value = node.literalType === 'string'
      ? `"${this.truncateString(String(node.value))}"`
      : String(node.value);
    const colored = this.color(value, 'green');
    return `${indent}${colored}${offset}`;
  }

  private printIdentifier(node: AST.IdentifierExpression, indent: string, offset: string): string {
    return `${indent}${this.color(node.name, 'cyan')}${offset}`;
  }

  private printBinary(node: AST.BinaryExpression, indent: string, offset: string): string {
    // Check if this is a simple arithmetic/comparison expression
    if (this.isSimpleExpression(node.left) && this.isSimpleExpression(node.right)) {
      const left = this.printInline(node.left);
      const op = this.color(` ${node.operator} `, 'yellow');
      const right = this.printInline(node.right);
      return `${indent}${left}${op}${right}${offset}`;
    }

    // For complex expressions, use multi-line format
    const op = this.color(node.operator, 'yellow');
    const header = `${indent}${op}${offset}`;
    this.indentLevel++;
    const left = this.printNode(node.left);
    const right = this.printNode(node.right);
    this.indentLevel--;
    return `${header}\n${left}\n${right}`;
  }

  private printUnary(node: AST.UnaryExpression, indent: string, offset: string): string {
    // Check if the operand is simple
    if (this.isSimpleExpression(node.operand)) {
      const op = this.color(node.operator, 'yellow');
      const operand = this.printInline(node.operand);
      return `${indent}${op}${operand}${offset}`;
    }

    // For complex operands, use multi-line format
    const op = this.color(node.operator, 'yellow');
    const header = `${indent}${op}${offset}`;
    this.indentLevel++;
    const operand = this.printNode(node.operand);
    this.indentLevel--;
    return `${header}\n${operand}`;
  }

  private printAssignment(node: AST.AssignmentExpression, indent: string, offset: string): string {
    const op = this.color(node.operator, 'magenta');
    const header = `${indent}${op}${offset}`;
    this.indentLevel++;
    const left = this.printNode(node.left);
    const right = this.printNode(node.right);
    this.indentLevel--;
    return `${header}\n${left}\n${right}`;
  }

  private printCall(node: AST.CallExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('call', 'blue')}${offset}`;
    this.indentLevel++;
    const callee = this.printNode(node.callee);
    const args = node.arguments.length > 0
      ? '\n' + node.arguments.map(arg => this.printNode(arg)).join('\n')
      : '';
    this.indentLevel--;
    return `${header}\n${callee}${args}`;
  }

  private printMember(node: AST.MemberExpression, indent: string, offset: string): string {
    const accessType = node.computed ? '[.]' : '.';
    const header = `${indent}${this.color(accessType, 'blue')}${offset}`;
    this.indentLevel++;
    const object = this.printNode(node.object);
    const property = this.printNode(node.property);
    this.indentLevel--;
    return `${header}\n${object}\n${property}`;
  }

  private printArray(node: AST.ArrayExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('array', 'magenta')} ${this.color(`[${node.elements.length}]`, 'dim')}${offset}`;
    if (node.elements.length === 0) {
      return header;
    }
    this.indentLevel++;
    const elements = node.elements.map(el => this.printNode(el)).join('\n');
    this.indentLevel--;
    return `${header}\n${elements}`;
  }

  private printObjectConstructor(node: AST.ObjectConstructorExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color(node.typeName + '{}', 'magenta')}${offset}`;
    if (node.fields.length === 0) {
      return header;
    }
    this.indentLevel++;
    const fields = node.fields.map(field => {
      const fieldIndent = this.getIndent();
      const name = this.color(field.name + ':', 'cyan');
      this.indentLevel++;
      const value = this.printNode(field.value);
      this.indentLevel--;
      return `${fieldIndent}${name}\n${value}`;
    }).join('\n');
    this.indentLevel--;
    return `${header}\n${fields}`;
  }

  private printParenthesized(node: AST.ParenthesizedExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('()', 'dim')}${offset}`;
    this.indentLevel++;
    const expr = this.printNode(node.expression);
    this.indentLevel--;
    return `${header}\n${expr}`;
  }

  private printCompound(node: AST.CompoundExpression, indent: string, offset: string): string {
    const braced = node.openBraceOffset !== 0 || node.closeBraceOffset !== 0;
    const symbol = braced ? '{}' : 'compound';
    const header = `${indent}${this.color(symbol, 'blue')} ${this.color(`[${node.expressions.length}]`, 'dim')}${offset}`;
    if (node.expressions.length === 0) {
      return header;
    }
    this.indentLevel++;
    const expressions = node.expressions.map(expr => this.printNode(expr)).join('\n');
    this.indentLevel--;
    return `${header}\n${expressions}`;
  }

  private printLambda(node: AST.LambdaExpression, indent: string, offset: string): string {
    const params = node.parameters.map(p => (p as any).name).join(', ');
    const header = `${indent}${this.color('λ', 'magenta')} ${this.color(`(${params})`, 'dim')}${offset}`;
    this.indentLevel++;
    const body = this.printNode(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printIf(node: AST.IfExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('if', 'red')}${offset}`;
    this.indentLevel++;
    const parts: string[] = [];

    const condIndent = this.getIndent();
    parts.push(`${condIndent}${this.color('?', 'dim')}`);
    this.indentLevel++;
    parts.push(this.printNode(node.condition));
    this.indentLevel--;

    if (node.thenBranch) {
      parts.push(`${condIndent}${this.color('then', 'dim')}`);
      this.indentLevel++;
      parts.push(this.printNode(node.thenBranch));
      this.indentLevel--;
    }

    if (node.elseBranch) {
      parts.push(`${condIndent}${this.color('else', 'dim')}`);
      this.indentLevel++;
      parts.push(this.printNode(node.elseBranch));
      this.indentLevel--;
    }

    this.indentLevel--;
    return `${header}\n${parts.join('\n')}`;
  }

  private printFor(node: AST.ForExpression, indent: string, offset: string): string {
    const vars: string[] = [];
    if (node.indexVariable) vars.push(node.indexVariable);
    vars.push(node.variable);
    const header = `${indent}${this.color('for', 'red')} ${this.color(`(${vars.join(' -> ')})`, 'dim')}${offset}`;

    this.indentLevel++;
    const parts: string[] = [];

    const subIndent = this.getIndent();
    parts.push(`${subIndent}${this.color('in', 'dim')}`);
    this.indentLevel++;
    parts.push(this.printNode(node.iterable));
    this.indentLevel--;

    parts.push(`${subIndent}${this.color('do', 'dim')}`);
    this.indentLevel++;
    parts.push(this.printNode(node.body));
    this.indentLevel--;

    this.indentLevel--;
    return `${header}\n${parts.join('\n')}`;
  }

  private printBlock(node: AST.BlockExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('block', 'red')}${offset}`;
    this.indentLevel++;
    const body = this.printNode(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printSet(node: AST.SetExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('set', 'magenta')}${offset}`;
    this.indentLevel++;
    const target = this.printNode(node.target);
    const value = this.printNode(node.value);
    this.indentLevel--;
    return `${header}\n${target}\n${value}`;
  }

  private printRange(node: AST.RangeExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('..', 'yellow')}${offset}`;
    this.indentLevel++;
    const start = this.printNode(node.start);
    const end = this.printNode(node.end);
    this.indentLevel--;
    return `${header}\n${start}\n${end}`;
  }

  private printReturn(node: AST.ReturnExpression, indent: string, offset: string): string {
    const header = `${indent}${this.color('return', 'red')}${offset}`;
    if (!node.value) {
      return header;
    }
    this.indentLevel++;
    const value = this.printNode(node.value);
    this.indentLevel--;
    return `${header}\n${value}`;
  }

  private printConstant(node: AST.ConstantDeclaration, indent: string, offset: string): string {
    const type = node.declaredType ? `: ${(node.declaredType as any).typeName}` : '';
    const header = `${indent}${this.color('const', 'blue')} ${this.color(node.name + type, 'cyan')}${offset}`;
    if (!node.initializer) {
      return header;
    }
    this.indentLevel++;
    const init = this.printNode(node.initializer);
    this.indentLevel--;
    return `${header}\n${init}`;
  }

  private printVariable(node: AST.VariableDeclaration, indent: string, offset: string): string {
    const type = (node.declaredType as any).typeName;
    const header = `${indent}${this.color('var', 'blue')} ${this.color(node.name + ': ' + type, 'cyan')}${offset}`;
    if (!node.initializer) {
      return header;
    }
    this.indentLevel++;
    const init = this.printNode(node.initializer);
    this.indentLevel--;
    return `${header}\n${init}`;
  }

  private printFunction(node: AST.FunctionDeclaration, indent: string, offset: string): string {
    const params = node.parameters.map(p => (p as any).name).join(', ');
    const ret = node.returnType ? `: ${(node.returnType as any).typeName}` : '';
    const header = `${indent}${this.color('func', 'blue')} ${this.color(node.name, 'cyan')}(${params})${ret}${offset}`;
    this.indentLevel++;
    const body = this.printNode(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printDataStructure(node: AST.DataStructureDeclaration, indent: string, offset: string): string {
    const header = `${indent}${this.color(node.kind, 'blue')} ${this.color(node.name, 'cyan')}${offset}`;
    if (node.body.length === 0) {
      return header;
    }
    this.indentLevel++;
    const body = node.body.map(member => this.printNode(member)).join('\n');
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printProgram(node: any, indent: string, offset: string): string {
    const header = `${indent}${this.color('Program', 'bold')}${offset}`;
    const parts: string[] = [header];

    if (node.usingStatements && node.usingStatements.length > 0) {
      this.indentLevel++;
      const usingIndent = this.getIndent();
      parts.push(`${usingIndent}${this.color('using:', 'dim')}`);
      this.indentLevel++;
      node.usingStatements.forEach((stmt: any) => {
        parts.push(`${this.getIndent()}${this.color(stmt.path, 'green')}`);
      });
      this.indentLevel -= 2;
    }

    if (node.declarations && node.declarations.length > 0) {
      this.indentLevel++;
      node.declarations.forEach((decl: any) => {
        parts.push(this.printNode(decl));
      });
      this.indentLevel--;
    }

    return parts.join('\n');
  }

  private getIndent(): string {
    return this.options.indent.repeat(this.indentLevel);
  }

  private getOffsetInfo(node: any): string {
    if (!this.options.showOffsets) return '';

    // Try to find any offset property
    const offset = node.tokenOffset ?? node.nameOffset ?? node.operatorOffset ?? node.keywordOffset;
    if (offset !== undefined) {
      return this.color(` @${offset}`, 'dim');
    }
    return '';
  }

  private color(text: string, colorName: keyof typeof colors): string {
    if (!this.options.useColors) return text;
    return colors[colorName] + text + colors.reset;
  }

  private truncateString(str: string): string {
    if (str.length <= this.options.maxStringLength) {
      return str;
    }
    return str.substring(0, this.options.maxStringLength - 3) + '...';
  }

  /**
   * Check if an expression is simple enough to print inline
   */
  private isSimpleExpression(node: AST.ASTNode): boolean {
    switch (node.type) {
      case 'Literal':
      case 'Identifier':
        return true;
      case 'UnaryExpression':
        const unary = node as AST.UnaryExpression;
        return this.isSimpleExpression(unary.operand);
      case 'BinaryExpression':
        const binary = node as AST.BinaryExpression;
        // Only inline if both operands are also simple
        return this.isSimpleExpression(binary.left) && this.isSimpleExpression(binary.right);
      case 'ParenthesizedExpression':
        const paren = node as AST.ParenthesizedExpression;
        return this.isSimpleExpression(paren.expression);
      case 'MemberExpression':
        const member = node as AST.MemberExpression;
        return this.isSimpleExpression(member.object) && this.isSimpleExpression(member.property);
      default:
        return false;
    }
  }

  /**
   * Print a node inline (for simple expressions)
   */
  private printInline(node: AST.ASTNode): string {
    switch (node.type) {
      case 'Literal':
        const literal = node as AST.LiteralExpression;
        const value = literal.literalType === 'string'
          ? `"${this.truncateString(String(literal.value))}"`
          : String(literal.value);
        return this.color(value, 'green');

      case 'Identifier':
        return this.color((node as AST.IdentifierExpression).name, 'cyan');

      case 'UnaryExpression':
        const unary = node as AST.UnaryExpression;
        const op = this.color(unary.operator, 'yellow');
        const operand = this.printInline(unary.operand);
        return `${op}${operand}`;

      case 'BinaryExpression':
        const binary = node as AST.BinaryExpression;
        const left = this.printInline(binary.left);
        const binOp = this.color(` ${binary.operator} `, 'yellow');
        const right = this.printInline(binary.right);
        return `(${left}${binOp}${right})`;

      case 'ParenthesizedExpression':
        const paren = node as AST.ParenthesizedExpression;
        const inner = this.printInline(paren.expression);
        return `(${inner})`;

      case 'MemberExpression':
        const member = node as AST.MemberExpression;
        const obj = this.printInline(member.object);
        const prop = this.printInline(member.property);
        const dot = member.computed ? '[' + prop + ']' : '.' + prop;
        return obj + this.color(dot, 'blue');

      default:
        // Fallback for complex expressions
        return this.color(`<${node.type}>`, 'dim');
    }
  }
}

/**
 * Print AST to console with default options
 */
export function printAST(node: AST.ASTNode | AST.ASTNode[] | null | undefined, options?: AstPrintOptions): void {
  console.log(prettyPrintAST(node, options));
}