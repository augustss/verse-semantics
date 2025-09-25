/**
 * Logical AST Printer
 *
 * Pretty prints the simplified logical AST in a compact, readable format
 */

import * as LAST from './types';

export interface PrintOptions {
  indent?: string;
  useColors?: boolean;
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
};

/**
 * Print a logical AST node
 */
export function printLogicalAST(node: LAST.Node | null, options: PrintOptions = {}): string {
  if (!node) return 'null';

  const printer = new LogicalASTPrinter({
    indent: options.indent ?? '  ',
    useColors: options.useColors ?? true,
    maxStringLength: options.maxStringLength ?? 40
  });

  return printer.print(node);
}

class LogicalASTPrinter {
  private options: Required<PrintOptions>;
  private indentLevel = 0;

  constructor(options: Required<PrintOptions>) {
    this.options = options;
  }

  print(node: LAST.Node): string {
    switch (node.type) {
      // Expressions
      case 'Literal':
        return this.printLiteral(node as LAST.Literal);
      case 'Identifier':
        return this.printIdentifier(node as LAST.Identifier);
      case 'BinaryOp':
        return this.printBinaryOp(node as LAST.BinaryOp);
      case 'UnaryOp':
        return this.printUnaryOp(node as LAST.UnaryOp);
      case 'Assignment':
        return this.printAssignment(node as LAST.Assignment);
      case 'MemberAccess':
        return this.printMemberAccess(node as LAST.MemberAccess);
      case 'Call':
        return this.printCall(node as LAST.Call);
      case 'Array':
        return this.printArray(node as LAST.Array);
      case 'ObjectConstruction':
        return this.printObjectConstruction(node as LAST.ObjectConstruction);
      case 'Range':
        return this.printRange(node as LAST.Range);
      case 'Lambda':
        return this.printLambda(node as LAST.Lambda);
      case 'Block':
        return this.printBlock(node as LAST.Block);
      case 'Set':
        return this.printSet(node as LAST.Set);

      // Control flow
      case 'If':
        return this.printIf(node as LAST.If);
      case 'For':
        return this.printFor(node as LAST.For);
      case 'Loop':
        return this.printLoop(node as LAST.Loop);
      case 'Case':
        return this.printCase(node as LAST.Case);
      case 'Break':
        return this.color('break', 'red');
      case 'Return':
        return this.printReturn(node as LAST.Return);

      // Concurrent constructs
      case 'Spawn':
        return this.printSpawn(node as LAST.Spawn);
      case 'Race':
        return this.printRace(node as LAST.Race);
      case 'Sync':
        return this.printSync(node as LAST.Sync);
      case 'Branch':
        return this.printBranch(node as LAST.Branch);

      // Declarations
      case 'ConstDecl':
        return this.printConstDecl(node as LAST.ConstDecl);
      case 'VarDecl':
        return this.printVarDecl(node as LAST.VarDecl);
      case 'FunctionDecl':
        return this.printFunctionDecl(node as LAST.FunctionDecl);
      case 'ClassDecl':
        return this.printClassDecl(node as LAST.ClassDecl);
      case 'StructDecl':
        return this.printStructDecl(node as LAST.StructDecl);
      case 'InterfaceDecl':
        return this.printInterfaceDecl(node as LAST.InterfaceDecl);
      case 'EnumDecl':
        return this.printEnumDecl(node as LAST.EnumDecl);

      // Types
      case 'Type':
        return this.printType(node as LAST.Type);

      // Program
      case 'Program':
        return this.printProgram(node as LAST.Program);

      default:
        return this.color(`<${(node as any).type}>`, 'dim');
    }
  }

  private printExpression(expr: LAST.Expression): string {
    return this.print(expr as LAST.Node);
  }

  private indent(): string {
    return this.options.indent.repeat(this.indentLevel);
  }

  private color(text: string, colorName: keyof typeof colors): string {
    if (!this.options.useColors) return text;
    return colors[colorName] + text + colors.reset;
  }

  private truncate(str: string): string {
    if (str.length <= this.options.maxStringLength) return str;
    return str.substring(0, this.options.maxStringLength - 3) + '...';
  }

  // Check if expression is simple enough to print inline
  private isSimple(expr: LAST.Expression): boolean {
    switch (expr.type) {
      case 'Literal':
      case 'Identifier':
        return true;
      case 'BinaryOp':
        const bin = expr as LAST.BinaryOp;
        return this.isSimple(bin.left) && this.isSimple(bin.right);
      case 'UnaryOp':
        return this.isSimple((expr as LAST.UnaryOp).operand);
      case 'MemberAccess':
        const mem = expr as LAST.MemberAccess;
        return this.isSimple(mem.object) && this.isSimple(mem.property);
      default:
        return false;
    }
  }

  // Print inline version of simple expressions
  private printSimple(expr: LAST.Expression): string {
    switch (expr.type) {
      case 'Literal':
        const lit = expr as LAST.Literal;
        const val = lit.literalType === 'string'
          ? `"${this.truncate(String(lit.value))}"`
          : String(lit.value);
        return this.color(val, 'green');
      case 'Identifier':
        return this.color((expr as LAST.Identifier).name, 'cyan');
      case 'BinaryOp':
        const bin = expr as LAST.BinaryOp;
        const left = this.printSimple(bin.left);
        const op = this.color(` ${bin.operator} `, 'yellow');
        const right = this.printSimple(bin.right);
        return `(${left}${op}${right})`;
      case 'UnaryOp':
        const un = expr as LAST.UnaryOp;
        return this.color(un.operator, 'yellow') + this.printSimple(un.operand);
      case 'MemberAccess':
        const mem = expr as LAST.MemberAccess;
        const obj = this.printSimple(mem.object);
        const prop = this.printSimple(mem.property);
        return mem.computed ? `${obj}[${prop}]` : `${obj}.${prop}`;
      default:
        return this.color('?', 'dim');
    }
  }

  // Expression printers

  private printLiteral(node: LAST.Literal): string {
    const value = node.literalType === 'string'
      ? `"${this.truncate(String(node.value))}"`
      : String(node.value);
    return this.indent() + this.color(value, 'green');
  }

  private printIdentifier(node: LAST.Identifier): string {
    return this.indent() + this.color(node.name, 'cyan');
  }

  private printBinaryOp(node: LAST.BinaryOp): string {
    if (this.isSimple(node.left) && this.isSimple(node.right)) {
      return this.indent() + this.printSimple(node.left) +
             this.color(` ${node.operator} `, 'yellow') +
             this.printSimple(node.right);
    }

    const ind = this.indent();
    const op = this.color(node.operator, 'yellow');
    this.indentLevel++;
    const left = this.printExpression(node.left);
    const right = this.printExpression(node.right);
    this.indentLevel--;
    return `${ind}${op}\n${left}\n${right}`;
  }

  private printUnaryOp(node: LAST.UnaryOp): string {
    if (this.isSimple(node.operand)) {
      return this.indent() + this.color(node.operator, 'yellow') + this.printSimple(node.operand);
    }

    const ind = this.indent();
    const op = this.color(node.operator, 'yellow');
    this.indentLevel++;
    const operand = this.printExpression(node.operand);
    this.indentLevel--;
    return `${ind}${op}\n${operand}`;
  }

  private printAssignment(node: LAST.Assignment): string {
    if (this.isSimple(node.left) && this.isSimple(node.right)) {
      return this.indent() + this.printSimple(node.left) +
             this.color(` ${node.operator} `, 'magenta') +
             this.printSimple(node.right);
    }

    const ind = this.indent();
    const op = this.color(node.operator, 'magenta');
    this.indentLevel++;
    const left = this.printExpression(node.left);
    const right = this.printExpression(node.right);
    this.indentLevel--;
    return `${ind}${op}\n${left}\n${right}`;
  }

  private printMemberAccess(node: LAST.MemberAccess): string {
    if (this.isSimple(node.object) && this.isSimple(node.property)) {
      return this.indent() + this.printSimple(node);
    }

    const ind = this.indent();
    const op = this.color(node.computed ? '[.]' : '.', 'blue');
    this.indentLevel++;
    const obj = this.printExpression(node.object);
    const prop = this.printExpression(node.property);
    this.indentLevel--;
    return `${ind}${op}\n${obj}\n${prop}`;
  }

  private printCall(node: LAST.Call): string {
    const ind = this.indent();
    const header = `${ind}${this.color('call', 'blue')}`;
    this.indentLevel++;
    const callee = this.printExpression(node.callee);
    const args = node.arguments.map(arg => this.printExpression(arg));
    this.indentLevel--;
    return [header, callee, ...args].join('\n');
  }

  private printArray(node: LAST.Array): string {
    const ind = this.indent();
    const header = `${ind}${this.color('array', 'magenta')} ${this.color(`[${node.elements.length}]`, 'dim')}`;
    if (node.elements.length === 0) return header;

    this.indentLevel++;
    const elements = node.elements.map(el => this.printExpression(el));
    this.indentLevel--;
    return [header, ...elements].join('\n');
  }

  private printObjectConstruction(node: LAST.ObjectConstruction): string {
    const ind = this.indent();
    const header = `${ind}${this.color(node.typeName + '{}', 'magenta')}`;
    if (node.fields.length === 0) return header;

    this.indentLevel++;
    const fields = node.fields.map(field => {
      const fieldInd = this.indent();
      const name = this.color(field.name + ':', 'cyan');
      this.indentLevel++;
      const value = this.printExpression(field.value);
      this.indentLevel--;
      return `${fieldInd}${name}\n${value}`;
    });
    this.indentLevel--;
    return [header, ...fields].join('\n');
  }

  private printRange(node: LAST.Range): string {
    if (this.isSimple(node.start) && this.isSimple(node.end)) {
      return this.indent() + this.printSimple(node.start) +
             this.color('..', 'yellow') +
             this.printSimple(node.end);
    }

    const ind = this.indent();
    const op = this.color('..', 'yellow');
    this.indentLevel++;
    const start = this.printExpression(node.start);
    const end = this.printExpression(node.end);
    this.indentLevel--;
    return `${ind}${op}\n${start}\n${end}`;
  }

  private printLambda(node: LAST.Lambda): string {
    const ind = this.indent();
    const params = node.parameters.map(p => p.name).join(', ');
    const header = `${ind}${this.color('λ', 'magenta')} (${params})`;
    this.indentLevel++;
    const body = this.printExpression(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printBlock(node: LAST.Block): string {
    const ind = this.indent();
    const header = `${ind}${this.color('block', 'blue')} ${this.color(`[${node.expressions.length}]`, 'dim')}`;
    if (node.expressions.length === 0) return header;

    this.indentLevel++;
    const exprs = node.expressions.map(expr => this.printExpression(expr));
    this.indentLevel--;
    return [header, ...exprs].join('\n');
  }

  private printSet(node: LAST.Set): string {
    const ind = this.indent();
    const header = `${ind}${this.color('set', 'magenta')}`;
    this.indentLevel++;
    const target = this.printExpression(node.target);
    const value = this.printExpression(node.value);
    this.indentLevel--;
    return `${header}\n${target}\n${value}`;
  }

  // Control flow printers

  private printIf(node: LAST.If): string {
    const ind = this.indent();
    const header = `${ind}${this.color('if', 'red')}`;
    const parts: string[] = [header];

    this.indentLevel++;
    const condInd = this.indent();
    parts.push(`${condInd}${this.color('?', 'dim')}`);
    this.indentLevel++;
    parts.push(this.printExpression(node.condition));
    this.indentLevel--;

    if (node.thenBranch) {
      parts.push(`${condInd}${this.color('then', 'dim')}`);
      this.indentLevel++;
      parts.push(this.printExpression(node.thenBranch));
      this.indentLevel--;
    }

    if (node.elseBranch) {
      parts.push(`${condInd}${this.color('else', 'dim')}`);
      this.indentLevel++;
      parts.push(this.printExpression(node.elseBranch));
      this.indentLevel--;
    }

    this.indentLevel--;
    return parts.join('\n');
  }

  private printFor(node: LAST.For): string {
    const ind = this.indent();
    const vars = node.indexVariable
      ? `${node.indexVariable} -> ${node.variable}`
      : node.variable;
    const header = `${ind}${this.color('for', 'red')} (${vars})`;

    this.indentLevel++;
    const parts: string[] = [header];
    const subInd = this.indent();

    parts.push(`${subInd}${this.color('in', 'dim')}`);
    this.indentLevel++;
    parts.push(this.printExpression(node.iterable));
    this.indentLevel--;

    parts.push(`${subInd}${this.color('do', 'dim')}`);
    this.indentLevel++;
    parts.push(this.printExpression(node.body));
    this.indentLevel--;

    this.indentLevel--;
    return parts.join('\n');
  }

  private printLoop(node: LAST.Loop): string {
    const ind = this.indent();
    const header = `${ind}${this.color('loop', 'red')}`;
    this.indentLevel++;
    const body = this.printExpression(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printCase(node: LAST.Case): string {
    const ind = this.indent();
    const header = `${ind}${this.color('case', 'red')}`;

    this.indentLevel++;
    const scrutinee = this.printExpression(node.scrutinee);
    const branches = node.branches.map(branch => {
      const branchInd = this.indent();
      const pattern = branch.pattern === '_'
        ? this.color('_', 'yellow')
        : this.printSimple(branch.pattern as LAST.Expression);
      const arrow = this.color(' => ', 'yellow');

      if (this.isSimple(branch.body)) {
        return `${branchInd}${pattern}${arrow}${this.printSimple(branch.body)}`;
      } else {
        this.indentLevel++;
        const body = this.printExpression(branch.body);
        this.indentLevel--;
        return `${branchInd}${pattern}${arrow}\n${body}`;
      }
    });
    this.indentLevel--;

    return [header, scrutinee, ...branches].join('\n');
  }

  private printReturn(node: LAST.Return): string {
    const ind = this.indent();
    const header = `${ind}${this.color('return', 'red')}`;
    if (!node.value) return header;

    if (this.isSimple(node.value)) {
      return `${header} ${this.printSimple(node.value)}`;
    }

    this.indentLevel++;
    const value = this.printExpression(node.value);
    this.indentLevel--;
    return `${header}\n${value}`;
  }

  // Concurrent construct printers

  private printSpawn(node: LAST.Spawn): string {
    const ind = this.indent();
    const header = `${ind}${this.color('spawn', 'magenta')}`;
    this.indentLevel++;
    const body = this.printExpression(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printRace(node: LAST.Race): string {
    const ind = this.indent();
    const header = `${ind}${this.color('race', 'magenta')}`;
    if (node.branches.length === 0) return header;

    this.indentLevel++;
    const branches = node.branches.map(branch => this.printExpression(branch));
    this.indentLevel--;
    return [header, ...branches].join('\n');
  }

  private printSync(node: LAST.Sync): string {
    const ind = this.indent();
    const header = `${ind}${this.color('sync', 'magenta')}`;
    if (node.operations.length === 0) return header;

    this.indentLevel++;
    const operations = node.operations.map(op => this.printExpression(op));
    this.indentLevel--;
    return [header, ...operations].join('\n');
  }

  private printBranch(node: LAST.Branch): string {
    const ind = this.indent();
    const header = `${ind}${this.color('branch', 'magenta')}`;
    if (node.branches.length === 0) return header;

    this.indentLevel++;
    const branches = node.branches.map(branch => this.printExpression(branch));
    this.indentLevel--;
    return [header, ...branches].join('\n');
  }

  // Declaration printers

  private printConstDecl(node: LAST.ConstDecl): string {
    const ind = this.indent();
    const type = node.declaredType ? `: ${node.declaredType.name}` : '';
    const specs = node.specifiers ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}` : '';
    const header = `${ind}${this.color('const', 'blue')} ${this.color(node.name + type, 'cyan')}${specs}`;

    if (!node.initializer) return header;

    if (this.isSimple(node.initializer)) {
      return `${header} = ${this.printSimple(node.initializer)}`;
    }

    this.indentLevel++;
    const init = this.printExpression(node.initializer);
    this.indentLevel--;
    return `${header}\n${init}`;
  }

  private printVarDecl(node: LAST.VarDecl): string {
    const ind = this.indent();
    const type = node.declaredType.name;
    const specs = node.specifiers ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}` : '';
    const header = `${ind}${this.color('var', 'blue')} ${this.color(node.name + ': ' + type, 'cyan')}${specs}`;

    if (!node.initializer) return header;

    if (this.isSimple(node.initializer)) {
      return `${header} = ${this.printSimple(node.initializer)}`;
    }

    this.indentLevel++;
    const init = this.printExpression(node.initializer);
    this.indentLevel--;
    return `${header}\n${init}`;
  }

  private printFunctionDecl(node: LAST.FunctionDecl): string {
    const ind = this.indent();
    const params = node.parameters.map(p => p.name).join(', ');
    const ret = node.returnType ? `: ${node.returnType.name}` : '';

    // Format visibility specifier separately from other specifiers
    const visibility = node.visibility ? ` ${this.color(`<${node.visibility}>`, 'yellow')}` : '';
    const specs = node.specifiers && node.specifiers.length > 0
      ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}`
      : '';

    const header = `${ind}${this.color('func', 'blue')} ${this.color(node.name, 'cyan')}${visibility}(${params})${ret}${specs}`;

    this.indentLevel++;
    const body = this.printExpression(node.body);
    this.indentLevel--;
    return `${header}\n${body}`;
  }

  private printClassDecl(node: LAST.ClassDecl): string {
    const ind = this.indent();
    let parentsStr = '';
    if (node.parents && node.parents.length > 0) {
      const parentsList = node.parents.map(parent => {
        if (this.isSimple(parent)) {
          return this.printSimple(parent);
        }
        // For complex parent expressions, just show the type
        return this.color('<complex>', 'dim');
      }).join(', ');
      parentsStr = `(${parentsList})`;
    }
    const specs = node.specifiers ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}` : '';
    const header = `${ind}${this.color('class', 'blue')} ${this.color(node.name, 'cyan')}${parentsStr}${specs}`;

    if (node.members.length === 0) return header;

    this.indentLevel++;
    const members = node.members.map(member => this.print(member as LAST.Node));
    this.indentLevel--;
    return [header, ...members].join('\n');
  }

  private printStructDecl(node: LAST.StructDecl): string {
    const ind = this.indent();
    const specs = node.specifiers ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}` : '';
    const header = `${ind}${this.color('struct', 'blue')} ${this.color(node.name, 'cyan')}${specs}`;

    if (node.members.length === 0) return header;

    this.indentLevel++;
    const members = node.members.map(member => this.print(member as LAST.Node));
    this.indentLevel--;
    return [header, ...members].join('\n');
  }

  private printInterfaceDecl(node: LAST.InterfaceDecl): string {
    const ind = this.indent();
    const specs = node.specifiers ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}` : '';
    const header = `${ind}${this.color('interface', 'blue')} ${this.color(node.name, 'cyan')}${specs}`;

    if (node.members.length === 0) return header;

    this.indentLevel++;
    const members = node.members.map(member => this.print(member as LAST.Node));
    this.indentLevel--;
    return [header, ...members].join('\n');
  }

  private printEnumDecl(node: LAST.EnumDecl): string {
    const ind = this.indent();
    const specs = node.specifiers ? ` ${this.color(`<${node.specifiers.join(',')}>`, 'dim')}` : '';
    const header = `${ind}${this.color('enum', 'blue')} ${this.color(node.name, 'cyan')}${specs}`;

    if (node.members.length === 0) return header;

    this.indentLevel++;
    const members = node.members.map(member => {
      const memberInd = this.indent();
      const value = member.value ? ` = ${this.printSimple(member.value)}` : '';
      return `${memberInd}${this.color(member.name, 'cyan')}${value}`;
    });
    this.indentLevel--;
    return [header, ...members].join('\n');
  }

  private printType(node: LAST.Type): string {
    let result = node.name;
    if (node.isOptional) result = '?' + result;
    if (node.isArray) result = '[]'.repeat(node.arrayDimensions || 1) + result;
    return this.color(result, 'yellow');
  }

  private printProgram(node: LAST.Program): string {
    const parts: string[] = [];
    const header = this.color('Program', 'bold');
    parts.push(header);

    if (node.usingPaths && node.usingPaths.length > 0) {
      this.indentLevel++;
      const usingHeader = this.indent() + this.color('using:', 'dim');
      parts.push(usingHeader);
      this.indentLevel++;
      node.usingPaths.forEach(path => {
        parts.push(this.indent() + this.color(path, 'green'));
      });
      this.indentLevel -= 2;
    }

    if (node.declarations.length > 0) {
      this.indentLevel++;
      node.declarations.forEach(decl => {
        parts.push(this.print(decl as LAST.Node));
      });
      this.indentLevel--;
    }

    return parts.join('\n');
  }
}