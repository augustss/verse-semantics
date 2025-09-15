// Pretty printer for reconstructing source code from AST
// This enables lossless round-trip parsing and printing

import { L } from '../ast/location';
import { Exp, SimpleName, Specifier, FuncParam, FuncDecl } from '../ast/expression';
import { Pat } from '../ast/pattern';
import { IdentExp } from '../ast/identifier';
import { TriviaList, toString as triviaToString } from '../ast/trivia';

export interface PrinterOptions {
  preserveOriginalFormatting: boolean;
  indentSize: number;
  useTabsForIndent: boolean;
}

export const defaultPrinterOptions: PrinterOptions = {
  preserveOriginalFormatting: true,
  indentSize: 4,
  useTabsForIndent: false
};

export class PrettyPrinter {
  private options: PrinterOptions;
  private currentIndent: number = 0;

  constructor(options: PrinterOptions = defaultPrinterOptions) {
    this.options = options;
  }

  // Main entry point for printing expressions
  print(exp: L<Exp>): string {
    return this.printLeading(exp) + this.printExp(exp.value) + this.printTrailing(exp);
  }

  private printLeading(exp: L<any>): string {
    if (this.options.preserveOriginalFormatting) {
      return triviaToString(exp.leadingTrivia);
    }
    return '';
  }

  private printTrailing(exp: L<any>): string {
    if (this.options.preserveOriginalFormatting) {
      return triviaToString(exp.trailingTrivia);
    }
    return '';
  }

  private printExp(exp: Exp): string {
    switch (exp.kind) {
      // Literals
      case 'Int':
        return exp.value.toString();
      case 'Float':
        return exp.value.toString();
      case 'String':
        return this.printString(exp.text, exp.interpolations);
      case 'True':
        return 'true';
      case 'False':
        return 'false';
      case 'Char':
        return `'${exp.value}'`;

      // Binary operators
      case 'Assign':
        return `${this.print(exp.left)} := ${this.print(exp.right)}`;
      case 'Add':
        return `${this.print(exp.left)} + ${this.print(exp.right)}`;
      case 'Subtract':
        return `${this.print(exp.left)} - ${this.print(exp.right)}`;
      case 'Multiply':
        return `${this.print(exp.left)} * ${this.print(exp.right)}`;
      case 'Divide':
        return `${this.print(exp.left)} / ${this.print(exp.right)}`;
      case 'And':
        return `${this.print(exp.left)} and ${this.print(exp.right)}`;
      case 'Or':
        return `${this.print(exp.left)} or ${this.print(exp.right)}`;
      case 'Less':
        return `${this.print(exp.left)} < ${this.print(exp.right)}`;
      case 'LessEqual':
        return `${this.print(exp.left)} <= ${this.print(exp.right)}`;
      case 'Greater':
        return `${this.print(exp.left)} > ${this.print(exp.right)}`;
      case 'GreaterEqual':
        return `${this.print(exp.left)} >= ${this.print(exp.right)}`;
      case 'NotEqual':
        return `${this.print(exp.left)} <> ${this.print(exp.right)}`;
      case 'Dot':
        return `${this.print(exp.left)}.${this.printIdent(exp.right)}`;
      case 'Range':
        return `${this.print(exp.left)}..${this.print(exp.right)}`;
      case 'Arrow':
        return `${this.print(exp.left)} => ${this.print(exp.right)}`;

      // Unary operators
      case 'Not':
        return `not ${this.print(exp.expr)}`;
      case 'PrefixMinus':
        return `-${this.print(exp.expr)}`;
      case 'PrefixPlus':
        return `+${this.print(exp.expr)}`;
      case 'PostfixIncrement':
        return `${this.print(exp.expr)}++`;
      case 'PostfixDecrement':
        return `${this.print(exp.expr)}--`;

      // Structural
      case 'Paren':
        return `(${this.print(exp.expr)})`;
      case 'Block':
        return `{${this.print(exp.expr)}}`;
      case 'Brace':
        return `{${this.print(exp.expr)}}`;
      case 'Array':
        return `array{${exp.elements.map(e => this.print(e)).join(', ')}}`;
      case 'List':
        return exp.elements.map(e => this.print(e)).join('; ');
      case 'Tuple':
        return `(${exp.elements.map(e => this.print(e)).join(', ')})`;

      // Function calls
      case 'ParenInvoke':
        return `${this.print(exp.func)}(${this.print(exp.arg)})`;
      case 'BracketInvoke':
        return `${this.print(exp.func)}[${this.print(exp.arg)}]`;

      // Control flow
      case 'If':
        return `if (${this.print(exp.cond)})`;
      case 'IfThen':
        return `if (${this.print(exp.cond)}): ${this.print(exp.then)}`;
      case 'IfElse':
        return `if (${this.print(exp.cond)}): ${this.print(exp.else)}`;
      case 'IfThenElse':
        return `if (${this.print(exp.cond)}): ${this.print(exp.then)} else: ${this.print(exp.else)}`;
      case 'For':
        return `for (${this.print(exp.expr)})`;
      case 'While':
        return `while (${this.print(exp.expr)})`;
      case 'Break':
        return 'break';
      case 'Continue':
        return 'continue';
      case 'Return':
        return exp.value ? `return ${this.print(exp.value)}` : 'return';

      // Declarations
      case 'Class':
        return `class: ${this.print(exp.body)}`;
      case 'Struct':
        return `struct: ${this.print(exp.body)}`;
      case 'Enum':
        return `enum: ${this.print(exp.body)}`;
      case 'Module':
        return `module: ${this.print(exp.body)}`;

      // Function declarations
      case 'FuncDecl':
        return this.printFuncDecl(exp.decl);

      // Specifiers
      case 'Specifier':
        return `<${exp.spec}>`;

      // Variables
      case 'ExpVar':
        return `var ${this.print(exp.expr)}`;
      case 'Set':
        return `set ${this.print(exp.target)} = ${this.print(exp.value)}`;

      default:
        // For any unhandled cases, return a placeholder
        return `[${(exp as any).kind}]`;
    }
  }

  private printString(text: string, interpolations: [L<Exp>, L<string>][]): string {
    if (interpolations.length === 0) {
      return `"${text}"`;
    }

    let result = '"';
    let lastPos = 0;

    for (const [expr, suffix] of interpolations) {
      // Add text before interpolation
      result += text.slice(lastPos);
      // Add interpolated expression
      result += `{${this.print(expr)}}`;
      // Add suffix text
      result += suffix.value;
      lastPos = suffix.value.length;
    }

    result += '"';
    return result;
  }

  private printFuncDecl(decl: FuncDecl): string {
    let result = decl.name;

    // Parameters
    if (decl.params.length > 0) {
      result += '(' + decl.params.map(p => this.printFuncParam(p)).join(', ') + ')';
    }

    // Specifiers
    if (decl.specifiers.length > 0) {
      result += decl.specifiers.map(s => `<${s}>`).join('');
    }

    // Return type
    if (decl.returnType) {
      result += `: ${this.print(decl.returnType)}`;
    }

    // Assignment operator
    result += decl.isDefinition ? ' := ' : ' = ';

    // Body
    result += this.print(decl.body);

    return result;
  }

  private printFuncParam(param: FuncParam): string {
    let result = '';

    if (param.name) {
      result += param.name;
    }

    if (param.pattern) {
      result += this.printPattern(param.pattern);
    }

    if (param.type) {
      result += `: ${this.print(param.type)}`;
    }

    if (param.defaultValue) {
      result += ` = ${this.print(param.defaultValue)}`;
    }

    return result;
  }

  private printPattern(pattern: L<Pat>): string {
    // This is a simplified pattern printer
    // In a full implementation, you'd handle all pattern types
    return this.printLeading(pattern) + '[pattern]' + this.printTrailing(pattern);
  }

  private printIdent(ident: L<IdentExp>): string {
    return this.printLeading(ident) + ident.value.name + this.printTrailing(ident);
  }

  private indent(): string {
    if (!this.options.preserveOriginalFormatting) {
      const char = this.options.useTabsForIndent ? '\t' : ' ';
      const size = this.options.useTabsForIndent ? 1 : this.options.indentSize;
      return char.repeat(this.currentIndent * size);
    }
    return '';
  }
}

// Convenience function for printing with default options
export function printAST(exp: L<Exp>, options?: Partial<PrinterOptions>): string {
  const printer = new PrettyPrinter({ ...defaultPrinterOptions, ...options });
  return printer.print(exp);
}