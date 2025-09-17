// Pretty printer for reconstructing source code from AST
// This enables lossless round-trip parsing and printing

import { L } from '../ast/location';
import { Exp, FuncParam, FuncDecl } from '../ast/expression';
import { Pat } from '../ast/pattern';
import { IdentExp } from '../ast/identifier';
import { toString as triviaToString } from '../ast/trivia';

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
  private originalSource: string;
  // private currentIndent: number = 0;

  constructor(options: PrinterOptions = defaultPrinterOptions, originalSource: string = '') {
    this.options = options;
    this.originalSource = originalSource;
  }

  // Comprehensive space reconstruction between any two AST nodes
  private getSpacingBetweenNodes(leftNode: L<any>, rightNode: L<any>): string {
    if (!this.options.preserveOriginalFormatting || !this.originalSource) {
      return ' '; // Default single space
    }

    const leftEnd = leftNode.loc.end.offset;
    const rightStart = rightNode.loc.start.offset;

    if (leftEnd < rightStart && rightStart <= this.originalSource.length) {
      return this.originalSource.slice(leftEnd, rightStart);
    }

    return ' '; // Default single space
  }

  // Get the exact source text for the entire expression span and extract spacing
  private getFullExpressionSpacing(leftNode: L<any>, rightNode: L<any>, op: string): string {
    if (!this.options.preserveOriginalFormatting || !this.originalSource) {
      return ` ${op} `; // Default spacing
    }

    // Find the actual end of the left content (excluding trailing whitespace)
    let leftContentEnd = leftNode.loc.start.offset;
    const leftSource = this.originalSource.slice(leftNode.loc.start.offset, leftNode.loc.end.offset);

    // Find the end of the actual content (non-whitespace)
    const trimmed = leftSource.trimEnd();
    leftContentEnd = leftNode.loc.start.offset + trimmed.length;

    const rightStart = rightNode.loc.start.offset;

    if (leftContentEnd < rightStart && rightStart <= this.originalSource.length) {
      const betweenText = this.originalSource.slice(leftContentEnd, rightStart);

      // Find the operator in the between text
      const opIndex = betweenText.indexOf(op);
      if (opIndex !== -1) {
        // Return the exact between text which should contain the operator with spacing
        return betweenText;
      }
    }

    return ` ${op} `; // Default spacing
  }

  // Print expression without trailing trivia
  private printWithoutTrailingTrivia(exp: L<Exp>): string {
    // Always use source positions if available to avoid trailing spaces
    if (this.originalSource && exp.loc) {
      const expStart = exp.loc.start.offset;
      const expEnd = exp.loc.end.offset;
      if (expStart >= 0 && expEnd <= this.originalSource.length) {
        // Get the source text and trim trailing whitespace
        const sourceText = this.originalSource.slice(expStart, expEnd);
        return sourceText.trimEnd();
      }
    }

    // Fallback to regular printing
    const printed = this.print(exp);

    // Try to trim trailing whitespace if no source available
    if (!this.originalSource) {
      return printed.trimEnd();
    }

    return printed;
  }

  // Print expression without leading trivia
  private printWithoutLeadingTrivia(exp: L<Exp>): string {
    const printed = this.print(exp);
    // If there's leading trivia, remove it by using source positions
    if (exp.leadingTrivia && exp.leadingTrivia.trivia.length > 0 && this.originalSource) {
      const expStart = exp.loc.start.offset;
      const expEnd = exp.loc.end.offset;
      return this.originalSource.slice(expStart, expEnd);
    }
    return printed;
  }

  // Main entry point for printing expressions
  print(exp: L<Exp>): string {
    return this.printLeading(exp) + this.printExp(exp.value, exp) + this.printTrailing(exp);
  }

  // Extract original spacing between two source positions
  private getOriginalSpacingBetween(leftEnd: number, rightStart: number): string {
    if (this.originalSource && leftEnd < rightStart && rightStart <= this.originalSource.length) {
      return this.originalSource.slice(leftEnd, rightStart);
    }
    return '';
  }

  // Print an expression with comprehensive spacing preservation
  printBinaryOp(left: L<Exp>, op: string, right: L<Exp>, defaultSpacing: boolean = true): string {
    const leftStr = this.printWithoutTrailingTrivia(left);
    const rightStr = this.printWithoutLeadingTrivia(right);

    // Use full expression spacing reconstruction
    const fullSpacing = this.getFullExpressionSpacing(left, right, op);

    // Debug for specific operators (disabled)
    // if (['as'].includes(op)) {
    //   console.log(`BINARY OP: op='${op}', leftStr='${leftStr}', rightStr='${rightStr}', fullSpacing='${fullSpacing}'`);
    //   console.log(`LEFT END: ${left.loc.end.offset}, RIGHT START: ${right.loc.start.offset}`);
    // }

    // The full spacing already includes the operator with correct surrounding spaces
    const opIndex = fullSpacing.indexOf(op);
    const beforeOp = fullSpacing.slice(0, opIndex);
    const afterOp = fullSpacing.slice(opIndex + op.length);

    // Handle case where operator is at start (no space before in between text)
    if (opIndex === 0 && beforeOp === '' && afterOp.length > 0) {
      // Add space before operator since it was likely absorbed into left span
      return `${leftStr} ${op}${afterOp}${rightStr}`;
    }

    return `${leftStr}${beforeOp}${op}${afterOp}${rightStr}`;

    // Fall back to default spacing behavior
    if (defaultSpacing) {
      return `${leftStr} ${op} ${rightStr}`;
    } else {
      return `${leftStr}${op}${rightStr}`;
    }
  }

  // Print binary operator using source positions for spacing (without printing left/right)
  printBinaryOpFromPositions(left: L<any>, op: string, right: L<any>, fallback: string): string {
    // Try to preserve original spacing by examining source positions
    if (this.options.preserveOriginalFormatting && this.originalSource) {
      const leftEnd = left.loc.end.offset;
      const rightStart = right.loc.start.offset;

      if (leftEnd < rightStart) {
        const betweenText = this.getOriginalSpacingBetween(leftEnd, rightStart);

        // Debug for colon operator
        // if (op === ':') {
        //   console.log(`COLON DEBUG: leftEnd=${leftEnd}, rightStart=${rightStart}, betweenText='${betweenText}', returning: '${betweenText}'`);
        // }

        // Find the operator in the between text and preserve surrounding spaces
        const opIndex = betweenText.indexOf(op);
        if (opIndex !== -1) {
          return betweenText;  // Return the exact spacing from source
        }
      }
    }

    // Fall back to provided default
    // console.log(`FALLBACK: Using default spacing '${fallback}' for operator '${op}'`);
    return fallback;
  }

  private printLeading(exp: L<any>): string {
    if (this.options.preserveOriginalFormatting && exp.leadingTrivia) {
      return triviaToString(exp.leadingTrivia);
    }
    return '';
  }

  private printTrailing(exp: L<any>): string {
    if (this.options.preserveOriginalFormatting && exp.trailingTrivia) {
      return triviaToString(exp.trailingTrivia);
    }
    return '';
  }

  // Determine if property declarations should use spacing based on context
  private shouldUseSpacingForProperty(_propertyNode: L<Exp>): boolean {
    // For systematic spacing, we should always preserve the original source spacing
    // The old heuristic-based approach doesn't work well
    return true;  // Always use the systematic spacing approach
  }

  private shouldAddParensToIf(cond: L<Exp>): boolean {
    // If already has parens, don't add more
    if (cond.value.kind === 'Paren') {
      return false;
    }

    // Check if original source had parentheses
    if (this.options.preserveOriginalFormatting && this.originalSource) {
      // Look for 'if' keyword and check what follows
      const ifIndex = this.originalSource.lastIndexOf('if ');
      if (ifIndex >= 0) {
        let afterIfPos = ifIndex + 3; // After 'if '
        // Skip whitespace
        while (afterIfPos < this.originalSource.length && this.originalSource[afterIfPos] === ' ') {
          afterIfPos++;
        }
        // Check if there's a parenthesis
        return this.originalSource[afterIfPos] === '(';
      }
    }

    // Default: add parens for complex expressions
    return true;
  }

  private printExp(exp: Exp, wrapper?: L<Exp>): string {
    switch (exp.kind) {
      // Literals
      case 'Int':
        return exp.value.toString();
      case 'Float':
        // Preserve .0 for floats that are whole numbers
        const floatStr = exp.value.toString();
        return floatStr.includes('.') ? floatStr : floatStr + '.0';
      case 'Ident':
        return exp.name;
      case 'String':
        // For lossless parsing, preserve original source if available
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const start = wrapper.loc.start.offset;
          const end = wrapper.loc.end.offset;
          if (start >= 0 && end <= this.originalSource.length) {
            return this.originalSource.substring(start, end);
          }
        }
        return this.printString(exp.text, exp.interpolations);
      case 'True':
        return 'true';
      case 'False':
        return 'false';
      case 'Char':
        // Try to preserve the original source representation if available
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const start = wrapper.loc.start.offset;
          const end = wrapper.loc.end.offset;
          if (start >= 0 && end <= this.originalSource.length) {
            // Return the exact source text for the character literal
            const sourceText = this.originalSource.substring(start, end);
            // Only return if it looks like a character literal
            if (sourceText.startsWith("'") && sourceText.endsWith("'")) {
              return sourceText;
            }
          }
        }

        // Fallback: escape common sequences
        let charStr = exp.value;

        // Common escape sequences that need to be preserved
        if (charStr === '\n') charStr = '\\n';
        else if (charStr === '\r') charStr = '\\r';
        else if (charStr === '\t') charStr = '\\t';
        else if (charStr === '\'') charStr = '\\\'';
        else if (charStr === '\\') charStr = '\\\\';

        return `'${charStr}'`;

      // Binary operators
      case 'Assign':
        const assignExp = exp as any;
        let assignResult = '';
        // Handle decorators if present
        if (assignExp.decorators && assignExp.decorators.length > 0) {
          assignResult += assignExp.decorators.join('\n') + '\n';
        }

        // Determine the operator from the original source
        let operator = ':='; // Default to assignment
        if (this.originalSource && exp.left.loc && exp.right.loc) {
          const leftEnd = exp.left.loc.end.offset;
          const rightStart = exp.right.loc.start.offset;
          let between = this.originalSource.substring(leftEnd, rightStart);

          // If between is just "= ", check if there's a : before it
          if (between.trim() === '=') {
            // Check if the left ends with ':'
            const leftText = this.originalSource.substring(exp.left.loc.start.offset, leftEnd);
            if (leftText.endsWith(':')) {
              operator = ':=';
            } else {
              operator = '=';
            }
          } else if (between.includes(':=')) {
            operator = ':=';
          } else if (between.includes('=')) {
            operator = '=';
          } else if (exp.left.value.kind === 'PropertyDecl') {
            // PropertyDecl typically uses = for type annotations
            operator = '=';
          }
        } else if (exp.left.value.kind === 'PropertyDecl') {
          operator = '=';
        }

        // For Pat nodes, we need to use print() not printWithoutTrailingTrivia to get correct formatting
        if (exp.left.value.kind === 'Pat') {
          const leftStr = this.print(exp.left);
          let rightStr = this.print(exp.right);

          // Special case: if right is also a Pat and contains block keywords, preserve from source
          if (exp.right.value.kind === 'Pat' && this.originalSource && exp.right.loc) {
            const rightSource = this.originalSource.substring(exp.right.loc.start.offset, exp.right.loc.end.offset);
            // Check if this is a block keyword that should preserve colon
            if (['try:', 'loop:', 'block:', 'spawn:'].some(kw => rightSource === kw)) {
              rightStr = rightSource;
            }
          }

          assignResult += `${leftStr} ${operator} ${rightStr}`;
        } else {
          const useSpacing = exp.left.value.kind !== 'PropertyDecl' ||
                            (exp.left.value.kind === 'PropertyDecl' && this.shouldUseSpacingForProperty(exp.left));
          assignResult += this.printBinaryOp(exp.left, operator, exp.right, useSpacing);
        }
        return assignResult;
      case 'Add':
        return this.printBinaryOp(exp.left, '+', exp.right);
      case 'Subtract':
        return this.printBinaryOp(exp.left, '-', exp.right);
      case 'Multiply':
        return this.printBinaryOp(exp.left, '*', exp.right);
      case 'Divide':
        return this.printBinaryOp(exp.left, '/', exp.right);
      case 'Exponent':
        return `${this.print(exp.left)} ^ ${this.print(exp.right)}`;
      case 'And':
        return this.printBinaryOp(exp.left, 'and', exp.right);
      case 'Or':
        return this.printBinaryOp(exp.left, 'or', exp.right);
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
      case 'As':
        return this.printBinaryOp(exp.left, 'as', exp.right);
      case 'Isa':
        return this.printBinaryOp(exp.left, 'isa', exp.right);
      case 'Arrow':
        // Preserve original arrow operator (-> vs =>)
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const start = wrapper.loc.start.offset;
          const end = wrapper.loc.end.offset;
          if (start >= 0 && end <= this.originalSource.length) {
            return this.originalSource.substring(start, end);
          }
        }
        return this.printBinaryOp(exp.left, '=>', exp.right);

      // Unary operators
      case 'Not':
        return `not ${this.print(exp.expr)}`;
      case 'PrefixMinus':
        return `-${this.print(exp.expr)}`;
      case 'PrefixPlus':
        return `+${this.print(exp.expr)}`;
      case 'PrefixMultiply':
        return `*${this.print(exp.expr)}`;
      case 'PostfixIncrement':
        return `${this.print(exp.expr)}++`;
      case 'PostfixDecrement':
        return `${this.print(exp.expr)}--`;

      // Structural
      case 'Paren':
        const parenExp = exp as any;
        let parenResult = '';

        // Handle decorators if present
        if (parenExp.decorators && parenExp.decorators.length > 0) {
          parenResult += parenExp.decorators.join('') + '';
        }

        parenResult += `(${this.print(exp.expr)})`;
        return parenResult;
      case 'Block':
        // For lossless mode, try to preserve the original block formatting including indentation
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          // Find the content that precedes this block to determine the context
          const blockStart = wrapper.loc.start.offset;

          // Look backwards to find where the block context starts (after : or keywords)
          let contextStart = blockStart - 1;
          while (contextStart >= 0 && this.originalSource[contextStart] !== ':') {
            contextStart--;
          }

          if (contextStart >= 0 && this.originalSource[contextStart] === ':') {
            // Found a colon, but check if the first element starts on the same line as the colon
            const blockEnd = wrapper.loc.end.offset;
            let blockWithIndent = this.originalSource.substring(contextStart + 1, blockEnd);

            // Check if the block content starts immediately after the colon (same line)
            if (blockWithIndent.length > 0 && blockWithIndent[0] !== '\n') {
              // The first content is on the same line as the colon, we need to add proper indentation
              // Split into lines and add indentation to each line
              const lines = blockWithIndent.split('\n');
              if (lines.length > 0) {
                // First line needs to be moved to next line with indentation
                lines[0] = '\n    ' + lines[0].trim();
                // Ensure other lines maintain their relative indentation
                for (let i = 1; i < lines.length; i++) {
                  if (lines[i].trim().length > 0 && !lines[i].startsWith('    ')) {
                    lines[i] = '    ' + lines[i].trim();
                  }
                }
                blockWithIndent = lines.join('\n');
              }
            }
            return blockWithIndent;
          }
        }

        // Check if this is inside an indented context (interface, module, etc.)
        // If the block contains a List, print with indentation
        if (exp.expr.value.kind === 'List') {
          const elements = exp.expr.value.elements;
          if (elements.length > 0) {
            if (this.options.preserveOriginalFormatting) {
              // In lossless mode, preserve original formatting including indentation
              return '\n' + this.printList(exp.expr.value);
            } else {
              const indentedElements = elements.map((elem: any) =>
                '    ' + this.print(elem)
              );
              return '\n' + indentedElements.join('\n');
            }
          } else {
            return '';
          }
        } else {
          // Single expression block - print with indentation
          if (this.options.preserveOriginalFormatting) {
            return '\n' + this.print(exp.expr);
          } else {
            return '\n    ' + this.print(exp.expr);
          }
        }
        break;
      case 'Brace':
        return `{${this.print(exp.expr)}}`;
      case 'Array':
        return `array{${exp.elements.map(e => this.print(e)).join(', ')}}`;
      case 'List':
        return this.printList(exp);
      case 'Tuple':
        if (exp.elements.length === 1) {
          // Single-element tuple needs trailing comma
          const elemStr = this.print(exp.elements[0]);

          // Try to preserve original spacing around the comma
          if (this.options.preserveOriginalFormatting && this.originalSource &&
              wrapper && wrapper.loc && exp.elements[0].loc) {
            const tupleEnd = wrapper.loc.end.offset;
            const elemEnd = exp.elements[0].loc.end.offset;

            // Look for comma after the element
            const afterElem = this.originalSource.substring(elemEnd, tupleEnd);
            const commaIdx = afterElem.indexOf(',');

            if (commaIdx >= 0) {
              // Extract exact spacing before and after comma
              const beforeComma = afterElem.substring(0, commaIdx);
              const afterComma = afterElem.substring(commaIdx + 1, afterElem.lastIndexOf(')'));
              return `(${elemStr}${beforeComma},${afterComma})`;
            }
          }

          return `(${elemStr},)`;
        } else {
          return `(${exp.elements.map(e => this.print(e)).join(', ')})`;
        }

      // Function calls
      case 'ParenInvoke':
        // For lossless parsing, preserve original source if available
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          let start = wrapper.loc.start.offset;
          let end = wrapper.loc.end.offset;


          // Check if the range already ends with a closing parenthesis
          const currentSubstring = this.originalSource.substring(start, end);
          if (!currentSubstring.endsWith(')') && end < this.originalSource.length && this.originalSource[end] === ')') {
            end++; // Include the closing parenthesis if it's not already included
          }

          if (start >= 0 && end <= this.originalSource.length) {
            return this.originalSource.substring(start, end);
          }
        }

        // Handle paren invoke arguments with proper comma separation
        let parenArgStr = '';
        if (exp.arg && exp.arg.value && exp.arg.value.kind === 'List' && exp.arg.value.elements) {
          parenArgStr = exp.arg.value.elements.map((e: any) => this.print(e)).join(', ');
        } else {
          parenArgStr = this.print(exp.arg);
        }
        return `${this.print(exp.func)}(${parenArgStr})`;
      case 'BracketInvoke':
        // For lossless parsing, preserve original source if available
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const start = wrapper.loc.start.offset;
          let end = wrapper.loc.end.offset;

          // Check if the range needs to include brackets - often they're not included in the location
          const currentSubstring = this.originalSource.substring(start, end);

          // If we don't end with ']', look for brackets after the end position
          if (!currentSubstring.endsWith(']')) {
            // Look for '[' at end position and ']' at end+1 position
            if (end < this.originalSource.length && this.originalSource[end] === '[' &&
                end + 1 < this.originalSource.length && this.originalSource[end + 1] === ']') {
              end += 2; // Include both brackets
            }
            // Fallback: just look for ']' at end position
            else if (end < this.originalSource.length && this.originalSource[end] === ']') {
              end++; // Include the closing bracket
            }
          }

          if (start >= 0 && end <= this.originalSource.length) {
            return this.originalSource.substring(start, end);
          }
        }

        // Handle bracket invoke arguments with proper comma separation
        let bracketArgStr = '';
        if (exp.arg && exp.arg.value && exp.arg.value.kind === 'List' && exp.arg.value.elements) {
          bracketArgStr = exp.arg.value.elements.map((e: any) => this.print(e)).join(', ');
        } else {
          bracketArgStr = this.print(exp.arg);
        }
        return `${this.print(exp.func)}[${bracketArgStr}]`;
      case 'BraceInvoke':
        // Handle brace invoke arguments with proper comma separation
        let braceArgStr = '';
        if (exp.arg && exp.arg.value && exp.arg.value.kind === 'List' && exp.arg.value.elements) {
          braceArgStr = exp.arg.value.elements.map((e: any) => this.print(e)).join(', ');
        } else {
          braceArgStr = this.print(exp.arg);
        }
        return `${this.print(exp.func)}{${braceArgStr}}`;

      // Control flow
      case 'If':
        // Check if original had parentheses
        let ifCond = this.print(exp.cond);
        if (this.shouldAddParensToIf(exp.cond)) {
          ifCond = `(${ifCond})`;
        }
        return `if ${ifCond}`;
      case 'IfThen':
        // Check if original had parentheses
        let ifThenCond = this.print(exp.cond);
        if (this.shouldAddParensToIf(exp.cond)) {
          ifThenCond = `(${ifThenCond})`;
        }

        if (!this.options.preserveOriginalFormatting || !this.originalSource) {
          return `if ${ifThenCond} then ${this.print(exp.then)}`;
        }

        let condThenSpacing = this.getSpacingBetweenNodes(exp.cond, exp.then);

        // Check if original uses 'then' or ':' syntax
        const thenIdx = condThenSpacing.indexOf('then');
        const colonIdx = condThenSpacing.indexOf(':');

        if (thenIdx >= 0) {
          // Uses 'then' syntax
          const afterThen = condThenSpacing.slice(thenIdx + 4);
          return `if ${ifThenCond} then${afterThen}${this.print(exp.then)}`;
        } else if (colonIdx >= 0) {
          // Uses ':' syntax
          const afterColon = condThenSpacing.slice(colonIdx + 1);
          return `if ${ifThenCond}:${afterColon}${this.print(exp.then)}`;
        } else {
          // Fallback to default
          return `if ${ifThenCond} then ${this.print(exp.then)}`;
        }
      case 'IfElse':
        const ifElseCond = exp.cond.value.kind === 'Paren' ? this.print(exp.cond) : `(${this.print(exp.cond)})`;
        let condElseSpacing = this.getSpacingBetweenNodes(exp.cond, exp.else);
        // Extract spacing after 'else' keyword
        const elseIdx = condElseSpacing.indexOf('else');
        if (elseIdx >= 0) {
          condElseSpacing = condElseSpacing.slice(elseIdx + 4); // Skip 'else'
        } else {
          condElseSpacing = ' '; // Default space
        }
        return `if ${ifElseCond} else${condElseSpacing}${this.print(exp.else)}`;
      case 'IfThenElse':
        // For if-then-else, use spacing-aware binary operations
        // Don't include trailing trivia since we're controlling spacing
        const condStr = this.printWithoutTrailingTrivia(exp.cond);
        const thenStr = this.printWithoutTrailingTrivia(exp.then);
        const elseStr = this.print(exp.else);

        if (this.options.preserveOriginalFormatting && this.originalSource &&
            exp.cond && exp.cond.loc && exp.then && exp.then.loc &&
            exp.else && exp.else.loc) {
          // Use actual source positions - find 'if' in source
          // Search backwards from condition start to find 'if'
          const condStart = exp.cond.loc.start.offset;
          let exprStart = 0;

          // Look for 'if' before condition
          const searchStart = Math.max(0, condStart - 10); // Look up to 10 chars back
          const searchText = this.originalSource.substring(searchStart, condStart);
          const ifIdx = searchText.lastIndexOf('if');
          if (ifIdx >= 0) {
            exprStart = searchStart + ifIdx;
          }

          const condEnd = exp.cond.loc.end.offset;
          const thenStart = exp.then.loc.start.offset;
          const thenEnd = exp.then.loc.end.offset;
          const elseStart = exp.else.loc.start.offset;

          // Extract the exact text segments
          const beforeCond = this.originalSource.substring(exprStart, exp.cond.loc.start.offset);
          const afterCond = this.originalSource.substring(condEnd, thenStart);
          const afterThen = this.originalSource.substring(thenEnd, elseStart);

          // Check if we have "if" at the start
          if (beforeCond.trim().startsWith('if')) {
            // Extract spacing between keywords
            const thenIdx = afterCond.indexOf('then');
            const elseIdx = afterThen.indexOf('else');

            if (thenIdx >= 0 && elseIdx >= 0) {
              let beforeThen = afterCond.substring(0, thenIdx);
              const afterThenKw = afterCond.substring(thenIdx + 4);
              let beforeElse = afterThen.substring(0, elseIdx);
              const afterElseKw = afterThen.substring(elseIdx + 4);

              // Special case: if condition is parenthesized and there's no space before 'then',
              // the parser might have included the space in the condition's location
              if (exp.cond.value.kind === 'Paren' && beforeThen === '' &&
                  condEnd > 0 && this.originalSource[condEnd - 1] === ' ') {
                beforeThen = ' ';
              }

              // Similar case for then expression: if there's no space before 'else',
              // the parser might have included the space in the then expression's location
              if (beforeElse === '' && thenEnd > 0 && this.originalSource[thenEnd - 1] === ' ') {
                beforeElse = ' ';
              }

              // Extract spacing after 'if'
              const ifIdx = beforeCond.indexOf('if');
              if (ifIdx >= 0) {
                const afterIf = beforeCond.substring(ifIdx + 2);

                // Fix double brace issue: the elseStr already contains double braces due to Brace processing
                let finalElseStr = elseStr;

                // If elseStr starts with '{{' and ends with '}}', remove the outer braces
                if (elseStr.startsWith('{{') && elseStr.endsWith('}}')) {
                  finalElseStr = elseStr.substring(1, elseStr.length - 1);
                }

                return `if${afterIf}${condStr}${beforeThen}then${afterThenKw}${thenStr}${beforeElse}else${afterElseKw}${finalElseStr}`;
              }
            }
          }
        }

        // Fallback: construct with standard spacing
        return `if ${condStr} then ${thenStr} else ${elseStr}`;
      case 'For':
        return `for (${this.print(exp.expr)})`;
      case 'ForEach':
        const baseFor = `for (${exp.loopVar.value} : ${this.print(exp.expr)}):`;
        if (exp.body) {
          // Check if body is an empty block (just colon with no content)
          if (exp.body?.value?.kind === 'Block') {
            const blockBody = exp.body.value.expr;
            // Check if block is empty (start and end at same position)
            if (!blockBody || (blockBody?.loc?.start?.offset === blockBody?.loc?.end?.offset)) {
              // Empty block, don't add newline
              return baseFor;
            }
            return `${baseFor}\n    ${this.print(exp.body)}`;
          }
          // Inline body
          return `${baseFor} ${this.print(exp.body)}`;
        }
        // No body, just the colon
        return baseFor;
      case 'ForEachIndexed':
        return `for (${exp.indexVar.value}, ${exp.itemVar.value} : ${this.print(exp.expr)}): ${this.print(exp.body)}`;
      case 'ForRange':
        const baseForRange = `for (${exp.loopVar.value} := ${this.print(exp.rangeExpr)}):`;
        if (exp.body) {
          // Check if body is an empty block
          if (exp.body?.value?.kind === 'Block') {
            const blockBody = exp.body.value.expr;
            if (!blockBody || (blockBody?.loc?.start?.offset === blockBody?.loc?.end?.offset)) {
              return baseForRange;
            }
            return `${baseForRange}\n    ${this.print(exp.body)}`;
          }
          return `${baseForRange} ${this.print(exp.body)}`;
        }
        return baseForRange;
      case 'Range':
        return `${this.print(exp.left)}..${this.print(exp.right)}`;
      case 'While':
        const whileExpr = this.print(exp.expr);
        const whileBody = exp.body ? this.print(exp.body) : '';

        // Check if this is an infinite loop (loop:) by checking if expr is True
        if (exp.expr.value.kind === 'True') {
          return `loop:${whileBody}`;
        }

        // Regular while loop - check original formatting
        if (this.options.preserveOriginalFormatting && this.originalSource && exp.expr && exp.expr.loc) {
          const exprStart = exp.expr.loc.start.offset;
          const beforeExpr = this.originalSource.substring(0, exprStart);

          // Check if original uses parentheses syntax: while (condition):
          if (beforeExpr.includes('while (') && this.originalSource.substring(exp.expr.loc.end.offset).startsWith('):')) {
            return `while (${whileExpr}):${whileBody}`;
          }
          // Otherwise use do syntax: while condition do body
          else {
            return `while ${whileExpr}${whileBody ? ` do ${whileBody}` : ''}`;
          }
        }

        // Default to parentheses syntax
        return `while (${whileExpr})${whileBody ? `: ${whileBody}` : ''}`;
      case 'Case':
        // Handle case statements
        const exprStr = this.print(exp.expr);

        // Try to preserve original spacing around parentheses
        let casePrefix = 'case (';
        let caseSuffix = '):';

        if (this.options.preserveOriginalFormatting && this.originalSource &&
            exp.expr && exp.expr.loc) {
          // Look for 'case' before the expression
          const exprStart = exp.expr.loc.start.offset;
          const searchStart = Math.max(0, exprStart - 10);
          const beforeExpr = this.originalSource.substring(searchStart, exprStart);
          const caseIdx = beforeExpr.lastIndexOf('case');

          if (caseIdx >= 0) {
            const afterCase = beforeExpr.substring(caseIdx + 4);
            // Preserve the exact spacing after 'case'
            casePrefix = 'case' + afterCase;
          }
        }

        if (exp.arms && exp.arms.length === 0) {
          // Empty case (incomplete statement)
          return `${casePrefix}${exprStr}${caseSuffix}`;
        }
        // Case with arms
        let caseStr = `${casePrefix}${exprStr}${caseSuffix}`;
        if (exp.arms && exp.arms.length > 0) {
          if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
            // In lossless mode, preserve the exact original formatting for the case body
            const caseStart = exp.expr.loc.end.offset; // After the case expression
            const caseEnd = wrapper.loc.end.offset;

            // Find the colon after the case expression
            let colonPos = caseStart;
            while (colonPos < this.originalSource.length && this.originalSource[colonPos] !== ':') {
              colonPos++;
            }

            if (colonPos < this.originalSource.length) {
              // Extract everything after the colon
              const caseBody = this.originalSource.substring(colonPos + 1, caseEnd);
              caseStr += caseBody; // Don't add another colon since caseStr already ends with ':'
            } else {
              // Fallback to constructed formatting
              const armsStr = exp.arms.map((arm: any) => {
                return `${this.print(arm.pattern)} => ${this.print(arm.result)}`;
              });
              caseStr += '\n' + armsStr.map((s: string) => '    ' + s).join('\n');
            }
          } else {
            // Check if arms should be on new lines (indented)
            const armsStr = exp.arms.map((arm: any) => {
              return `${this.print(arm.pattern)} => ${this.print(arm.result)}`;
            });

            // If original source available, check for indentation pattern
            if (this.originalSource) {
              // Arms are likely indented on new lines
              caseStr += '\n' + armsStr.map((s: string) => '        ' + s).join('\n');
            } else {
              // Default formatting
              caseStr += '\n' + armsStr.map((s: string) => '    ' + s).join('\n');
            }
          }
        }
        return caseStr;
      case 'Break':
        return 'break';
      case 'Continue':
        return 'continue';
      case 'Return':
        return exp.value ? `return ${this.print(exp.value)}` : 'return';

      // Declarations
      case 'Class':
        const expClass = exp as any; // Type assertion to handle different Class variants
        let classResult = 'class';

        // Add specifiers/modifiers like <abstract><epic_internal>
        if (expClass.specifiers && expClass.specifiers.length > 0) {
          expClass.specifiers.forEach((spec: string) => {
            classResult += `<${spec}>`;
          });
        }

        // Add parentheses with parents if any
        if (expClass.parents && expClass.parents.length > 0) {
          classResult += `(${expClass.parents.map((parent: any) => parent.value).join(', ')})`;
        } else if (expClass.parent) {
          // Handle old-style parent field
          classResult += `(${this.print(expClass.parent)})`;
        }
        // No else clause - don't add () when there are no parents

        // Handle class body - if it's a Block, print without braces
        if (exp.body && exp.body.value && exp.body.value.kind === 'Block') {
          if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
            // In lossless mode, find the actual indented content by looking at the source
            const blockStart = exp.body.loc.start.offset;
            const blockEnd = exp.body.loc.end.offset;

            // Find the newline that precedes the block content
            let searchStart = blockStart - 1;
            while (searchStart >= 0 && this.originalSource[searchStart] !== '\n') {
              searchStart--;
            }

            if (searchStart >= 0) {
              // Extract from the newline onwards to include indentation
              const blockWithIndent = this.originalSource.substring(searchStart, blockEnd);
              return `${classResult}:${blockWithIndent}`;
            } else {
              // Fallback if no newline found
              return `${classResult}:${this.print(exp.body)}`;
            }
          } else {
            const blockContent = exp.body.value.expr;
            if (blockContent && blockContent.value) {
              if (blockContent.value.kind === 'List' && blockContent.value.elements.length === 0) {
                // Empty class body
                return `${classResult}:`;
              } else if (blockContent.value.kind === 'List') {
                // Multiple members in class body
                const elements = blockContent.value.elements;
                const indentedElements = elements.map((elem: any) =>
                  '        ' + this.print(elem).split('\n').join('\n        ')
                );
                return `${classResult}:\n${indentedElements.join('\n')}`;
              } else {
                // Single member in class body
                return `${classResult}:\n        ${this.print(blockContent)}`;
              }
            }
          }
        }
        return `${classResult}: ${this.print(exp.body)}`;
      case 'Struct':
        return `struct: ${this.print(exp.body)}`;
      case 'Enum':
        if ('body' in exp) {
          return `enum: ${this.print(exp.body)}`;
        } else {
          return `enum`;
        }
      case 'EnumDecl':
        const enumName = this.print(exp.name);
        const enumValues = exp.values.map((v: any) => v.value.name).join(', ');
        const specifiers = exp.specifiers && exp.specifiers.length > 0 ? exp.specifiers.map(s => `<${s}>`).join('') : '';
        return `${enumName} := enum${specifiers}{${enumValues}}`;
      case 'Module':
        return this.printModule(exp);

      // Function declarations
      case 'FuncDecl':
        return this.printFuncDecl(exp.decl);

      // Specifiers
      case 'Specifier':
        return `<${exp.spec}>`;

      // Variables
      case 'ExpVar':
        if (exp.pattern) {
          // Print pattern without trailing spaces
          const patternStr = this.printWithoutTrailingTrivia(exp.pattern);
          let result = `var ${patternStr}`;

          if (exp.type) {
            // Try to preserve original spacing around colon
            if (this.options.preserveOriginalFormatting && this.originalSource &&
                exp.pattern && exp.pattern.loc && exp.type.loc) {
              // Find where the actual pattern name ends (without trailing spaces)
              const patternStart = exp.pattern.loc.start.offset;
              let actualPatternEnd = patternStart;

              // Find the end of the pattern name (skip spaces)
              while (actualPatternEnd < this.originalSource.length &&
                     this.originalSource[actualPatternEnd] &&
                     !/[\s:]/.test(this.originalSource[actualPatternEnd])) {
                actualPatternEnd++;
              }

              const typeStart = exp.type.loc.start.offset;

              // Get everything between the actual pattern end and type start
              const betweenWithColon = this.originalSource.substring(actualPatternEnd, typeStart);
              const colonIdx = betweenWithColon.indexOf(':');

              if (colonIdx >= 0) {
                // Extract spacing before and after colon
                const beforeColon = betweenWithColon.substring(0, colonIdx);
                let afterColon = betweenWithColon.substring(colonIdx + 1);

                // Check if the type will print with a leading '?'
                const typeStr = this.print(exp.type);

                // If afterColon ends with '?' and typeStr starts with '?', remove the duplicate
                if (afterColon.endsWith('?') && typeStr.startsWith('?')) {
                  afterColon = afterColon.slice(0, -1);
                }

                result += beforeColon + ':' + afterColon + typeStr;
              } else {
                // Fallback if colon not found
                result += ' : ' + this.print(exp.type);
              }
            } else {
              // Fallback without source
              result += ' : ' + this.print(exp.type);
            }
          }

          // Preserve spacing around equals
          let equalsSpacing = ' = ';

          if (this.options.preserveOriginalFormatting && this.originalSource &&
              exp.expr && exp.expr.loc) {
            const exprStart = exp.expr.loc.start.offset;
            // Look for = before the expression
            const searchStart = Math.max(0, exprStart - 5);
            const beforeExpr = this.originalSource.substring(searchStart, exprStart);
            const equalsIdx = beforeExpr.lastIndexOf('=');

            if (equalsIdx >= 0) {
              // Extract spacing around =
              const beforeEquals = equalsIdx > 0 && beforeExpr[equalsIdx - 1] === ' ' ? ' ' : '';
              const afterEquals = beforeExpr.substring(equalsIdx + 1);
              equalsSpacing = beforeEquals + '=' + afterEquals;
            }
          }

          result += equalsSpacing + this.print(exp.expr);
          return result;
        } else {
          return `var ${this.print(exp.expr)}`;
        }
      case 'Set':
        // Check if this is a compound assignment (e.g., set Counter += 1)
        // In compound assignments, the value is a binary operation where left side equals the target
        if (exp.value && exp.value.value &&
            (exp.value.value.kind === 'Add' || exp.value.value.kind === 'Subtract' ||
             exp.value.value.kind === 'Multiply' || exp.value.value.kind === 'Divide') &&
            exp.value.value.left && exp.target) {

          // Check if the left side of the operation is the same as the target
          const targetExp = exp.target.value;
          const leftExp = exp.value.value.left.value;

          let targetName: string | undefined;
          let leftName: string | undefined;

          if (targetExp?.kind === 'Pat' && targetExp.pattern?.kind === 'Name' && targetExp.pattern.ident?.kind === 'IdentName') {
            targetName = targetExp.pattern.ident.name;
          }

          if (leftExp?.kind === 'Pat' && leftExp.pattern?.kind === 'Name' && leftExp.pattern.ident?.kind === 'IdentName') {
            leftName = leftExp.pattern.ident.name;
          }

          if (targetName && leftName && targetName === leftName) {
            // This is a compound assignment
            const op = exp.value.value.kind === 'Add' ? '+=' :
                       exp.value.value.kind === 'Subtract' ? '-=' :
                       exp.value.value.kind === 'Multiply' ? '*=' : '/=';
            return `set ${this.print(exp.target)} ${op} ${this.print(exp.value.value.right)}`;
          }
        }

        // Regular set statement
        return `set ${this.print(exp.target)} = ${this.print(exp.value)}`;

      // Assignment operators
      case 'InfixColonEqual':
        // Handle object construction pattern where right side is a List with mixed assignments
        if (exp.right && exp.right.value && exp.right.value.kind === 'List' && exp.right.value.elements) {
          const elements = exp.right.value.elements;
          if (elements.length > 0) {
            // This is object construction - use no spaces around :=
            const firstValue = this.print(elements[0]);
            const leftAssign = `${this.print(exp.left)}:=${firstValue}`;

            // Remaining elements should be other assignments
            const otherAssigns = elements.slice(1).map((e: any) => {
              if (e.value && e.value.kind === 'Assign') {
                return `${this.print(e.value.left)}:=${this.print(e.value.right)}`;
              }
              return this.print(e);
            });

            return [leftAssign, ...otherAssigns].join(', ');
          }
        }
        // Regular assignment - use spaces around :=
        return `${this.print(exp.left)} := ${this.print(exp.right)}`;

      // Pattern handling
      case 'Pat':
        // Check if this is a standalone block keyword that should preserve colon
        if (this.originalSource && exp.pattern) {
          const pattern = exp.pattern as any;
          if (pattern.kind === 'Name' && pattern.ident && pattern.ident.name) {
            const name = pattern.ident.name;
            if (['try', 'loop', 'block', 'spawn', 'race', 'sync', 'branch'].includes(name)) {
              // Look for this keyword in source and preserve the colon if present
              const keywordIdx = this.originalSource.lastIndexOf(name);
              if (keywordIdx >= 0) {
                const afterKeyword = keywordIdx + name.length;
                if (afterKeyword < this.originalSource.length &&
                    this.originalSource[afterKeyword] === ':') {
                  return name + ':';
                }
              }
            }
          }
        }
        return this.printPat(exp);

      // Advanced language constructs
      case 'GenericType':
        return `${this.print(exp.base)}<${exp.typeArgs.map(arg => this.print(arg)).join(', ')}>`;

      case 'Attribute':
        if (exp.args && exp.args.length > 0) {
          return `@${exp.name}(${exp.args.map(arg => this.print(arg)).join(', ')})`;
        } else {
          return `@${exp.name}`;
        }

      case 'ClassDecl':
        let classStr = `${this.print(exp.name)} := class`;
        // Add attributes/modifiers like <abstract><epic_internal>
        if (exp.attributes && exp.attributes.length > 0) {
          exp.attributes.forEach((attr: any) => {
            if (attr.value && attr.value.kind === 'Ident') {
              classStr += `<${attr.value.name}>`;
            }
          });
        }
        // Add type parameters (if different from attributes)
        if (exp.typeParams && exp.typeParams.length > 0) {
          // Only add if not already handled as attributes
          classStr += `<${exp.typeParams.map((p: any) => this.print(p)).join(', ')}>`;
        }
        // Add base class or empty parentheses
        if (exp.baseClass) {
          classStr += `(${this.print(exp.baseClass)})`;
        } else if (!exp.body || (exp.attributes && exp.attributes.length > 0)) {
          classStr += '()';
        }
        // Add body
        if (exp.body) {
          classStr += `:${this.print(exp.body)}`;
        } else {
          classStr += ':';
        }
        return classStr;

      case 'InterfaceDecl':
        let interfaceStr = '';
        // Add the interface name if present
        if (exp.name) {
          interfaceStr = `${this.print(exp.name)} := `;
        }
        interfaceStr += 'interface';
        if (exp.typeParams && exp.typeParams.length > 0) {
          interfaceStr += `<${exp.typeParams.map(p => this.print(p)).join(', ')}>`;
        }
        interfaceStr += ':';

        // Preserve original formatting for body
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const colonPos = this.originalSource.lastIndexOf(':', wrapper.loc.end.offset);
          if (colonPos >= 0 && colonPos < wrapper.loc.end.offset) {
            const afterColon = this.originalSource.substring(colonPos + 1, wrapper.loc.end.offset);
            // Preserve whatever follows the colon (could be newline, spaces, or nothing)
            interfaceStr += afterColon;
          }
        } else if (exp.body && exp.body.value &&
                   exp.body.value.kind === 'Block' &&
                   exp.body.value.expr?.value?.kind === 'List' &&
                   exp.body.value.expr.value.elements?.length > 0) {
          // Only add newline and body if there are actual elements
          interfaceStr += `\n${this.print(exp.body)}`;
        }

        return interfaceStr;

      case 'ModuleDecl':
        let moduleStr = '';
        // Add the module name if present
        if (exp.name) {
          moduleStr = `${this.print(exp.name)} := `;
        }
        moduleStr += 'module';
        if (exp.typeParams && exp.typeParams.length > 0) {
          moduleStr += `<${exp.typeParams.map(p => this.print(p)).join(', ')}>`;
        }
        moduleStr += ':';

        // Preserve original formatting for body
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const colonPos = this.originalSource.lastIndexOf(':', wrapper.loc.end.offset);
          if (colonPos >= 0 && colonPos < wrapper.loc.end.offset) {
            const afterColon = this.originalSource.substring(colonPos + 1, wrapper.loc.end.offset);
            // Preserve whatever follows the colon (could be newline, spaces, or nothing)
            moduleStr += afterColon;
          }
        } else if (exp.body && exp.body.value &&
                   exp.body.value.kind === 'Block' &&
                   exp.body.value.expr?.value?.kind === 'List' &&
                   exp.body.value.expr.value.elements?.length > 0) {
          // Only add newline and body if there are actual elements
          moduleStr += `\n${this.print(exp.body)}`;
        }

        return moduleStr;

      case 'PropertyDecl':
        let propStr = this.print(exp.name);

        if (exp.type) {
          // Use comprehensive spacing preservation
          const spacing = this.getSpacingBetweenNodes(exp.name, exp.type);

          // Check if spacing already contains attributes to avoid duplication
          let shouldAddAttributes = true;
          if (exp.attributes && exp.attributes.length > 0) {
            // Check if any attribute is already in the spacing
            const hasAttributeInSpacing = exp.attributes.some(attr => {
              const attrExp = attr.value;
              if (attrExp.kind === 'Attribute') {
                return spacing.includes(`<${attrExp.name}>`);
              }
              return false;
            });
            shouldAddAttributes = !hasAttributeInSpacing;
          }

          // Add attributes/specifiers like <public> only if not already in spacing
          if (shouldAddAttributes && exp.attributes && exp.attributes.length > 0) {
            propStr += exp.attributes.map(attr => {
              const attrExp = attr.value;
              if (attrExp.kind === 'Attribute') {
                return `<${attrExp.name}>`;
              } else {
                return `<${this.print(attr)}>`;
              }
            }).join('');
          }

          propStr += spacing;
          propStr += this.print(exp.type);
        } else {
          // No type, so add attributes manually
          if (exp.attributes && exp.attributes.length > 0) {
            propStr += exp.attributes.map(attr => {
              const attrExp = attr.value;
              if (attrExp.kind === 'Attribute') {
                return `<${attrExp.name}>`;
              } else {
                return `<${this.print(attr)}>`;
              }
            }).join('');
          }
        }

        if (exp.value) {
          propStr += ` = ${this.print(exp.value)}`;
        }

        return propStr;

      case 'MethodDecl':
        let methodStr = this.print(exp.name);
        if (exp.typeParams && exp.typeParams.length > 0) {
          methodStr += `<${exp.typeParams.map(p => this.print(p)).join(', ')}>`;
        }
        if (exp.params && exp.params.length > 0) {
          methodStr += `(${exp.params.map(p => this.print(p)).join(', ')})`;
        } else {
          methodStr += '()';
        }
        if (exp.returnType) {
          methodStr += `:${this.print(exp.returnType)}`;
        }
        if (exp.body) {
          methodStr += ` = ${this.print(exp.body)}`;
        }
        return methodStr;

      case 'Interface':
        return `interface:${this.print(exp.body)}`;

      case 'Decorator':
        // For lossless parsing, preserve original source if available
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          const start = wrapper.loc.start.offset;
          const end = wrapper.loc.end.offset;
          if (start >= 0 && end <= this.originalSource.length) {
            return this.originalSource.substring(start, end);
          }
        }

        // If we have the original source and the decorator shows {...}, get the real content
        if (this.originalSource && (exp as any).name && (exp as any).name.includes('{...}')) {
          // Try to extract from original source
          const decoratorName = (exp as any).name.split('{')[0]; // e.g., "@editable"
          const startIdx = this.originalSource.indexOf(decoratorName);
          if (startIdx >= 0) {
            // Find the matching closing brace
            let braceCount = 0;
            let i = startIdx + decoratorName.length;
            let foundOpen = false;
            let endIdx = i;

            while (i < this.originalSource.length) {
              if (this.originalSource[i] === '{') {
                braceCount++;
                foundOpen = true;
              } else if (this.originalSource[i] === '}') {
                braceCount--;
                if (braceCount === 0 && foundOpen) {
                  endIdx = i + 1;
                  break;
                }
              }
              i++;
            }

            if (endIdx > startIdx) {
              return this.originalSource.substring(startIdx, endIdx);
            }
          }
        }
        return (exp as any).name;

      case 'Lam':
        // Lambda expression - check if original uses 'lambda' or '=>' syntax
        if (this.options.preserveOriginalFormatting && this.originalSource && wrapper && wrapper.loc) {
          // Look for lambda keyword in the range of this specific lambda expression
          const lambdaStart = wrapper.loc.start.offset;
          const lambdaEnd = wrapper.loc.end.offset;
          const lambdaSource = this.originalSource.substring(lambdaStart, lambdaEnd);

          if (lambdaSource.startsWith('lambda')) {
            // This is lambda syntax, preserve original formatting
            return lambdaSource;
          }
        }

        // Default to arrow syntax: param => body
        // Try to preserve original spacing around '=>'
        // Don't include trailing trivia for param since we're controlling the spacing
        const paramStr = this.printWithoutTrailingTrivia(exp.param);
        const bodyStr = this.print(exp.body);

        if (this.options.preserveOriginalFormatting && this.originalSource &&
            exp.param && exp.param.loc && exp.body && exp.body.loc) {
          // Use actual source positions to find the arrow
          const paramEnd = exp.param.loc.end.offset;
          const bodyStart = exp.body.loc.start.offset;

          // Look for => between param and body
          const betweenText = this.originalSource.substring(paramEnd, bodyStart);
          const arrowIndex = betweenText.indexOf('=>');

          if (arrowIndex >= 0) {
            // Extract exact spacing before and after =>
            let spaceBefore = betweenText.substring(0, arrowIndex);
            const spaceAfter = betweenText.substring(arrowIndex + 2);

            // If spaceBefore is empty but the original has a space before =>,
            // it means the param loc includes the space. Check one char back.
            if (spaceBefore === '' && paramEnd > 0 && this.originalSource[paramEnd - 1] === ' ') {
              spaceBefore = ' ';
            }

            // Debug: console.log('Lam spacing:', { betweenText, spaceBefore, spaceAfter, paramStr, bodyStr });
            return `${paramStr}${spaceBefore}=>${spaceAfter}${bodyStr}`;
          }
        }

        return `${paramStr} => ${bodyStr}`;

      default:
        // For any unhandled cases, return a placeholder
        return `[${(exp as any).kind}]`;
    }
  }

  private printString(text: string, interpolations: [L<Exp>, L<string>][]): string {
    if (interpolations.length === 0) {
      // Escape special characters back to their escape sequences
      let escapedText = text
        .replace(/\\/g, '\\\\')  // Backslash must be first
        .replace(/\n/g, '\\n')
        .replace(/\r/g, '\\r')
        .replace(/\t/g, '\\t')
        .replace(/"/g, '\\"');

      return `"${escapedText}"`;
    }

    // Handle interpolations based on whether we have single or multiple interpolations
    let result = '"' + text;

    // Sort interpolations by their position to process them in correct order
    const sortedInterpolations = [...interpolations].sort((a, b) =>
      a[0].loc.start.offset - b[0].loc.start.offset
    );

    if (interpolations.length === 1) {
      // Single interpolation: analyze position to handle spacing correctly
      const [expr, suffix] = sortedInterpolations[0];
      const interpolationText = `{${this.print(expr)}}${suffix.value}`;
      const interpStartOffset = expr.loc.start.offset;

      const spaceIndex = result.indexOf(' ');
      if (spaceIndex >= 0) {
        if (interpStartOffset === 2) {
          // Interpolation at start: "{greeting} world!"
          // AST text: " world!" -> result should be: "" + {greeting} + " world!"
          // The space at index 1 should be preserved as part of what comes after
          result = '"' + interpolationText + result.substring(spaceIndex);
        } else {
          // Interpolation in middle: "Hello {name}!"
          // AST text: "Hello !" -> result should be: "Hello {name}!"
          result = result.substring(0, spaceIndex) +
                  ' ' + interpolationText +
                  result.substring(spaceIndex + 1);
        }
      } else {
        // Edge case: no space found
        result = result + interpolationText;
      }
    } else {
      // Multiple interpolations: simple approach using text segmentation
      // For complex cases, split the AST text and interleave with interpolations

      // Strategy: identify likely separation points in the text
      // and distribute interpolations accordingly

      if (interpolations.length === 2 && text.length >= 2) {
        // Special case for 2 interpolations
        // For "{Greeting}, {Target.GetName()}!" with text ", !"
        // We want: "" + {Greeting} + ", " + {Target.GetName()} + "!"

        const [expr1, suffix1] = sortedInterpolations[0];
        const [expr2, suffix2] = sortedInterpolations[1];

        // Check if this is the double-space case (like "Start  middle  end")
        const doubleSpaceIndex = text.indexOf('  ');
        if (doubleSpaceIndex >= 0) {
          // Split on double spaces - this is the classic case
          const parts = text.split(/\s{2,}/);
          if (parts.length === 3) {
            result = '"' + parts[0] + ' ' + `{${this.print(expr1)}}${suffix1.value}` +
                    ' ' + parts[1] + ' ' + `{${this.print(expr2)}}${suffix2.value}` +
                    ' ' + parts[2];
          } else {
            // Fallback for double-space case
            result = '"' + `{${this.print(expr1)}}${suffix1.value}` + text + `{${this.print(expr2)}}${suffix2.value}`;
          }
        } else {
          // Look for punctuation followed by space (like ", !")
          let splitPoint = -1;
          for (let i = 0; i < text.length - 1; i++) {
            if (text[i].match(/[,;:.]/) && text[i + 1] === ' ') {
              splitPoint = i + 1; // Include the punctuation in first part
              break;
            }
          }

          if (splitPoint > 0) {
            // Split found: first part goes between interpolations
            const middlePart = text.substring(0, splitPoint + 1); // Include the space
            const endPart = text.substring(splitPoint + 1);

            result = '"' + `{${this.print(expr1)}}${suffix1.value}` +
                    middlePart + `{${this.print(expr2)}}${suffix2.value}` +
                    endPart;
          } else {
            // No clear split, distribute evenly
            const midPoint = Math.floor(text.length / 2);
            const firstPart = text.substring(0, midPoint);
            const secondPart = text.substring(midPoint);

            result = '"' + `{${this.print(expr1)}}${suffix1.value}` +
                    firstPart + `{${this.print(expr2)}}${suffix2.value}` +
                    secondPart;
          }
        }
      } else {
        // General case: distribute interpolations evenly across text segments
        const textSegments = text.split(/(\s+|[,;:.]\s*)/);
        const segmentsPerInterp = Math.max(1, Math.floor(textSegments.length / (interpolations.length + 1)));

        let segmentIndex = 0;
        let reconstructed = '"';

        for (let i = 0; i < interpolations.length; i++) {
          const [expr, suffix] = sortedInterpolations[i];

          // Add text segments before this interpolation
          let segmentsToAdd = [];
          for (let j = 0; j < segmentsPerInterp && segmentIndex < textSegments.length; j++) {
            segmentsToAdd.push(textSegments[segmentIndex]);
            segmentIndex++;
          }

          reconstructed += segmentsToAdd.join('');
          reconstructed += `{${this.print(expr)}}${suffix.value}`;
        }

        // Add remaining segments
        while (segmentIndex < textSegments.length) {
          reconstructed += textSegments[segmentIndex];
          segmentIndex++;
        }

        result = reconstructed;
      }
    }

    result += '"';
    return result;
  }

  private printFuncDecl(decl: FuncDecl): string {
    let result = decl.name;

    // Pre-specifiers (like <override>)
    if (decl.preSpecifiers.length > 0) {
      result += decl.preSpecifiers.map(s => `<${s}>`).join('');
    }

    // Parameters (always include parentheses for function declarations)
    result += '(' + decl.params.map(p => this.printFuncParam(p)).join(', ') + ')';

    // Post-specifiers (like <suspends>)
    if (decl.postSpecifiers.length > 0) {
      result += decl.postSpecifiers.map(s => `<${s}>`).join('');
    }

    // Return type (preserve original spacing)
    if (decl.returnType) {
      if (this.options.preserveOriginalFormatting && this.originalSource && decl.returnType.loc) {
        // Get the text right before the return type
        const returnTypeStart = decl.returnType.loc.start.offset;
        // Look backward from return type position to find the colon
        let colonPos = returnTypeStart - 1;
        while (colonPos >= 0 && this.originalSource[colonPos] !== ':') {
          colonPos--;
        }

        if (colonPos >= 0) {
          // Extract the colon and any surrounding spaces
          const beforeColon = colonPos > 0 && this.originalSource[colonPos - 1] === ' ' ? ' ' : '';
          const afterColon = this.originalSource.substring(colonPos + 1, returnTypeStart);
          result += `${beforeColon}:${afterColon}`;
        } else {
          // Fallback
          result += ':';
        }
        result += this.print(decl.returnType);
      } else {
        // Fallback - no parameters = no space, with parameters = space
        if (decl.params.length === 0) {
          result += `:${this.print(decl.returnType)}`;
        } else {
          result += ` : ${this.print(decl.returnType)}`;
        }
      }
    }

    // Assignment operator and body (only if there's a body)
    if (decl.body) {
      // Use original spacing preservation if available
      if (this.options.preserveOriginalFormatting && this.originalSource && decl.body?.loc) {
        // Look for the assignment operator in the specific context near the function body
        const assignOp = decl.isDefinition ? ':=' : '=';
        const bodyStart = decl.body.loc.start.offset;

        // Look backward from the body to find the assignment operator
        let assignPos = bodyStart - 1;
        while (assignPos >= 0 && /\s/.test(this.originalSource[assignPos])) {
          assignPos--; // Skip whitespace and newlines before body
        }

        // Now look for the assignment operator just before this position
        const beforeAssign = assignPos - assignOp.length + 1;
        if (beforeAssign >= 0 && this.originalSource.substring(beforeAssign, assignPos + 1) === assignOp) {
          // Found the assignment operator, check spacing around it
          const spaceBefore = beforeAssign > 0 && this.originalSource[beforeAssign - 1] === ' ';
          const spaceAfter = assignPos + 1 < bodyStart && this.originalSource[assignPos + 1] === ' ';

          if (spaceBefore && spaceAfter) {
            result += ' ' + assignOp + ' ';
          } else if (spaceBefore) {
            result += ' ' + assignOp;
          } else if (spaceAfter) {
            result += assignOp + ' ';
          } else {
            result += assignOp;
          }
        } else {
          // Fallback to no space after assignment operator
          result += assignOp;
        }
      } else {
        // Fallback to no space after assignment operator (matching original format)
        result += decl.isDefinition ? ':=' : '=';
      }
      result += this.print(decl.body);
    }

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
      // Try to preserve original spacing around the colon
      let colonSpacing = ' : ';

      if (this.options.preserveOriginalFormatting && this.originalSource && param.type.loc) {
        // Look for colon before the type
        const typeStart = param.type.loc.start.offset;
        const searchStart = Math.max(0, typeStart - 5);
        const beforeType = this.originalSource.substring(searchStart, typeStart);
        const colonIdx = beforeType.lastIndexOf(':');

        if (colonIdx >= 0) {
          // Check spacing around colon
          const beforeColon = colonIdx > 0 && beforeType[colonIdx - 1] === ' ' ? ' ' : '';
          const afterColon = beforeType.substring(colonIdx + 1);
          colonSpacing = beforeColon + ':' + afterColon;
        }
      }

      result += colonSpacing + this.print(param.type);
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

  private printList(exp: any): string {
    let result = '';

    // Handle decorators if present
    if (exp.decorators && exp.decorators.length > 0) {
      result += exp.decorators.join(' ');
      if (exp.elements && exp.elements.length > 0) {
        result += '(';
        if (this.options.preserveOriginalFormatting) {
          // For lossless parsing, print elements without adding commas
          // The trivia between elements contains the original formatting
          result += exp.elements.map((e: any) => this.printLeading(e) + this.printExp(e.value, e)).join('');
        } else {
          result += exp.elements.map((e: any) => this.print(e)).join(', ');
        }
        result += ')';
      } else {
        result += '()';
      }
    } else {
      // Handle elements normally
      if (exp.elements && exp.elements.length > 0) {
        if (this.options.preserveOriginalFormatting && this.originalSource) {
          // Try to preserve original spacing between elements
          let elementsResult = '';
          for (let i = 0; i < exp.elements.length; i++) {
            const elem = exp.elements[i];
            elementsResult += this.printLeading(elem) + this.printExp(elem.value, elem);

            // Add spacing between elements based on original source, but only if the next element doesn't already have leading trivia
            if (i < exp.elements.length - 1 && elem.loc && exp.elements[i + 1].loc) {
              const nextElem = exp.elements[i + 1];
              const thisEnd = elem.loc.end.offset;
              const nextStart = exp.elements[i + 1].loc.start.offset;
              // Check if the next element has leading trivia that would include the spacing
              const hasLeadingTrivia = nextElem.leadingTrivia && nextElem.leadingTrivia.trivia && nextElem.leadingTrivia.trivia.length > 0;

              if (!hasLeadingTrivia) {
                if (thisEnd < nextStart && nextStart <= this.originalSource.length) {
                  // Preserve the exact spacing from the source
                  let spacing = this.originalSource.substring(thisEnd, nextStart);

                  // Special case: if the element contains BracketInvoke/ParenInvoke and the spacing starts with their closing chars,
                  // skip them since the lossless printing already includes them
                  function containsInvokeAtEnd(node: any): boolean {
                    if (!node || typeof node !== 'object') return false;
                    if (node.kind === 'BracketInvoke' || node.kind === 'ParenInvoke') return true;
                    // Check nested structures (like assignment with invoke on right side)
                    if (node.right) return containsInvokeAtEnd(node.right);
                    if (node.left) return containsInvokeAtEnd(node.left);
                    if (node.value) return containsInvokeAtEnd(node.value);
                    return false;
                  }

                  if (containsInvokeAtEnd(elem.value) && spacing.startsWith(']')) {
                    spacing = spacing.substring(1);
                  }
                  if (containsInvokeAtEnd(elem.value) && spacing.startsWith(')')) {
                    spacing = spacing.substring(1);
                  }

                  elementsResult += spacing;
                }
              } else if (thisEnd === nextStart) {
                // Elements are adjacent but there might be spacing in the original
                // Check if we need to add spacing by examining the actual content
                const nextElement = exp.elements[i + 1];

                // If the next element is a keyword like 'and', 'or', we probably need a space
                if (nextElement.value && nextElement.value.kind === 'Pat' &&
                    nextElement.value.pattern && nextElement.value.pattern.ident &&
                    ['and', 'or', 'as', 'isa'].includes(nextElement.value.pattern.ident.name)) {
                  elementsResult += ' ';
                }
              }
            } else {
              // Last element - add its trailing trivia
              elementsResult += this.printTrailing(elem);
            }
          }
          result += elementsResult;
        } else if (this.options.preserveOriginalFormatting) {
          // Fallback if no source - still need to avoid double trailing trivia
          result += exp.elements.map((e: any, i: number) => {
            const base = this.printLeading(e) + this.printExp(e.value, e);
            // Only add trailing trivia for the last element
            return i === exp.elements.length - 1 ? base + this.printTrailing(e) : base;
          }).join('');
        } else {
          result += exp.elements.map((e: any) => this.print(e)).join(', ');
        }
      }
    }

    return result;
  }

  private printPat(exp: any): string {
    // If we have the original source, use it
    if (this.originalSource && exp.loc) {
      const start = exp.loc.start.offset;
      const end = exp.loc.end.offset;
      if (start >= 0 && end <= this.originalSource.length) {
        let sourceText = this.originalSource.slice(start, end);

        // Only remove trailing colon for class/interface declarations
        // Keep it for try:, loop:, etc.
        if (exp.pattern && exp.pattern.ident && exp.pattern.ident.name) {
          const name = exp.pattern.ident.name;
          // Don't remove colon from keywords that use block syntax
          if (!['try', 'loop', 'block', 'spawn', 'race', 'sync', 'branch'].includes(name)) {
            // Remove trailing colon for class names
            sourceText = sourceText.replace(/\s*:\s*$/, '');
          }
        }

        return sourceText.trim();
      }
    }

    let result = '';

    if (exp.pattern) {
      if (exp.pattern.kind === 'Name' && exp.pattern.ident) {
        if (exp.pattern.ident.kind === 'IdentName') {
          result += exp.pattern.ident.name;
        }
      }
    }

    // Handle specifier
    if (exp.specifier && exp.specifier.kind === 'Specifier') {
      result += `<${exp.specifier.spec}>`;
    }

    return result;
  }

  private printModule(exp: any): string {
    // Check if this is a using statement (Module with String body)
    if (exp.body && exp.body.value && exp.body.value.kind === 'String') {
      return `using { /${exp.body.value.text}}`;
    }

    // Check if the original source actually had "module:" at the start
    // If not, this is incorrectly wrapped by the parser and we should unwrap it
    // Use the body's location to check the source
    if (this.originalSource && exp.body && exp.body.loc) {
      const start = exp.body.loc.start.offset;
      // Check a bit before the body start to see if there's "module:"
      const checkStart = Math.max(0, start - 20); // Increase range to catch full module keyword
      const sourceCheck = this.originalSource.substring(checkStart, start);
      if (!sourceCheck.includes('module:') && !sourceCheck.includes('dule:')) {
        // This is not actually a module, just print the body content
        if (exp.body && exp.body.value) {
          if (exp.body.value.kind === 'List') {
            // Check for lambda misparsed as FuncDecl
            const elements = exp.body.value.elements;
            if (elements.length >= 1) {
              const firstElem = elements[0];
              if (firstElem?.value?.kind === 'FuncDecl' &&
                  firstElem.value.decl.name === 'lambda' &&
                  this.originalSource) {
                // This is a lambda expression misparsed as function + other elements
                // Return the original source
                const moduleStart = exp.body.loc.start.offset;
                const moduleEnd = exp.body.loc.end.offset;
                return this.originalSource.substring(moduleStart, moduleEnd);
              }
            }
            // Multiple elements - print them all with preserved spacing
            return this.printList(exp.body.value);
          } else {
            return this.print(exp.body);
          }
        }
      }
    }

    // Check if it's an empty module (Block with empty List)
    if (exp.body && exp.body.value && exp.body.value.kind === 'Block' &&
        exp.body.value.expr && exp.body.value.expr.value &&
        exp.body.value.expr.value.kind === 'List' &&
        exp.body.value.expr.value.elements.length === 0) {
      return `module:`;
    }

    // Otherwise, it's a regular module with body
    // Check if body is a Block (indented content without braces)
    if (exp.body && exp.body.value && exp.body.value.kind === 'Block') {
      // Print the block content without braces, with proper indentation
      const blockContent = exp.body.value.expr;
      if (blockContent && blockContent.value) {
        if (blockContent.value.kind === 'List') {
          // Multiple statements in the module body
          const elements = blockContent.value.elements;
          const indentedElements = elements.map((elem: any) =>
            '    ' + this.print(elem).split('\n').join('\n    ')
          );
          return `module:\n${indentedElements.join('\n')}`;
        } else {
          // Single statement in the module body
          return `module:\n    ${this.print(blockContent)}`;
        }
      }
    }

    // Fallback to default behavior
    return `module:\n    ${this.print(exp.body)}`;
  }

  private printIdent(ident: L<IdentExp>): string {
    const identValue = ident.value;
    let name = '';
    if (identValue.kind === 'IdentName') {
      name = identValue.name;
    } else if (identValue.kind === 'IdentQualName') {
      name = identValue.name.value;
    } else if (identValue.kind === 'IdentPath') {
      name = identValue.path.label.value;
    }
    return this.printLeading(ident) + name + this.printTrailing(ident);
  }

}

// Convenience function for printing with default options
export function printAST(exp: L<Exp>, options?: Partial<PrinterOptions>): string {
  const printer = new PrettyPrinter({ ...defaultPrinterOptions, ...options });

  // Special handling for top-level modules that contain Lists (file-level modules)
  // but not single using statements or explicit module declarations
  if (exp.value.kind === 'Module' &&
      exp.value.body && exp.value.body.value &&
      exp.value.body.value.kind === 'List') {

    // For lossless parsing, transfer Module-level trivia to the body
    if (options?.preserveOriginalFormatting && exp.leadingTrivia) {
      // Create a new body with combined trivia
      const body = { ...exp.value.body };
      const existingTrivia = body.leadingTrivia?.trivia || [];
      const moduleTrivia = exp.leadingTrivia.trivia || [];

      body.leadingTrivia = {
        trivia: [...moduleTrivia, ...existingTrivia]
      };

      // Also transfer trailing trivia if present
      if (exp.trailingTrivia) {
        const existingTrailing = body.trailingTrivia?.trivia || [];
        const moduleTrailing = exp.trailingTrivia.trivia || [];

        body.trailingTrivia = {
          trivia: [...existingTrailing, ...moduleTrailing]
        };
      }

      return printer.print(body);
    }

    return printer.print(exp.value.body);
  }

  return printer.print(exp);
}