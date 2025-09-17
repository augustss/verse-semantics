import { L } from './ast/location';
import { Exp } from './ast/expression';

// Simple AST-to-source printer for basic Verse constructs
export function printExp(exp: L<Exp>): string {
    const inner = exp.value;

    switch (inner.kind) {
        // Literals
        case 'Int':
            return inner.value.toString().replace('n', '');
        case 'Float':
            return inner.value.toString();
        case 'String':
            return `"${inner.text}"`;
        case 'True':
            return 'true';
        case 'False':
            return 'false';
        case 'Char':
            return `'${inner.value}'`;

        // Identifiers and patterns
        case 'Pat':
            if (inner.pattern.kind === 'Name' && inner.pattern.ident.kind === 'IdentName') {
                return inner.pattern.ident.name;
            }
            return '[pattern]';

        // Binary operators
        case 'Add':
            return `${printExp(inner.left)} + ${printExp(inner.right)}`;
        case 'Subtract':
            return `${printExp(inner.left)} - ${printExp(inner.right)}`;
        case 'Multiply':
            return `${printExp(inner.left)} * ${printExp(inner.right)}`;
        case 'Divide':
            return `${printExp(inner.left)} / ${printExp(inner.right)}`;
        case 'Exponent':
            return `${printExp(inner.left)} ^ ${printExp(inner.right)}`;
        case 'And':
            return `${printExp(inner.left)} and ${printExp(inner.right)}`;
        case 'Or':
            return `${printExp(inner.left)} or ${printExp(inner.right)}`;
        case 'Less':
            return `${printExp(inner.left)} < ${printExp(inner.right)}`;
        case 'Greater':
            return `${printExp(inner.left)} > ${printExp(inner.right)}`;
        // case 'Equal': // This node type no longer exists

        // Variable declarations
        case 'ExpVar':
            return `var ${printExp(inner.expr)}`;
        case 'Assign':
            return `${printExp(inner.left)} := ${printExp(inner.right)}`;

        // Function calls
        case 'ParenInvoke':
            return `${printExp(inner.func)}(${printExp(inner.arg)})`;

        // Control flow
        case 'If':
            const condition = printExp(inner.cond);
            return `if (${condition})`;

        // Array/List
        case 'Array':
            if (inner.elements && inner.elements.length > 0) {
                const elements = inner.elements.map(elem => printExp(elem)).join(', ');
                return `array{${elements}}`;
            }
            return 'array{}';

        // Parentheses
        case 'Paren':
            return `(${printExp(inner.expr)})`;

        // Tuples
        case 'Tuple':
            if (inner.elements.length === 1) {
                return `(${printExp(inner.elements[0])}, )`;
            } else {
                const elements = inner.elements.map(elem => printExp(elem)).join(', ');
                return `(${elements})`;
            }

        // For unimplemented constructs, show kind
        default:
            return `[${inner.kind}]`;
    }
}

// Main entry point for printing
export function printAST(ast: L<Exp>): string {
    return printExp(ast);
}