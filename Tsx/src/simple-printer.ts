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
        case 'And':
            return `${printExp(inner.left)} and ${printExp(inner.right)}`;
        case 'Or':
            return `${printExp(inner.left)} or ${printExp(inner.right)}`;
        case 'Less':
            return `${printExp(inner.left)} < ${printExp(inner.right)}`;
        case 'Greater':
            return `${printExp(inner.left)} > ${printExp(inner.right)}`;
        case 'Equal':
            return `${printExp(inner.left)} = ${printExp(inner.right)}`;

        // Variable declarations
        case 'ExpVar':
            return `var ${printExp(inner.expr)}`;
        case 'Assign':
            return `${printExp(inner.left)} := ${printExp(inner.right)}`;

        // Function calls
        case 'Apply':
            if (inner.args && inner.args.length > 0) {
                const args = inner.args.map(arg => printExp(arg)).join(', ');
                return `${printExp(inner.func)}(${args})`;
            }
            return `${printExp(inner.func)}()`;

        // Control flow
        case 'If':
            const condition = printExp(inner.condition);
            const thenExpr = printExp(inner.then);
            const elseExpr = inner.else ? printExp(inner.else) : null;

            if (elseExpr) {
                return `if (${condition}) then ${thenExpr} else ${elseExpr}`;
            } else {
                return `if (${condition}) then ${thenExpr}`;
            }

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

        // For unimplemented constructs, show kind
        default:
            return `[${inner.kind}]`;
    }
}

// Main entry point for printing
export function printAST(ast: L<Exp>): string {
    return printExp(ast);
}