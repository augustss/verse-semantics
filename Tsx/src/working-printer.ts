import { L } from './ast/location';
import { Exp } from './ast/expression';

// Working printer for constructs we know work
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

        // Patterns
        case 'Pat':
            if (inner.pattern.kind === 'Name' && inner.pattern.ident.kind === 'IdentName') {
                return inner.pattern.ident.name;
            }
            return '[pattern]';

        // Binary operators we know exist
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

        // Variable declarations
        case 'ExpVar':
            if (inner.pattern) {
                return `var ${printExp(inner.pattern)} := ${printExp(inner.expr)}`;
            } else {
                return `var ${printExp(inner.expr)}`;
            }
        case 'Assign':
            return `${printExp(inner.left)} := ${printExp(inner.right)}`;

        // For anything else, show the kind
        default:
            return `[${inner.kind}]`;
    }
}

export function printAST(ast: L<Exp>): string {
    return printExp(ast);
}