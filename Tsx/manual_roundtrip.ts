import { parseVersee } from './src/parser/parser';
import { L } from './src/ast/location';
import { Exp } from './src/ast/expression';

// Basic manual reconstruction - not complete but demonstrates the concept
function reconstructSource(exp: L<Exp>): string {
    const inner = exp.value;

    switch (inner.kind) {
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
        case 'Pat':
            if (inner.pattern.kind === 'Name' && inner.pattern.ident.kind === 'IdentName') {
                return inner.pattern.ident.name;
            }
            return '[unknown pattern]';
        case 'Add':
            return `${reconstructSource(inner.left)} + ${reconstructSource(inner.right)}`;
        case 'Subtract':
            return `${reconstructSource(inner.left)} - ${reconstructSource(inner.right)}`;
        case 'Multiply':
            return `${reconstructSource(inner.left)} * ${reconstructSource(inner.right)}`;
        case 'ExpVar':
            return `var ${reconstructSource(inner.expr)}`;
        default:
            return `[${inner.kind}]`;
    }
}

function testRoundtrip(source: string) {
    console.log(`=== Testing: "${source}" ===`);

    const parseResult = parseVersee(source);

    if (!parseResult.success) {
        console.log('❌ Parse failed:', parseResult.error);
        return;
    }

    console.log('✓ Parse successful');

    const reconstructed = reconstructSource(parseResult.value);
    console.log(`Original:      "${source}"`);
    console.log(`Reconstructed: "${reconstructed}"`);

    if (source.replace(/\s+/g, ' ').trim() === reconstructed.replace(/\s+/g, ' ').trim()) {
        console.log('🎉 Perfect match (ignoring whitespace)!');
    } else {
        console.log('⚠️  Differences found');
    }

    // Test if reconstructed source parses to same AST structure
    const reparseResult = parseVersee(reconstructed);
    if (reparseResult.success) {
        const originalKind = parseResult.value.value.kind;
        const reconstructedKind = reparseResult.value.value.kind;

        if (originalKind === reconstructedKind) {
            console.log('✓ Reconstructed source has same AST node type');
        } else {
            console.log(`⚠️  AST types differ: ${originalKind} vs ${reconstructedKind}`);
        }
    } else {
        console.log('❌ Reconstructed source failed to parse');
    }

    console.log();
}

// Test cases
const testCases = [
    '42',
    'true',
    'false',
    '"hello"',
    'x',
    'x + 5',
    'a * b',
    '10 - 3'
];

for (const test of testCases) {
    testRoundtrip(test);
}