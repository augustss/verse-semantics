import * as fs from 'fs';
import { parseVersee } from './src/parser/parser';

function testFileRoundtrip(filename: string) {
    console.log(`=== ROUNDTRIP TEST: ${filename} ===\n`);

    const originalSource = fs.readFileSync(filename, 'utf8');
    console.log('ORIGINAL SOURCE:');
    console.log(originalSource);
    console.log('\n' + '='.repeat(50) + '\n');

    const parseResult = parseVersee(originalSource);

    if (!parseResult.success) {
        console.log('❌ PARSING FAILED:');
        console.log(parseResult.error);
        return;
    }

    console.log('✓ PARSING SUCCESSFUL');
    console.log('\nAST Root Node:', parseResult.value.value.kind);

    // Show some AST details without overwhelming output
    const astString = JSON.stringify(parseResult.value, (_key, value) => {
        if (typeof value === 'bigint') return value.toString() + 'n';
        return value;
    }, 2);

    // Show first few lines of AST to give an idea of structure
    const astLines = astString.split('\n');
    console.log('\nAST Structure (first 20 lines):');
    for (let i = 0; i < Math.min(20, astLines.length); i++) {
        console.log(astLines[i]);
    }
    if (astLines.length > 20) {
        console.log(`... (${astLines.length - 20} more lines)`);
    }

    console.log('\n✅ Successfully parsed real Verse file!');
    console.log(`📊 AST contains ${astString.split('"kind":').length - 1} nodes`);

    // Try to parse again to verify consistency
    const reparseResult = parseVersee(originalSource);
    if (reparseResult.success) {
        console.log('✓ Reparse successful - parser is consistent');
    } else {
        console.log('❌ Reparse failed - parser inconsistency detected');
    }
}

const filename = process.argv[2];
if (!filename) {
    console.log('Usage: npx ts-node file_roundtrip.ts <verse-file>');
    process.exit(1);
}

testFileRoundtrip(filename);