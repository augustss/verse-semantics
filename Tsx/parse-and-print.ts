#!/usr/bin/env npx ts-node

import * as fs from 'fs';
import { parseVersee } from './src/parser/parser';
import { printAST } from './src/working-printer';

function parseAndPrint(source: string): void {
    console.log('=== ORIGINAL ===');
    console.log(source);
    console.log('\n=== PARSING ===');

    const result = parseVersee(source);

    if (!result.success) {
        console.log('❌ Parse failed:', result.error.message);
        console.log(`Position: line ${result.error.position.line}, column ${result.error.position.column}`);
        return;
    }

    console.log('✓ Parse successful');
    console.log('\n=== RECONSTRUCTED ===');

    try {
        const reconstructed = printAST(result.value);
        console.log(reconstructed);

        console.log('\n=== COMPARISON ===');
        if (source.trim() === reconstructed.trim()) {
            console.log('🎉 Perfect match!');
        } else {
            console.log('⚠️  Differences found:');
            console.log(`Original:      "${source.trim()}"`);
            console.log(`Reconstructed: "${reconstructed.trim()}"`);
        }
    } catch (error) {
        console.log('❌ Failed to reconstruct:', error);
    }
}

function main() {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        console.log('Usage:');
        console.log('  npx ts-node parse-and-print.ts <file.verse>           # Parse file');
        console.log('  npx ts-node parse-and-print.ts --expr "x + 5"        # Parse expression');
        console.log('  npx ts-node parse-and-print.ts --interactive          # Interactive mode');
        return;
    }

    if (args[0] === '--expr') {
        if (args.length < 2) {
            console.log('Error: --expr requires an expression');
            return;
        }
        parseAndPrint(args[1]);
    } else if (args[0] === '--interactive') {
        console.log('Interactive mode - enter expressions (Ctrl+C to exit):');
        process.stdin.setEncoding('utf8');
        process.stdout.write('> ');

        process.stdin.on('data', (input) => {
            const expr = input.toString().trim();
            if (expr) {
                parseAndPrint(expr);
            }
            process.stdout.write('> ');
        });
    } else {
        // File mode
        const filename = args[0];
        try {
            const content = fs.readFileSync(filename, 'utf8');
            parseAndPrint(content);
        } catch (error: any) {
            console.log('Error reading file:', error.message);
        }
    }
}

if (require.main === module) {
    main();
}