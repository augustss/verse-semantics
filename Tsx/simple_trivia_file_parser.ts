import { parseVersee } from './src/parser/parser';

// For now, let's try to understand the current parsing behavior
function testFileStructure(input: string) {
    console.log(`=== Testing file structure: "${input}" ===`);

    const result = parseVersee(input);

    if (result.success) {
        console.log('✓ Parsed successfully');
        console.log('Root AST kind:', result.value.value.kind);

        // Check if it's a module with a body
        if (result.value.value.kind === 'Module') {
            console.log('Module body kind:', result.value.value.body?.value?.kind);
        }
    } else {
        console.log('✗ Parse failed:', result.error.message);
    }
    console.log();
}

// Test different file structures
testFileStructure('42');
testFileStructure('  42');
testFileStructure('42\n');
testFileStructure('  42  ');