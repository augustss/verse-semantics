const { parseTopLevel, parse } = require('./dist/index.js');

const testCode = `CaptureControl := class:
    @editable
    Device:enabled_device = enabled_device{}`;

console.log('Testing complex class with decorator...');
console.log('Original:');
console.log(testCode);

let result = parseTopLevel(testCode, true);

if (result) {
    console.log('\n✅ Parsing successful');
    console.log('AST Structure (simplified):');

    const printASTSimplified = (obj, depth = 0) => {
        const indent = '  '.repeat(depth);
        if (typeof obj === 'object' && obj !== null) {
            if (obj.type) {
                console.log(`${indent}${obj.type}`);
                if (obj.type === 'Program' && obj.declarations) {
                    obj.declarations.forEach((decl, i) => printASTSimplified(decl, depth + 1));
                } else if (obj.type === 'ClassExpression' && obj.body) {
                    console.log(`${indent}  body: ${obj.body.length} members`);
                    obj.body.forEach((member, i) => {
                        console.log(`${indent}    [${i}] ${member.type}`);
                        if (member.name) {
                            console.log(`${indent}      name: ${member.name.value}`);
                        }
                    });
                }
            }
        }
    };

    printASTSimplified(result);
} else {
    console.log('❌ Parsing failed');
}