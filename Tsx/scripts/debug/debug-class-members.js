#!/usr/bin/env node

const { parseTopLevel } = require('./dist/index');

console.log('Testing class member parsing...\n');

const test = `builder := class {
  value: string = ""
  Build(): string = value
}`;

const result = parseTopLevel(test, false);

if (result && result.declarations.length > 0) {
    const classDecl = result.declarations[0];
    console.log('Class declaration type:', classDecl.type);

    if (classDecl.type === 'ConstDeclaration' && classDecl.value.type === 'ClassExpression') {
        const classExpr = classDecl.value;
        console.log('Class expression type:', classExpr.type);
        console.log('Class members:', classExpr.members?.length || 0);

        if (classExpr.members) {
            classExpr.members.forEach((member, i) => {
                console.log(`  Member ${i}: type=${member.type}`);
            });
        }
    }
}