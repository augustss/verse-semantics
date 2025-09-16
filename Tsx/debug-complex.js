#!/usr/bin/env node

require('ts-node').register();
const { parseVersee } = require('./src/parser/parser.ts');

// Test a file with function definitions
const testFile = `# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }

capture_point_event_handler := class:
    CapturePointIndex:int
    var CaptureWeightOfThisPoint:int = 0

    GetTeamDirectionOfAgent<private>(MyAgent:agent):int=
        return 1`;

console.log('Testing complex file structure...');
const result = parseVersee(testFile);

if (result.success) {
    console.log('SUCCESS!');
    console.log('Parsed:', result.value.value.kind);
} else {
    console.log('FAILED:', result.error);
    console.log('At position:', result.error.position);
    console.log('Context around error:');
    const start = Math.max(0, result.error.position.offset - 40);
    const end = Math.min(testFile.length, result.error.position.offset + 40);
    console.log(testFile.slice(start, end));
    console.log(' '.repeat(result.error.position.offset - start) + '^');
}