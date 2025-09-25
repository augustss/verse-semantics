#!/usr/bin/env node

/**
 * Print all parseset tests showing original and reconstructed forms
 */

const fs = require('fs');
const path = require('path');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { parseExpression } = require('../dist/parser');
const { reconstructFromAST } = require('../dist/pretty-printer/ast-reconstructor');

/**
 * Extract tests from a parseset file
 */
function extractTests(content) {
  const tests = [];
  const lines = content.split('\n');

  let i = 0;
  while (i < lines.length) {
    if (lines[i].startsWith('#!')) {
      const type = lines[i].substring(2).trim();
      i++;

      // Skip comment lines
      while (i < lines.length && lines[i].startsWith('#')) {
        i++;
      }

      // Collect source lines
      const sourceLines = [];
      while (i < lines.length && !lines[i].startsWith('#!')) {
        sourceLines.push(lines[i]);
        i++;
      }

      // Trim trailing empty lines
      while (sourceLines.length > 0 && sourceLines[sourceLines.length - 1].trim() === '') {
        sourceLines.pop();
      }

      if (sourceLines.length > 0) {
        tests.push({
          type: type,
          source: sourceLines.join('\n')
        });
      }
    } else {
      i++;
    }
  }

  return tests;
}

/**
 * Process a single test case
 */
function processTest(test, testNumber) {
  const { type, source } = test;
  const isTopLevel = type.toLowerCase().includes('toplevel') ||
                     type.toLowerCase().includes('top-level') ||
                     type.toLowerCase().includes('declaration');
  const isError = type.toLowerCase().includes('error');

  // Skip error tests as they won't parse
  if (isError) {
    return null;
  }

  try {
    let ast;
    let reconstructed;

    if (isTopLevel) {
      ast = parseProgram(source);
      reconstructed = reconstructFromAST(source, ast, {
        includeTrailingTrivia: true
      });
    } else {
      ast = parseExpression(source);
      reconstructed = reconstructFromAST(source, ast, {
        includeTrailingTrivia: true
      });
    }

    return {
      testNumber,
      type,
      original: source,
      reconstructed
    };
  } catch (error) {
    // Skip tests that fail to parse
    return null;
  }
}

/**
 * Process parseset file
 */
function processFile(filepath) {
  const content = fs.readFileSync(filepath, 'utf8');
  const filename = path.basename(filepath);
  const tests = extractTests(content);

  console.log(`====== File: ${filename} ======`);
  console.log();

  tests.forEach((test, index) => {
    const result = processTest(test, index + 1);
    if (result) {
      console.log(`--- Test ${result.testNumber} (${result.type}) ---`);
      console.log('ORIGINAL:');
      console.log(result.original);
      console.log();
      console.log('RECONSTRUCTED:');
      console.log(result.reconstructed);
      console.log();
    }
  });
}

/**
 * Main function
 */
function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    // Process all parseset files in tests directory
    const testsDir = path.join(__dirname, '..', 'tests');
    const files = fs.readdirSync(testsDir)
      .filter(file => file.endsWith('.parseset'))
      .sort();

    files.forEach(file => {
      processFile(path.join(testsDir, file));
    });
  } else {
    // Process specified files
    args.forEach(arg => {
      if (fs.existsSync(arg)) {
        if (fs.statSync(arg).isDirectory()) {
          const files = fs.readdirSync(arg)
            .filter(file => file.endsWith('.parseset'))
            .sort();

          files.forEach(file => {
            processFile(path.join(arg, file));
          });
        } else {
          processFile(arg);
        }
      }
    });
  }
}

if (require.main === module) {
  main();
}