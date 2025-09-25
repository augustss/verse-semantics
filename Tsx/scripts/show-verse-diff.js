#!/usr/bin/env node

/**
 * Show differences between original and reconstructed Verse files
 */

const fs = require('fs');
const path = require('path');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { reconstructFromAST } = require('../dist/pretty-printer/ast-reconstructor');

// Colors for terminal output
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const BLUE = '\x1b[34m';
const CYAN = '\x1b[36m';
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';

function visualizeWhitespace(str) {
  return str
    .replace(/\t/g, '→')
    .replace(/ /g, '·')
    .replace(/\r/g, '⏎')
    .replace(/\n/g, '\\n\n');
}

function showDiff(original, reconstructed, filename) {
  console.log(`\n${CYAN}${BOLD}File: ${filename}${RESET}`);
  console.log('='.repeat(80));

  if (original === reconstructed) {
    console.log(`${GREEN}✓ PERFECT MATCH${RESET}`);
    return true;
  }

  console.log(`${YELLOW}△ MISMATCH${RESET} (${original.length}→${reconstructed.length})`);

  // Find first difference
  let diffPos = 0;
  while (diffPos < Math.min(original.length, reconstructed.length) &&
         original[diffPos] === reconstructed[diffPos]) {
    diffPos++;
  }

  // Show context around difference
  const contextStart = Math.max(0, diffPos - 20);
  const contextEnd = Math.min(original.length, diffPos + 20);

  console.log(`\n${BOLD}Original (around position ${diffPos}):${RESET}`);
  console.log(`${DIM}"${visualizeWhitespace(original.substring(contextStart, contextEnd))}"${RESET}`);

  console.log(`\n${BOLD}Reconstructed:${RESET}`);
  const recEnd = Math.min(reconstructed.length, contextStart + (contextEnd - contextStart));
  console.log(`${DIM}"${visualizeWhitespace(reconstructed.substring(contextStart, recEnd))}"${RESET}`);

  if (diffPos < Math.min(original.length, reconstructed.length)) {
    const origChar = original[diffPos];
    const recChar = reconstructed[diffPos];
    console.log(`\n${BOLD}First difference at position ${diffPos}:${RESET}`);
    console.log(`  Original: "${origChar}" (${origChar.charCodeAt(0)})`);
    console.log(`  Reconstructed: "${recChar}" (${recChar.charCodeAt(0)})`);
  }

  return false;
}

function testFile(filepath) {
  try {
    const source = fs.readFileSync(filepath, 'utf8');
    const ast = parseProgram(source);
    const reconstructed = reconstructFromAST(source, ast);

    return showDiff(source, reconstructed, path.basename(filepath));
  } catch (error) {
    console.log(`${RED}✗ ERROR parsing ${path.basename(filepath)}: ${error.message}${RESET}`);
    return false;
  }
}

function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.log('Usage: node show-verse-diff.js <file1.verse> [file2.verse] ...');
    console.log('   or: node show-verse-diff.js verse-files-flat/*.verse');
    process.exit(1);
  }

  let perfectCount = 0;
  let totalCount = 0;

  for (const filepath of args) {
    if (fs.existsSync(filepath)) {
      const isPerfect = testFile(filepath);
      if (isPerfect) perfectCount++;
      totalCount++;
    } else {
      console.log(`${RED}✗ File not found: ${filepath}${RESET}`);
    }
  }

  console.log(`\n${BOLD}SUMMARY${RESET}`);
  console.log(`Perfect matches: ${perfectCount}/${totalCount} (${(perfectCount/totalCount*100).toFixed(1)}%)`);
  console.log(`Mismatches: ${totalCount - perfectCount}/${totalCount} (${((totalCount-perfectCount)/totalCount*100).toFixed(1)}%)`);
}

if (require.main === module) {
  main();
}