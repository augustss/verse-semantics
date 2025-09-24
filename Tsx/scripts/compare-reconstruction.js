#!/usr/bin/env node

/**
 * Compare original input with reconstructed output for parseset tests
 */

const fs = require('fs');
const path = require('path');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { parseExpression } = require('../dist/parser');
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
 * Test reconstruction for a single test case
 */
function testReconstruction(test, testNumber) {
  const { type, source } = test;
  const isTopLevel = type.toLowerCase().includes('toplevel') ||
                     type.toLowerCase().includes('top-level') ||
                     type.toLowerCase().includes('declaration');

  try {
    let ast;
    let reconstructed;

    if (isTopLevel) {
      // Parse as program
      ast = parseProgram(source);
      reconstructed = reconstructFromAST(source, ast, {
        includeTrailingTrivia: true
      });
    } else {
      // Parse as expression
      ast = parseExpression(source);
      reconstructed = reconstructFromAST(source, ast, {
        includeTrailingTrivia: true
      });
    }

    const isMatch = reconstructed === source;

    return {
      success: true,
      ast,
      reconstructed,
      isMatch,
      type: isTopLevel ? 'program' : 'expression'
    };
  } catch (error) {
    return {
      success: false,
      error: error.message || error.toString()
    };
  }
}

/**
 * Show a visual diff between two strings
 */
function showDiff(original, reconstructed) {
  const minLen = Math.min(original.length, reconstructed.length);
  let firstDiff = -1;

  // Find first difference
  for (let i = 0; i < minLen; i++) {
    if (original[i] !== reconstructed[i]) {
      firstDiff = i;
      break;
    }
  }

  if (firstDiff === -1 && original.length !== reconstructed.length) {
    firstDiff = minLen;
  }

  if (firstDiff !== -1) {
    const start = Math.max(0, firstDiff - 20);
    const end = Math.min(original.length, firstDiff + 20);

    console.log(`    ${YELLOW}First difference at position ${firstDiff}:${RESET}`);
    console.log(`    ${DIM}Original: ${RESET}"${original.substring(start, end).replace(/\n/g, '\\n').replace(/\r/g, '\\r')}"`);
    console.log(`    ${DIM}Reconstructed: ${RESET}"${reconstructed.substring(start, Math.min(reconstructed.length, end)).replace(/\n/g, '\\n').replace(/\r/g, '\\r')}"`);
  }
}

/**
 * Format string for display with visible whitespace
 */
function formatForDisplay(str) {
  return str.replace(/\t/g, '→   ')
           .replace(/\r/g, '⏎')
           .replace(/\n/g, '↵\n');
}

/**
 * Main function
 */
function main() {
  const args = process.argv.slice(2);
  const options = {
    showPerfect: args.includes('--perfect'),
    showMismatches: args.includes('--mismatches') || args.length === 0,
    showErrors: args.includes('--errors') || args.length === 0,
    showAll: args.includes('--all'),
    maxTests: args.includes('--limit') ? parseInt(args[args.indexOf('--limit') + 1]) : undefined,
    file: args.find(arg => arg.endsWith('.parseset'))
  };

  // If --all is specified, show everything
  if (options.showAll) {
    options.showPerfect = true;
    options.showMismatches = true;
    options.showErrors = true;
  }

  const testsDir = path.join(__dirname, '../tests');
  let filesToTest;

  if (options.file) {
    filesToTest = [options.file];
  } else {
    filesToTest = fs.readdirSync(testsDir)
      .filter(f => f.endsWith('.parseset'))
      .sort();
  }

  console.log(`\n${BOLD}RECONSTRUCTION COMPARISON${RESET}`);
  console.log(`${BOLD}Testing ${filesToTest.length} parseset files${RESET}\n`);
  console.log('=' .repeat(80));

  let totalTests = 0;
  let perfectMatches = 0;
  let mismatches = 0;
  let errors = 0;

  for (const fileName of filesToTest) {
    const filePath = path.join(testsDir, fileName);
    if (!fs.existsSync(filePath)) continue;

    const content = fs.readFileSync(filePath, 'utf-8');
    const tests = extractTests(content);

    console.log(`\n${CYAN}${BOLD}File: ${fileName}${RESET} (${tests.length} tests)`);
    console.log('-' .repeat(60));

    let fileTests = 0;
    let filePerfect = 0;
    let fileMismatches = 0;
    let fileErrors = 0;

    for (let i = 0; i < tests.length; i++) {
      if (options.maxTests && totalTests >= options.maxTests) break;

      const test = tests[i];
      const result = testReconstruction(test, i + 1);

      totalTests++;
      fileTests++;

      if (result.success) {
        if (result.isMatch) {
          perfectMatches++;
          filePerfect++;

          if (options.showPerfect) {
            console.log(`\n${GREEN}✓ Test ${i + 1}${RESET} (${test.type}) - ${GREEN}PERFECT MATCH${RESET}`);
            console.log(`${BLUE}Original:${RESET}\n${formatForDisplay(test.source)}`);
          }
        } else {
          mismatches++;
          fileMismatches++;

          if (options.showMismatches) {
            console.log(`\n${YELLOW}△ Test ${i + 1}${RESET} (${test.type}) - ${YELLOW}MISMATCH${RESET}`);
            console.log(`${BLUE}Original (${test.source.length} chars):${RESET}`);
            console.log(formatForDisplay(test.source));
            console.log(`${CYAN}Reconstructed (${result.reconstructed.length} chars):${RESET}`);
            console.log(formatForDisplay(result.reconstructed));
            showDiff(test.source, result.reconstructed);
          }
        }
      } else {
        errors++;
        fileErrors++;

        if (options.showErrors) {
          console.log(`\n${RED}✗ Test ${i + 1}${RESET} (${test.type}) - ${RED}ERROR${RESET}`);
          console.log(`${BLUE}Original:${RESET}\n${formatForDisplay(test.source)}`);
          console.log(`${RED}Error: ${result.error}${RESET}`);
        }
      }
    }

    // File summary
    const fileSuccessRate = fileTests > 0 ? ((filePerfect / fileTests) * 100).toFixed(1) : '0.0';
    console.log(`\n${DIM}File summary: ${filePerfect}/${fileTests} perfect (${fileSuccessRate}%), ${fileMismatches} mismatches, ${fileErrors} errors${RESET}`);

    if (options.maxTests && totalTests >= options.maxTests) break;
  }

  // Overall summary
  console.log('\n' + '=' .repeat(80));
  console.log(`${BOLD}OVERALL SUMMARY${RESET}\n`);

  const successRate = totalTests > 0 ? ((perfectMatches / totalTests) * 100).toFixed(1) : '0.0';
  console.log(`Total tests:      ${totalTests}`);
  console.log(`Perfect matches:  ${perfectMatches}/${totalTests} (${successRate}%)`);
  console.log(`Mismatches:       ${mismatches}/${totalTests} (${((mismatches/totalTests)*100).toFixed(1)}%)`);
  console.log(`Errors:           ${errors}/${totalTests} (${((errors/totalTests)*100).toFixed(1)}%)`);

  console.log(`\n${BOLD}Usage:${RESET}`);
  console.log(`  ${DIM}--perfect${RESET}    Show perfect matches`);
  console.log(`  ${DIM}--mismatches${RESET} Show reconstruction mismatches (default)`);
  console.log(`  ${DIM}--errors${RESET}     Show parsing errors (default)`);
  console.log(`  ${DIM}--all${RESET}        Show everything`);
  console.log(`  ${DIM}--limit N${RESET}    Limit to N tests total`);
  console.log(`  ${DIM}filename.parseset${RESET} Test only specific file`);

  console.log('\n' + '=' .repeat(80));
}

if (require.main === module) {
  main();
}