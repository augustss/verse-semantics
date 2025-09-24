#!/usr/bin/env node

/**
 * Detailed analysis of reconstruction issues on real Verse files
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
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';

function findFirstDifference(original, reconstructed) {
  const minLen = Math.min(original.length, reconstructed.length);
  for (let i = 0; i < minLen; i++) {
    if (original[i] !== reconstructed[i]) {
      return {
        position: i,
        originalChar: original[i],
        reconstructedChar: reconstructed[i],
        context: {
          original: original.substring(Math.max(0, i - 20), i + 20),
          reconstructed: reconstructed.substring(Math.max(0, i - 20), i + 20)
        }
      };
    }
  }

  if (original.length !== reconstructed.length) {
    return {
      position: minLen,
      originalChar: original.length > minLen ? original[minLen] : '<END>',
      reconstructedChar: reconstructed.length > minLen ? reconstructed[minLen] : '<END>',
      lengthDiff: original.length - reconstructed.length
    };
  }

  return null;
}

function categorizeReconstructionIssue(original, reconstructed, difference) {
  if (!difference) return 'unknown';

  const context = difference.context;
  if (!context) return 'length-mismatch';

  // Check for whitespace/formatting issues
  if (/^\s$/.test(difference.originalChar) || /^\s$/.test(difference.reconstructedChar)) {
    return 'whitespace';
  }

  // Check for comment issues
  if (context.original.includes('#') || context.reconstructed.includes('#')) {
    return 'comments';
  }

  // Check for decorator/attribute issues (@editable, etc.)
  if (context.original.includes('@') || context.reconstructed.includes('@')) {
    return 'decorators';
  }

  // Check for string literal issues
  if (context.original.includes('"') || context.reconstructed.includes('"')) {
    return 'strings';
  }

  // Check for brace/bracket issues
  if (/[{}\[\]()]/.test(difference.originalChar) || /[{}\[\]()]/.test(difference.reconstructedChar)) {
    return 'brackets';
  }

  // Check for operator issues
  if (/[+\-*/%:=<>!&|]/.test(difference.originalChar) || /[+\-*/%:=<>!&|]/.test(difference.reconstructedChar)) {
    return 'operators';
  }

  return 'other';
}

function analyzeFile(filePath) {
  const fileName = path.basename(filePath);
  const content = fs.readFileSync(filePath, 'utf-8');

  try {
    const ast = parseProgram(content);
    const reconstructed = reconstructFromAST(content, ast, { includeTrailingTrivia: true });

    const isMatch = reconstructed === content;
    const result = {
      file: fileName,
      success: true,
      perfectMatch: isMatch,
      declarations: ast.declarations.length,
      usingStatements: ast.usingStatements.length,
      originalLength: content.length,
      reconstructedLength: reconstructed.length
    };

    if (!isMatch) {
      const diff = findFirstDifference(content, reconstructed);
      result.difference = diff;
      result.issueCategory = categorizeReconstructionIssue(content, reconstructed, diff);
    }

    return result;
  } catch (error) {
    return {
      file: fileName,
      success: false,
      error: error.message
    };
  }
}

function main() {
  const verseFlatDir = path.join(__dirname, '../verse-files-flat');

  if (!fs.existsSync(verseFlatDir)) {
    console.error(`${RED}Error: verse-files-flat directory not found${RESET}`);
    process.exit(1);
  }

  const files = fs.readdirSync(verseFlatDir)
    .filter(f => f.endsWith('.verse'))
    .sort();

  console.log(`\n${BOLD}RECONSTRUCTION ANALYSIS REPORT${RESET}`);
  console.log(`${BOLD}Testing ${files.length} real-world Verse files${RESET}\n`);
  console.log('=' .repeat(80));

  const results = [];
  const categories = {};
  let perfect = 0;
  let failed = 0;

  for (const file of files) {
    const filePath = path.join(verseFlatDir, file);
    const result = analyzeFile(filePath);
    results.push(result);

    if (result.success) {
      if (result.perfectMatch) {
        perfect++;
        console.log(`${GREEN}✓${RESET} ${file.substring(0, 70).padEnd(70)} ${GREEN}PERFECT${RESET}`);
      } else {
        const category = result.issueCategory;
        categories[category] = (categories[category] || 0) + 1;

        const lengthInfo = result.originalLength !== result.reconstructedLength
          ? ` (${result.originalLength}→${result.reconstructedLength})`
          : '';

        console.log(`${YELLOW}△${RESET} ${file.substring(0, 70).padEnd(70)} ${YELLOW}${category}${RESET}${lengthInfo}`);

        if (result.difference && result.difference.position < 200) {
          // Show first difference for files with early issues
          const pos = result.difference.position;
          const orig = JSON.stringify(result.difference.originalChar);
          const recon = JSON.stringify(result.difference.reconstructedChar);
          console.log(`    ${DIM}@${pos}: ${orig} → ${recon}${RESET}`);
        }
      }
    } else {
      failed++;
      console.log(`${RED}✗${RESET} ${file.substring(0, 70).padEnd(70)} ${RED}PARSE FAILED${RESET}`);
    }
  }

  // Summary statistics
  console.log('\n' + '=' .repeat(80));
  console.log(`${BOLD}SUMMARY STATISTICS${RESET}\n`);

  const total = results.length;
  const parsed = total - failed;
  const attempted = results.filter(r => r.success).length;

  console.log(`Total files:           ${total}`);
  console.log(`Successfully parsed:   ${parsed}/${total} (${((parsed/total)*100).toFixed(1)}%)`);
  console.log(`Perfect reconstruction: ${perfect}/${attempted} (${((perfect/attempted)*100).toFixed(1)}%)`);
  console.log(`Reconstruction issues:  ${attempted - perfect}/${attempted} (${(((attempted-perfect)/attempted)*100).toFixed(1)}%)`);

  if (failed > 0) {
    console.log(`Parse failures:        ${failed}/${total} (${((failed/total)*100).toFixed(1)}%)`);
  }

  // Issue categories breakdown
  if (Object.keys(categories).length > 0) {
    console.log(`\n${BOLD}RECONSTRUCTION ISSUE CATEGORIES${RESET}\n`);

    const sortedCategories = Object.entries(categories)
      .sort((a, b) => b[1] - a[1]);

    for (const [category, count] of sortedCategories) {
      const percentage = ((count / attempted) * 100).toFixed(1);
      console.log(`${category.padEnd(15)}: ${count.toString().padStart(3)} files (${percentage}%)`);
    }
  }

  // Sample perfect matches
  const perfectFiles = results.filter(r => r.perfectMatch).slice(0, 5);
  if (perfectFiles.length > 0) {
    console.log(`\n${BOLD}SAMPLE PERFECT RECONSTRUCTIONS${RESET}\n`);
    for (const file of perfectFiles) {
      console.log(`${GREEN}✓${RESET} ${file.file} (${file.declarations} decls, ${file.usingStatements} using)`);
    }
  }

  // Sample problematic files
  const problematicFiles = results
    .filter(r => r.success && !r.perfectMatch && r.difference && r.difference.position < 100)
    .slice(0, 3);

  if (problematicFiles.length > 0) {
    console.log(`\n${BOLD}SAMPLE EARLY RECONSTRUCTION ISSUES${RESET}\n`);
    for (const file of problematicFiles) {
      console.log(`${YELLOW}△${RESET} ${file.file}:`);
      console.log(`    Issue: ${file.issueCategory} at position ${file.difference.position}`);
      console.log(`    Expected: ${JSON.stringify(file.difference.originalChar)}`);
      console.log(`    Got:      ${JSON.stringify(file.difference.reconstructedChar)}`);
      if (file.difference.context) {
        console.log(`    Context:  "${file.difference.context.original.replace(/\n/g, '\\n')}"`);
      }
      console.log();
    }
  }

  console.log('=' .repeat(80));
}

if (require.main === module) {
  main();
}