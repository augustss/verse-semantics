#!/usr/bin/env node

/**
 * Test parsing of real Verse files from verse-files-flat directory
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

function testFile(filePath, options = {}) {
  const fileName = path.basename(filePath);
  const fileContent = fs.readFileSync(filePath, 'utf-8');

  try {
    // Try to parse the file
    const startTime = Date.now();
    const ast = parseProgram(fileContent);
    const parseTime = Date.now() - startTime;

    let result = {
      success: true,
      parseTime,
      declarations: ast.declarations.length,
      usingStatements: ast.usingStatements.length,
      reconstructed: false,
      reconstructionMatch: false
    };

    // Try reconstruction if requested
    if (options.reconstruct) {
      try {
        // Reconstruct directly from the Program AST node
        const reconstructed = reconstructFromAST(fileContent, ast, {
          includeTrailingTrivia: true
        });
        result.reconstructed = true;
        result.reconstructionMatch = reconstructed === fileContent;
      } catch (e) {
        result.reconstructionError = e.message;
      }
    }

    return result;
  } catch (error) {
    return {
      success: false,
      error: error.message || error.toString(),
      line: error.position?.line,
      column: error.position?.column
    };
  }
}

function main() {
  const args = process.argv.slice(2);
  const options = {
    reconstruct: args.includes('--reconstruct'),
    verbose: args.includes('--verbose'),
    summary: args.includes('--summary')
  };

  const verseFlatDir = path.join(__dirname, '../verse-files-flat');

  if (!fs.existsSync(verseFlatDir)) {
    console.error(`${RED}Error: verse-files-flat directory not found${RESET}`);
    process.exit(1);
  }

  const files = fs.readdirSync(verseFlatDir)
    .filter(f => f.endsWith('.verse'))
    .sort();

  console.log(`\n${BOLD}Testing ${files.length} Verse files${RESET}\n`);
  console.log('=' .repeat(80));

  let stats = {
    total: files.length,
    parsed: 0,
    failed: 0,
    reconstructed: 0,
    perfectMatch: 0,
    totalParseTime: 0,
    errors: {}
  };

  for (const file of files) {
    const filePath = path.join(verseFlatDir, file);
    const result = testFile(filePath, options);

    if (result.success) {
      stats.parsed++;
      stats.totalParseTime += result.parseTime;

      // Update reconstruction stats
      if (options.reconstruct && result.reconstructed) {
        stats.reconstructed++;
        if (result.reconstructionMatch) {
          stats.perfectMatch++;
        }
      }

      if (!options.summary) {
        const status = `${GREEN}✓${RESET}`;
        const details = `${result.declarations} decls, ${result.usingStatements} using`;
        let reconstructInfo = '';

        if (options.reconstruct && result.reconstructed) {
          if (result.reconstructionMatch) {
            reconstructInfo = ` ${GREEN}[perfect]${RESET}`;
          } else {
            reconstructInfo = ` ${YELLOW}[mismatch]${RESET}`;
          }
        } else if (options.reconstruct && result.reconstructionError) {
          reconstructInfo = ` ${RED}[recon failed]${RESET}`;
        }

        console.log(`${status} ${file.substring(0, 60).padEnd(60)} ${DIM}${details}${RESET}${reconstructInfo}`);
      }
    } else {
      stats.failed++;

      // Track error types
      const errorKey = result.error.substring(0, 50);
      stats.errors[errorKey] = (stats.errors[errorKey] || 0) + 1;

      if (!options.summary) {
        const status = `${RED}✗${RESET}`;
        const location = result.line ? ` at ${result.line}:${result.column}` : '';
        console.log(`${status} ${file.substring(0, 60).padEnd(60)} ${RED}${result.error.substring(0, 40)}${location}${RESET}`);
      } else if (options.verbose) {
        console.log(`${RED}✗${RESET} ${file}: ${result.error}`);
      }
    }
  }

  // Print summary
  console.log('\n' + '=' .repeat(80));
  console.log(`${BOLD}SUMMARY${RESET}\n`);

  const parseRate = ((stats.parsed / stats.total) * 100).toFixed(1);
  const avgParseTime = (stats.totalParseTime / stats.parsed).toFixed(2);

  console.log(`Files tested:    ${stats.total}`);
  console.log(`Successfully parsed: ${stats.parsed}/${stats.total} (${parseRate}%)`);
  console.log(`Failed to parse:     ${stats.failed}/${stats.total}`);
  console.log(`Average parse time:  ${avgParseTime}ms`);

  if (options.reconstruct && stats.parsed > 0) {
    const reconRate = ((stats.reconstructed / stats.parsed) * 100).toFixed(1);
    const perfectRate = ((stats.perfectMatch / stats.reconstructed) * 100).toFixed(1);
    console.log(`\nReconstruction:`);
    console.log(`  Attempted:     ${stats.reconstructed}/${stats.parsed} (${reconRate}%)`);
    console.log(`  Perfect match: ${stats.perfectMatch}/${stats.reconstructed} (${perfectRate}%)`);
  }

  if (stats.failed > 0) {
    console.log(`\n${BOLD}Common errors:${RESET}`);
    const sortedErrors = Object.entries(stats.errors)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5);

    for (const [error, count] of sortedErrors) {
      console.log(`  ${count}x: ${error}`);
    }
  }

  // Exit with error code if any files failed
  process.exit(stats.failed > 0 ? 1 : 0);
}

if (require.main === module) {
  main();
}