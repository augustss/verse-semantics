import * as fs from 'fs';
import * as path from 'path';
import { parse, parseTopLevel, prettyPrint, toCompactString } from '../src';

interface TestCase {
  description: string;
  input: string;
  lineNumber: number;
  parserType: 'Expression' | 'TopLevel';
  expectedResult: 'Valid' | 'Error';
}

function parseParsesetFile(content: string): TestCase[] {
  const lines = content.split('\n');
  const tests: TestCase[] = [];
  let currentDescription = '';
  let currentInput: string[] = [];
  let testStartLine = 0;
  let inTest = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.startsWith('#! ')) {
      // Save previous test if exists
      if (inTest && currentInput.length > 0) {
        const [expectedResult, parserType] = parseTestHeader(currentDescription);
        tests.push({
          description: currentDescription,
          input: currentInput.join('\n'),
          lineNumber: testStartLine,
          parserType,
          expectedResult
        });
      }

      // Start new test
      currentDescription = line.substring(3).trim();
      currentInput = [];
      testStartLine = i + 1;
      inTest = true;
    } else if (inTest) {
      // Continue collecting input until next test or end
      if (i === lines.length - 1 || (i < lines.length - 1 && lines[i + 1].startsWith('#! '))) {
        if (line.trim() !== '') {
          currentInput.push(line);
        }
        const [expectedResult, parserType] = parseTestHeader(currentDescription);
        tests.push({
          description: currentDescription,
          input: currentInput.join('\n'),
          lineNumber: testStartLine,
          parserType,
          expectedResult
        });
        currentInput = [];
        inTest = false;
      } else if (line.trim() !== '' || currentInput.length > 0) {
        currentInput.push(line);
      }
    }
  }

  // Don't forget the last test if file doesn't end with a new test marker
  if (inTest && currentInput.length > 0) {
    const [expectedResult, parserType] = parseTestHeader(currentDescription);
    tests.push({
      description: currentDescription,
      input: currentInput.join('\n'),
      lineNumber: testStartLine,
      parserType,
      expectedResult
    });
  }

  return tests;
}

function parseTestHeader(header: string): ['Valid' | 'Error', 'Expression' | 'TopLevel'] {
  // Parse headers like "Valid Expression", "Error TopLevel", etc.
  const parts = header.split(' ');
  if (parts.length >= 2) {
    const expectedResult = parts[0] as 'Valid' | 'Error';
    const parserType = parts[1] as 'Expression' | 'TopLevel';
    return [expectedResult, parserType];
  }

  // Default to Valid Expression for backward compatibility
  return ['Valid', 'Expression'];
}

function runTest(test: TestCase, fileName: string): { passed: boolean; error?: string } {
  try {
    let ast: any;

    // Use the appropriate parser based on test type (with quiet mode for testing)
    if (test.parserType === 'TopLevel') {
      ast = parseTopLevel(test.input, true);
    } else {
      ast = parse(test.input, true);
    }

    // Check if parsing succeeded/failed as expected
    const parseSucceeded = ast !== null;

    if (test.expectedResult === 'Valid' && !parseSucceeded) {
      return { passed: false, error: 'Expected valid parse but parsing failed' };
    }

    if (test.expectedResult === 'Error' && parseSucceeded) {
      return { passed: false, error: 'Expected parse error but parsing succeeded' };
    }

    if (test.expectedResult === 'Error' && !parseSucceeded) {
      // This is expected - error case passed
      return { passed: true };
    }

    // For valid cases, check lossless reconstruction
    if (parseSucceeded) {
      const reconstructed = prettyPrint(ast);
      const isLossless = test.input === reconstructed;

      if (!isLossless) {
        return {
          passed: false,
          error: `Not lossless:\n  Original:      "${test.input}"\n  Reconstructed: "${reconstructed}"`
        };
      }
    }

    return { passed: true };
  } catch (e) {
    if (test.expectedResult === 'Error') {
      // Exception was expected for error cases
      return { passed: true };
    }
    return { passed: false, error: `Exception: ${e}` };
  }
}

function runParsesetFile(filePath: string) {
  const content = fs.readFileSync(filePath, 'utf-8');
  const tests = parseParsesetFile(content);
  const fileName = path.basename(filePath);

  console.log(`\nRunning ${fileName}:`);
  console.log('=' .repeat(50));

  let passed = 0;
  let failed = 0;
  const failures: { test: TestCase; error?: string }[] = [];

  for (const test of tests) {
    const result = runTest(test, fileName);

    if (result.passed) {
      passed++;
    } else {
      failed++;
      failures.push({ test, error: result.error });
    }
  }

  // Show summary first
  console.log(`Summary: ${passed} passed, ${failed} failed`);

  // Show details for failures only
  if (failures.length > 0) {
    console.log(`\nFailures:`);
    for (const failure of failures) {
      console.log(`✗ ${failure.test.expectedResult} ${failure.test.parserType}: ${failure.test.description} (line ${failure.test.lineNumber})`);
      console.log(`  Input: "${failure.test.input}"`);
      if (failure.error) {
        console.log(`  Error: ${failure.error}`);
      }
    }
  }

  return { passed, failed };
}

// Recursively find all .parseset files
function findParsesetFiles(dir: string): string[] {
  const files: string[] = [];
  const entries = fs.readdirSync(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...findParsesetFiles(fullPath));
    } else if (entry.name.endsWith('.parseset')) {
      files.push(fullPath);
    }
  }

  return files.sort();
}

// Main execution
const testsDir = path.join(__dirname);
const args = process.argv.slice(2);

let parsesetFiles: string[];
if (args.length > 0) {
  // Run specific files provided as arguments
  parsesetFiles = args.map(arg => {
    if (path.isAbsolute(arg)) {
      return arg;
    } else if (arg.includes('/')) {
      // Relative path
      return path.resolve(arg);
    } else {
      // Just filename, search in tests directory
      const found = findParsesetFiles(testsDir).find(file =>
        path.basename(file) === arg || path.basename(file, '.parseset') === arg
      );
      if (!found) {
        console.error(`Error: Could not find parseset file: ${arg}`);
        process.exit(1);
      }
      return found;
    }
  });
} else {
  // Run all files (default behavior)
  parsesetFiles = findParsesetFiles(testsDir);
}

let totalPassed = 0;
let totalFailed = 0;

for (const file of parsesetFiles) {
  const { passed, failed } = runParsesetFile(file);
  totalPassed += passed;
  totalFailed += failed;
}

console.log('\n' + '='.repeat(50));
console.log(`TOTAL: ${totalPassed} passed, ${totalFailed} failed`);

if (totalFailed > 0) {
  process.exit(1);
}