const fs = require("fs");
const path = require("path");

// Suppress all logs
const originalLog = console.log;
const originalError = console.error;
console.log = () => {};
console.error = () => {};

const { parseProgram } = require("../dist/parser/top-level-parser.js");

// Restore for our output
console.log = originalLog;
console.error = originalError;

const dir = "verse-files-flat";
const files = fs.readdirSync(dir).filter(f => f.endsWith(".verse")).slice(0, 200);

console.log("Analyzing 200 real-world Verse files...\n");

const failures = {};
const examples = {};
let successCount = 0;
let partialCount = 0;
let failureCount = 0;

for (const file of files) {
  const filePath = path.join(dir, file);
  const source = fs.readFileSync(filePath, "utf-8");
  const lines = source.split(/\r?\n/);

  // Capture error from logs
  let capturedError = null;
  let errorPosition = null;
  console.error = (msg, ...args) => {
    if (msg && msg.includes("Failed to parse")) {
      const match = msg.match(/Failed to parse declaration: (.*)/);
      if (match) capturedError = match[1];
    }
    if (msg && msg.includes("Current token:")) {
      // Try to extract position
      const posMatch = msg.match(/line: (\d+)/);
      if (posMatch) errorPosition = parseInt(posMatch[1]);
    }
  };

  try {
    console.log = () => {};
    const ast = parseProgram(source);
    console.log = originalLog;

    if (ast.declarations.length === 0) {
      failureCount++;
    } else if (capturedError) {
      partialCount++;
      const key = capturedError.split(":")[0].trim();
      failures[key] = (failures[key] || 0) + 1;

      // Save an example with context
      if (!examples[key] && errorPosition) {
        const startLine = Math.max(0, errorPosition - 3);
        const endLine = Math.min(lines.length, errorPosition + 3);
        examples[key] = {
          file: path.basename(file),
          line: errorPosition,
          snippet: lines.slice(startLine, endLine).join("\n"),
          parsedBefore: ast.declarations.length
        };
      }
    } else {
      successCount++;
    }
  } catch (err) {
    failureCount++;
    const key = "Exception";
    failures[key] = (failures[key] || 0) + 1;
  }
}

console.error = originalError;

console.log("Summary:");
console.log(`  Fully parsed: ${successCount}`);
console.log(`  Partially parsed: ${partialCount}`);
console.log(`  Failed completely: ${failureCount}`);
console.log(`  Total: ${successCount + partialCount + failureCount}`);

console.log("\nMost common parsing failures:\n");
const sorted = Object.entries(failures).sort((a, b) => b[1] - a[1]);

sorted.slice(0, 10).forEach(([error, count]) => {
  console.log(`${count}x: ${error}`);
  if (examples[error]) {
    const ex = examples[error];
    console.log(`    Example: ${ex.file} (line ${ex.line}, parsed ${ex.parsedBefore} declarations before failure)`);
    console.log(`    Context:`);
    ex.snippet.split("\n").forEach(line => console.log(`      ${line}`));
    console.log();
  }
});