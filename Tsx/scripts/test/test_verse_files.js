const fs = require('fs');
const path = require('path');
const { parseTopLevel } = require('./dist/index.js');

// Get first 10 verse files
const verseDir = './verse-files-flat';
const files = fs.readdirSync(verseDir).filter(f => f.endsWith('.verse')).slice(0, 10);

console.log(`Testing ${files.length} Verse files...\n`);

let successCount = 0;
const failures = [];

files.forEach((filename, index) => {
  const filepath = path.join(verseDir, filename);
  const content = fs.readFileSync(filepath, 'utf8');

  console.log(`\n${index + 1}. Testing: ${filename}`);
  console.log(`Content length: ${content.length} chars`);

  try {
    const result = parseTopLevel(content, false);
    if (result) {
      console.log('✅ SUCCESS');
      successCount++;
    } else {
      console.log('❌ PARSE FAILED');
      failures.push({
        file: filename,
        content: content.substring(0, 200) + '...',
        error: 'Parse returned null'
      });
    }
  } catch (error) {
    console.log(`❌ ERROR: ${error.message}`);
    failures.push({
      file: filename,
      content: content.substring(0, 200) + '...',
      error: error.message
    });
  }
});

console.log(`\n\n=== SUMMARY ===`);
console.log(`Success: ${successCount}/${files.length} (${(successCount/files.length*100).toFixed(1)}%)`);

if (failures.length > 0) {
  console.log(`\n=== FAILURES ===`);
  failures.forEach((failure, i) => {
    console.log(`\n${i + 1}. ${failure.file}`);
    console.log(`Error: ${failure.error}`);
    console.log(`Content start:\n${failure.content}`);
  });
}