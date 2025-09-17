import { parseVersee } from './src/parser/parser';
import { PrettyPrinter } from './src/printer/pretty-printer';
import * as fs from 'fs';

const filename = process.argv[2];
if (!filename) {
  console.log('Usage: npx ts-node roundtrip_test.ts <filename>');
  process.exit(1);
}

function findLineDifferences(original: string, printed: string): void {
  const originalLines = original.split(/\r?\n/);
  const printedLines = printed.split(/\r?\n/);

  const maxLines = Math.max(originalLines.length, printedLines.length);
  let foundDifferences = false;

  for (let i = 0; i < maxLines; i++) {
    const origLine = originalLines[i] || '';
    const printLine = printedLines[i] || '';

    if (origLine !== printLine) {
      if (!foundDifferences) {
        console.log('\n=== LINE-BY-LINE DIFFERENCES ===');
        foundDifferences = true;
      }
      console.log(`\nLine ${i + 1} mismatch:`);
      console.log(`Original: ${JSON.stringify(origLine)}`);
      console.log(`Printed:  ${JSON.stringify(printLine)}`);
    }
  }

  if (!foundDifferences) {
    console.log('\n✓ All lines match perfectly!');
  }
}

function isParseTestFile(filename: string): boolean {
  return filename.includes('.parsetest');
}

function splitParseTestFile(content: string): string[] {
  const sections: string[] = [];
  const lines = content.split(/\r?\n/);
  let currentSection: string[] = [];

  for (const line of lines) {
    if (line.trim().startsWith('#! Valid')) {
      // Start a new section
      if (currentSection.length > 0) {
        sections.push(currentSection.join('\n'));
      }
      currentSection = [];
    } else if (!line.trim().startsWith('#') && line.trim() !== '') {
      // Add non-comment, non-empty lines to current section
      currentSection.push(line);
    }
  }

  // Add the last section
  if (currentSection.length > 0) {
    sections.push(currentSection.join('\n'));
  }

  return sections.filter(section => section.trim() !== '');
}

try {
  const content = fs.readFileSync(filename, 'utf8');
  console.log('Testing roundtrip for: ' + filename);
  console.log('File size: ' + content.length + ' characters');

  if (isParseTestFile(filename)) {
    // Handle parsetest files by splitting into sections
    const sections = splitParseTestFile(content);
    console.log(`Found ${sections.length} test sections`);

    let totalSections = 0;
    let successfulSections = 0;
    let perfectMatches = 0;

    for (let i = 0; i < sections.length; i++) {
      const section = sections[i];
      console.log(`\n--- Testing section ${i + 1} ---`);

      totalSections++;
      const parseResult = parseVersee(section);

      if (!parseResult.success) {
        console.log(`✗ Section ${i + 1} parsing failed:`, parseResult.error?.message || 'Unknown error');
        continue;
      }

      console.log(`✓ Section ${i + 1} parsed successfully`);
      successfulSections++;

      const printer = new PrettyPrinter(undefined, section);
      const printed = printer.print(parseResult.value);

      if (section === printed) {
        console.log(`🎉 Section ${i + 1}: Perfect match!`);
        perfectMatches++;
      } else {
        console.log(`⚠️  Section ${i + 1}: Differences found`);
        console.log('Original:');
        console.log(section);
        console.log('Printed:');
        console.log(printed);
        findLineDifferences(section, printed);
      }
    }

    console.log(`\n📊 PARSETEST SUMMARY:`);
    console.log(`- Total sections: ${totalSections}`);
    console.log(`- Successfully parsed: ${successfulSections} (${(successfulSections/totalSections*100).toFixed(1)}%)`);
    console.log(`- Perfect roundtrip matches: ${perfectMatches} (${(perfectMatches/totalSections*100).toFixed(1)}%)`);

  } else {
    // Handle regular Verse files
    const parseResult = parseVersee(content);

    if (!parseResult.success) {
      console.log('\n✗ PARSING FAILED');
      console.log('Error:', parseResult.error);
      process.exit(1);
    }

    console.log('✓ PARSING SUCCESSFUL');

    const printer = new PrettyPrinter(undefined, content);
    const printed = printer.print(parseResult.value);

    console.log('Printed size: ' + printed.length + ' characters');

    if (content === printed) {
      console.log('\n🎉 PERFECT ROUNDTRIP MATCH!');
      console.log('Original and printed content are identical.');
    } else {
      console.log('\n⚠️  ROUNDTRIP MISMATCH');
      console.log('Original and printed content differ.');

      findLineDifferences(content, printed);

      let diffCount = 0;
      const minLength = Math.min(content.length, printed.length);
      for (let i = 0; i < minLength; i++) {
        if (content[i] !== printed[i]) {
          diffCount++;
        }
      }
      diffCount += Math.abs(content.length - printed.length);

      console.log(`\n📊 SUMMARY:`);
      console.log(`- Total character differences: ${diffCount}`);
      console.log(`- Original length: ${content.length}`);
      console.log(`- Printed length: ${printed.length}`);
      console.log(`- Accuracy: ${((1 - diffCount / Math.max(content.length, printed.length)) * 100).toFixed(1)}%`);
    }
  }

} catch (error) {
  console.error('Error:', error);
}