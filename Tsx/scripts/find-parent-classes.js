#!/usr/bin/env node

/**
 * Script to find and print all parent class relationships in Verse files
 */

const fs = require('fs');
const path = require('path');
const { parseProgram } = require('../dist/parser/top-level-parser');
const { simplifyProgram } = require('../dist/logical-ast');

// Parse command line arguments
const args = process.argv.slice(2);
const options = {
  verbose: args.includes('--verbose'),
  dir: args.find(arg => !arg.startsWith('--')) || 'verse-files-flat'
};

// Get all .verse files
const verseDir = path.resolve(options.dir);
if (!fs.existsSync(verseDir)) {
  console.error(`Directory not found: ${verseDir}`);
  process.exit(1);
}

const files = fs.readdirSync(verseDir)
  .filter(f => f.endsWith('.verse'))
  .sort();

console.log(`\n📁 Scanning for parent classes in: ${options.dir}`);
console.log(`   Total files: ${files.length}`);
console.log('═'.repeat(80));

const parentRelationships = [];
let filesWithParents = 0;
let totalClasses = 0;
let classesWithParents = 0;

files.forEach((file, index) => {
  const filePath = path.join(verseDir, file);
  const content = fs.readFileSync(filePath, 'utf-8');

  try {
    // Parse and convert to logical AST
    const ast = parseProgram(content);
    const logical = simplifyProgram(ast);

    // Find all class declarations with parents
    if (logical.declarations) {
      logical.declarations.forEach(decl => {
        if (decl && decl.type === 'ClassDecl') {
          totalClasses++;
          if (decl.parents && decl.parents.length > 0) {
            classesWithParents++;

            // Extract parent names
            const parentNames = decl.parents.map(parent => {
              // Handle different parent expression types
              if (parent.type === 'Identifier') {
                return parent.name;
              } else if (parent.type === 'MemberAccess') {
                // Handle qualified names like Verse.widget
                const parts = [];
                let current = parent;
                while (current) {
                  if (current.type === 'MemberAccess') {
                    if (current.property.type === 'Identifier') {
                      parts.unshift(current.property.name);
                    }
                    current = current.object;
                  } else if (current.type === 'Identifier') {
                    parts.unshift(current.name);
                    break;
                  } else {
                    parts.unshift('<complex>');
                    break;
                  }
                }
                return parts.join('.');
              } else {
                return `<${parent.type}>`;
              }
            });

            parentRelationships.push({
              file: file,
              className: decl.name,
              parents: parentNames,
              specifiers: decl.specifiers
            });
          }
        }
      });
    }

    // Track files with parent relationships
    const fileHasParents = logical.declarations?.some(decl =>
      decl?.type === 'ClassDecl' && decl.parents && decl.parents.length > 0
    );
    if (fileHasParents) filesWithParents++;

  } catch (error) {
    // Silently skip files that fail to parse
    if (options.verbose) {
      console.log(`⚠️  Failed to parse ${file}: ${error.message}`);
    }
  }

  // Progress indicator
  if ((index + 1) % 50 === 0) {
    process.stdout.write(`\rProcessed: ${index + 1}/${files.length}`);
  }
});

console.log(`\rProcessed: ${files.length}/${files.length}`);

// Print results
console.log('\n' + '═'.repeat(80));
console.log('📊 Statistics:');
console.log(`  Total classes found: ${totalClasses}`);
console.log(`  Classes with parents: ${classesWithParents}`);
console.log(`  Files with parent relationships: ${filesWithParents}`);
console.log(`  Total parent relationships: ${parentRelationships.length}`);

// Group by parent class
const parentCounts = {};
parentRelationships.forEach(rel => {
  rel.parents.forEach(parent => {
    parentCounts[parent] = (parentCounts[parent] || 0) + 1;
  });
});

// Sort by frequency
const sortedParents = Object.entries(parentCounts)
  .sort(([,a], [,b]) => b - a);

console.log('\n🏆 Most Common Parent Classes:');
sortedParents.slice(0, 15).forEach(([parent, count]) => {
  console.log(`  ${parent}: ${count} subclasses`);
});

// Show all parent relationships
console.log('\n📋 All Parent Relationships:');
console.log('─'.repeat(80));

// Group by parent for better readability
const byParent = {};
parentRelationships.forEach(rel => {
  rel.parents.forEach(parent => {
    if (!byParent[parent]) byParent[parent] = [];
    byParent[parent].push({
      child: rel.className,
      file: rel.file,
      specifiers: rel.specifiers
    });
  });
});

// Print grouped by parent
Object.keys(byParent).sort().forEach(parent => {
  console.log(`\n🔸 ${parent} (${byParent[parent].length} subclasses):`);
  byParent[parent].forEach(rel => {
    const specs = rel.specifiers ? ` <${rel.specifiers.join(' ')}>` : '';
    console.log(`   - ${rel.child}${specs}`);
    if (options.verbose) {
      console.log(`     (${rel.file})`);
    }
  });
});

// Show unique parent lists (multiple inheritance)
const multipleParents = parentRelationships.filter(rel => rel.parents.length > 1);
if (multipleParents.length > 0) {
  console.log('\n🔄 Classes with Multiple Parents:');
  multipleParents.forEach(rel => {
    console.log(`  ${rel.className} extends [${rel.parents.join(', ')}]`);
    if (options.verbose) {
      console.log(`    (${rel.file})`);
    }
  });
} else {
  console.log('\n✅ No multiple inheritance found (all classes have single parent)');
}