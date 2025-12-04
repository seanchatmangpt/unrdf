#!/usr/bin/env node

/**
 * Comprehensive Oxigraph refactoring - replace ALL N3 DataFactory usage with Oxigraph dataFactory
 */

import { readFileSync, writeFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';

const stats = {
  files: 0,
  modified: 0,
  skipped: 0,
  patterns: {
    n3Imports: 0,
    dataFactoryReplaced: 0,
    storeConstructor: 0,
  }
};

function refactorFile(fullPath) {
  let content = readFileSync(fullPath, 'utf-8');
  const original = content;
  let modified = false;

  // Pattern 1: Replace ALL N3 DataFactory imports with oxigraph dataFactory
  if (content.match(/from\s+['"]n3['"]/)) {
    // Remove DataFactory from N3 imports completely
    content = content.replace(
      /import\s*{\s*([^}]*)\bDataFactory\b[^}]*}\s*from\s*['"]n3['"];?\s*/g,
      (match, otherImports) => {
        const remaining = otherImports
          .split(',')
          .map(i => i.trim())
          .filter(i => i && i !== 'DataFactory' && i !== 'Store')
          .join(', ');

        if (remaining) {
          return `import { ${remaining} } from 'n3';\n`;
        }
        return '';
      }
    );

    // Remove Store-only imports
    content = content.replace(
      /import\s*{\s*Store\s*}\s*from\s*['"]n3['"];?\s*/g,
      ''
    );

    // Add Oxigraph imports if not present
    if (!content.includes("from '@unrdf/oxigraph'") && !content.includes('from "unrdf"')) {
      // Find the first import statement
      const firstImportMatch = content.match(/^import\s+.*$/m);
      if (firstImportMatch) {
        const insertPos = firstImportMatch.index + firstImportMatch[0].length;
        content = content.slice(0, insertPos) +
                 "\nimport { createStore, dataFactory } from '@unrdf/oxigraph';" +
                 content.slice(insertPos);
      }
    }

    stats.patterns.n3Imports++;
    modified = true;
  }

  // Pattern 2: Replace DataFactory usage with dataFactory
  if (content.includes('DataFactory.')) {
    content = content.replace(/\bDataFactory\./g, 'dataFactory.');
    stats.patterns.dataFactoryReplaced++;
    modified = true;
  }

  // Pattern 3: Replace destructuring from DataFactory
  if (content.match(/const\s+{\s*[^}]+\s*}\s*=\s*DataFactory/)) {
    content = content.replace(
      /const\s+{\s*([^}]+)\s*}\s*=\s*DataFactory;?/g,
      'const { $1 } = dataFactory;'
    );
    stats.patterns.dataFactoryReplaced++;
    modified = true;
  }

  // Pattern 4: Replace createStore() with createStore()
  if (content.match(/new\s+Store\s*\(/)) {
    content = content.replace(/new\s+Store\s*\(\s*\)/g, 'createStore()');
    content = content.replace(/new\s+Store\s*\(([^)]+)\)/g, 'createStore($1)');
    stats.patterns.storeConstructor++;
    modified = true;
  }

  // Clean up duplicate imports
  const oxigraphImports = content.match(/import\s+{[^}]*}\s+from\s+['"]@unrdf\/oxigraph['"];?/g);
  if (oxigraphImports && oxigraphImports.length > 1) {
    // Merge all oxigraph imports
    const allImports = new Set();
    oxigraphImports.forEach(imp => {
      const match = imp.match(/{\s*([^}]+)\s*}/);
      if (match) {
        match[1].split(',').forEach(i => allImports.add(i.trim()));
      }
    });

    // Remove all oxigraph imports
    content = content.replace(
      /import\s+{[^}]*}\s+from\s+['"]@unrdf\/oxigraph['"];?\s*/g,
      ''
    );

    // Add single merged import
    const firstImport = content.match(/^import\s+.*$/m);
    if (firstImport && allImports.size > 0) {
      const insertPos = firstImport.index + firstImport[0].length;
      content = content.slice(0, insertPos) +
               `\nimport { ${Array.from(allImports).join(', ')} } from '@unrdf/oxigraph';` +
               content.slice(insertPos);
    }
  }

  if (modified && content !== original) {
    writeFileSync(fullPath, content, 'utf-8');
    stats.modified++;
    return true;
  }

  stats.skipped++;
  return false;
}

// Find all .mjs files in examples and playground
function findFiles(dir, files = []) {
  const items = readdirSync(dir);

  for (const item of items) {
    const fullPath = join(dir, item);
    const stat = statSync(fullPath);

    if (stat.isDirectory() && !item.includes('node_modules') && !item.includes('.git')) {
      findFiles(fullPath, files);
    } else if (item.endsWith('.mjs') &&
               (fullPath.includes('/examples/') ||
                fullPath.includes('/playground/') ||
                fullPath.includes('/smoke-test/'))) {
      files.push(fullPath);
    }
  }

  return files;
}

console.log('🚀 Comprehensive Oxigraph Refactoring...\n');

const files = findFiles('/Users/sac/unrdf/packages')
  .concat(findFiles('/Users/sac/unrdf/playground'));

console.log(`📁 Found ${files.length} example files\n`);

for (const file of files) {
  const success = refactorFile(file);
  stats.files++;
  if (success) {
    console.log(`✅ ${file.replace('/Users/sac/unrdf/', '')}`);
  }
}

console.log('\n📊 Refactoring Summary:');
console.log('━'.repeat(60));
console.log(`Files processed: ${stats.files}`);
console.log(`Files modified:  ${stats.modified}`);
console.log(`Files skipped:   ${stats.skipped}`);
console.log('\n📝 Pattern Replacements:');
console.log(`  N3 imports removed:     ${stats.patterns.n3Imports}`);
console.log(`  DataFactory replaced:   ${stats.patterns.dataFactoryReplaced}`);
console.log(`  Store constructors:     ${stats.patterns.storeConstructor}`);
console.log('━'.repeat(60));
console.log('\n✨ Refactoring complete!');
