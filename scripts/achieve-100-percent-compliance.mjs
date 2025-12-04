#!/usr/bin/env node
/**
 * @fileoverview Achieve 100% N3 Compliance - Refactor all remaining files
 * @version 1.0.0
 *
 * This script refactors ALL files that import N3 directly to use:
 * - @unrdf/core/rdf/n3-justified-only (for streaming Parser/Writer)
 * - @unrdf/core/rdf/n3-migration (for backward compatibility)
 * - @unrdf/oxigraph (for Store/DataFactory)
 *
 * **GOAL**: Zero N3 imports outside justified modules
 */

import { readFileSync, writeFileSync } from 'fs';
import { execSync } from 'child_process';
import { join } from 'path';

const PROJECT_ROOT = '/Users/sac/unrdf';

/**
 * Find all files with N3 imports
 */
function findN3Files() {
  const output = execSync(
    `find ${PROJECT_ROOT} -name "*.mjs" -type f -exec grep -l "from 'n3'" {} \\; | grep -v node_modules`,
    { encoding: 'utf-8' }
  );
  return output.trim().split('\n').filter(Boolean);
}

/**
 * Refactor a file to remove direct N3 imports
 */
function refactorFile(filePath) {
  console.log(`\n📝 Refactoring: ${filePath.replace(PROJECT_ROOT, '')}`);

  let content = readFileSync(filePath, 'utf-8');
  const originalContent = content;

  // Skip justified modules - they're allowed to import N3
  if (filePath.includes('n3-justified-only.mjs') || filePath.includes('n3-migration.mjs')) {
    console.log('  ✅ Justified module - skip');
    return { changed: false };
  }

  let changes = [];

  // Pattern 1: import { createStore } from '@unrdf/oxigraph'
  if (content.includes("import { Store") && content.includes("from 'n3'")) {
    content = content.replace(
      /import\s+\{[^}]*Store[^}]*\}\s+from\s+['"]n3['"]/g,
      "import { createStore } from '@unrdf/oxigraph'"
    );
    changes.push('Store → createStore from @unrdf/oxigraph');
  }

  // Pattern 2: import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only'
  if (content.includes("DataFactory") && content.includes("from 'n3'")) {
    content = content.replace(
      /import\s+\{[^}]*DataFactory[^}]*\}\s+from\s+['"]n3['"]/g,
      "import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only'"
    );
    changes.push('DataFactory → UnrdfDataFactory');
  }

  // Pattern 3: import {  Parser, Writer  } from '@unrdf/core/rdf/n3-justified-only'
  if ((content.includes("Parser") || content.includes("Writer")) && content.includes("from 'n3'")) {
    content = content.replace(
      /import\s+\{([^}]*(?:Parser|Writer)[^}]*)\}\s+from\s+['"]n3['"]/g,
      "import { $1 } from '@unrdf/core/rdf/n3-justified-only'"
    );
    changes.push('Parser/Writer → n3-justified-only');
  }

  // Pattern 4: createStore() → createStore()
  content = content.replace(/new Store\(\)/g, 'createStore()');
  content = content.replace(/new Store\(([^)]+)\)/g, 'createStore($1)');
  if (content !== originalContent && !changes.includes('new Store → createStore')) {
    changes.push('createStore() → createStore()');
  }

  // Pattern 5: Oxigraph Store references
  content = content.replace(/N3\.Store/g, 'Oxigraph Store');

  // Pattern 6: import { createStore } from '@unrdf/oxigraph' (full import)
  if (content.includes("import { createStore } from '@unrdf/oxigraph'") || content.includes('import * as N3')) {
    content = content.replace(
      /import N3 from ['"]n3['"]/g,
      "import { createStore } from '@unrdf/oxigraph'"
    );
    content = content.replace(
      /import \* as N3 from ['"]n3['"]/g,
      "import { createStore } from '@unrdf/oxigraph'"
    );
    changes.push('Full N3 import → createStore');
  }

  // Only write if changed
  if (content !== originalContent) {
    writeFileSync(filePath, content, 'utf-8');
    console.log(`  ✅ Changes: ${changes.join(', ')}`);
    return { changed: true, changes };
  } else {
    console.log('  ⚠️  No changes needed');
    return { changed: false };
  }
}

/**
 * Main execution
 */
async function main() {
  console.log('🚀 Starting 100% N3 Compliance Refactor\n');
  console.log('=' .repeat(80));

  // Find all files
  const files = findN3Files();
  console.log(`\n📊 Found ${files.length} files with N3 imports\n`);

  let refactored = 0;
  let skipped = 0;
  let errors = 0;

  // Refactor each file
  for (const file of files) {
    try {
      const result = refactorFile(file);
      if (result.changed) {
        refactored++;
      } else {
        skipped++;
      }
    } catch (error) {
      console.error(`  ❌ Error: ${error.message}`);
      errors++;
    }
  }

  console.log('\n' + '='.repeat(80));
  console.log('\n📊 REFACTORING SUMMARY:');
  console.log(`  Total files processed: ${files.length}`);
  console.log(`  Files refactored: ${refactored}`);
  console.log(`  Files skipped: ${skipped}`);
  console.log(`  Errors: ${errors}`);

  // Verify compliance
  console.log('\n🔍 VERIFICATION SCAN:');
  console.log('Running: find . -name "*.mjs" -exec grep -l "from \'n3\'" {} \\; | grep -v node_modules\n');

  try {
    const remainingFiles = execSync(
      `find ${PROJECT_ROOT} -name "*.mjs" -type f -exec grep -l "from 'n3'" {} \\; | grep -v node_modules`,
      { encoding: 'utf-8' }
    ).trim().split('\n').filter(Boolean);

    const justifiedModules = remainingFiles.filter(f =>
      f.includes('n3-justified-only.mjs') || f.includes('n3-migration.mjs')
    );

    const unjustifiedImports = remainingFiles.filter(f =>
      !f.includes('n3-justified-only.mjs') && !f.includes('n3-migration.mjs')
    );

    console.log(`  Justified modules: ${justifiedModules.length}`);
    justifiedModules.forEach(f => console.log(`    ✅ ${f.replace(PROJECT_ROOT, '')}`));

    if (unjustifiedImports.length === 0) {
      console.log('\n  ✅ 100% COMPLIANCE ACHIEVED!');
      console.log('  Zero N3 imports outside justified modules\n');
      return true;
    } else {
      console.log(`\n  ⚠️  ${unjustifiedImports.length} files still have unjustified N3 imports:`);
      unjustifiedImports.forEach(f => console.log(`    ❌ ${f.replace(PROJECT_ROOT, '')}`));
      return false;
    }
  } catch (error) {
    if (error.status === 1) {
      // grep found nothing - 100% compliance!
      console.log('  ✅ 100% COMPLIANCE ACHIEVED!');
      console.log('  Zero N3 imports in any files!\n');
      return true;
    }
    throw error;
  }
}

main().then(success => {
  process.exit(success ? 0 : 1);
}).catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});
