#!/usr/bin/env node
/**
 * UNRDF v5 Migration Codemod
 * Automatically migrates N3.js code to Oxigraph
 *
 * @usage node scripts/migrate-v5.mjs <directory>
 */

import { readFileSync, writeFileSync } from 'fs';
import { execSync } from 'child_process';

const targetDir = process.argv[2] || './src';

console.log('🔄 UNRDF v5 Migration Tool\n');
console.log(`📂 Target: ${targetDir}\n`);

// Find all .mjs and .js files
let files;
try {
  files = execSync(`find ${targetDir} -type f \\( -name "*.mjs" -o -name "*.js" \\)`, { encoding: 'utf-8' })
    .split('\n')
    .filter(f => f.trim() && !f.includes('node_modules'));
} catch (err) {
  console.error('❌ Error finding files:', err.message);
  process.exit(1);
}

console.log(`📝 Found ${files.length} files to scan\n`);

let totalChanges = 0;

for (const file of files) {
  try {
    let content = readFileSync(file, 'utf-8');
    let changed = false;

    // Migration 1: new Store() → createStore()
    if (content.includes('new Store(')) {
      content = content.replace(/new Store\(\)/g, 'createStore()');
      changed = true;
      console.log(`  ✅ ${file}: new Store() → createStore()`);
    }

    // Migration 2: import { Store } from 'n3' → import { createStore } from '@unrdf/oxigraph'
    if (content.includes("from 'n3'") && !file.includes('n3-justified-only') && !file.includes('n3-migration')) {
      content = content.replace(
        /import\s*{\s*Store\s*}\s*from\s*['"]n3['"]/g,
        "import { createStore } from '@unrdf/oxigraph'"
      );
      changed = true;
      console.log(`  ✅ ${file}: Updated N3 Store import → Oxigraph`);
    }

    // Migration 3: import { DataFactory } from 'n3' → import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only'
    if (content.includes("DataFactory") && content.includes("from 'n3'")) {
      content = content.replace(
        /import\s*{\s*DataFactory\s*}\s*from\s*['"]n3['"]/g,
        "import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only'"
      );
      changed = true;
      console.log(`  ✅ ${file}: Updated DataFactory import`);
    }

    // Migration 4: import { Parser, Writer } from 'n3' → from justified module
    if ((content.includes("Parser") || content.includes("Writer")) && content.includes("from 'n3'")) {
      content = content.replace(
        /import\s*{\s*(Parser|Writer|StreamParser|StreamWriter)([^}]*)\s*}\s*from\s*['"]n3['"]/g,
        "import { $1$2 } from '@unrdf/core/rdf/n3-justified-only'"
      );
      changed = true;
      console.log(`  ✅ ${file}: Updated Parser/Writer imports`);
    }

    if (changed) {
      writeFileSync(file, content);
      totalChanges++;
    }
  } catch (err) {
    console.error(`  ❌ ${file}: ${err.message}`);
  }
}

console.log(`\n✅ Migration complete: ${totalChanges} files modified\n`);

if (totalChanges > 0) {
  console.log('📋 Next Steps:');
  console.log('  1. Review changes: git diff');
  console.log('  2. Run tests: pnpm test');
  console.log('  3. Commit: git add -A && git commit -m "refactor: migrate to Oxigraph v5 API"');
} else {
  console.log('✨ No migrations needed - your code is already v5 compatible!');
}
