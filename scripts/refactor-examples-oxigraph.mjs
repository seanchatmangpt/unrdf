#!/usr/bin/env node

/**
 * Refactor example files to use Oxigraph createStore() instead of N3 Store
 *
 * This script performs bulk refactoring of example files to migrate from N3.Store
 * to @unrdf/oxigraph createStore() pattern.
 */

import { readFileSync, writeFileSync } from 'fs';
import { execSync } from 'child_process';

const filesToRefactor = [
  // Package examples with new Store()
  'packages/cli/examples/graph-commands/src/custom-commands.mjs',
  'packages/cli/examples/graph-commands/test/example.test.mjs',
  'packages/core/examples/basic-store/src/index.mjs',
  'packages/core/examples/basic-store/test/example.test.mjs',
  'packages/core/examples/rdf-parsing/src/index.mjs',
  'packages/core/examples/rdf-parsing/test/example.test.mjs',
  'packages/core/examples/sparql-queries/src/index.mjs',
  'packages/core/examples/sparql-queries/test/example.test.mjs',
  'packages/dark-matter/examples/basic.mjs',
  'packages/dark-matter/examples/index-advisor/src/index.mjs',
  'packages/dark-matter/examples/query-optimization/src/index.mjs',
  'packages/knowledge-engine/examples/basic-inference/src/index.mjs',
  'packages/knowledge-engine/examples/basic-inference/test/example.test.mjs',
  'packages/knowledge-engine/examples/sparql-rules/src/index.mjs',
  'packages/knowledge-engine/examples/sparql-rules/test/example.test.mjs',
  'packages/streaming/examples/change-feeds/src/index.mjs',
  'packages/streaming/examples/change-feeds/test/example.test.mjs',
  'packages/streaming/examples/real-time-sync/src/index.mjs',
  'packages/streaming/examples/real-time-sync/test/example.test.mjs',

  // Playground examples
  'playground/full-stack-example/apps/server/src/index.mjs',
  'playground/papers-thesis-cli/src/integration/sparql.mjs',
  'playground/smoke-test/01-quick-start.mjs',
  'playground/smoke-test/02-simple-knowledge-graph.mjs',
  'playground/smoke-test/composables-test.mjs',

  // Additional N3 imports without Store
  'packages/cli/examples/format-conversion/src/converter.mjs',
  'packages/cli/examples/format-conversion/test/example.test.mjs',
  'packages/composables/examples/reactive-graphs/test/example.test.mjs',
  'packages/dark-matter/examples/query-optimization/test/example.test.mjs',
  'packages/hooks/examples/basic.mjs',
  'packages/hooks/examples/hook-chains/src/index.mjs',
  'packages/hooks/examples/knowledge-hook-manager-usage.mjs',
  'packages/hooks/examples/policy-hooks/src/index.mjs',
  'playground/full-stack-example/apps/server/test/server.test.mjs',
];

const stats = {
  filesProcessed: 0,
  filesModified: 0,
  filesSkipped: 0,
  filesError: 0,
  patterns: {
    storeConstructor: 0,
    n3Imports: 0,
    dataFactory: 0,
    storeOperations: 0,
  }
};

/**
 * Refactor a single file to use Oxigraph patterns
 */
function refactorFile(filePath) {
  const fullPath = `/Users/sac/unrdf/${filePath}`;

  try {
    let content = readFileSync(fullPath, 'utf-8');
    const originalContent = content;
    let modified = false;

    // Pattern 1: Replace N3 Store constructor with createStore()
    if (content.includes('new Store()') || content.includes('new Store(')) {
      content = content.replace(/new Store\(\)/g, 'createStore()');
      content = content.replace(/new Store\(([^)]+)\)/g, 'createStore($1)');
      stats.patterns.storeConstructor++;
      modified = true;
    }

    // Pattern 2: Replace N3 imports with Oxigraph imports
    if (content.includes("from 'n3'")) {
      // Replace Store import
      if (content.match(/import\s*{\s*Store[^}]*}\s*from\s*['"]n3['"]/)) {
        content = content.replace(
          /import\s*{\s*Store\s*}\s*from\s*['"]n3['"];?/g,
          "import { createStore } from '@unrdf/oxigraph';"
        );

        // Handle multiple imports from N3
        content = content.replace(
          /import\s*{\s*([^}]*),\s*Store\s*([^}]*)\s*}\s*from\s*['"]n3['"];?/g,
          (match, before, after) => {
            const otherImports = [before, after].filter(s => s.trim()).join(', ');
            return otherImports
              ? `import { ${otherImports} } from 'n3';\nimport { createStore } from '@unrdf/oxigraph';`
              : "import { createStore } from '@unrdf/oxigraph';";
          }
        );

        stats.patterns.n3Imports++;
        modified = true;
      }

      // Replace DataFactory imports
      if (content.includes('DataFactory')) {
        content = content.replace(
          /DataFactory\.namedNode/g,
          'dataFactory.namedNode'
        );
        content = content.replace(
          /DataFactory\.literal/g,
          'dataFactory.literal'
        );
        content = content.replace(
          /DataFactory\.quad/g,
          'dataFactory.quad'
        );

        // Add dataFactory import if needed
        if (!content.includes("import { dataFactory }")) {
          content = content.replace(
            /import { createStore } from '@unrdf\/oxigraph';/,
            "import { createStore, dataFactory } from '@unrdf/oxigraph';"
          );
        }

        stats.patterns.dataFactory++;
        modified = true;
      }
    }

    // Pattern 3: Store operations (only if using new API style)
    // Keep backward compatible methods for now
    // store.addQuad() → store.add() (optional)
    // store.removeQuad() → store.delete() (optional)

    if (modified) {
      writeFileSync(fullPath, content, 'utf-8');
      stats.filesModified++;
      console.log(`✅ Refactored: ${filePath}`);
    } else {
      stats.filesSkipped++;
      console.log(`⏭️  Skipped (no changes): ${filePath}`);
    }

    stats.filesProcessed++;
  } catch (error) {
    stats.filesError++;
    console.error(`❌ Error processing ${filePath}:`, error.message);
  }
}

// Main execution
console.log('🚀 Starting Oxigraph refactoring of example files...\n');
console.log(`📁 Total files to process: ${filesToRefactor.length}\n`);

for (const file of filesToRefactor) {
  refactorFile(file);
}

console.log('\n📊 Refactoring Summary:');
console.log('━'.repeat(50));
console.log(`Files processed: ${stats.filesProcessed}`);
console.log(`Files modified:  ${stats.filesModified}`);
console.log(`Files skipped:   ${stats.filesSkipped}`);
console.log(`Files with errors: ${stats.filesError}`);
console.log('\n📝 Pattern Replacements:');
console.log(`  Store constructors: ${stats.patterns.storeConstructor}`);
console.log(`  N3 imports:         ${stats.patterns.n3Imports}`);
console.log(`  DataFactory calls:  ${stats.patterns.dataFactory}`);
console.log(`  Store operations:   ${stats.patterns.storeOperations}`);
console.log('━'.repeat(50));

if (stats.filesModified > 0) {
  console.log('\n✨ Refactoring complete! Run tests to validate changes.');
  console.log('\n💡 Suggested next steps:');
  console.log('   1. npm run test -- packages/*/examples');
  console.log('   2. Review changes: git diff packages/*/examples playground/');
  console.log('   3. Commit changes: git add . && git commit -m "refactor: migrate examples to Oxigraph createStore()"');
}
