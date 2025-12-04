#!/usr/bin/env node

/**
 * Fix test files to use Oxigraph imports
 */

import { readFileSync, writeFileSync } from 'fs';

const testFiles = [
  'packages/core/examples/basic-store/test/example.test.mjs',
  'packages/streaming/examples/change-feeds/test/example.test.mjs',
  'packages/streaming/examples/real-time-sync/test/example.test.mjs',
  'packages/cli/examples/graph-commands/test/example.test.mjs',
  'packages/cli/examples/format-conversion/test/example.test.mjs',
  'packages/composables/examples/reactive-graphs/test/example.test.mjs',
  'packages/core/examples/rdf-parsing/test/example.test.mjs',
  'packages/core/examples/sparql-queries/test/example.test.mjs',
  'packages/dark-matter/examples/query-optimization/test/example.test.mjs',
  'packages/knowledge-engine/examples/basic-inference/test/example.test.mjs',
  'packages/knowledge-engine/examples/sparql-rules/test/example.test.mjs',
  'playground/full-stack-example/apps/server/test/server.test.mjs',
];

let totalFixed = 0;

for (const file of testFiles) {
  const fullPath = `/Users/sac/unrdf/${file}`;

  try {
    let content = readFileSync(fullPath, 'utf-8');
    const original = content;

    // Replace N3 Store/DataFactory imports with Oxigraph
    content = content.replace(
      /import\s*{\s*([^}]*\bStore\b[^}]*)\s*}\s*from\s*['"]n3['"];?/g,
      (match, imports) => {
        // Keep other imports but replace Store with createStore
        const otherImports = imports
          .split(',')
          .map(i => i.trim())
          .filter(i => i !== 'Store' && i !== 'DataFactory' && i)
          .join(', ');

        let result = "import { createStore, dataFactory } from '@unrdf/oxigraph';";
        if (otherImports) {
          result += `\nimport { ${otherImports} } from 'n3';`;
        }
        return result;
      }
    );

    // Replace DataFactory usage
    content = content.replace(/DataFactory\./g, 'dataFactory.');
    content = content.replace(/const { ([^}]+) } = DataFactory;/g, 'const { $1 } = dataFactory;');

    // Replace Store with createStore in tests (but not in imports which we already handled)
    content = content.replace(/new Store\(\)/g, 'createStore()');

    if (content !== original) {
      writeFileSync(fullPath, content, 'utf-8');
      totalFixed++;
      console.log(`✅ Fixed: ${file}`);
    } else {
      console.log(`⏭️  Skipped (no changes): ${file}`);
    }
  } catch (error) {
    console.error(`❌ Error: ${file}:`, error.message);
  }
}

console.log(`\n✨ Fixed ${totalFixed} test files`);
