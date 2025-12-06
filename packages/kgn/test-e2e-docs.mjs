#!/usr/bin/env node
/**
 * End-to-end test: JSDoc → RDF → MDX
 */

import { generateDocs } from './src/doc-generator/index.mjs';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

console.log('🧪 End-to-End Documentation Generator Test\n');
console.log('Testing: JSDoc → RDF → MDX pipeline\n');

const testFile = join(__dirname, 'src/filters/rdf.js');
const rootDir = join(__dirname, '../..');

try {
  const result = await generateDocs(testFile, { rootDir });

  console.log('✅ Phase 1: JSDoc Parsing');
  console.log(`   Exports: ${result.parsed.exports.length}`);
  console.log(`   Imports: ${result.parsed.imports.length}\n`);

  console.log('✅ Phase 2: RDF Generation');
  console.log(`   Turtle: ${result.rdf.length} chars`);
  console.log(`   JSON-LD exports: ${result.jsonld['code:exports'].length}\n`);

  console.log('✅ Phase 3: MDX Generation');
  console.log(`   MDX length: ${result.mdx.length} chars\n`);

  console.log('📄 Sample MDX Output:\n');
  console.log(result.mdx.substring(0, 800) + '\n... (truncated)\n');

  // Verification checks
  const checks = [
    { test: 'MDX has title', pass: result.mdx.includes('# ') },
    { test: 'MDX has function signatures', pass: result.mdx.includes('```typescript') },
    { test: 'MDX has parameters table', pass: result.mdx.includes('| Name | Type |') },
    { test: 'MDX has examples', pass: result.mdx.includes('**Examples:**') },
    { test: 'MDX is valid markdown', pass: !result.mdx.includes('undefined') },
  ];

  console.log('🔍 Verification:\n');
  checks.forEach(({ test, pass }) => {
    console.log(`  ${pass ? '✅' : '❌'} ${test}`);
  });

  const allPassed = checks.every(c => c.pass);
  console.log(`\n${allPassed ? '✅ E2E TEST PASSED' : '❌ E2E TEST FAILED'}`);

  process.exit(allPassed ? 0 : 1);

} catch (error) {
  console.error('❌ Test failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
