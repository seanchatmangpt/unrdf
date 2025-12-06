#!/usr/bin/env node
/**
 * Test JSDoc parser
 */

import { parseFile } from './src/doc-generator/parser.mjs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Test parsing the RDF filters file
const testFile = join(__dirname, 'src/filters/rdf.js');
const rootDir = join(__dirname, '../..');

console.log('🧪 Testing JSDoc Parser\n');
console.log(`Parsing: ${testFile}\n`);

try {
  const result = parseFile(testFile, rootDir);

  console.log('✅ Parse successful!\n');
  console.log(`File: ${result.relativePath}`);
  console.log(`Exports: ${result.exports.length}`);
  console.log(`Imports: ${result.imports.length}`);
  console.log(`Comments: ${result.comments}\n`);

  // Show first few exports
  console.log('📦 Sample Exports:\n');
  result.exports.slice(0, 3).forEach(exp => {
    console.log(`- ${exp.name} (${exp.type})`);
    if (exp.description) {
      console.log(`  Description: ${exp.description.substring(0, 60)}...`);
    }
    if (exp.params) {
      console.log(`  Params: ${exp.params.map(p => `${p.name}: ${p.type}`).join(', ')}`);
    }
    if (exp.returns) {
      console.log(`  Returns: ${exp.returns.type}`);
    }
    console.log();
  });

  console.log(`\n✅ Parser test PASSED`);
  process.exit(0);

} catch (error) {
  console.error('❌ Parse failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
