#!/usr/bin/env node
/**
 * Test RDF Builder
 */

import { parseFile } from './src/doc-generator/parser.mjs';
import { buildRDFGraph, buildJSONLD } from './src/doc-generator/rdf-builder.mjs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const testFile = join(__dirname, 'src/filters/rdf.js');
const rootDir = join(__dirname, '../..');

console.log('🧪 Testing RDF Builder\n');

try {
  // Parse file
  const parsed = parseFile(testFile, rootDir);
  console.log(`✅ Parsed: ${parsed.relativePath}`);
  console.log(`   Exports: ${parsed.exports.length}\n`);

  // Build Turtle RDF
  console.log('📝 Generating Turtle RDF...\n');
  const turtle = buildRDFGraph(parsed);
  console.log(turtle.substring(0, 1000) + '\n... (truncated)\n');

  // Build JSON-LD
  console.log('📝 Generating JSON-LD...\n');
  const jsonld = buildJSONLD(parsed);
  console.log(JSON.stringify(jsonld, null, 2).substring(0, 800) + '\n... (truncated)\n');

  // Verify RDF contains key elements
  const checks = [
    { test: 'Contains prefixes', pass: turtle.includes('@prefix code:') },
    { test: 'Contains file resource', pass: turtle.includes('a fs:File') },
    { test: 'Contains function exports', pass: turtle.includes('a code:Function') },
    { test: 'Contains parameters', pass: turtle.includes('code:param') },
    { test: 'JSON-LD has context', pass: jsonld['@context'] !== undefined },
    { test: 'JSON-LD has exports', pass: jsonld['code:exports'].length > 0 },
  ];

  console.log('🔍 Verification Checks:\n');
  checks.forEach(({ test, pass }) => {
    console.log(`  ${pass ? '✅' : '❌'} ${test}`);
  });

  const allPassed = checks.every(c => c.pass);

  console.log(`\n${allPassed ? '✅ RDF Builder test PASSED' : '❌ RDF Builder test FAILED'}`);
  process.exit(allPassed ? 0 : 1);

} catch (error) {
  console.error('❌ Test failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
