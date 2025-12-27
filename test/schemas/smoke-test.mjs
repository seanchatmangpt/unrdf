/**
 * Quick smoke test for Batch 3 schemas
 */

import { readFileSync } from 'fs';
import { z } from 'zod';

const manifest = JSON.parse(
  readFileSync(new URL('../../src/schemas/batch3-manifest.json', import.meta.url), 'utf-8')
);

console.log('🧪 Batch 3 Schema Smoke Test\n');

// Test 1: Manifest exists and has correct coverage
console.log('Test 1: Manifest Coverage');
console.log(`  Target: ${manifest.coverage.target}`);
console.log(`  Achieved: ${manifest.coverage.achieved}`);
console.log(`  Percentage: ${manifest.coverage.percentage}`);
const pass1 = manifest.coverage.achieved >= 194;
console.log(`  ✅ ${pass1 ? 'PASS' : 'FAIL'}: Coverage >= 194\n`);

// Test 2: Schema files exist
console.log('Test 2: Schema File Count');
console.log(`  Files: ${manifest.files.length}`);
const pass2 = manifest.files.length >= 250;
console.log(`  ✅ ${pass2 ? 'PASS' : 'FAIL'}: Files >= 250\n`);

// Test 3: Module coverage
console.log('Test 3: Module Coverage');
const moduleCount = Object.keys(manifest.modules).length;
console.log(`  Modules: ${moduleCount}`);
const pass3 = moduleCount >= 20;
console.log(`  ✅ ${pass3 ? 'PASS' : 'FAIL'}: Modules >= 20\n`);

// Test 4: Import and validate a schema
console.log('Test 4: Schema Import & Validation');
try {
  const { isProtectedNamespaceParamsSchema, isProtectedNamespaceReturnSchema } = await import(
    '../../src/admission/forbidden-operations.schema.mjs'
  );

  // Test params schema
  const validParams = ['http://example.com'];
  isProtectedNamespaceParamsSchema.parse([validParams[0]]);
  console.log('  ✅ PASS: Params schema validates');

  // Test return schema
  const validReturn = true;
  isProtectedNamespaceReturnSchema.parse(validReturn);
  console.log('  ✅ PASS: Return schema validates\n');

  var pass4 = true;
} catch (error) {
  console.log(`  ❌ FAIL: ${error.message}\n`);
  var pass4 = false;
}

// Test 5: Sample schema exports
console.log('Test 5: Sample Schema Exports');
const samples = [
  '../../src/admission/forbidden-operations.schema.mjs',
  '../../src/composables/use-validator.schema.mjs',
  '../../src/commands/validate.schema.mjs',
];

let pass5 = true;
for (const sample of samples) {
  try {
    const module = await import(sample);
    if (!module.default) {
      throw new Error('No default export');
    }
    console.log(`  ✅ PASS: ${sample.split('/').pop()}`);
  } catch (error) {
    console.log(`  ❌ FAIL: ${sample.split('/').pop()} - ${error.message}`);
    pass5 = false;
  }
}
console.log();

// Summary
console.log('━'.repeat(70));
console.log('Summary:\n');
const allPass = pass1 && pass2 && pass3 && pass4 && pass5;
console.log(`  Test 1 (Manifest Coverage): ${pass1 ? '✅ PASS' : '❌ FAIL'}`);
console.log(`  Test 2 (File Count): ${pass2 ? '✅ PASS' : '❌ FAIL'}`);
console.log(`  Test 3 (Module Coverage): ${pass3 ? '✅ PASS' : '❌ FAIL'}`);
console.log(`  Test 4 (Schema Validation): ${pass4 ? '✅ PASS' : '❌ FAIL'}`);
console.log(`  Test 5 (Sample Exports): ${pass5 ? '✅ PASS' : '❌ FAIL'}`);
console.log();
console.log(`Overall: ${allPass ? '🎉 ALL TESTS PASSED' : '❌ SOME TESTS FAILED'}`);
console.log('━'.repeat(70) + '\n');

process.exit(allPass ? 0 : 1);
