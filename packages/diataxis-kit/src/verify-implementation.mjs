#!/usr/bin/env node
/**
 * @file Quick verification of diataxis-kit implementations
 */

import { stableStringify, stableEqual } from './stable-json.mjs';
import { hashObject, hashString, hashFile } from './hash.mjs';
import { createDiataxisEntry, validateDiataxisEntry, ensureMinimumDiataxis } from './diataxis-schema.mjs';

let errors = 0;

function assert(condition, message) {
  if (!condition) {
    console.error(`❌ FAIL: ${message}`);
    errors++;
  } else {
    console.log(`✅ PASS: ${message}`);
  }
}

console.log('=== Testing stable-json.mjs ===\n');

// Test 1: Key sorting
const unsorted = { z: 1, a: 2, m: 3 };
const json = stableStringify(unsorted, { indent: 0 });
assert(json === '{"a":2,"m":3,"z":1}', 'Keys should be sorted alphabetically');

// Test 2: Array order preserved
const withArray = { b: [3, 1, 2], a: 1 };
const jsonArray = stableStringify(withArray, { indent: 0 });
assert(jsonArray === '{"a":1,"b":[3,1,2]}', 'Array order should be preserved');

// Test 3: Nested object sorting
const nested = { z: { z: 1, a: 2 }, a: 1 };
const jsonNested = stableStringify(nested, { indent: 0 });
assert(jsonNested === '{"a":1,"z":{"a":2,"z":1}}', 'Nested objects should have sorted keys');

// Test 4: stableEqual
const obj1 = { a: 1, b: 2 };
const obj2 = { b: 2, a: 1 };
assert(stableEqual(obj1, obj2), 'Objects with same content should be equal');

const obj3 = { a: 1, b: 3 };
assert(!stableEqual(obj1, obj3), 'Objects with different content should not be equal');

console.log('\n=== Testing hash.mjs ===\n');

// Test 5: hashString produces consistent output
const str = 'test string';
const hash1 = hashString(str);
const hash2 = hashString(str);
assert(hash1 === hash2, 'hashString should be deterministic');
assert(hash1.length === 64, 'SHA256 hash should be 64 hex characters');
assert(/^[a-f0-9]{64}$/.test(hash1), 'Hash should be valid hex');

// Test 6: hashObject uses stable stringify
const hashObj1 = hashObject({ b: 2, a: 1 });
const hashObj2 = hashObject({ a: 1, b: 2 });
assert(hashObj1 === hashObj2, 'hashObject should ignore key order');

console.log('\n=== Testing diataxis-schema.mjs ===\n');

// Test 7: createDiataxisEntry with minimal input
const entry1 = createDiataxisEntry('test-package', '1.0.0', {});
assert(entry1.packageName === 'test-package', 'Package name should be set');
assert(entry1.version === '1.0.0', 'Version should be set');
assert(entry1.generatedAt, 'Timestamp should be generated');
assert(Array.isArray(entry1.tutorials), 'Tutorials should be an array');
assert(Array.isArray(entry1.howtos), 'Howtos should be an array');
assert(entry1.reference && typeof entry1.reference === 'object', 'Reference should be an object');
assert(entry1.explanation && typeof entry1.explanation === 'object', 'Explanation should be an object');

// Test 8: DETERMINISTIC mode
process.env.DETERMINISTIC = '1';
const entry2 = createDiataxisEntry('test-package', '1.0.0', {});
assert(entry2.generatedAt === '2000-01-01T00:00:00.000Z', 'DETERMINISTIC=1 should use fixed timestamp');
delete process.env.DETERMINISTIC;

// Test 9: validateDiataxisEntry
const validation1 = validateDiataxisEntry(entry1);
assert(validation1.valid, 'Valid entry should pass validation');
assert(validation1.errors.length === 0, 'Valid entry should have no errors');

// Test 10: Invalid entry fails validation
const invalidEntry = { packageName: 'test' };
const validation2 = validateDiataxisEntry(invalidEntry);
assert(!validation2.valid, 'Invalid entry should fail validation');
assert(validation2.errors.length > 0, 'Invalid entry should have errors');

// Test 11: ensureMinimumDiataxis
const minimalEntry = createDiataxisEntry('test', '1.0.0', {
  tutorials: [],
  howtos: [],
  reference: null,
  explanation: null
});
const ensured = ensureMinimumDiataxis(minimalEntry);
assert(ensured.reference.items.length === 0, 'Empty reference should be created');
assert(ensured.explanation.concepts.length === 0, 'Empty explanation should be created');
assert(ensured.confidence.reference === 0, 'Empty reference should have 0 confidence');
assert(ensured.confidence.explanation === 0, 'Empty explanation should have 0 confidence');

// Test 12: Evidence fingerprint
const entry3 = createDiataxisEntry('test', '1.0.0', {
  readmeHeadings: ['Installation', 'Usage'],
  docsFiles: ['README.md'],
  examplesFiles: ['example.js']
});
assert(entry3.evidence.fingerprint.length === 64, 'Fingerprint should be SHA256 hash');
assert(entry3.evidence.readmeHeadings.length === 2, 'Evidence should preserve headings');

// Test 13: Tutorial/HowTo ID generation
const entry4 = createDiataxisEntry('test', '1.0.0', {
  tutorials: [
    { title: 'Getting Started with Test', goal: 'Learn basics', prerequisites: [], stepsOutline: [], confidenceScore: 0.8, source: ['README'] }
  ],
  howtos: [
    { title: 'How to Install Test', task: 'Install', context: 'Setup', steps: [], confidenceScore: 0.9, source: ['README'] }
  ]
});
assert(entry4.tutorials[0].id === 'getting-started-with-test', 'Tutorial ID should be generated from title');
assert(entry4.howtos[0].id === 'how-to-install-test', 'HowTo ID should be generated from title');

console.log('\n=== Summary ===\n');
if (errors === 0) {
  console.log('✅ All tests passed!');
  process.exit(0);
} else {
  console.log(`❌ ${errors} test(s) failed`);
  process.exit(1);
}
