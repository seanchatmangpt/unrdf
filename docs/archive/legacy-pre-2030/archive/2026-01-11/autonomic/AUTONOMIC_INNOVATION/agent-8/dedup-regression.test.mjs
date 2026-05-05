/**
 * Regression Test - Deduplication Changes
 * Verifies that store adapter works with canonical mock-store from test/mocks
 */

import { strict as assert } from 'assert';
import { createStoreAdapter } from './store-adapter.mjs';

console.log('Testing store adapter with canonical mock-store...');

// Test 1: Create adapter
const adapter = createStoreAdapter();
assert(typeof adapter.addQuad === 'function', 'Has addQuad method');
assert(typeof adapter.deleteQuad === 'function', 'Has deleteQuad method');
assert(typeof adapter.queryQuads === 'function', 'Has queryQuads method');
console.log('✅ Store adapter created successfully');

// Test 2: Add quad
const quad = {
  subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
  predicate: { termType: 'NamedNode', value: 'http://example.org/knows' },
  object: { termType: 'NamedNode', value: 'http://example.org/bob' },
  graph: { termType: 'DefaultGraph', value: '' }
};

adapter.addQuad(quad);
const quads = adapter.queryQuads(null, null, null, null);
assert(quads.length === 1, 'Added quad successfully');
console.log('✅ Add quad works');

// Test 3: Query quads
const aliceQuads = adapter.queryQuads(
  { termType: 'NamedNode', value: 'http://example.org/alice' },
  null,
  null,
  null
);
assert(aliceQuads.length === 1, 'Query finds alice quad');
console.log('✅ Query quads works');

// Test 4: Delete quad
adapter.deleteQuad(quad);
const afterDelete = adapter.queryQuads(null, null, null, null);
assert(afterDelete.length === 0, 'Deleted quad successfully');
console.log('✅ Delete quad works');

console.log('\n🎉 All deduplication regression tests passed!');
