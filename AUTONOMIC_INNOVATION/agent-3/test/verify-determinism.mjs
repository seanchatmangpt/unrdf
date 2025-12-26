#!/usr/bin/env node
/**
 * @fileoverview Verify lens compilation and execution determinism (100 iterations)
 */

import { createStore } from '@unrdf/oxigraph';
import { defineLens, compileLens, executeLensToGraph } from '../src/index.mjs';

console.log('🔍 Determinism Verification (100 iterations)\n');

// Create test lens
function createTestLens() {
  return defineLens('determinism-test-v1', {
    namespace: 'https://test.example.org/',
    prefixes: {
      schema: 'http://schema.org/',
      xsd: 'http://www.w3.org/2001/XMLSchema#'
    },
    conventions: { idField: 'id' }
  }, {
    TestEntity: {
      subject: {
        pattern: '{namespace}TestEntity/{id}',
        keys: ['id']
      },
      type: 'schema:Thing',
      predicates: {
        field1: { iri: 'schema:field1', datatype: 'xsd:string', required: true },
        field2: { iri: 'schema:field2', datatype: 'xsd:integer', required: true },
        field3: { iri: 'schema:field3', datatype: 'xsd:string', required: false }
      }
    }
  });
}

const testPayload = {
  id: 'test-123',
  field1: 'value1',
  field2: 42,
  field3: 'optional-value'
};

console.log('Test payload:', JSON.stringify(testPayload, null, 2));
console.log('\nRunning 100 iterations...\n');

const compilationHashes = new Set();
const iris = new Set();
const quadCounts = new Set();
let firstQuadSet = null;

for (let i = 0; i < 100; i++) {
  // Create lens and compile
  const lens = createTestLens();
  const compiled = compileLens(lens);

  // Track compilation hash
  compilationHashes.add(compiled.canonicalHash);

  // Execute transformation
  const store = createStore();
  const quads = executeLensToGraph(testPayload, compiled, store, 'TestEntity');

  // Track IRI
  iris.add(quads[0].subject.value);

  // Track quad count
  quadCounts.add(quads.length);

  // Store first quad set for comparison
  if (i === 0) {
    firstQuadSet = quads.map(q => ({
      s: q.subject.value,
      p: q.predicate.value,
      o: q.object.value,
      dt: q.object.datatype ? q.object.datatype.value : null
    }));
  } else {
    // Verify quads match first iteration
    const currentQuads = quads.map(q => ({
      s: q.subject.value,
      p: q.predicate.value,
      o: q.object.value,
      dt: q.object.datatype ? q.object.datatype.value : null
    }));

    const mismatch = JSON.stringify(firstQuadSet) !== JSON.stringify(currentQuads);
    if (mismatch) {
      console.error(`❌ Quad mismatch at iteration ${i + 1}`);
      console.error('Expected:', firstQuadSet);
      console.error('Got:', currentQuads);
      process.exit(1);
    }
  }

  if ((i + 1) % 10 === 0) {
    process.stdout.write(`  ${i + 1}/100 iterations complete\r`);
  }
}

console.log('  100/100 iterations complete\n');

// Report results
console.log('📊 Results:');
console.log('─────────────────────────────────────────');
console.log(`Unique compilation hashes: ${compilationHashes.size}`);
console.log(`Unique IRIs generated:     ${iris.size}`);
console.log(`Unique quad counts:        ${quadCounts.size}`);
console.log('─────────────────────────────────────────\n');

// Print actual values
console.log('Compilation hash:', [...compilationHashes][0]);
console.log('Subject IRI:     ', [...iris][0]);
console.log('Quad count:      ', [...quadCounts][0]);
console.log('');

// Verify
const allDeterministic = (
  compilationHashes.size === 1 &&
  iris.size === 1 &&
  quadCounts.size === 1
);

if (allDeterministic) {
  console.log('✅ 100/100 hashes identical');
  console.log('✅ 100/100 IRIs identical');
  console.log('✅ 100/100 quad sets identical');
  console.log('\n🎉 Determinism verified: PASS\n');
  process.exit(0);
} else {
  console.log('❌ Determinism verification: FAIL\n');
  process.exit(1);
}
