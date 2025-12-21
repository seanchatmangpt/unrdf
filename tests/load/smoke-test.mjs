/**
 * Smoke Test - 10 second validation
 * Verify basic functionality before long-running tests
 */

import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

function generateQuad(id) {
  return quad(
    namedNode(`http://example.org/entity/${id}`),
    namedNode('http://schema.org/name'),
    literal(`Entity ${id}`)
  );
}

async function smokeTest() {
  console.log('💨 Smoke Test (10s)\n');
  const startMem = process.memoryUsage().heapUsed;

  // Test 1: Create store
  console.log('1. Creating store...');
  const store = createStore();
  console.log('   ✅ Store created');

  // Test 2: Insert data
  console.log('2. Inserting 1000 quads...');
  for (let i = 0; i < 1000; i++) {
    store.add(generateQuad(i));
  }
  console.log('   ✅ Insert complete');

  // Test 3: Query
  console.log('3. Querying data...');
  let count = 0;
  for (const _ of store.match()) {
    count++;
    if (count >= 100) break;
  }
  console.log(`   ✅ Query returned ${count} results`);

  // Test 4: Delete
  console.log('4. Deleting quads...');
  const toDelete = [];
  for (const q of store.match()) {
    toDelete.push(q);
    if (toDelete.length >= 100) break;
  }
  toDelete.forEach(q => store.delete(q));
  console.log(`   ✅ Deleted ${toDelete.length} quads`);

  // Test 5: Sustained ops
  console.log('5. Running 5s sustained load...');
  const duration = 5000;
  let ops = 0;
  const start = Date.now();

  while (Date.now() - start < duration) {
    store.add(generateQuad(ops + 10000));
    ops++;
  }

  const opsPerSec = (ops / 5).toFixed(0);
  console.log(`   ✅ ${ops} ops in 5s (${opsPerSec} ops/sec)`);

  // Memory check
  const endMem = process.memoryUsage().heapUsed;
  const growthMB = ((endMem - startMem) / 1024 / 1024).toFixed(2);
  console.log(`6. Memory: ${growthMB} MB growth`);

  console.log('\n✅ Smoke test PASSED');
  console.log(`   Throughput: ${opsPerSec} ops/sec`);
  console.log(`   Memory: ${growthMB} MB`);
}

smokeTest().catch(err => {
  console.error('❌ Smoke test FAILED:', err);
  process.exit(1);
});
