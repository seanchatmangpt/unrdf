/**
 * @file Example usage of substrate adapter layer
 * @module agent-9/example
 */

import {
  createSubstrate,
  createOperation,
  apply,
  query,
  snapshot,
  beginTransaction,
  commit,
  rollback,
  getStats
} from './index.mjs';

/**
 * Basic substrate example
 */
function basicExample() {
  console.log('\n=== Basic Substrate Example ===\n');

  // Create substrate
  const substrate = createSubstrate();
  console.log('✓ Created substrate');

  // Create operations
  const ops = [
    createOperation('INSERT', {
      subject: 'ex:John',
      predicate: 'rdf:type',
      object: 'ex:Person'
    }),
    createOperation('INSERT', {
      subject: 'ex:John',
      predicate: 'ex:name',
      object: '"John Doe"'
    }),
    createOperation('INSERT', {
      subject: 'ex:Jane',
      predicate: 'rdf:type',
      object: 'ex:Person'
    })
  ];
  console.log(`✓ Created ${ops.length} operations`);

  // Apply operations
  const result = apply(substrate, ops);
  console.log(`✓ Applied operations: ${result.affected} triples affected`);

  // Query substrate
  const results = query(substrate, 'SELECT * WHERE { ?s ?p ?o }');
  console.log(`✓ Query returned ${results.length} results`);

  // Get statistics
  const stats = getStats(substrate);
  console.log('✓ Substrate stats:', stats);

  return substrate;
}

/**
 * Transaction example
 */
function transactionExample() {
  console.log('\n=== Transaction Example ===\n');

  const substrate = createSubstrate();

  // Begin transaction
  const tx = beginTransaction(substrate);
  console.log('✓ Started transaction:', tx.id);

  // Apply operations in transaction
  const ops = [
    createOperation('INSERT', {
      subject: 'ex:Alice',
      predicate: 'rdf:type',
      object: 'ex:Person'
    }),
    createOperation('INSERT', {
      subject: 'ex:Alice',
      predicate: 'ex:age',
      object: '"30"'
    })
  ];

  apply(substrate, ops);
  console.log('✓ Applied operations in transaction');

  // Commit transaction
  const commitResult = commit(tx);
  console.log('✓ Committed transaction:', commitResult);

  // Check stats
  const stats = getStats(substrate);
  console.log('✓ Final stats:', stats);

  return substrate;
}

/**
 * Rollback example
 */
function rollbackExample() {
  console.log('\n=== Rollback Example ===\n');

  const substrate = createSubstrate();

  // Initial data
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Bob',
      predicate: 'rdf:type',
      object: 'ex:Person'
    })
  ]);

  const initialStats = getStats(substrate);
  console.log('✓ Initial state:', initialStats);

  // Begin transaction
  const tx = beginTransaction(substrate);

  // Apply operations
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Invalid',
      predicate: 'ex:data',
      object: '"bad"'
    })
  ]);

  const txStats = getStats(substrate);
  console.log('✓ During transaction:', txStats);

  // Rollback
  const rollbackResult = rollback(tx);
  console.log('✓ Rolled back transaction:', rollbackResult);

  // Verify rollback
  const finalStats = getStats(substrate);
  console.log('✓ After rollback:', finalStats);
  console.log(`✓ Triple count restored: ${initialStats.tripleCount === finalStats.tripleCount}`);

  return substrate;
}

/**
 * Snapshot example
 */
function snapshotExample() {
  console.log('\n=== Snapshot Example ===\n');

  const substrate = createSubstrate();

  // Add initial data
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Version1',
      predicate: 'ex:value',
      object: '"initial"'
    })
  ]);

  // Create snapshot
  const snap = snapshot(substrate);
  console.log('✓ Created snapshot (frozen:', snap.frozen + ')');

  // Modify original
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Version2',
      predicate: 'ex:value',
      object: '"modified"'
    })
  ]);

  const originalStats = getStats(substrate);
  const snapshotStats = getStats(snap);

  console.log('✓ Original substrate:', originalStats);
  console.log('✓ Snapshot substrate:', snapshotStats);
  console.log(`✓ Snapshot immutable: ${originalStats.tripleCount !== snapshotStats.tripleCount}`);

  return { substrate, snap };
}

/**
 * Run all examples
 */
function runExamples() {
  console.log('╔═══════════════════════════════════════════════════╗');
  console.log('║   Substrate Adapter Layer - Examples             ║');
  console.log('╚═══════════════════════════════════════════════════╝');

  try {
    basicExample();
    transactionExample();
    rollbackExample();
    snapshotExample();

    console.log('\n✅ All examples completed successfully\n');
  } catch (error) {
    console.error('\n❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run examples if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runExamples();
}

export { basicExample, transactionExample, rollbackExample, snapshotExample };
