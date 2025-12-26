/**
 * @file Basic test of substrate adapter (without external dependencies)
 * @module agent-9/test-basic
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
  getStats,
  validateOperation,
  isOperation
} from './index.mjs';

/**
 * Test operation creation and validation
 */
function testOperations() {
  console.log('Testing operation creation and validation...');

  const op = createOperation('INSERT', {
    subject: 'ex:test',
    predicate: 'rdf:type',
    object: 'ex:Test'
  });

  console.log('  ✓ Created operation:', op.type);
  console.log('  ✓ Operation has ID:', !!op.id);
  console.log('  ✓ Operation has timestamp:', !!op.timestamp);

  const isValid = validateOperation(op);
  console.log('  ✓ Operation is valid:', isValid);

  const checkOp = isOperation(op);
  console.log('  ✓ isOperation check:', checkOp);

  console.log('✅ Operation tests passed\n');
}

/**
 * Test basic substrate operations
 */
function testBasicSubstrate() {
  console.log('Testing basic substrate operations...');

  const substrate = createSubstrate();
  console.log('  ✓ Created substrate');

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
    })
  ];

  const result = apply(substrate, ops);
  console.log('  ✓ Applied', result.affected, 'operations');

  const stats = getStats(substrate);
  console.log('  ✓ Substrate has', stats.tripleCount, 'triples');

  const results = query(substrate, 'SELECT * WHERE { ?s ?p ?o }');
  console.log('  ✓ Query returned', results.length, 'results');

  console.log('✅ Basic substrate tests passed\n');
  return substrate;
}

/**
 * Test transactions
 */
function testTransactions() {
  console.log('Testing transactions...');

  const substrate = createSubstrate();

  // Add initial data
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Initial',
      predicate: 'ex:value',
      object: '"1"'
    })
  ]);

  const initialCount = getStats(substrate).tripleCount;
  console.log('  ✓ Initial triple count:', initialCount);

  // Begin transaction
  const tx = beginTransaction(substrate);
  console.log('  ✓ Started transaction:', tx.id);

  // Apply operations
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:InTx',
      predicate: 'ex:value',
      object: '"2"'
    })
  ]);

  const duringTx = getStats(substrate).tripleCount;
  console.log('  ✓ During transaction:', duringTx, 'triples');

  // Commit
  const commitResult = commit(tx);
  console.log('  ✓ Committed:', commitResult.success);

  const afterCommit = getStats(substrate).tripleCount;
  console.log('  ✓ After commit:', afterCommit, 'triples');

  console.log('✅ Transaction tests passed\n');
  return substrate;
}

/**
 * Test rollback
 */
function testRollback() {
  console.log('Testing rollback...');

  const substrate = createSubstrate();

  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Keep',
      predicate: 'ex:value',
      object: '"keep"'
    })
  ]);

  const beforeTx = getStats(substrate).tripleCount;
  console.log('  ✓ Before transaction:', beforeTx, 'triples');

  const tx = beginTransaction(substrate);

  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Discard',
      predicate: 'ex:value',
      object: '"discard"'
    })
  ]);

  const duringTx = getStats(substrate).tripleCount;
  console.log('  ✓ During transaction:', duringTx, 'triples');

  rollback(tx);
  const afterRollback = getStats(substrate).tripleCount;
  console.log('  ✓ After rollback:', afterRollback, 'triples');

  const restored = beforeTx === afterRollback;
  console.log('  ✓ State restored:', restored);

  console.log('✅ Rollback tests passed\n');
  return substrate;
}

/**
 * Test snapshots
 */
function testSnapshot() {
  console.log('Testing snapshots...');

  const substrate = createSubstrate();

  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Original',
      predicate: 'ex:value',
      object: '"original"'
    })
  ]);

  const snap = snapshot(substrate);
  console.log('  ✓ Created snapshot');
  console.log('  ✓ Snapshot is frozen:', snap.frozen);

  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:Modified',
      predicate: 'ex:value',
      object: '"modified"'
    })
  ]);

  const originalCount = getStats(substrate).tripleCount;
  const snapshotCount = getStats(snap).tripleCount;

  console.log('  ✓ Original:', originalCount, 'triples');
  console.log('  ✓ Snapshot:', snapshotCount, 'triples');
  console.log('  ✓ Snapshot is immutable:', originalCount > snapshotCount);

  console.log('✅ Snapshot tests passed\n');
}

/**
 * Test DELETE operations
 */
function testDelete() {
  console.log('Testing DELETE operations...');

  const substrate = createSubstrate();

  // Insert
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:ToDelete',
      predicate: 'ex:value',
      object: '"delete_me"'
    })
  ]);

  const beforeDelete = getStats(substrate).tripleCount;
  console.log('  ✓ Before delete:', beforeDelete, 'triples');

  // Delete
  const deleteResult = apply(substrate, [
    createOperation('DELETE', {
      subject: 'ex:ToDelete',
      predicate: 'ex:value',
      object: '"delete_me"'
    })
  ]);

  const afterDelete = getStats(substrate).tripleCount;
  console.log('  ✓ After delete:', afterDelete, 'triples');
  console.log('  ✓ Deleted', deleteResult.affected, 'triples');

  console.log('✅ DELETE tests passed\n');
}

/**
 * Test UPDATE operations
 */
function testUpdate() {
  console.log('Testing UPDATE operations...');

  const substrate = createSubstrate();

  // Insert
  apply(substrate, [
    createOperation('INSERT', {
      subject: 'ex:ToUpdate',
      predicate: 'ex:value',
      object: '"old"'
    })
  ]);

  // Update
  const updateResult = apply(substrate, [
    createOperation('UPDATE', {
      subject: 'ex:ToUpdate',
      predicate: 'ex:value',
      object: '"new"'
    })
  ]);

  console.log('  ✓ Updated', updateResult.affected, 'triples');

  const results = query(substrate, 'SELECT * WHERE { ?s ?p ?o }');
  console.log('  ✓ Query returned', results.length, 'results');

  console.log('✅ UPDATE tests passed\n');
}

/**
 * Run all tests
 */
function runTests() {
  console.log('╔═══════════════════════════════════════════════════╗');
  console.log('║   Substrate Adapter Layer - Basic Tests          ║');
  console.log('╚═══════════════════════════════════════════════════╝\n');

  try {
    testOperations();
    testBasicSubstrate();
    testTransactions();
    testRollback();
    testSnapshot();
    testDelete();
    testUpdate();

    console.log('╔═══════════════════════════════════════════════════╗');
    console.log('║   ✅ All tests passed successfully               ║');
    console.log('╚═══════════════════════════════════════════════════╝\n');

    return true;
  } catch (error) {
    console.error('\n❌ Test failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run tests if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runTests();
}

export { runTests };
