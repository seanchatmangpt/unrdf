/**
 * Proof: Connection Lifecycle Guard
 * 
 * Demonstrates that invalid connection operations are IMPOSSIBLE.
 * 
 * State machine: Disconnected → Connecting → Connected → Closing → Closed
 * 
 * PROOF REQUIREMENTS:
 * 1. Test RUNS and completes
 * 2. Invalid operations THROW ConnectionStateError
 * 3. State transitions are ENFORCED
 * 4. Error messages are SPECIFIC
 */

import { GuardedConnection, ConnectionStateError } from './connection-lifecycle.mjs';
import assert from 'node:assert';

// Test utilities
async function assertThrowsAsync(fn, expectedMessage, testName) {
  try {
    await fn();
    console.error(`❌ ${testName}: Expected error but none was thrown`);
    process.exit(1);
  } catch (err) {
    if (err instanceof ConnectionStateError && err.message.includes(expectedMessage)) {
      console.log(`✅ ${testName}: Correctly threw "${err.message}"`);
    } else {
      console.error(`❌ ${testName}: Wrong error: ${err.message}`);
      process.exit(1);
    }
  }
}

// =============================================================================
// TEST 1: Normal lifecycle (Disconnected → Connected → Closed)
// =============================================================================
async function test1_normalLifecycle() {
  const conn = new GuardedConnection();
  
  // Initially disconnected
  assert.strictEqual(conn.getState(), 'disconnected', 'Should start disconnected');
  
  // Connect
  await conn.connect({ url: 'http://localhost:7878' });
  assert.strictEqual(conn.getState(), 'connected', 'Should be connected');
  
  // Query works when connected
  const result = await conn.query('SELECT * WHERE { ?s ?p ?o }');
  assert.ok(result, 'Query should succeed');
  
  // Get stats works when connected
  const stats = conn.getStats();
  assert.strictEqual(stats.queryCount, 1, 'Query count should be 1');
  
  // Close
  await conn.close();
  assert.strictEqual(conn.getState(), 'closed', 'Should be closed');
  
  console.log('✅ Test 1: Normal lifecycle (Disconnected → Connected → Closed) works');
}

// =============================================================================
// TEST 2: Query before connect is PREVENTED
// =============================================================================
async function test2_queryBeforeConnect() {
  const conn = new GuardedConnection();
  
  await assertThrowsAsync(
    () => conn.query('SELECT * WHERE { ?s ?p ?o }'),
    'disconnected',
    'Test 2: Query before connect'
  );
  
  console.log('✅ Test 2: Query before connect prevented');
}

// =============================================================================
// TEST 3: Use-after-close is PREVENTED
// =============================================================================
async function test3_useAfterClose() {
  const conn = new GuardedConnection();
  
  await conn.connect({ url: 'http://localhost:7878' });
  await conn.query('SELECT * WHERE { ?s ?p ?o }');  // Works
  await conn.close();
  
  // All operations should now fail
  await assertThrowsAsync(
    () => conn.query('SELECT * WHERE { ?s ?p ?o }'),
    'closed',
    'Test 3a: Query after close'
  );
  
  await assertThrowsAsync(
    () => conn.connect({ url: 'http://localhost:7878' }),
    'closed',
    'Test 3b: Connect after close'
  );
  
  console.log('✅ Test 3: Use-after-close prevented (POKA-YOKE WORKS)');
}

// =============================================================================
// TEST 4: Double-connect is PREVENTED
// =============================================================================
async function test4_doubleConnect() {
  const conn = new GuardedConnection();
  
  await conn.connect({ url: 'http://localhost:7878' });
  
  // Second connect should fail
  await assertThrowsAsync(
    () => conn.connect({ url: 'http://localhost:7878' }),
    'already connected',
    'Test 4: Double connect'
  );
  
  await conn.close();
  console.log('✅ Test 4: Double-connect prevented');
}

// =============================================================================
// TEST 5: Double-close is PREVENTED
// =============================================================================
async function test5_doubleClose() {
  const conn = new GuardedConnection();
  
  await conn.connect({ url: 'http://localhost:7878' });
  await conn.close();
  
  // Second close should fail
  await assertThrowsAsync(
    () => conn.close(),
    'already closed',
    'Test 5: Double close'
  );
  
  console.log('✅ Test 5: Double-close prevented');
}

// =============================================================================
// TEST 6: Close before connect is PREVENTED
// =============================================================================
async function test6_closeBeforeConnect() {
  const conn = new GuardedConnection();
  
  await assertThrowsAsync(
    () => conn.close(),
    'not connected',
    'Test 6: Close before connect'
  );
  
  console.log('✅ Test 6: Close before connect prevented');
}

// =============================================================================
// RUN ALL TESTS
// =============================================================================
async function runAllTests() {
  console.log('\n=== Poka-Yoke Proof: Connection Lifecycle Guard ===\n');
  
  await test1_normalLifecycle();
  await test2_queryBeforeConnect();
  await test3_useAfterClose();
  await test4_doubleConnect();
  await test5_doubleClose();
  await test6_closeBeforeConnect();
  
  console.log('\n✅ ALL TESTS PASSED (6/6)');
  console.log('🎯 PROOF COMPLETE: Invalid connection operations are IMPOSSIBLE\n');
}

runAllTests().catch(err => {
  console.error('\n❌ TEST SUITE FAILED:', err);
  process.exit(1);
});
