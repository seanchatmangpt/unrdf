/**
 * Proof: Immutable Receipts (Tampering Prevention)
 * 
 * Demonstrates that receipt tampering is IMPOSSIBLE via Object.freeze.
 * 
 * PROOF REQUIREMENTS:
 * 1. Test RUNS and completes
 * 2. Receipts are deeply frozen
 * 3. Modification attempts fail silently or throw
 * 4. Hash integrity is preserved
 */

import { createImmutableReceipt, isFrozen, isDeeplyFrozen } from './immutable-receipt.mjs';
import assert from 'node:assert';

// =============================================================================
// TEST 1: Receipt is frozen
// =============================================================================
async function test1_receiptFrozen() {
  const receipt = await createImmutableReceipt('test-event', { value: 42 });
  
  // Verify receipt is frozen
  assert.ok(isFrozen(receipt), 'Receipt should be frozen');
  assert.ok(isDeeplyFrozen(receipt), 'Receipt should be deeply frozen');
  
  console.log('✅ Test 1: Receipt is deeply frozen');
}

// =============================================================================
// TEST 2: Top-level tampering prevented
// =============================================================================
async function test2_topLevelTamperingPrevented() {
  const receipt = await createImmutableReceipt('test-event', { value: 42 });
  
  const originalHash = receipt.hash;
  const originalEventType = receipt.eventType;
  
  // Attempt to modify top-level fields
  // In strict mode: throws TypeError
  // In non-strict mode: silently ignored
  try {
    receipt.hash = 'forged-hash';
    receipt.eventType = 'malicious-event';
    receipt.receiptType = 'fake';
  } catch (err) {
    // Expected in strict mode
    console.log(`   [Strict mode] Tampering threw: ${err.message}`);
  }
  
  // Verify fields unchanged
  assert.strictEqual(receipt.hash, originalHash, 'Hash should be unchanged');
  assert.strictEqual(receipt.eventType, originalEventType, 'EventType should be unchanged');
  
  console.log('✅ Test 2: Top-level field tampering prevented');
}

// =============================================================================
// TEST 3: Payload tampering prevented (deep freeze)
// =============================================================================
async function test3_payloadTamperingPrevented() {
  const receipt = await createImmutableReceipt('test-event', {
    value: 42,
    nested: { data: 'secret' },
  });
  
  const originalValue = receipt.payload.value;
  const originalNested = receipt.payload.nested.data;
  
  // Attempt to modify payload
  try {
    receipt.payload.value = 999;
    receipt.payload.nested.data = 'tampered';
    receipt.payload.newField = 'injected';
  } catch (err) {
    console.log(`   [Strict mode] Payload tampering threw: ${err.message}`);
  }
  
  // Verify payload unchanged (deep freeze)
  assert.strictEqual(receipt.payload.value, originalValue, 'Payload.value should be unchanged');
  assert.strictEqual(receipt.payload.nested.data, originalNested, 'Nested data should be unchanged');
  assert.strictEqual(receipt.payload.newField, undefined, 'New fields should not be added');
  
  console.log('✅ Test 3: Payload tampering prevented (deep freeze works)');
}

// =============================================================================
// TEST 4: Hash cannot be forged
// =============================================================================
async function test4_hashCannotBeForged() {
  const receipt1 = await createImmutableReceipt('test-event', { value: 42 });
  const receipt2 = await createImmutableReceipt('test-event', { value: 99 });
  
  // Receipts should have different hashes
  assert.notStrictEqual(receipt1.hash, receipt2.hash, 'Different payloads should have different hashes');
  
  // Attempt to make receipt1 look like receipt2 by changing hash
  const originalHash1 = receipt1.hash;
  try {
    receipt1.hash = receipt2.hash;
  } catch (err) {
    // Expected in strict mode
  }
  
  // Verify hash unchanged
  assert.strictEqual(receipt1.hash, originalHash1, 'Hash tampering should fail');
  assert.notStrictEqual(receipt1.hash, receipt2.hash, 'Hashes should still differ');
  
  console.log('✅ Test 4: Hash forgery prevented');
}

// =============================================================================
// TEST 5: Deterministic receipts are immutable
// =============================================================================
async function test5_deterministicReceiptsImmutable() {
  // Enable deterministic mode
  process.env.DETERMINISTIC = '1';
  
  const receipt1 = await createImmutableReceipt('test', { value: 42 });
  const receipt2 = await createImmutableReceipt('test', { value: 42 });
  
  // Same input should yield same hash in deterministic mode
  assert.strictEqual(receipt1.hash, receipt2.hash, 'Deterministic hashes should match');
  
  // Both should be frozen
  assert.ok(isFrozen(receipt1), 'Receipt 1 should be frozen');
  assert.ok(isFrozen(receipt2), 'Receipt 2 should be frozen');
  
  // Cleanup
  delete process.env.DETERMINISTIC;
  
  console.log('✅ Test 5: Deterministic receipts are immutable');
}

// =============================================================================
// RUN ALL TESTS
// =============================================================================
async function runAllTests() {
  console.log('\n=== Poka-Yoke Proof: Immutable Receipts ===\n');
  
  await test1_receiptFrozen();
  await test2_topLevelTamperingPrevented();
  await test3_payloadTamperingPrevented();
  await test4_hashCannotBeForged();
  await test5_deterministicReceiptsImmutable();
  
  console.log('\n✅ ALL TESTS PASSED (5/5)');
  console.log('🎯 PROOF COMPLETE: Receipt tampering is IMPOSSIBLE\n');
}

runAllTests().catch(err => {
  console.error('\n❌ TEST SUITE FAILED:', err);
  process.exit(1);
});
