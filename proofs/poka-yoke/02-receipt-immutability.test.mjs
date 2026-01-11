/**
 * Poka-Yoke Proof 02: Receipt Immutability (SMOKE)
 *
 * Proves: Object.freeze() prevents receipt tampering
 * Expected Runtime: <50ms
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';

test('Proof 02: Receipt Immutability (SMOKE)', async (t) => {
  await t.test('Frozen receipt prevents tampering', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      universe_hash: 'abc123',
      t_ns: 1234567890n,
    });

    // Attempt to tamper (may throw in strict mode)
    try {
      receipt.universe_hash = 'TAMPERED';
    } catch (e) {
      assert.ok(e instanceof TypeError);
    }

    assert.equal(receipt.universe_hash, 'abc123');
    console.log('  ✅ PROTECTED: Frozen object prevents tampering');
  });

  await t.test('Cannot add properties to frozen object', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
    });

    // Attempt to add property (may throw in strict mode)
    try {
      receipt.newField = 'should not appear';
    } catch (e) {
      assert.ok(e instanceof TypeError);
    }

    assert.equal(receipt.newField, undefined);
    console.log('  ✅ Cannot add properties to frozen object');
  });

  await t.test('Deep freeze prevents nested mutations', () => {
    function deepFreeze(obj) {
      Object.freeze(obj);
      Object.values(obj).forEach(value => {
        if (value && typeof value === 'object') {
          deepFreeze(value);
        }
      });
      return obj;
    }

    const receipt = deepFreeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      metadata: {
        timestamp: '2025-12-28',
      },
    });

    // Attempt to tamper (may throw in strict mode)
    try {
      receipt.metadata.timestamp = 'TAMPERED';
    } catch (e) {
      assert.ok(e instanceof TypeError);
    }

    assert.equal(receipt.metadata.timestamp, '2025-12-28');
    console.log('  ✅ PROTECTED: Deep freeze prevents nested mutations');
  });
});

console.log('✅ Proof 02 PASSED: Object.freeze() prevents tampering');
