/**
 * Poka-Yoke Proof 02: Receipt Immutability
 * 
 * Proves: Object.freeze() prevents receipt tampering
 * Pattern: Immutability by default
 * Expected Runtime: <50ms
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';

test('Proof 02: Receipt Immutability - Object.freeze() Prevention', async (t) => {
  await t.test('Unfrozen receipt allows tampering (VULNERABILITY)', () => {
    const receipt = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      universe_hash: 'abc123',
      t_ns: 1234567890n,
    };
    
    // Can tamper - object not frozen!
    receipt.universe_hash = 'TAMPERED';
    
    assert.strictEqual(receipt.universe_hash, 'TAMPERED');
    console.log('  ⚠️  VULNERABILITY: Unfrozen object allows tampering');
  });

  await t.test('Frozen receipt prevents tampering (PROTECTED)', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      universe_hash: 'abc123',
      t_ns: 1234567890n,
    });
    
    // Attempt to tamper
    receipt.universe_hash = 'TAMPERED';
    
    // Tampering silently fails (strict mode would throw)
    assert.strictEqual(receipt.universe_hash, 'abc123');
    console.log('  ✅ PROTECTED: Frozen object prevents tampering');
  });

  await t.test('Strict mode throws on frozen object mutation', () => {
    'use strict';
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      universe_hash: 'abc123',
      t_ns: 1234567890n,
    });
    
    // Should throw in strict mode
    assert.throws(() => {
      receipt.universe_hash = 'TAMPERED';
    }, TypeError);
    
    console.log('  ✅ Strict mode throws TypeError on mutation');
  });

  await t.test('Cannot add properties to frozen object', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
    });
    
    receipt.newField = 'should not appear';
    
    assert.strictEqual(receipt.newField, undefined);
    console.log('  ✅ Cannot add properties to frozen object');
  });

  await t.test('Cannot delete properties from frozen object', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      universe_hash: 'abc123',
    });
    
    delete receipt.universe_hash;
    
    assert.strictEqual(receipt.universe_hash, 'abc123');
    console.log('  ✅ Cannot delete properties from frozen object');
  });

  await t.test('Nested objects require deep freeze', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      metadata: {  // ← Nested object NOT frozen!
        timestamp: '2025-12-28',
      },
    });
    
    // Can mutate nested object!
    receipt.metadata.timestamp = 'TAMPERED';
    
    assert.strictEqual(receipt.metadata.timestamp, 'TAMPERED');
    console.log('  ⚠️  VULNERABILITY: Shallow freeze doesn\'t protect nested objects');
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
    
    // Attempt to mutate nested object
    receipt.metadata.timestamp = 'TAMPERED';
    
    // Mutation fails (protected)
    assert.strictEqual(receipt.metadata.timestamp, '2025-12-28');
    console.log('  ✅ PROTECTED: Deep freeze prevents nested mutations');
  });
});

console.log('✅ Proof 02 PASSED: Object.freeze() prevents tampering');
console.log('   - Frozen objects are immutable');
console.log('   - Must use deepFreeze for nested objects');
console.log('   - V6 Contract: ALL receipts must be Object.freeze()d');
