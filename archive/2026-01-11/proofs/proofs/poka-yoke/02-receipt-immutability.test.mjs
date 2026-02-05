/**
 * Poka-Yoke Proof 02: Receipt Immutability (SMOKE)
 *
 * Proves: Object.freeze() prevents receipt tampering
 * Expected Runtime: <50ms
 */

import { describe, it, expect } from 'vitest';

describe('Proof 02: Receipt Immutability (SMOKE)', () => {
  it('Frozen receipt prevents tampering', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
      universe_hash: 'abc123',
      t_ns: 1234567890n,
    });

    // Attempt to tamper (may throw in strict mode)
    try {
      receipt.universe_hash = 'TAMPERED';
    } catch (e) {
      expect(e).toBeInstanceOf(TypeError);
    }

    expect(receipt.universe_hash).toBe('abc123');
  });

  it('Cannot add properties to frozen object', () => {
    const receipt = Object.freeze({
      id: '550e8400-e29b-41d4-a716-446655440000',
    });

    // Attempt to add property (may throw in strict mode)
    try {
      receipt.newField = 'should not appear';
    } catch (e) {
      expect(e).toBeInstanceOf(TypeError);
    }

    expect(receipt.newField).toBeUndefined();
  });

  it('Deep freeze prevents nested mutations', () => {
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
      expect(e).toBeInstanceOf(TypeError);
    }

    expect(receipt.metadata.timestamp).toBe('2025-12-28');
  });
});
