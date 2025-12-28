/**
 * Determinism Tests for withReceipt HOF
 *
 * L3 Maturity Requirement: Same input → Same hash (ALWAYS)
 *
 * Tests verify:
 * 1. Deterministic hash generation (multiple runs = identical hashes)
 * 2. Idempotency (second execution produces same receipt)
 * 3. Context injection works correctly
 */

import { describe, it, expect } from 'vitest';
import { withReceipt } from '../src/adapters.mjs';
import { computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

describe('withReceipt Determinism (L3 Maturity)', () => {
  describe('Hash Determinism', () => {
    it('produces identical hashes on multiple runs with same context', async () => {
      // GIVEN: Same function, same input, same context
      const testFn = (data) => data.map((x) => x * 2);
      const testData = [1, 2, 3];
      const context = {
        t_ns: 1234567890000000000n, // Fixed timestamp
      };
      const options = {
        operation: 'testOperation',
        context,
        startTime: 100,
        endTime: 200,
      };

      // Create wrapped function
      const wrappedFn = withReceipt(testFn, options);

      // WHEN: Execute multiple times (pass array as single argument)
      const { receipt: receipt1 } = await wrappedFn(testData);
      const { receipt: receipt2 } = await wrappedFn(testData);
      const { receipt: receipt3 } = await wrappedFn(testData);

      // THEN: All receipts should be IDENTICAL
      expect(receipt1).toEqual(receipt2);
      expect(receipt2).toEqual(receipt3);

      // Hash the receipts
      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);
      const hash3 = await computeBlake3(receipt3);

      // Hashes MUST be identical
      expect(hash1).toBe(hash2);
      expect(hash2).toBe(hash3);

      console.log('✅ Deterministic hash:', hash1);
    });

    it('produces different hashes with different timestamps', async () => {
      // GIVEN: Same function, same input, DIFFERENT timestamps
      const testFn = (x) => x * 2;
      const testData = [5];

      const context1 = { t_ns: 1000000000000000000n };
      const context2 = { t_ns: 2000000000000000000n };

      const wrappedFn1 = withReceipt(testFn, {
        operation: 'testOp',
        context: context1,
        startTime: 100,
        endTime: 200,
      });

      const wrappedFn2 = withReceipt(testFn, {
        operation: 'testOp',
        context: context2,
        startTime: 100,
        endTime: 200,
      });

      // WHEN: Execute with different contexts
      const { receipt: receipt1 } = await wrappedFn1(testData[0]);
      const { receipt: receipt2 } = await wrappedFn2(testData[0]);

      // THEN: Receipts should be DIFFERENT (different timestamps)
      expect(receipt1.timestamp).not.toBe(receipt2.timestamp);

      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).not.toBe(hash2);
    });

    it('produces identical hashes with same context for complex objects', async () => {
      // GIVEN: Complex nested data structure
      const testFn = (data) => ({
        ...data,
        processed: true,
        nested: {
          value: data.value * 2,
          meta: { timestamp: data.meta?.timestamp || 0 },
        },
      });

      const testData = [
        {
          value: 42,
          meta: { timestamp: 999, extra: 'test' },
          array: [1, 2, 3],
        },
      ];

      const context = {
        t_ns: 5000000000000000000n,
      };

      const options = {
        operation: 'complexOp',
        context,
        startTime: 50,
        endTime: 100,
      };

      const wrappedFn = withReceipt(testFn, options);

      // WHEN: Execute multiple times
      const { receipt: receipt1 } = await wrappedFn(testData);
      const { receipt: receipt2 } = await wrappedFn(testData);

      // THEN: Hashes MUST be identical
      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).toBe(hash2);
      console.log('✅ Complex object deterministic hash:', hash1);
    });
  });

  describe('Idempotency', () => {
    it('second execution produces identical receipt when context is fixed', async () => {
      // GIVEN: Idempotent function with fixed context
      const processData = withReceipt(
        (data) => data.map((x) => x * 2),
        {
          operation: 'processData',
          context: { t_ns: 7777777777777777777n },
          startTime: 10,
          endTime: 20,
        }
      );

      // WHEN: Execute twice
      const firstRun = await processData([10, 20, 30]);
      const secondRun = await processData([10, 20, 30]);

      // THEN: Results and receipts MUST be identical
      expect(firstRun.result).toEqual(secondRun.result);
      expect(firstRun.receipt).toEqual(secondRun.receipt);

      const hash1 = await computeBlake3(firstRun.receipt);
      const hash2 = await computeBlake3(secondRun.receipt);

      expect(hash1).toBe(hash2);
      console.log('✅ Idempotent hash:', hash1);
    });
  });

  describe('Deterministic Serialization', () => {
    it('serializes arguments deterministically regardless of property order', async () => {
      // GIVEN: Function that accepts object arguments
      const testFn = (obj) => obj.a + obj.b;

      const context = { t_ns: 9999999999999999999n };
      const options = {
        operation: 'sumOp',
        context,
        startTime: 0,
        endTime: 1,
      };

      const wrappedFn = withReceipt(testFn, options);

      // WHEN: Pass objects with same data but different property definition order
      // (JavaScript objects retain insertion order, but deterministicSerialize
      // should sort keys alphabetically)
      const obj1 = { a: 1, b: 2 };
      const obj2 = { b: 2, a: 1 }; // Same data, different order

      const { receipt: receipt1 } = await wrappedFn(obj1);
      const { receipt: receipt2 } = await wrappedFn(obj2);

      // THEN: Receipts should be IDENTICAL (deterministic serialization)
      expect(receipt1.args).toBe(receipt2.args);

      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).toBe(hash2);
      console.log('✅ Property order-independent hash:', hash1);
    });

    it('handles arrays deterministically', async () => {
      // GIVEN: Function with array arguments
      const testFn = (arr) => arr.reduce((sum, x) => sum + x, 0);

      const context = { t_ns: 1111111111111111111n };
      const wrappedFn = withReceipt(testFn, {
        operation: 'sumArray',
        context,
        startTime: 5,
        endTime: 10,
      });

      // WHEN: Execute multiple times with same array
      const { receipt: receipt1 } = await wrappedFn([1, 2, 3, 4, 5]);
      const { receipt: receipt2 } = await wrappedFn([1, 2, 3, 4, 5]);

      // THEN: Hashes MUST be identical
      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).toBe(hash2);
    });

    it('handles nested objects and arrays deterministically', async () => {
      // GIVEN: Complex nested structure
      const testFn = (data) => data;

      const complexData = {
        nested: {
          array: [{ id: 3 }, { id: 1 }, { id: 2 }],
          obj: { z: 'last', a: 'first', m: 'middle' },
        },
        numbers: [9, 8, 7],
      };

      const context = { t_ns: 3333333333333333333n };
      const wrappedFn = withReceipt(testFn, {
        operation: 'identity',
        context,
        startTime: 1,
        endTime: 2,
      });

      // WHEN: Execute multiple times
      const { receipt: receipt1 } = await wrappedFn(complexData);
      const { receipt: receipt2 } = await wrappedFn(complexData);

      // THEN: Serialization MUST be deterministic
      expect(receipt1.args).toBe(receipt2.args);
      expect(receipt1.result).toBe(receipt2.result);

      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).toBe(hash2);
      console.log('✅ Nested structure deterministic hash:', hash1);
    });
  });

  describe('Edge Cases', () => {
    it('handles null and undefined deterministically', async () => {
      const testFn = (val) => val;

      const context = { t_ns: 4444444444444444444n };
      const wrappedFn = withReceipt(testFn, {
        operation: 'identity',
        context,
        startTime: 0,
        endTime: 1,
      });

      const { receipt: receipt1 } = await wrappedFn(null);
      const { receipt: receipt2 } = await wrappedFn(null);

      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).toBe(hash2);
    });

    it('handles bigint values deterministically', async () => {
      const testFn = (val) => val * 2n;

      const context = { t_ns: 6666666666666666666n };
      const wrappedFn = withReceipt(testFn, {
        operation: 'doubleBigInt',
        context,
        startTime: 0,
        endTime: 1,
      });

      const { receipt: receipt1 } = await wrappedFn(123456789012345678901234567890n);
      const { receipt: receipt2 } = await wrappedFn(123456789012345678901234567890n);

      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      expect(hash1).toBe(hash2);
    });

    it('handles functions that throw errors deterministically', async () => {
      const testFn = () => {
        throw new Error('Deterministic error');
      };

      const context = { t_ns: 8888888888888888888n };
      const wrappedFn = withReceipt(testFn, {
        operation: 'errorOp',
        context,
        startTime: 0,
        endTime: 1,
      });

      // WHEN: Function throws
      let error1, error2;
      try {
        await wrappedFn();
      } catch (err) {
        error1 = err;
      }

      try {
        await wrappedFn();
      } catch (err) {
        error2 = err;
      }

      // THEN: Same error should be thrown
      expect(error1).toBeDefined();
      expect(error2).toBeDefined();
      expect(error1.message).toBe(error2.message);
    });
  });

  describe('Non-Deterministic Behavior (Warning)', () => {
    it('warns that omitting context leads to non-deterministic behavior', async () => {
      // GIVEN: No context provided (uses Date.now())
      const testFn = (x) => x * 2;

      const wrappedFn = withReceipt(testFn, {
        operation: 'nonDeterministic',
        // NO context provided - will use Date.now()
      });

      // WHEN: Execute twice (with different real-world timestamps)
      const { receipt: receipt1 } = await wrappedFn(5);

      // Small delay to ensure different Date.now() values
      await new Promise((resolve) => setTimeout(resolve, 10));

      const { receipt: receipt2 } = await wrappedFn(5);

      // THEN: Timestamps WILL be different (non-deterministic)
      expect(receipt1.timestamp).not.toBe(receipt2.timestamp);

      const hash1 = await computeBlake3(receipt1);
      const hash2 = await computeBlake3(receipt2);

      // Hashes WILL be different
      expect(hash1).not.toBe(hash2);

      console.warn(
        '⚠️  Without context.t_ns, withReceipt is NON-DETERMINISTIC:',
        hash1,
        '≠',
        hash2
      );
    });
  });
});
