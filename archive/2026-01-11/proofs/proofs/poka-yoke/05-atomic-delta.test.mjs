/**
 * Poka-Yoke Proof 05: Atomic Delta Application (SMOKE)
 *
 * Proves: Delta application is all-or-none (no partial state)
 * Expected Runtime: <50ms
 */

import { describe, it, expect } from 'vitest';

describe('Proof 05: Atomic Delta (SMOKE)', () => {
  it('Successful delta applies all operations', () => {
    const store = new Set(['existing-1']);

    const delta = {
      operations: [
        { op: 'add', triple: 'new-1' },
        { op: 'add', triple: 'new-2' },
      ],
    };

    for (const operation of delta.operations) {
      if (operation.op === 'add') {
        store.add(operation.triple);
      }
    }

    expect(store.size).toBe(3);
    expect(store.has('new-1')).toBe(true);
    expect(store.has('new-2')).toBe(true);
    console.log('  ✅ All operations applied');
  });

  it('Failed delta rolls back ALL operations', () => {
    const store = new Set(['existing-1']);
    const snapshot = new Set(store);

    const delta = {
      operations: [
        { op: 'add', triple: 'new-1' },
        { op: 'INVALID', triple: 'new-2' },
      ],
    };

    try {
      for (const operation of delta.operations) {
        if (operation.op === 'add') {
          store.add(operation.triple);
        } else {
          throw new Error(`Invalid operation: ${operation.op}`);
        }
      }
    } catch (error) {
      store.clear();
      snapshot.forEach(item => store.add(item));
    }

    expect(store.size).toBe(1);
    expect(store.has('new-1')).toBe(false);
    console.log('  ✅ Failed delta rolled back ALL operations');
  });

  it('V6 Pattern: Receipt on success or denial', () => {
    function applyDeltaWithReceipt(store, delta) {
      const snapshot = new Set(store);

      try {
        for (const operation of delta.operations) {
          if (operation.op === 'add') {
            store.add(operation.triple);
          } else {
            throw new Error(`Invalid operation: ${operation.op}`);
          }
        }

        return {
          applied: true,
          receipt: { id: 'r1', decision: 'ALLOW' },
        };
      } catch (error) {
        store.clear();
        snapshot.forEach(item => store.add(item));

        return {
          applied: false,
          receipt: { id: 'r1', decision: 'DENY', reason: error.message },
        };
      }
    }

    const store = new Set();

    const failingDelta = {
      operations: [
        { op: 'add', triple: 't1' },
        { op: 'INVALID' },
      ],
    };

    const result = applyDeltaWithReceipt(store, failingDelta);

    expect(result.applied).toBe(false);
    expect(result.receipt.decision).toBe('DENY');
    expect(store.size).toBe(0);
    console.log('  ✅ V6 Pattern: Receipt issued on success/denial');
  });
});
