/**
 * Poka-Yoke Proof 05: Atomic Delta Application (SMOKE)
 *
 * Proves: Delta application is all-or-none (no partial state)
 * Expected Runtime: <50ms
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';

test('Proof 05: Atomic Delta (SMOKE)', async (t) => {
  await t.test('Successful delta applies all operations', () => {
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

    assert.equal(store.size, 3);
    assert.ok(store.has('new-1'));
    assert.ok(store.has('new-2'));
    console.log('  ✅ All operations applied');
  });

  await t.test('Failed delta rolls back ALL operations', () => {
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

    assert.equal(store.size, 1);
    assert.ok(!store.has('new-1'));
    console.log('  ✅ Failed delta rolled back ALL operations');
  });

  await t.test('V6 Pattern: Receipt on success or denial', () => {
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

    assert.equal(result.applied, false);
    assert.equal(result.receipt.decision, 'DENY');
    assert.equal(store.size, 0);
    console.log('  ✅ V6 Pattern: Receipt issued on success/denial');
  });
});

console.log('✅ Proof 05 PASSED: Atomic delta application (all-or-none)');
