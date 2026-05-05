/**
 * Tests for snapshot integration (optional KGC-4D)
 * @module agent-8/test/freeze
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createAtomicStore } from '../src/store.mjs';
import { applyCapsule } from '../src/apply.mjs';
import { snapshotsEnabled } from '../src/freeze.mjs';
import { dataFactory } from '@unrdf/oxigraph';

describe('Snapshot Integration', () => {
  it('checks snapshot enablement flag', () => {
    const store1 = createAtomicStore({ enableSnapshots: false });
    const store2 = createAtomicStore({ enableSnapshots: true });

    expect(snapshotsEnabled(store1)).toBe(false);
    expect(snapshotsEnabled(store2)).toBe(true);
  });

  it('creates store with snapshots disabled by default', () => {
    const store = createAtomicStore();
    expect(snapshotsEnabled(store)).toBe(false);
  });

  it('validates snapshot enablement', () => {
    const store = createAtomicStore({ enableSnapshots: false });

    expect(() => snapshotsEnabled(null)).toBeFalsy();
    expect(snapshotsEnabled(store)).toBe(false);
  });
});

describe('Snapshot Operations (Mocked)', () => {
  let store;

  beforeEach(() => {
    store = createAtomicStore({ enableSnapshots: true, nodeId: 'snap-test' });
  });

  it('store is configured for snapshots', () => {
    expect(store.snapshotsEnabled).toBe(true);
  });

  it('stores receipts independently of snapshots', async () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/test'),
            predicate: dataFactory.namedNode('http://ex.org/prop'),
            object: dataFactory.literal('value')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);

    expect(store.getReceipts()).toHaveLength(1);
    expect(store.size()).toBe(1);
  });

  // Note: Actual snapshot tests would require KGC-4D integration
  // These are structural tests only
});

describe('Snapshot Error Handling', () => {
  it('handles snapshots disabled gracefully', () => {
    const store = createAtomicStore({ enableSnapshots: false });

    // Attempting snapshot operations should be detectable
    expect(snapshotsEnabled(store)).toBe(false);
  });

  it('validates store has snapshot capability flag', () => {
    const enabledStore = createAtomicStore({ enableSnapshots: true });
    const disabledStore = createAtomicStore({ enableSnapshots: false });

    expect(enabledStore.snapshotsEnabled).toBe(true);
    expect(disabledStore.snapshotsEnabled).toBe(false);
  });
});

describe('Store State Management', () => {
  let store;

  beforeEach(() => {
    store = createAtomicStore({ nodeId: 'state-test' });
  });

  it('clears store state', () => {
    // Add data
    store.add(dataFactory.quad(
      dataFactory.namedNode('http://ex.org/s'),
      dataFactory.namedNode('http://ex.org/p'),
      dataFactory.literal('o')
    ));

    expect(store.size()).toBe(1);

    // Clear
    store.clear();

    expect(store.size()).toBe(0);
  });

  it('preserves receipts after clear', async () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/test'),
            predicate: dataFactory.namedNode('http://ex.org/prop'),
            object: dataFactory.literal('value')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);
    expect(store.getReceipts()).toHaveLength(1);

    // Clear store
    store.clear();

    // Receipts should still exist
    expect(store.getReceipts()).toHaveLength(1);
    expect(store.size()).toBe(0);
  });

  it('rebuilds state from capsule replay', async () => {
    const capsules = [
      {
        delta: {
          add: [
            {
              subject: dataFactory.namedNode('http://ex.org/s1'),
              predicate: dataFactory.namedNode('http://ex.org/p'),
              object: dataFactory.literal('1')
            }
          ]
        }
      },
      {
        delta: {
          add: [
            {
              subject: dataFactory.namedNode('http://ex.org/s2'),
              predicate: dataFactory.namedNode('http://ex.org/p'),
              object: dataFactory.literal('2')
            }
          ]
        }
      }
    ];

    // Apply all capsules
    for (const capsule of capsules) {
      await applyCapsule(store, capsule);
    }

    const originalSize = store.size();
    expect(originalSize).toBe(2);

    // Clear and replay
    store.clear();
    expect(store.size()).toBe(0);

    // Note: In real implementation, we'd replay from stored capsules
    // This demonstrates the pattern
    for (const capsule of capsules) {
      await applyCapsule(store, capsule);
    }

    expect(store.size()).toBe(originalSize);
  });
});

describe('Receipt Chain Integrity', () => {
  it('maintains receipt chain across operations', async () => {
    const store = createAtomicStore({ nodeId: 'chain-integrity' });

    const capsules = [
      {
        delta: {
          add: [
            {
              subject: dataFactory.namedNode('http://ex.org/s1'),
              predicate: dataFactory.namedNode('http://ex.org/p'),
              object: dataFactory.literal('1')
            }
          ]
        }
      },
      {
        delta: {
          add: [
            {
              subject: dataFactory.namedNode('http://ex.org/s2'),
              predicate: dataFactory.namedNode('http://ex.org/p'),
              object: dataFactory.literal('2')
            }
          ]
        }
      },
      {
        delta: {
          add: [
            {
              subject: dataFactory.namedNode('http://ex.org/s3'),
              predicate: dataFactory.namedNode('http://ex.org/p'),
              object: dataFactory.literal('3')
            }
          ]
        }
      }
    ];

    let parentHash = null;
    for (const capsule of capsules) {
      const receipt = await applyCapsule(store, capsule, { parentHash });
      expect(receipt.parentHash).toBe(parentHash);
      parentHash = receipt.hash;
    }

    // Verify chain in stored receipts
    const receipts = store.getReceipts();
    expect(receipts).toHaveLength(3);

    expect(receipts[0].parentHash).toBeNull();
    expect(receipts[1].parentHash).toBe(receipts[0].hash);
    expect(receipts[2].parentHash).toBe(receipts[1].hash);
  });
});
