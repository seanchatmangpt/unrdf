/**
 * Tests for atomic capsule application
 * @module agent-8/test/apply
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createAtomicStore } from '../src/store.mjs';
import { applyCapsule, applyBatch, validateCapsule } from '../src/apply.mjs';
import { dataFactory } from '@unrdf/oxigraph';

describe('Atomic Apply', () => {
  let store;

  beforeEach(() => {
    store = createAtomicStore({ nodeId: 'test-node' });
  });

  it('applies simple add-only capsule', async () => {
    // Define capsule
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/alice'),
            predicate: dataFactory.namedNode('http://ex.org/name'),
            object: dataFactory.literal('Alice')
          }
        ],
        del: []
      }
    };

    // Apply
    const receipt = await applyCapsule(store, capsule);

    // Verify
    expect(receipt.success).toBe(true);
    expect(receipt.hash).toBeTruthy();
    expect(receipt.hash).toHaveLength(64); // BLAKE3 hex
    expect(receipt.stats.added).toBe(1);
    expect(receipt.stats.deleted).toBe(0);
    expect(store.size()).toBe(1);
  });

  it('applies delete operations', async () => {
    // Setup: add quad first
    const quad = dataFactory.quad(
      dataFactory.namedNode('http://ex.org/bob'),
      dataFactory.namedNode('http://ex.org/age'),
      dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')),
      dataFactory.namedNode('urn:autonomic:universe')
    );
    store.add(quad);
    expect(store.size()).toBe(1);

    // Define delete capsule
    const capsule = {
      delta: {
        add: [],
        del: [
          {
            subject: dataFactory.namedNode('http://ex.org/bob'),
            predicate: dataFactory.namedNode('http://ex.org/age'),
            object: dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
          }
        ]
      }
    };

    // Apply
    const receipt = await applyCapsule(store, capsule);

    // Verify deletion
    expect(receipt.success).toBe(true);
    expect(receipt.stats.deleted).toBe(1);
    expect(receipt.stats.added).toBe(0);
    expect(store.size()).toBe(0);
  });

  it('rolls back on error (atomicity)', async () => {
    // Start with valid quad
    const validQuad = dataFactory.quad(
      dataFactory.namedNode('http://ex.org/valid'),
      dataFactory.namedNode('http://ex.org/pred'),
      dataFactory.literal('valid')
    );
    store.add(validQuad);
    expect(store.size()).toBe(1);

    // Invalid capsule (missing required fields)
    const badCapsule = {
      delta: {
        add: [
          {
            // Missing predicate - will fail validation
            subject: dataFactory.namedNode('http://ex.org/bad'),
            object: dataFactory.literal('invalid')
          }
        ],
        del: []
      }
    };

    // Expect failure
    await expect(applyCapsule(store, badCapsule)).rejects.toThrow();

    // Verify no partial state - original quad still there
    expect(store.size()).toBe(1);
  });

  it('generates hash chain with parent', async () => {
    const capsule1 = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/s1'),
            predicate: dataFactory.namedNode('http://ex.org/p1'),
            object: dataFactory.literal('o1')
          }
        ],
        del: []
      }
    };

    const capsule2 = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/s2'),
            predicate: dataFactory.namedNode('http://ex.org/p2'),
            object: dataFactory.literal('o2')
          }
        ],
        del: []
      }
    };

    const receipt1 = await applyCapsule(store, capsule1);
    const receipt2 = await applyCapsule(store, capsule2, {
      parentHash: receipt1.hash
    });

    expect(receipt2.parentHash).toBe(receipt1.hash);
    expect(receipt1.parentHash).toBeNull();
    expect(store.size()).toBe(2);
  });

  it('handles mixed add and delete operations', async () => {
    // Setup initial state
    const quad1 = dataFactory.quad(
      dataFactory.namedNode('http://ex.org/item1'),
      dataFactory.namedNode('http://ex.org/status'),
      dataFactory.literal('old')
    );
    store.add(quad1);

    // Mixed capsule
    const capsule = {
      delta: {
        del: [
          {
            subject: dataFactory.namedNode('http://ex.org/item1'),
            predicate: dataFactory.namedNode('http://ex.org/status'),
            object: dataFactory.literal('old')
          }
        ],
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/item1'),
            predicate: dataFactory.namedNode('http://ex.org/status'),
            object: dataFactory.literal('new')
          },
          {
            subject: dataFactory.namedNode('http://ex.org/item2'),
            predicate: dataFactory.namedNode('http://ex.org/type'),
            object: dataFactory.literal('item')
          }
        ]
      }
    };

    const receipt = await applyCapsule(store, capsule);

    expect(receipt.success).toBe(true);
    expect(receipt.stats.deleted).toBe(1);
    expect(receipt.stats.added).toBe(2);
    expect(store.size()).toBe(2);
  });

  it('validates capsule structure', () => {
    const validCapsule = {
      delta: {
        add: [],
        del: []
      }
    };

    const validated = validateCapsule(validCapsule);
    expect(validated).toBeTruthy();
    expect(validated.delta).toBeDefined();
  });

  it('rejects invalid capsule', () => {
    const invalidCapsule = {
      // Missing delta field
      metadata: {}
    };

    expect(() => validateCapsule(invalidCapsule)).toThrow(TypeError);
  });

  it('stores receipts in chain', async () => {
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

    const receipts = store.getReceipts();
    expect(receipts).toHaveLength(1);
    expect(receipts[0].hash).toBeTruthy();

    const lastReceipt = store.getLastReceipt();
    expect(lastReceipt).toEqual(receipts[0]);
  });
});

describe('Batch Apply', () => {
  let store;

  beforeEach(() => {
    store = createAtomicStore({ nodeId: 'batch-test' });
  });

  it('applies multiple capsules sequentially', async () => {
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

    const receipts = await applyBatch(store, capsules);

    // Verify receipts
    expect(receipts).toHaveLength(3);
    expect(receipts[0].parentHash).toBeNull();
    expect(receipts[1].parentHash).toBe(receipts[0].hash);
    expect(receipts[2].parentHash).toBe(receipts[1].hash);

    // Verify final state
    expect(store.size()).toBe(3);
  });

  it('stops on first error', async () => {
    const capsules = [
      {
        delta: {
          add: [
            {
              subject: dataFactory.namedNode('http://ex.org/good'),
              predicate: dataFactory.namedNode('http://ex.org/p'),
              object: dataFactory.literal('valid')
            }
          ]
        }
      },
      {
        delta: {
          add: [
            {
              // Invalid - missing predicate
              subject: dataFactory.namedNode('http://ex.org/bad'),
              object: dataFactory.literal('invalid')
            }
          ]
        }
      }
    ];

    await expect(applyBatch(store, capsules)).rejects.toThrow();

    // First capsule should be applied, second should fail
    expect(store.size()).toBe(1);
    expect(store.getReceipts()).toHaveLength(1);
  });
});
