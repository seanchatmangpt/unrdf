/**
 * Hash stability and determinism tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { createCapsule } from '../src/capsule.mjs';
import { hashCapsule } from '../src/hash.mjs';

describe('Deterministic Hashing', () => {
  it('produces identical hash for identical capsules with same timestamp', async () => {
    const intent = {
      ops: [{ type: 'create', subject: 'ex:s1', graph: 'ex:Class' }],
    };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    // Create first capsule
    const capsule1 = await createCapsule(intent, delta, guard);

    // Create second capsule with same timestamp
    const capsule2 = await createCapsule(intent, delta, guard);
    capsule2.receipt.timestamp = capsule1.receipt.timestamp;

    // Recompute hashes
    const hash1 = await hashCapsule(capsule1);
    const hash2 = await hashCapsule(capsule2);

    assert.strictEqual(hash1, hash2);
  });

  it('produces different hash for different timestamps', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule1 = await createCapsule(intent, delta, guard);
    // Small delay to ensure different timestamp
    await new Promise((resolve) => setTimeout(resolve, 2));
    const capsule2 = await createCapsule(intent, delta, guard);

    assert.notStrictEqual(capsule1.id, capsule2.id);
  });

  it('produces consistent hash regardless of operation order with canonicalization', async () => {
    const intent1 = {
      ops: [
        { type: 'create', subject: 'ex:s1', graph: 'ex:Class' },
        { type: 'create', subject: 'ex:s2', graph: 'ex:Class' },
      ],
    };

    const intent2 = {
      ops: [
        { type: 'create', subject: 'ex:s2', graph: 'ex:Class' },
        { type: 'create', subject: 'ex:s1', graph: 'ex:Class' },
      ],
    };

    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule1 = await createCapsule(intent1, delta, guard);
    const capsule2 = await createCapsule(intent2, delta, guard);

    // Use same timestamp
    capsule2.receipt.timestamp = capsule1.receipt.timestamp;

    const hash1 = await hashCapsule(capsule1);
    const hash2 = await hashCapsule(capsule2);

    assert.strictEqual(hash1, hash2);
  });

  it('canonicalizes parent order', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const parents1 = ['zzz', 'aaa', 'mmm'];
    const parents2 = ['aaa', 'mmm', 'zzz'];

    const capsule1 = await createCapsule(intent, delta, guard, parents1);
    const capsule2 = await createCapsule(intent, delta, guard, parents2);

    // Use same timestamp
    capsule2.receipt.timestamp = capsule1.receipt.timestamp;

    const hash1 = await hashCapsule(capsule1);
    const hash2 = await hashCapsule(capsule2);

    assert.strictEqual(hash1, hash2);
    assert.deepStrictEqual(capsule1.receipt.parents, ['aaa', 'mmm', 'zzz']);
  });

  it('produces BLAKE3 hash format', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    // BLAKE3 produces 64-character hex string
    assert.match(capsule.id, /^[a-f0-9]{64}$/);
  });
});
