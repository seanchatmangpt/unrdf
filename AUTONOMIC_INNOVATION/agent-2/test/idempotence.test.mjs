/**
 * Idempotence guarantee tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { createStore } from '../../../packages/oxigraph/src/index.mjs';
import { planCapsule, planCapsuleIdempotent } from '../src/planner.mjs';

describe('Idempotence Guarantees', () => {
  it('planCapsule produces same delta when called twice on same store', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'set',
          subject: 'http://example.org/s',
          predicate: 'http://example.org/p',
          object: 'value',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule1 = await planCapsule(intent, store, profile);

    // Plan again with ORIGINAL store (not modified)
    const capsule2 = await planCapsule(intent, store, profile);

    // Deltas should be identical (different timestamps, but same operations)
    assert.deepStrictEqual(capsule1.delta.add, capsule2.delta.add);
    assert.deepStrictEqual(capsule1.delta.del, capsule2.delta.del);
  });

  it('planCapsuleIdempotent is idempotent by design', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'create',
          subject: 'http://example.org/s1',
          graph: 'http://example.org/Class',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule1 = await planCapsuleIdempotent(intent, store, profile);
    const capsule2 = await planCapsuleIdempotent(intent, store, profile);

    // Deltas should be identical
    assert.deepStrictEqual(capsule1.delta.add, capsule2.delta.add);
    assert.deepStrictEqual(capsule1.delta.del, capsule2.delta.del);
  });

  it('empty intent produces empty delta', async () => {
    const store = createStore();
    const intent = { ops: [] };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await planCapsule(intent, store, profile);

    assert.strictEqual(capsule.delta.add.length, 0);
    assert.strictEqual(capsule.delta.del.length, 0);
  });

  it('repeated set operations produce consistent result', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'set',
          subject: 'http://example.org/s',
          predicate: 'http://example.org/p',
          object: 'value1',
        },
        {
          type: 'set',
          subject: 'http://example.org/s',
          predicate: 'http://example.org/p',
          object: 'value1',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await planCapsule(intent, store, profile);

    // Second set replaces first, so we get del + add
    // Final state has one value
    assert.ok(capsule.delta.add.length >= 1);
    const lastAdd = capsule.delta.add[capsule.delta.add.length - 1];
    assert.strictEqual(lastAdd.object.value, 'value1');
  });

  it('link/unlink operations are idempotent', async () => {
    const store = createStore();

    // Add initial link
    const intent1 = {
      ops: [
        {
          type: 'link',
          subject: 'http://example.org/s1',
          predicate: 'http://example.org/relatedTo',
          target: 'http://example.org/s2',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule1 = await planCapsule(intent1, store, profile);

    // Try adding same link again
    const capsule2 = await planCapsule(intent1, store, profile);

    // Should produce same delta
    assert.deepStrictEqual(capsule1.delta.add, capsule2.delta.add);
  });

  it('handles idempotent create operations', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'create',
          subject: 'http://example.org/s1',
          graph: 'http://example.org/Class',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule1 = await planCapsule(intent, store, profile);
    const capsule2 = await planCapsule(intent, store, profile);

    // Should produce same delta (both create same triple)
    assert.deepStrictEqual(capsule1.delta.add, capsule2.delta.add);
  });
});
