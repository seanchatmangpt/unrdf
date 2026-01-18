/**
 * Capsule creation and serialization tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { createCapsule, validateCapsule } from '../src/capsule.mjs';

describe('Capsule Creation', () => {
  it('creates valid capsule with all required fields', async () => {
    const intent = {
      ops: [
        { type: 'set', subject: 'ex:s', predicate: 'ex:p', object: 'value' },
      ],
    };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    assert.ok(capsule.id);
    assert.strictEqual(capsule.version, 'capsule/v1');
    assert.strictEqual(capsule.receipt.hash, capsule.id);
    assert.strictEqual(capsule.intent.ops.length, 1);
    assert.deepStrictEqual(capsule.delta.add, []);
    assert.deepStrictEqual(capsule.delta.del, []);
  });

  it('creates capsule with parent hashes', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };
    const parents = ['abc123', 'def456'];

    const capsule = await createCapsule(intent, delta, guard, parents);

    assert.deepStrictEqual(capsule.receipt.parents, ['abc123', 'def456']);
  });

  it('validates capsule schema', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: ['test-profile'],
    };

    const capsule = await createCapsule(intent, delta, guard);
    const validated = validateCapsule(capsule);

    assert.deepStrictEqual(validated, capsule);
  });

  it('creates capsule with complex intent operations', async () => {
    const intent = {
      ops: [
        { type: 'create', subject: 'ex:s1', graph: 'ex:Class' },
        {
          type: 'link',
          subject: 'ex:s1',
          predicate: 'ex:relatedTo',
          target: 'ex:s2',
        },
        {
          type: 'set',
          subject: 'ex:s1',
          predicate: 'ex:name',
          object: 'Test',
        },
      ],
    };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    assert.strictEqual(capsule.intent.ops.length, 3);
    assert.strictEqual(capsule.intent.ops[0].type, 'create');
    assert.strictEqual(capsule.intent.ops[1].type, 'link');
    assert.strictEqual(capsule.intent.ops[2].type, 'set');
  });

  it('has high-precision timestamp', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    // ISO 8601 format from KGC-4D
    assert.match(
      capsule.receipt.timestamp,
      /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z$/
    );
    assert.ok(capsule.receipt.timestamp.length > 0);
  });
});
