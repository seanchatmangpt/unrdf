/**
 * Intent compilation tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { createStore } from '../../../packages/oxigraph/src/index.mjs';
import { planCapsule } from '../src/planner.mjs';

describe('Intent → Delta Compilation', () => {
  it('compiles set intent into delta', async () => {
    const store = createStore();
    const intent = {
      ops: [
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

    assert.strictEqual(capsule.delta.add.length, 1);
    assert.strictEqual(capsule.delta.del.length, 0);
    assert.strictEqual(capsule.delta.add[0].subject, 'http://example.org/s');
    assert.strictEqual(capsule.delta.add[0].predicate, 'http://example.org/p');
    assert.strictEqual(capsule.delta.add[0].object.value, 'value1');
  });

  it('compiles create intent into delta', async () => {
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

    const capsule = await planCapsule(intent, store, profile);

    assert.strictEqual(capsule.delta.add.length, 1);
    assert.strictEqual(capsule.delta.add[0].subject, 'http://example.org/s1');
    assert.strictEqual(
      capsule.delta.add[0].predicate,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
    );
    assert.strictEqual(
      capsule.delta.add[0].object.value,
      'http://example.org/Class'
    );
  });

  it('compiles link intent into delta', async () => {
    const store = createStore();
    const intent = {
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

    const capsule = await planCapsule(intent, store, profile);

    assert.strictEqual(capsule.delta.add.length, 1);
    assert.strictEqual(capsule.delta.add[0].subject, 'http://example.org/s1');
    assert.strictEqual(
      capsule.delta.add[0].predicate,
      'http://example.org/relatedTo'
    );
    assert.strictEqual(capsule.delta.add[0].object.value, 'http://example.org/s2');
    assert.strictEqual(capsule.delta.add[0].object.type, 'NamedNode');
  });

  it('compiles unlink intent into delta', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'unlink',
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

    const capsule = await planCapsule(intent, store, profile);

    assert.strictEqual(capsule.delta.del.length, 1);
    assert.strictEqual(capsule.delta.add.length, 0);
    assert.strictEqual(capsule.delta.del[0].subject, 'http://example.org/s1');
  });

  it('enforces guard maxQuads limit', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'set',
          subject: 'http://example.org/s1',
          predicate: 'http://example.org/p1',
          object: 'v1',
        },
        {
          type: 'set',
          subject: 'http://example.org/s2',
          predicate: 'http://example.org/p2',
          object: 'v2',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 1, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    await assert.rejects(
      () => planCapsule(intent, store, profile),
      /Delta exceeds maxQuads: 2 > 1/
    );
  });

  it('handles multiple operations in sequence', async () => {
    const store = createStore();
    const intent = {
      ops: [
        {
          type: 'create',
          subject: 'http://example.org/s1',
          graph: 'http://example.org/Class',
        },
        {
          type: 'link',
          subject: 'http://example.org/s1',
          predicate: 'http://example.org/relatedTo',
          target: 'http://example.org/s2',
        },
        {
          type: 'set',
          subject: 'http://example.org/s1',
          predicate: 'http://example.org/name',
          object: 'Test',
        },
      ],
    };
    const profile = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await planCapsule(intent, store, profile);

    assert.strictEqual(capsule.delta.add.length, 3);
    assert.strictEqual(capsule.delta.del.length, 0);
  });
});
