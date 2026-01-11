/**
 * Tamper detection and parent chain verification tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { createCapsule } from '../src/capsule.mjs';
import { verifyCapsule, detectTampering } from '../src/verify.mjs';
import { hashWithParentChain } from '../src/hash.mjs';

describe('Capsule Verification', () => {
  it('verifies valid capsule', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);
    const result = await verifyCapsule(capsule);

    assert.strictEqual(result.ok, true);
    assert.strictEqual(result.reason, undefined);
  });

  it('detects tampered delta', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    // Tamper with delta
    capsule.delta.add.push({
      subject: 'http://example.org/evil',
      subjectType: 'NamedNode',
      predicate: 'http://example.org/p',
      object: { value: 'tampered', type: 'Literal' },
      graph: '',
    });

    const result = await verifyCapsule(capsule);

    assert.strictEqual(result.ok, false);
    assert.match(result.reason, /Hash mismatch/);
  });

  it('detects tampered intent', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    // Tamper with intent
    capsule.intent.ops.push({
      type: 'set',
      subject: 'ex:evil',
      predicate: 'ex:p',
      object: 'tampered',
    });

    const result = await verifyCapsule(capsule);

    assert.strictEqual(result.ok, false);
    assert.match(result.reason, /Hash mismatch/);
  });

  it('detects tampered guard limits', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);

    // Tamper with guard
    capsule.guard.limits.maxQuads = 1000;

    const result = await verifyCapsule(capsule);

    assert.strictEqual(result.ok, false);
    assert.match(result.reason, /Hash mismatch/);
  });

  it('verifies parent hashes match expected', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const parents = ['abc123', 'def456'];
    const capsule = await createCapsule(intent, delta, guard, parents);

    const result = await verifyCapsule(capsule, parents);

    assert.strictEqual(result.ok, true);
  });

  it('detects parent hash mismatch', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const parents = ['abc123', 'def456'];
    const capsule = await createCapsule(intent, delta, guard, parents);

    const result = await verifyCapsule(capsule, ['wrong123']);

    assert.strictEqual(result.ok, false);
    assert.match(result.reason, /Parent mismatch/);
  });

  it('detectTampering returns true for tampered capsule', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);
    capsule.delta.add.push({
      subject: 'ex:tampered',
      subjectType: 'NamedNode',
      predicate: 'ex:p',
      object: { value: 'evil', type: 'Literal' },
      graph: '',
    });

    const tampered = await detectTampering(capsule);

    assert.strictEqual(tampered, true);
  });

  it('detectTampering returns false for valid capsule', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    const capsule = await createCapsule(intent, delta, guard);
    const tampered = await detectTampering(capsule);

    assert.strictEqual(tampered, false);
  });

  it('verifies parent chain integrity', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    // Create parent capsule
    const parent = await createCapsule(intent, delta, guard);

    // Create child capsule
    const child = await createCapsule(intent, delta, guard, [parent.id]);

    // Verify with parent chain
    const parentMap = new Map([[parent.id, parent]]);
    const result = await hashWithParentChain(child, parentMap);

    assert.strictEqual(result.hash, child.id);
    assert.ok(result.parentChain.includes(parent.id));
  });

  it('detects invalid guard limits', async () => {
    const intent = { ops: [] };
    const delta = { add: [], del: [] };
    const guard = {
      limits: { maxQuads: -1, maxDepth: 5, timeout: 5000 },
      profiles: [],
    };

    // Create capsule bypassing validation
    const capsule = {
      id: 'test',
      version: 'capsule/v1',
      intent,
      delta,
      guard,
      receipt: {
        hash: 'test',
        parents: [],
        timestamp: '2024-01-01T00:00:00.000000000Z',
      },
    };

    const result = await verifyCapsule(capsule);

    assert.strictEqual(result.ok, false);
    // Schema validation catches it or guard limit check
    assert.ok(
      result.reason.includes('Schema validation failed') ||
        result.reason.includes('Guard limits must be positive integers')
    );
  });
});
