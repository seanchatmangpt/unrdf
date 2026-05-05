/**
 * @file Agent 2: Capsule IR Test Suite
 * @description Comprehensive tests for capsule operations and hashing
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';

import {
  planCapsule,
  compileCapsuleToDeltas,
  verifyCapsule,
  applyCapsule,
  canonicalizeCapsule,
  hashCapsule,
  hashReceipt,
  CapsuleSchema,
} from './index.mjs';

describe('Agent 2: Capsule IR Primitive', () => {
  describe('Schema Validation', () => {
    test('Valid capsule passes schema validation', () => {
      const validCapsule = {
        intent: [
          { op: 'set', target: 'user:123', value: 'John Doe' },
          { op: 'link', target: 'user:123', value: 'org:456' },
        ],
        delta: [
          { op: 'add', subject: 'user:123', predicate: 'name', object: '"John Doe"' },
          { op: 'add', subject: 'user:123', predicate: 'memberOf', object: 'org:456' },
        ],
        guard: {
          limits: { maxDeltas: 1000, maxDepth: 10, timeout: 5000 },
          profiles: ['user-profile'],
          invariants: ['user:123 exists'],
        },
      };

      const result = CapsuleSchema.safeParse(validCapsule);
      assert.ok(result.success, 'Valid capsule should pass schema validation');
      assert.equal(result.data.intent.length, 2);
      assert.equal(result.data.delta.length, 2);
    });

    test('Invalid capsule fails schema validation', () => {
      const invalidCapsule = {
        intent: [], // Empty intent array (invalid - must have at least 1)
        delta: [],
        guard: {},
      };

      const result = CapsuleSchema.safeParse(invalidCapsule);
      assert.ok(!result.success, 'Invalid capsule should fail schema validation');
      assert.ok(result.error.message.includes('at least one intent'));
    });

    test('Invalid operations are rejected', () => {
      const invalidOp = {
        intent: [
          { op: 'invalid_op', target: 'user:123', value: 'test' }, // Invalid op
        ],
        delta: [],
        guard: {},
      };

      const result = CapsuleSchema.safeParse(invalidOp);
      assert.ok(!result.success, 'Invalid operation should be rejected');
    });
  });

  describe('Deterministic Canonicalization', () => {
    test('Same intent/delta produces identical canonical string (run twice)', () => {
      const capsule = {
        intent: [
          { op: 'set', target: 'user:123', value: 'Alice' },
          { op: 'create', target: 'user:456', value: 'Bob' },
        ],
        delta: [
          { op: 'add', subject: 'user:123', predicate: 'name', object: '"Alice"' },
          { op: 'add', subject: 'user:456', predicate: 'name', object: '"Bob"' },
        ],
        guard: {
          limits: { maxDeltas: 1000, maxDepth: 10, timeout: 5000 },
          profiles: ['test-profile'],
          invariants: [],
        },
      };

      const canonical1 = canonicalizeCapsule(capsule);
      const canonical2 = canonicalizeCapsule(capsule);

      assert.equal(canonical1, canonical2, 'Canonicalization must be deterministic');
      assert.ok(canonical1.length > 0, 'Canonical string must not be empty');
    });

    test('Different order inputs produce same canonical output', () => {
      const capsule1 = {
        intent: [
          { op: 'set', target: 'a', value: '1' },
          { op: 'create', target: 'b', value: '2' },
        ],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
      };

      const capsule2 = {
        intent: [
          { op: 'create', target: 'b', value: '2' },
          { op: 'set', target: 'a', value: '1' },
        ],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
      };

      const canonical1 = canonicalizeCapsule(capsule1);
      const canonical2 = canonicalizeCapsule(capsule2);

      assert.equal(canonical1, canonical2, 'Order should not affect canonical form');
    });

    test('Empty arrays handled correctly', () => {
      const capsule = {
        intent: [{ op: 'set', target: 'test', value: 'val' }],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
      };

      const canonical = canonicalizeCapsule(capsule);
      assert.ok(canonical.includes('[]'), 'Empty arrays should be preserved');
      assert.ok(canonical.length > 0);
    });
  });

  describe('Idempotence of Hashing', () => {
    test('hashCapsule called 100 times produces identical hash', () => {
      const capsule = {
        intent: [{ op: 'set', target: 'test:1', value: 'value1' }],
        delta: [{ op: 'add', subject: 'test:1', predicate: 'prop', object: '"value1"' }],
        guard: {
          limits: { maxDeltas: 1000, maxDepth: 10, timeout: 5000 },
          profiles: [],
          invariants: [],
        },
      };

      const hashes = [];
      for (let i = 0; i < 100; i++) {
        hashes.push(hashCapsule(capsule));
      }

      // All hashes must be identical
      const uniqueHashes = new Set(hashes);
      assert.equal(uniqueHashes.size, 1, 'All 100 hashes must be identical');

      const hash = hashes[0];
      assert.equal(hash.length, 64, 'Hash must be 64 characters (SHA-256 hex)');
      assert.ok(/^[a-f0-9]{64}$/.test(hash), 'Hash must be valid hex');
    });

    test('Hash stability across different capsule instances', () => {
      const createCapsule = () => ({
        intent: [{ op: 'set', target: 'x', value: '42' }],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
      });

      const hash1 = hashCapsule(createCapsule());
      const hash2 = hashCapsule(createCapsule());

      assert.equal(hash1, hash2, 'Identical content must produce identical hash');
    });
  });

  describe('Receipt Chain Verification', () => {
    test('Valid receipt chain passes verification', () => {
      const parentHash = 'a'.repeat(64); // Valid 64-char hex
      const capsule = {
        intent: [{ op: 'set', target: 'test', value: 'val' }],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
        receipt: {
          hash: 'b'.repeat(64),
          parents: [parentHash],
          timestamp: Date.now(),
        },
      };

      const result = verifyCapsule(capsule);
      assert.ok(result.valid, `Valid receipt should pass: ${result.errors.join('; ')}`);
      assert.equal(result.errors.length, 0);
    });

    test('Parent hash mismatch detected', () => {
      const invalidParentHash = 'not-a-valid-hash'; // Invalid format
      const capsule = {
        intent: [{ op: 'set', target: 'test', value: 'val' }],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
        receipt: {
          hash: 'c'.repeat(64),
          parents: [invalidParentHash],
          timestamp: Date.now(),
        },
      };

      const result = verifyCapsule(capsule);
      assert.ok(!result.valid, 'Invalid parent hash should be detected');
      assert.ok(result.errors.some((e) => e.includes('Invalid parent hash')));
    });

    test('Empty parent array allowed (genesis capsule)', () => {
      const capsule = {
        intent: [{ op: 'create', target: 'genesis', value: 'init' }],
        delta: [],
        guard: { limits: {}, profiles: [], invariants: [] },
        receipt: {
          hash: 'd'.repeat(64),
          parents: [],
          timestamp: Date.now(),
        },
      };

      const result = verifyCapsule(capsule);
      assert.ok(result.valid, 'Genesis capsule with no parents should be valid');
      assert.equal(result.errors.length, 0);
    });

    test('Receipt hash is deterministic with parents', () => {
      const parents = ['a'.repeat(64), 'b'.repeat(64)];
      const receiptData = { timestamp: 1234567890 };

      const hash1 = hashReceipt(receiptData, parents);
      const hash2 = hashReceipt(receiptData, parents);

      assert.equal(hash1, hash2, 'Receipt hash must be deterministic');
      assert.equal(hash1.length, 64);
      assert.ok(/^[a-f0-9]{64}$/.test(hash1));
    });
  });

  describe('Capsule Immutability', () => {
    test('planCapsule does NOT modify input intent', () => {
      const originalIntent = [
        { op: 'set', target: 'test:1', value: 'original' },
      ];
      const intentCopy = JSON.stringify(originalIntent);

      planCapsule(originalIntent);

      assert.equal(
        JSON.stringify(originalIntent),
        intentCopy,
        'Input intent must not be modified'
      );
    });

    test('compileCapsuleToDeltas does NOT mutate input capsule', () => {
      const capsule = planCapsule([{ op: 'set', target: 'test', value: 'val' }]);
      const capsuleCopy = JSON.stringify(capsule);

      const lensMap = new Map([
        [
          'set',
          (intent) => [
            { op: 'add', subject: intent.target, predicate: 'value', object: `"${intent.value}"` },
          ],
        ],
      ]);

      compileCapsuleToDeltas(capsule, lensMap);

      assert.equal(
        JSON.stringify(capsule),
        capsuleCopy,
        'Input capsule must not be mutated'
      );
    });

    test('applyCapsule does NOT mutate input capsule', () => {
      const capsule = {
        intent: [{ op: 'set', target: 'test', value: 'val' }],
        delta: [{ op: 'add', subject: 'test', predicate: 'value', object: '"val"' }],
        guard: { limits: { maxDeltas: 1000, maxDepth: 10, timeout: 5000 }, profiles: [], invariants: [] },
      };
      const capsuleCopy = JSON.stringify(capsule);

      applyCapsule(capsule);

      assert.equal(
        JSON.stringify(capsule),
        capsuleCopy,
        'applyCapsule must not mutate input'
      );
    });

    test('Deep freeze verification - nested objects not modified', () => {
      const intent = [
        { op: 'set', target: 'nested:1', value: 'test', profile: 'custom' },
      ];

      const capsule = planCapsule(intent);

      // Attempt to modify (should not affect original)
      try {
        capsule.intent[0].value = 'modified';
      } catch {
        // Strict mode may throw
      }

      // Original intent should be unchanged
      assert.equal(intent[0].value, 'test', 'Original intent must remain unchanged');
    });
  });

  describe('Core Operations', () => {
    test('planCapsule creates valid capsule structure', () => {
      const intent = [
        { op: 'create', target: 'user:789', value: 'Charlie' },
      ];

      const capsule = planCapsule(intent, 'user-profile');

      assert.equal(capsule.intent.length, 1);
      assert.equal(capsule.delta.length, 0);
      assert.ok(capsule.guard);
      assert.ok(capsule.guard.profiles.includes('user-profile'));
      assert.equal(capsule.guard.limits.maxDeltas, 1000);
    });

    test('compileCapsuleToDeltas translates intent to deltas', () => {
      const capsule = planCapsule([
        { op: 'set', target: 'entity:1', value: 'name1' },
        { op: 'set', target: 'entity:2', value: 'name2' },
      ]);

      const lensMap = new Map([
        [
          'set',
          (intent) => [
            { op: 'add', subject: intent.target, predicate: 'name', object: `"${intent.value}"` },
          ],
        ],
      ]);

      const compiled = compileCapsuleToDeltas(capsule, lensMap);

      assert.equal(compiled.delta.length, 2, 'Should have 2 deltas');
      assert.equal(compiled.delta[0].subject, 'entity:1');
      assert.equal(compiled.delta[1].subject, 'entity:2');
    });

    test('applyCapsule generates receipt with correct structure', () => {
      const capsule = {
        intent: [{ op: 'set', target: 'test', value: 'val' }],
        delta: [{ op: 'add', subject: 'test', predicate: 'value', object: '"val"' }],
        guard: { limits: { maxDeltas: 1000, maxDepth: 10, timeout: 5000 }, profiles: [], invariants: [] },
      };

      const parents = ['e'.repeat(64)];
      const result = applyCapsule(capsule, parents);

      assert.ok(result.receipt, 'Result should have receipt');
      assert.ok(result.hash, 'Result should have content hash');
      assert.ok(result.appliedAt, 'Result should have appliedAt timestamp');
      assert.equal(result.receipt.parents.length, 1);
      assert.equal(result.receipt.parents[0], parents[0]);
      assert.ok(/^[a-f0-9]{64}$/.test(result.receipt.hash));
    });

    test('verifyCapsule detects delta count exceeding limit', () => {
      const capsule = {
        intent: [{ op: 'set', target: 'test', value: 'val' }],
        delta: new Array(1001).fill({ op: 'add', subject: 's', predicate: 'p', object: 'o' }),
        guard: { limits: { maxDeltas: 1000, maxDepth: 10, timeout: 5000 }, profiles: [], invariants: [] },
      };

      const result = verifyCapsule(capsule);
      assert.ok(!result.valid, 'Should detect delta count exceeding limit');
      assert.ok(result.errors.some((e) => e.includes('exceeds limit')));
    });
  });
});
