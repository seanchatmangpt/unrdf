/**
 * Determinism Audit Tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { validateDeterminism } from '../src/determinism-audit.mjs';

describe('Determinism Audit', () => {
  it('should produce identical hashes across 2 runs for deterministic function', async () => {
    const mockDemo = async ({ seed }) => {
      // Simple deterministic operation
      return {
        result: seed * 2,
        computed: seed + 100,
        str: `seed-${seed}`
      };
    };

    const result = await validateDeterminism(mockDemo, 2);

    assert.equal(result.deterministic, true,
      `Determinism failed:\n${JSON.stringify(result.mismatches, null, 2)}`);

    assert.equal(result.mismatches.length, 0, 'Should have no mismatches');
    assert.ok(result.evidence.length > 0, 'Should have evidence');
    assert.equal(result.runs, 2, 'Should have run 2 times');
    assert.equal(result.seed, 12345, 'Should use seed 12345');
    assert.ok(Object.keys(result.hashes).length > 0, 'Should have hashes');
  });

  it('should detect non-deterministic operations', async () => {
    const nonDeterministicDemo = async ({ seed }) => {
      // Use Math.random to create non-determinism
      return {
        result: Math.random(),
        seed
      };
    };

    const result = await validateDeterminism(nonDeterministicDemo, 2);

    assert.equal(result.deterministic, false,
      'Should detect non-determinism');

    assert.ok(result.mismatches.length > 0, 'Should have mismatches');
  });

  it('should validate input with Zod schema', async () => {
    const mockDemo = async ({ seed }) => ({ seed });

    // Should accept valid inputs
    await assert.doesNotReject(
      async () => await validateDeterminism(mockDemo, 2),
      'Should accept valid inputs'
    );

    // Should reject invalid runs count
    await assert.rejects(
      async () => await validateDeterminism(mockDemo, 1),
      /runs/i,
      'Should reject runs < 2'
    );

    await assert.rejects(
      async () => await validateDeterminism(mockDemo, 1.5),
      'Should reject non-integer runs'
    );
  });

  it('should handle errors in demo function', async () => {
    const errorDemo = async ({ seed }) => {
      throw new Error('Test error');
    };

    const result = await validateDeterminism(errorDemo, 2);

    // Should still complete and compare
    assert.ok(result.hashes, 'Should have hashes even with errors');
    assert.equal(result.runs, 2, 'Should have run 2 times');
  });

  it('should produce identical hashes for 3+ runs', async () => {
    const mockDemo = async ({ seed }) => {
      return {
        value: seed * 3,
        array: [1, 2, 3, seed]
      };
    };

    const result = await validateDeterminism(mockDemo, 3);

    assert.equal(result.deterministic, true, 'Should be deterministic across 3 runs');
    assert.equal(result.mismatches.length, 0, 'Should have no mismatches');
    assert.equal(result.runs, 3, 'Should have run 3 times');
  });
});
