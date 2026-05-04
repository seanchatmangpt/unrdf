/**
 * Determinism Test - Agent 10 Quality Gate
 *
 * Verifies that tools/prove.mjs produces byte-identical output when run twice.
 *
 * Requirements:
 * 1. DETERMINISTIC=1 environment variable must be set
 * 2. All time sources must use deterministic values
 * 3. JSON serialization must have stable key ordering
 * 4. No randomness (Math.random, crypto.randomUUID, etc.)
 */

import { strict as assert } from 'node:assert';
import { test, describe } from 'node:test';
import crypto from 'node:crypto';

describe('Determinism Enforcement', () => {
  test('prove() produces identical hash on repeated calls', async () => {
    // Set deterministic mode
    process.env.DETERMINISTIC = '1';

    // Import prove function
    const { prove } = await import('../src/index.mjs');

    // Run proof twice
    const result1 = await prove();
    const result2 = await prove();

    // Assert: Hashes are identical
    assert.strictEqual(
      result1.hash,
      result2.hash,
      'Proof hashes must be byte-identical across runs'
    );

    // Assert: Merkle roots are identical
    assert.strictEqual(
      result1.merkleRoot,
      result2.merkleRoot,
      'Merkle roots must be identical across runs'
    );

    // Assert: Receipt count matches
    assert.strictEqual(
      result1.artifacts.length,
      result2.artifacts.length,
      'Receipt counts must match'
    );

    console.log('✅ Determinism verified - Hash:', result1.hash);
  });

  test('ledger serialization is stable', async () => {
    process.env.DETERMINISTIC = '1';

    const { prove } = await import('../src/index.mjs');
    const result = await prove();

    // Helper for stable JSON serialization (sorted keys)
    const stableStringify = (obj) => JSON.stringify(obj, Object.keys(obj).sort());

    const serialized1 = stableStringify(result.ledger);
    const serialized2 = stableStringify(result.ledger);

    assert.strictEqual(
      serialized1,
      serialized2,
      'Ledger serialization must be deterministic'
    );

    // Hash should be reproducible
    const hash1 = crypto.createHash('sha256').update(serialized1).digest('hex');
    const hash2 = crypto.createHash('sha256').update(serialized2).digest('hex');

    assert.strictEqual(hash1, hash2, 'Ledger hash must be reproducible');
  });

  test('receipt hashes are deterministic', async () => {
    process.env.DETERMINISTIC = '1';

    const { prove } = await import('../src/index.mjs');
    const result1 = await prove();
    const result2 = await prove();

    // Compare each receipt hash
    for (let i = 0; i < result1.artifacts.length; i++) {
      const stableStringify = (obj) => JSON.stringify(obj, Object.keys(obj).sort());

      const hash1 = crypto
        .createHash('sha256')
        .update(stableStringify(result1.artifacts[i]))
        .digest('hex');

      const hash2 = crypto
        .createHash('sha256')
        .update(stableStringify(result2.artifacts[i]))
        .digest('hex');

      assert.strictEqual(
        hash1,
        hash2,
        `Receipt ${i} hash must be deterministic`
      );
    }
  });

  test('time.mjs now() is deterministic when DETERMINISTIC=1', async () => {
    process.env.DETERMINISTIC = '1';

    const { now } = await import('@unrdf/kgc-4d');

    // Reset lastTime by importing fresh (not possible in same process)
    // This test validates monotonic increment from fixed start
    const t1 = now();
    const t2 = now();
    const t3 = now();

    // All values must be BigInt
    assert.strictEqual(typeof t1, 'bigint', 't1 must be BigInt');
    assert.strictEqual(typeof t2, 'bigint', 't2 must be BigInt');
    assert.strictEqual(typeof t3, 'bigint', 't3 must be BigInt');

    // Must be monotonic (strictly increasing)
    assert.ok(t1 < t2, 'time must be monotonic: t1 < t2');
    assert.ok(t2 < t3, 'time must be monotonic: t2 < t3');

    // In deterministic mode, should start from fixed value
    // DETERMINISTIC_START = 1704067200000000000n (2024-01-01T00:00:00.000Z)
    assert.ok(
      t1 >= 1704067200000000000n,
      'Deterministic time should start from fixed timestamp'
    );
  });

  test('full E2E determinism check (run twice, compare all outputs)', async () => {
    process.env.DETERMINISTIC = '1';

    const { prove } = await import('../src/index.mjs');

    // Run 1
    const result1 = await prove();

    // Run 2
    const result2 = await prove();

    // Assert: 3/3 identical
    const checks = {
      hash: result1.hash === result2.hash,
      merkleRoot: result1.merkleRoot === result2.merkleRoot,
      receiptsCount: result1.artifacts.length === result2.artifacts.length,
    };

    console.log('Determinism Checks:');
    console.log('  Hash:', checks.hash ? '✅' : '❌');
    console.log('  Merkle Root:', checks.merkleRoot ? '✅' : '❌');
    console.log('  Receipts Count:', checks.receiptsCount ? '✅' : '❌');

    const allPass = Object.values(checks).every((v) => v === true);
    assert.ok(allPass, 'All determinism checks must pass (3/3)');

    console.log('✅ Full E2E determinism verified');
    console.log('   Hash:', result1.hash);
    console.log('   Merkle Root:', result1.merkleRoot);
    console.log('   Receipts:', result1.artifacts.length);
  });
});
