/**
 * End-to-End Workflow Tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { runE2E } from '../src/e2e.mjs';

describe('End-to-End Workflow', () => {
  it('should complete workflow with all validations', async () => {
    const result = await runE2E();

    assert.equal(result.success, true,
      `E2E failed:\n${result.evidence.join('\n')}`);

    assert.equal(result.summary.totalSteps, 12,
      'Should complete all 12 steps');

    assert.ok(result.summary.allHashesComputed >= 8,
      'Should compute at least 8 hashes');

    assert.equal(result.summary.deterministic, true,
      'Should be deterministic');
  });

  it('should validate receipt chain integrity', async () => {
    const result = await runE2E();

    assert.equal(result.results.receiptChain, 'VALID',
      'Receipt chain should be valid');
  });

  it('should correctly identify non-commutative operations', async () => {
    const result = await runE2E();

    assert.ok(result.results.commutativityCheck.includes('PASS'),
      'Should correctly identify non-commutativity');
  });

  it('should compute all required hashes', async () => {
    const result = await runE2E();

    // Check that all expected hashes are present
    const expectedHashes = [
      'profileHash',
      'lensHash',
      'capsule1Hash',
      'capsule2Hash',
      'receipt1Hash',
      'receipt2Hash',
      'impact1Hash',
      'impact2Hash',
      'facadeHash'
    ];

    for (const hashKey of expectedHashes) {
      assert.ok(result.results[hashKey], `Should have ${hashKey}`);
      assert.equal(typeof result.results[hashKey], 'string', `${hashKey} should be a string`);
      assert.equal(result.results[hashKey].length, 64, `${hashKey} should be 64 characters (SHA256)`);
    }
  });

  it('should provide evidence for all steps', async () => {
    const result = await runE2E();

    assert.ok(Array.isArray(result.evidence), 'Should have evidence array');
    assert.equal(result.evidence.length, 12, 'Should have evidence for all 12 steps');

    // Each evidence entry should be a string
    for (const ev of result.evidence) {
      assert.equal(typeof ev, 'string', 'Evidence should be strings');
    }
  });

  it('should pass determinism audit', async () => {
    const result = await runE2E();

    assert.equal(result.results.determinismAudit, 'PASS',
      'Determinism audit should pass');
  });

  it('should handle mock mode when agents are unavailable', async () => {
    const result = await runE2E();

    // Should succeed even if agents 2-9 are not available
    assert.equal(result.success, true, 'Should succeed in mock or full mode');

    // Check if running in mock mode
    if (result.summary.mode) {
      assert.ok(result.summary.mode.includes('MOCK'), 'Should indicate mock mode');
    }
  });
});
