/**
 * @file Enhanced bounds tests - Soft limits, quotas, rate limiting
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import { EnhancedBoundsChecker } from '../src/enhanced-bounds.mjs';

// =============================================================================
// Soft Limits Tests
// =============================================================================

describe('Enhanced Bounds - Soft Limits', () => {
  /** @type {EnhancedBoundsChecker} */
  let checker;

  beforeEach(() => {
    checker = new EnhancedBoundsChecker({
      max_files_touched: 100,
      max_bytes_changed: 10000,
      max_tool_ops: 1000,
      warnings: {
        filesThreshold: 0.8,
        bytesThreshold: 0.9,
        opsThreshold: 0.75,
      },
    });
  });

  it('should emit warning receipt at 80% threshold', async () => {
    // Use 80 files (80% of 100)
    const receipt = await checker.checkAndRecord({
      type: 'file_touch',
      files: 80,
    });

    assert.equal(receipt.admit, true);
    assert.equal(receipt.warnings.length, 1);
    assert.equal(receipt.warnings[0].type, 'warning');
    assert.match(receipt.warnings[0].message, /80%/);
  });

  it('should emit error receipt at 100% threshold', async () => {
    // Use 100 files (100% of 100)
    const receipt = await checker.checkAndRecord({
      type: 'file_touch',
      files: 100,
    });

    assert.equal(receipt.admit, true); // First time should admit
    assert.equal(receipt.warnings.length, 0);
    assert.equal(receipt.errors.length, 0);

    // Next operation should fail
    const receipt2 = await checker.checkAndRecord({
      type: 'file_touch',
      files: 1,
    });

    assert.equal(receipt2.admit, false);
    assert.equal(receipt2.errors.length, 1);
    assert.equal(receipt2.errors[0].type, 'error');
  });

  it('should emit warning for bytes at 90% threshold', async () => {
    // Use 9000 bytes (90% of 10000)
    const receipt = await checker.checkAndRecord({
      type: 'bytes_change',
      bytes: 9000,
    });

    assert.equal(receipt.admit, true);
    assert.equal(receipt.warnings.length, 1);
    assert.equal(receipt.warnings[0].metric, 'bytes');
    assert.equal(receipt.warnings[0].percentage, 90);
  });

  it('should emit warning for ops at 75% threshold', async () => {
    // Use 750 ops (75% of 1000)
    const receipt = await checker.checkAndRecord({
      type: 'tool_op',
      ops: 750,
    });

    assert.equal(receipt.admit, true);
    assert.equal(receipt.warnings.length, 1);
    assert.equal(receipt.warnings[0].metric, 'ops');
    assert.match(receipt.warnings[0].message, /75%/);
  });
});

// =============================================================================
// Per-Agent Quota Tests
// =============================================================================

describe('Enhanced Bounds - Per-Agent Quotas', () => {
  /** @type {EnhancedBoundsChecker} */
  let checker;

  beforeEach(() => {
    checker = new EnhancedBoundsChecker({
      max_files_touched: 1000,
      max_bytes_changed: 100000,
    });
  });

  it('should enforce per-agent file quota', async () => {
    checker.setAgentQuota('agent:backend-dev', {
      max_files: 10,
      max_bytes: 5000,
    });

    // Agent can use up to 10 files
    const receipt1 = await checker.checkAndRecord(
      {
        type: 'file_touch',
        files: 5,
      },
      'agent:backend-dev'
    );
    assert.equal(receipt1.admit, true);

    const receipt2 = await checker.checkAndRecord(
      {
        type: 'file_touch',
        files: 4,
      },
      'agent:backend-dev'
    );
    assert.equal(receipt2.admit, true);

    // Should reject when quota exceeded
    const receipt3 = await checker.checkAndRecord(
      {
        type: 'file_touch',
        files: 2,
      },
      'agent:backend-dev'
    );
    assert.equal(receipt3.admit, false);
    assert.equal(receipt3.errors.length, 1);
    assert.match(receipt3.errors[0].message, /quota/i);
  });

  it('should enforce per-agent byte quota', async () => {
    checker.setAgentQuota('agent:tester', {
      max_bytes: 1000,
    });

    const receipt1 = await checker.checkAndRecord(
      {
        type: 'bytes_change',
        bytes: 800,
      },
      'agent:tester'
    );
    assert.equal(receipt1.admit, true);

    const receipt2 = await checker.checkAndRecord(
      {
        type: 'bytes_change',
        bytes: 300,
      },
      'agent:tester'
    );
    assert.equal(receipt2.admit, false);
  });

  it('should track separate quotas for different agents', async () => {
    checker.setAgentQuota('agent:A', { max_files: 5 });
    checker.setAgentQuota('agent:B', { max_files: 10 });

    // Agent A uses 5 files
    await checker.checkAndRecord({ type: 'file_touch', files: 5 }, 'agent:A');

    // Agent B can still use 10 files
    const receipt = await checker.checkAndRecord(
      { type: 'file_touch', files: 8 },
      'agent:B'
    );
    assert.equal(receipt.admit, true);

    // Agent A cannot use more
    const receipt2 = await checker.checkAndRecord(
      { type: 'file_touch', files: 1 },
      'agent:A'
    );
    assert.equal(receipt2.admit, false);
  });
});

// =============================================================================
// Rate Limiting Tests
// =============================================================================

describe('Enhanced Bounds - Rate Limiting', () => {
  /** @type {EnhancedBoundsChecker} */
  let checker;

  beforeEach(() => {
    checker = new EnhancedBoundsChecker({
      max_files_touched: 1000,
    });
  });

  it('should enforce rate limit per agent', async () => {
    checker.setRateLimit({
      maxOpsPerSecond: 5,
      windowSizeMs: 1000,
    });

    // First 5 operations should succeed
    for (let i = 0; i < 5; i++) {
      const receipt = await checker.checkAndRecord(
        { type: 'op', files: 1 },
        'agent:worker'
      );
      assert.equal(receipt.admit, true);
    }

    // 6th operation should be rate limited
    const receipt = await checker.checkAndRecord(
      { type: 'op', files: 1 },
      'agent:worker'
    );
    assert.equal(receipt.admit, false);
    assert.ok(receipt.errors.some((e) => e.metric === 'rate_limit'));
  });

  it('should allow operations after window expires', async () => {
    checker.setRateLimit({
      maxOpsPerSecond: 3,
      windowSizeMs: 100, // Short window for testing
    });

    // Fill the window
    for (let i = 0; i < 3; i++) {
      await checker.checkAndRecord({ type: 'op', files: 1 }, 'agent:test');
    }

    // Should be rate limited
    const receipt1 = await checker.checkAndRecord(
      { type: 'op', files: 1 },
      'agent:test'
    );
    assert.equal(receipt1.admit, false);

    // Wait for window to expire
    await new Promise((resolve) => setTimeout(resolve, 150));

    // Should now succeed
    const receipt2 = await checker.checkAndRecord(
      { type: 'op', files: 1 },
      'agent:test'
    );
    assert.equal(receipt2.admit, true);
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('Enhanced Bounds - Integration', () => {
  /** @type {EnhancedBoundsChecker} */
  let checker;

  beforeEach(() => {
    checker = new EnhancedBoundsChecker({
      max_files_touched: 100,
      max_bytes_changed: 10000,
      max_tool_ops: 1000,
      warnings: {
        filesThreshold: 0.8,
        bytesThreshold: 0.9,
      },
    });

    checker.setAgentQuota('agent:backend-dev', {
      max_files: 50,
      max_bytes: 5000,
    });

    checker.setRateLimit({
      maxOpsPerSecond: 10,
      windowSizeMs: 1000,
    });
  });

  it('should enforce all limits simultaneously', async () => {
    // This should pass (within all limits)
    const receipt1 = await checker.checkAndRecord(
      {
        type: 'composite',
        files: 40,
        bytes: 4000,
        ops: 1,
      },
      'agent:backend-dev'
    );
    assert.equal(receipt1.admit, true);
    assert.equal(receipt1.warnings.length, 0);

    // This should trigger soft limit warning (80% of 50 files = 40 files already used)
    const receipt2 = await checker.checkAndRecord(
      {
        type: 'composite',
        files: 5,
        bytes: 500,
        ops: 1,
      },
      'agent:backend-dev'
    );
    assert.equal(receipt2.admit, true);
    // May have warnings for global or agent limits

    // This should fail agent quota
    const receipt3 = await checker.checkAndRecord(
      {
        type: 'composite',
        files: 10,
        bytes: 100,
        ops: 1,
      },
      'agent:backend-dev'
    );
    assert.equal(receipt3.admit, false);
  });

  it('should provide detailed receipt with all checks', async () => {
    const receipt = await checker.checkAndRecord(
      {
        type: 'file_write',
        files: 1,
        bytes: 100,
        ops: 1,
      },
      'agent:backend-dev'
    );

    assert.ok(receipt.receipt_id);
    assert.ok(receipt.timestamp);
    assert.equal(receipt.agentId, 'agent:backend-dev');
    assert.equal(receipt.operation, 'file_write');
    assert.ok(Array.isArray(receipt.warnings));
    assert.ok(Array.isArray(receipt.errors));
    assert.equal(typeof receipt.admit, 'boolean');
  });

  it('should track usage correctly', async () => {
    await checker.checkAndRecord(
      {
        type: 'op1',
        files: 10,
        bytes: 1000,
        ops: 5,
      },
      'agent:backend-dev'
    );

    const globalUsage = checker.getUsage();
    assert.equal(globalUsage.files_touched, 10);
    assert.equal(globalUsage.bytes_changed, 1000);
    assert.equal(globalUsage.tool_ops, 5);

    const agentUsage = checker.getAgentUsage('agent:backend-dev');
    assert.equal(agentUsage.files_touched, 10);
    assert.equal(agentUsage.bytes_changed, 1000);
    assert.equal(agentUsage.tool_ops, 5);
  });

  it('should filter receipts by type', async () => {
    // Create warning
    await checker.checkAndRecord({ type: 'op', files: 80 });

    // Create error
    checker.setAgentQuota('agent:test', { max_files: 1 });
    await checker.checkAndRecord({ type: 'op', files: 10 }, 'agent:test');

    const warnings = checker.getReceiptsByType('warning');
    const errors = checker.getReceiptsByType('error');

    assert.ok(warnings.length > 0);
    assert.ok(errors.length > 0);
  });
});

// =============================================================================
// Usage Tracking Tests
// =============================================================================

describe('Enhanced Bounds - Usage Tracking', () => {
  it('should track global usage across all agents', async () => {
    const checker = new EnhancedBoundsChecker({
      max_files_touched: 1000,
      max_bytes_changed: 100000,
    });

    await checker.checkAndRecord({ type: 'op', files: 10 }, 'agent:A');
    await checker.checkAndRecord({ type: 'op', files: 20 }, 'agent:B');
    await checker.checkAndRecord({ type: 'op', bytes: 1000 }, 'agent:A');

    const usage = checker.getUsage();
    assert.equal(usage.files_touched, 30);
    assert.equal(usage.bytes_changed, 1000);
  });

  it('should track per-agent usage separately', async () => {
    const checker = new EnhancedBoundsChecker({
      max_files_touched: 1000,
    });

    await checker.checkAndRecord({ type: 'op', files: 10 }, 'agent:A');
    await checker.checkAndRecord({ type: 'op', files: 20 }, 'agent:B');

    const usageA = checker.getAgentUsage('agent:A');
    const usageB = checker.getAgentUsage('agent:B');

    assert.equal(usageA.files_touched, 10);
    assert.equal(usageB.files_touched, 20);
  });

  it('should reset usage correctly', async () => {
    const checker = new EnhancedBoundsChecker({
      max_files_touched: 100,
    });

    await checker.checkAndRecord({ type: 'op', files: 50 }, 'agent:test');

    checker.reset();

    const usage = checker.getUsage();
    assert.equal(usage.files_touched, 0);
    assert.equal(checker.getReceipts().length, 0);
  });
});

// =============================================================================
// Custom Warning Thresholds Tests
// =============================================================================

describe('Enhanced Bounds - Custom Warning Thresholds', () => {
  it('should use custom warning thresholds', async () => {
    const checker = new EnhancedBoundsChecker({
      max_files_touched: 100,
      warnings: {
        filesThreshold: 0.5, // Warn at 50%
      },
    });

    // At 50%, should warn
    const receipt1 = await checker.checkAndRecord({
      type: 'op',
      files: 50,
    });
    assert.equal(receipt1.warnings.length, 1);
    assert.equal(receipt1.warnings[0].percentage, 50);

    // Below 50%, should not warn
    checker.reset();
    const receipt2 = await checker.checkAndRecord({
      type: 'op',
      files: 40,
    });
    assert.equal(receipt2.warnings.length, 0);
  });

  it('should support different thresholds for different metrics', async () => {
    const checker = new EnhancedBoundsChecker({
      max_files_touched: 100,
      max_bytes_changed: 10000,
      warnings: {
        filesThreshold: 0.5,
        bytesThreshold: 0.9,
      },
    });

    const receipt = await checker.checkAndRecord({
      type: 'op',
      files: 50, // 50% - should warn
      bytes: 8000, // 80% - should not warn
    });

    assert.equal(receipt.warnings.length, 1);
    assert.equal(receipt.warnings[0].metric, 'files');
  });
});
