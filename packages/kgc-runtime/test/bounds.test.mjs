/**
 * @file Bounds checker tests - Capacity limits enforcement
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import { BoundsChecker, enforceBounds } from '../src/bounds.mjs';

describe('BoundsChecker', () => {
  /** @type {BoundsChecker} */
  let checker;

  beforeEach(() => {
    checker = new BoundsChecker({
      max_files_touched: 10,
      max_bytes_changed: 1000,
      max_tool_ops: 50,
      max_runtime_ms: 5000,
      max_graph_rewrites: 25,
    });
  });

  describe('Individual Bounds Enforcement', () => {
    it('should enforce max_files_touched limit', () => {
      for (let i = 0; i < 10; i++) {
        const result = checker.canExecute({ type: 'file_touch', files: 1 });
        assert.equal(result, true, `File ${i + 1} should be allowed`);
        checker.recordOperation({ type: 'file_touch', files: 1 });
      }

      const result = checker.canExecute({ type: 'file_touch', files: 1 });
      assert.equal(result, false, 'Should reject file touch when limit reached');
    });

    it('should enforce max_bytes_changed limit', () => {
      const result1 = checker.canExecute({ type: 'bytes_change', bytes: 500 });
      assert.equal(result1, true);
      checker.recordOperation({ type: 'bytes_change', bytes: 500 });

      const result2 = checker.canExecute({ type: 'bytes_change', bytes: 400 });
      assert.equal(result2, true);
      checker.recordOperation({ type: 'bytes_change', bytes: 400 });

      const result3 = checker.canExecute({ type: 'bytes_change', bytes: 200 });
      assert.equal(result3, false, 'Should reject when bytes limit exceeded');
    });

    it('should enforce max_tool_ops limit', () => {
      for (let i = 0; i < 50; i++) {
        const result = checker.canExecute({ type: 'tool_op', ops: 1 });
        assert.equal(result, true, `Op ${i + 1} should be allowed`);
        checker.recordOperation({ type: 'tool_op', ops: 1 });
      }

      const result = checker.canExecute({ type: 'tool_op', ops: 1 });
      assert.equal(result, false, 'Should reject tool op when limit reached');
    });

    it('should enforce max_runtime_ms limit', () => {
      const result1 = checker.canExecute({ type: 'runtime', ms: 3000 });
      assert.equal(result1, true);
      checker.recordOperation({ type: 'runtime', ms: 3000 });

      const result2 = checker.canExecute({ type: 'runtime', ms: 1500 });
      assert.equal(result2, true);
      checker.recordOperation({ type: 'runtime', ms: 1500 });

      const result3 = checker.canExecute({ type: 'runtime', ms: 1000 });
      assert.equal(result3, false, 'Should reject when runtime limit exceeded');
    });

    it('should enforce max_graph_rewrites limit', () => {
      for (let i = 0; i < 25; i++) {
        const result = checker.canExecute({ type: 'graph_rewrite', rewrites: 1 });
        assert.equal(result, true, `Rewrite ${i + 1} should be allowed`);
        checker.recordOperation({ type: 'graph_rewrite', rewrites: 1 });
      }

      const result = checker.canExecute({ type: 'graph_rewrite', rewrites: 1 });
      assert.equal(result, false, 'Should reject rewrite when limit reached');
    });
  });

  describe('Cumulative Tracking', () => {
    it('should track cumulative file touches', () => {
      checker.recordOperation({ type: 'file_touch', files: 3 });
      assert.equal(checker.getUsage().files_touched, 3);

      checker.recordOperation({ type: 'file_touch', files: 5 });
      assert.equal(checker.getUsage().files_touched, 8);

      checker.recordOperation({ type: 'file_touch', files: 2 });
      assert.equal(checker.getUsage().files_touched, 10);
    });

    it('should track cumulative bytes changed', () => {
      checker.recordOperation({ type: 'bytes_change', bytes: 250 });
      assert.equal(checker.getUsage().bytes_changed, 250);

      checker.recordOperation({ type: 'bytes_change', bytes: 500 });
      assert.equal(checker.getUsage().bytes_changed, 750);

      checker.recordOperation({ type: 'bytes_change', bytes: 100 });
      assert.equal(checker.getUsage().bytes_changed, 850);
    });

    it('should track all metrics simultaneously', () => {
      checker.recordOperation({ type: 'file_touch', files: 2 });
      checker.recordOperation({ type: 'bytes_change', bytes: 300 });
      checker.recordOperation({ type: 'tool_op', ops: 10 });
      checker.recordOperation({ type: 'runtime', ms: 1000 });
      checker.recordOperation({ type: 'graph_rewrite', rewrites: 5 });

      const usage = checker.getUsage();
      assert.equal(usage.files_touched, 2);
      assert.equal(usage.bytes_changed, 300);
      assert.equal(usage.tool_ops, 10);
      assert.equal(usage.runtime_ms, 1000);
      assert.equal(usage.graph_rewrites, 5);
    });

    it('should respect all bounds when checking composite operations', () => {
      checker.recordOperation({ type: 'file_touch', files: 9 });
      checker.recordOperation({ type: 'bytes_change', bytes: 900 });

      // Should allow operation within both bounds
      const result1 = checker.canExecute({
        type: 'composite',
        files: 1,
        bytes: 50
      });
      assert.equal(result1, true);

      // Should reject if any bound would be exceeded
      const result2 = checker.canExecute({
        type: 'composite',
        files: 2,
        bytes: 50
      });
      assert.equal(result2, false, 'Should reject when files bound exceeded');
    });
  });

  describe('Stress Tests with Max Values', () => {
    it('should handle max_files=100 stress test', () => {
      const stressChecker = new BoundsChecker({ max_files_touched: 100 });

      for (let i = 0; i < 100; i++) {
        assert.equal(stressChecker.canExecute({ type: 'file_touch', files: 1 }), true);
        stressChecker.recordOperation({ type: 'file_touch', files: 1 });
      }

      assert.equal(stressChecker.getUsage().files_touched, 100);
      assert.equal(stressChecker.canExecute({ type: 'file_touch', files: 1 }), false);
    });

    it('should handle max_bytes=10MB stress test', () => {
      const stressChecker = new BoundsChecker({ max_bytes_changed: 10485760 }); // 10MB

      // Add 1MB chunks
      for (let i = 0; i < 10; i++) {
        assert.equal(stressChecker.canExecute({ type: 'bytes_change', bytes: 1048576 }), true);
        stressChecker.recordOperation({ type: 'bytes_change', bytes: 1048576 });
      }

      assert.equal(stressChecker.getUsage().bytes_changed, 10485760);
      assert.equal(stressChecker.canExecute({ type: 'bytes_change', bytes: 1 }), false);
    });

    it('should handle max_ops=1000 stress test', () => {
      const stressChecker = new BoundsChecker({ max_tool_ops: 1000 });

      for (let i = 0; i < 1000; i++) {
        assert.equal(stressChecker.canExecute({ type: 'tool_op', ops: 1 }), true);
        stressChecker.recordOperation({ type: 'tool_op', ops: 1 });
      }

      assert.equal(stressChecker.getUsage().tool_ops, 1000);
      assert.equal(stressChecker.canExecute({ type: 'tool_op', ops: 1 }), false);
    });

    it('should handle max_runtime=300000ms stress test', () => {
      const stressChecker = new BoundsChecker({ max_runtime_ms: 300000 }); // 5 minutes

      // Add 30 second chunks
      for (let i = 0; i < 10; i++) {
        assert.equal(stressChecker.canExecute({ type: 'runtime', ms: 30000 }), true);
        stressChecker.recordOperation({ type: 'runtime', ms: 30000 });
      }

      assert.equal(stressChecker.getUsage().runtime_ms, 300000);
      assert.equal(stressChecker.canExecute({ type: 'runtime', ms: 1 }), false);
    });

    it('should handle max_rewrites=500 stress test', () => {
      const stressChecker = new BoundsChecker({ max_graph_rewrites: 500 });

      for (let i = 0; i < 500; i++) {
        assert.equal(stressChecker.canExecute({ type: 'graph_rewrite', rewrites: 1 }), true);
        stressChecker.recordOperation({ type: 'graph_rewrite', rewrites: 1 });
      }

      assert.equal(stressChecker.getUsage().graph_rewrites, 500);
      assert.equal(stressChecker.canExecute({ type: 'graph_rewrite', rewrites: 1 }), false);
    });
  });
});

describe('enforceBounds', () => {
  const bounds = {
    max_files_touched: 5,
    max_bytes_changed: 500,
    max_tool_ops: 20,
    max_runtime_ms: 2000,
    max_graph_rewrites: 10,
  };

  describe('Receipt Generation', () => {
    it('should return admission receipt when within bounds', () => {
      const operation = { type: 'file_touch', files: 2 };
      const receipt = enforceBounds(operation, bounds);

      assert.equal(receipt.admit, true);
      assert.ok(receipt.bound_used, 'Should specify bound used');
      assert.ok(receipt.receipt_id, 'Should have receipt ID');
      assert.ok(receipt.timestamp, 'Should have timestamp');
    });

    it('should return rejection receipt when exceeding bounds', () => {
      const operation = { type: 'file_touch', files: 10 };
      const receipt = enforceBounds(operation, bounds);

      assert.equal(receipt.admit, false);
      assert.ok(receipt.reason, 'Should provide rejection reason');
      assert.equal(receipt.bound_violated, 'files');
      assert.ok(receipt.receipt_id, 'Should have receipt ID');
    });

    it('should generate unique receipt IDs', () => {
      const receipt1 = enforceBounds({ type: 'file_touch', files: 1 }, bounds);
      const receipt2 = enforceBounds({ type: 'file_touch', files: 1 }, bounds);

      assert.notEqual(receipt1.receipt_id, receipt2.receipt_id);
    });

    it('should include parent_receipt_id when provided', () => {
      const parentId = '123e4567-e89b-12d3-a456-426614174000';
      const receipt = enforceBounds(
        { type: 'file_touch', files: 1 },
        bounds,
        { parent_receipt_id: parentId }
      );

      assert.equal(receipt.parent_receipt_id, parentId);
    });

    it('should create chainable receipts', () => {
      const receipt1 = enforceBounds({ type: 'file_touch', files: 1 }, bounds);
      const receipt2 = enforceBounds(
        { type: 'bytes_change', bytes: 100 },
        bounds,
        { parent_receipt_id: receipt1.receipt_id }
      );
      const receipt3 = enforceBounds(
        { type: 'tool_op', ops: 5 },
        bounds,
        { parent_receipt_id: receipt2.receipt_id }
      );

      assert.equal(receipt2.parent_receipt_id, receipt1.receipt_id);
      assert.equal(receipt3.parent_receipt_id, receipt2.receipt_id);
    });
  });

  describe('Denial Receipts', () => {
    it('should generate receipt when files limit exceeded', () => {
      const receipt = enforceBounds({ type: 'file_touch', files: 100 }, bounds);

      assert.equal(receipt.admit, false);
      assert.equal(receipt.bound_violated, 'files');
      assert.match(receipt.reason, /exceeded max_files/i);
    });

    it('should generate receipt when bytes limit exceeded', () => {
      const receipt = enforceBounds({ type: 'bytes_change', bytes: 1000 }, bounds);

      assert.equal(receipt.admit, false);
      assert.equal(receipt.bound_violated, 'bytes');
      assert.match(receipt.reason, /exceeded max_bytes/i);
    });

    it('should generate receipt when ops limit exceeded', () => {
      const receipt = enforceBounds({ type: 'tool_op', ops: 50 }, bounds);

      assert.equal(receipt.admit, false);
      assert.equal(receipt.bound_violated, 'ops');
      assert.match(receipt.reason, /exceeded max_tool_ops/i);
    });

    it('should generate receipt when runtime limit exceeded', () => {
      const receipt = enforceBounds({ type: 'runtime', ms: 5000 }, bounds);

      assert.equal(receipt.admit, false);
      assert.equal(receipt.bound_violated, 'runtime');
      assert.match(receipt.reason, /exceeded max_runtime/i);
    });

    it('should generate receipt when rewrites limit exceeded', () => {
      const receipt = enforceBounds({ type: 'graph_rewrite', rewrites: 20 }, bounds);

      assert.equal(receipt.admit, false);
      assert.equal(receipt.bound_violated, 'rewrites');
      assert.match(receipt.reason, /exceeded max_graph_rewrites/i);
    });
  });

  describe('Stress Test Receipts', () => {
    it('should handle high-volume receipt generation', () => {
      const stressBounds = {
        max_files_touched: 1000,
        max_bytes_changed: 10485760,
        max_tool_ops: 1000,
        max_runtime_ms: 300000,
        max_graph_rewrites: 500,
      };

      const receipts = [];
      for (let i = 0; i < 100; i++) {
        const receipt = enforceBounds({ type: 'file_touch', files: 1 }, stressBounds);
        receipts.push(receipt);
      }

      // All should be admitted
      assert.equal(receipts.filter(r => r.admit).length, 100);

      // All should have unique IDs
      const ids = new Set(receipts.map(r => r.receipt_id));
      assert.equal(ids.size, 100);
    });
  });
});
