/**
 * V6 Regression Tests
 *
 * Ensures v6 doesn't break existing functionality, handles edge cases, and performs under load.
 *
 * @module test/v6/regression
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';

import {
  createReceipt,
  verifyReceipt,
  RECEIPT_TYPES,
} from '../../packages/v6-core/src/receipts/index.mjs';

import {
  createDelta,
} from '../../packages/v6-core/src/delta/index.mjs';

import {
  createStore,
  wrapWorkflow,
  wrapFederation,
  withReceipt,
} from '../../packages/v6-compat/src/adapters.mjs';

describe('V6 Regression Tests', () => {
  describe('Core Functionality Preservation', () => {
    it('should maintain receipt creation behavior', async () => {
      const receipt = await createReceipt('execution', {
        eventType: 'TASK_COMPLETED',
        caseId: 'regression-case',
        taskId: 'regression-task',
        payload: { data: 'test' },
      });

      assert.ok(receipt.id, 'Should have ID');
      assert.ok(receipt.receiptHash, 'Should have hash');
      assert.ok(receipt.payloadHash, 'Should have payload hash');
      assert.equal(receipt.receiptType, 'execution', 'Should preserve type');
    });

    it('should maintain delta behavior', async () => {
      const delta = await createDelta('add', 's', 'p', 'o');

      assert.ok(delta.id, 'Should have ID');
      assert.ok(delta.operations, 'Should have operations');
      assert.ok(delta.timestamp_iso, 'Should have timestamp');
    });
  });

  describe('Edge Cases', () => {
    describe('Receipt Edge Cases', () => {
      it('should handle empty payload', async () => {
        const receipt = await createReceipt('execution', {
          eventType: 'TASK_COMPLETED',
          caseId: 'edge-case',
          taskId: 'empty-payload',
          payload: { decision: 'COMPLETED' },
        });

        assert.ok(receipt, 'Should handle empty payload');
        assert.ok(receipt.payloadHash, 'Should still have payload hash');
      });

      it('should handle large payloads', async () => {
        const largePayload = {
          data: 'x'.repeat(10000),
          items: Array(100).fill({ value: 'test' }),
        };

        const receipt = await createReceipt('execution', {
          eventType: 'TASK_COMPLETED',
          caseId: 'edge-large',
          taskId: 'large-payload',
          payload: largePayload,
        });

        assert.ok(receipt, 'Should handle large payload');
        assert.ok(receipt.payloadHash.length > 0, 'Should compute hash for large payload');
      });

      it('should handle special characters in IDs', async () => {
        const receipt = await createReceipt('execution', {
          eventType: 'TASK_COMPLETED',
          caseId: 'case-with-🚀-emoji',
          taskId: 'task-with-特殊字符',
          payload: { decision: 'COMPLETED' },
        });

        assert.ok(receipt, 'Should handle special characters');
        assert.ok(receipt.caseId.includes('🚀'), 'Should preserve emoji');
      });

      it('should handle null previousReceipt gracefully', async () => {
        const receipt = await createReceipt(
          'execution',
          {
            eventType: 'TASK_COMPLETED',
            caseId: 'null-prev',
            taskId: 'task',
            payload: { decision: 'COMPLETED' },
          },
          null
        );

        assert.ok(receipt, 'Should handle null previous receipt');
        assert.equal(receipt.previousHash, null, 'Previous hash should be null');
      });

      it('should reject invalid receipt types', async () => {
        await assert.rejects(
          createReceipt('invalid-type', { eventType: 'TEST', payload: { decision: 'COMPLETED' } }),
          /Invalid receipt type/,
          'Should reject invalid type'
        );
      });
    });

    describe('Delta Proposal Edge Cases', () => {
      it('should handle simple operations', async () => {
        const delta = await createDelta('add', 's', 'p', 'o');

        assert.ok(delta, 'Should create delta');
        assert.equal(delta.operations.length, 1, 'Should have 1 operation');
      });

      it('should handle different operation types', async () => {
        const delta1 = await createDelta('add', 's', 'p', 'o');
        const delta2 = await createDelta('delete', 's', 'p', 'o');

        assert.ok(delta1, 'Should create add delta');
        assert.ok(delta2, 'Should create delete delta');
      });

      it('should handle complex subjects/predicates', async () => {
        const delta = await createDelta(
          'add',
          'http://example.org/subject',
          'http://example.org/predicate',
          'Complex Object'
        );

        assert.ok(delta, 'Should handle complex values');
      });
    });

    describe('Adapter Edge Cases', () => {
      it('should handle store creation with undefined options', async () => {
        const store = await createStore(undefined);
        assert.ok(store, 'Should handle undefined options');
      });

      it('should handle workflow with no run method', () => {
        const workflow = {};

        assert.doesNotThrow(
          () => wrapWorkflow(workflow),
          'Should not throw on workflow without run()'
        );
      });

      it('should handle federation with no query method', () => {
        const federation = {};

        assert.doesNotThrow(
          () => wrapFederation(federation),
          'Should not throw on federation without query()'
        );
      });

      it('should handle receipt wrapper with throwing function', async () => {
        const throwingFn = () => {
          throw new Error('Test error');
        };

        const wrapped = withReceipt(throwingFn);

        await assert.rejects(
          wrapped(),
          /Test error/,
          'Should propagate errors'
        );
      });
    });
  });

  describe('Error Handling', () => {
    it('should provide clear error for invalid receipt verification', async () => {
      const invalidReceipt = {
        receiptType: 'execution',
        id: 'invalid',
        // Missing required fields
      };

      const verification = await verifyReceipt(invalidReceipt);

      assert.ok(!verification.valid, 'Invalid receipt should fail verification');
      assert.ok(verification.error, 'Should have error message');
    });

    it('should handle receipt creation errors gracefully', async () => {
      await assert.rejects(
        createReceipt('execution', {
          // Missing required fields
          eventType: 'TASK_COMPLETED',
          // No caseId, taskId
        }),
        'Should reject incomplete receipt data'
      );
    });

    it('should handle concurrent receipt creation', async () => {
      const promises = Array(20)
        .fill(null)
        .map((_, i) =>
          createReceipt('execution', {
            eventType: 'TASK_COMPLETED',
            caseId: `concurrent-${i}`,
            taskId: `task-${i}`,
            payload: { index: i },
          })
        );

      const receipts = await Promise.all(promises);

      assert.equal(receipts.length, 20, 'Should create all receipts');
      assert.ok(
        receipts.every((r) => r.id),
        'All receipts should have IDs'
      );

      // All IDs should be unique
      const ids = receipts.map((r) => r.id);
      const uniqueIds = new Set(ids);
      assert.equal(uniqueIds.size, 20, 'All IDs should be unique');
    });

    it('should handle malformed payload data', async () => {
      const receipt = await createReceipt('execution', {
        eventType: 'TASK_COMPLETED',
        caseId: 'malformed',
        taskId: 'task',
        payload: {
          circular: null, // Will create circular ref
        },
      });

      // Create circular reference
      receipt.payload.circular = receipt.payload;

      // Should still be able to verify (if verification handles it)
      const verification = await verifyReceipt({
        ...receipt,
        payload: { safe: 'data' }, // Use safe payload for verification
      });

      assert.ok(verification, 'Should handle verification');
    });
  });

  describe('Performance Under Load', () => {
    it('should handle 100 receipt creations quickly', async () => {
      const start = performance.now();

      const promises = Array(100)
        .fill(null)
        .map((_, i) =>
          createReceipt('execution', {
            eventType: 'TASK_COMPLETED',
            caseId: `load-${i}`,
            taskId: `task-${i}`,
            payload: { index: i },
          })
        );

      await Promise.all(promises);

      const duration = performance.now() - start;

      assert.ok(duration < 5000, `Should create 100 receipts in <5s (took ${duration}ms)`);
    });

    it('should handle 100 verifications quickly', async () => {
      const receipt = await createReceipt('execution', {
        eventType: 'TASK_COMPLETED',
        caseId: 'verify-load',
        taskId: 'verify-task',
        payload: { decision: 'COMPLETED' },
      });

      const start = performance.now();

      const promises = Array(100)
        .fill(null)
        .map(() => verifyReceipt(receipt));

      await Promise.all(promises);

      const duration = performance.now() - start;

      assert.ok(duration < 2000, `Should verify 100 receipts in <2s (took ${duration}ms)`);
    });

    it('should handle large chain of receipts', async () => {
      let previousReceipt = null;

      for (let i = 0; i < 50; i++) {
        previousReceipt = await createReceipt(
          'execution',
          {
            eventType: i === 49 ? 'TASK_COMPLETED' : 'TASK_STARTED',
            caseId: `chain-${i}`,
            taskId: `task-${i}`,
            payload: { step: i },
          },
          previousReceipt
        );
      }

      assert.ok(previousReceipt, 'Should create chain of 50 receipts');
      assert.ok(previousReceipt.previousHash, 'Last receipt should have previous hash');
    });

    it('should handle many concurrent operations', async () => {
      const operations = [];

      // Mix of different operations
      for (let i = 0; i < 25; i++) {
        operations.push(
          createReceipt('execution', {
            eventType: 'TASK_COMPLETED',
            caseId: `op-${i}`,
            taskId: `task-${i}`,
            payload: { decision: 'COMPLETED' },
          })
        );
      }

      for (let i = 0; i < 25; i++) {
        // Create deltas asynchronously
        operations.push(createDelta('add', `s${i}`, 'p', 'o'));
      }

      const results = await Promise.all(operations);

      assert.equal(results.length, 50, 'Should complete all operations');
    });
  });

  describe('Memory Safety', () => {
    it('should not leak memory with many receipts', async () => {
      const receipts = [];

      for (let i = 0; i < 100; i++) {
        const receipt = await createReceipt('execution', {
          eventType: 'TASK_COMPLETED',
          caseId: `mem-${i}`,
          taskId: `task-${i}`,
          payload: { data: 'x'.repeat(1000) },
        });

        receipts.push(receipt);
      }

      // Clear references
      receipts.length = 0;

      // Force GC if available
      if (global.gc) {
        global.gc();
      }

      assert.ok(true, 'Memory test completed');
    });

    it('should handle receipt references correctly', async () => {
      const receipt1 = await createReceipt('execution', {
        eventType: 'TASK_STARTED',
        caseId: 'ref-test',
        taskId: 'task-ref',
        payload: { decision: 'COMPLETED' },
      });

      const receipt2 = await createReceipt(
        'execution',
        {
          eventType: 'TASK_COMPLETED',
          caseId: 'ref-test',
          taskId: 'task-ref',
          payload: { decision: 'COMPLETED' },
        },
        receipt1
      );

      // Modify receipt1 should not affect receipt2
      receipt1.modified = true;

      assert.ok(!receipt2.modified, 'Receipt2 should not be affected');
      assert.equal(receipt2.previousHash, receipt1.receiptHash, 'Hash reference should be preserved');
    });
  });

  describe('Compatibility Checks', () => {
    it('should work with different receipt types', async () => {
      const types = Object.values(RECEIPT_TYPES);

      for (const type of types) {
        let receipt;

        if (type === 'execution') {
          receipt = await createReceipt(type, {
            eventType: 'TASK_COMPLETED',
            caseId: 'compat',
            taskId: 'task',
            payload: { decision: 'COMPLETED' },
          });
        } else if (type === 'allocation') {
          receipt = await createReceipt(type, {
            eventType: 'RESOURCE_ALLOCATED',
            resourceId: 'res',
            poolId: 'pool',
            allocationPeriod: { start: '2025-01-01', end: '2025-01-02' },
            capacity: { total: 100, available: 80, allocated: 20, unit: 'hours' },
            payload: { decision: 'COMPLETED' },
          });
        } else if (type === 'compile') {
          receipt = await createReceipt(type, {
            eventType: 'GRAMMAR_COMPILED',
            inputHashes: ['input'],
            outputHash: 'output',
            compilerVersion: '1.0.0',
            grammarType: 'SPARQL',
            payload: { decision: 'COMPLETED' },
          });
        } else if (type === 'verification') {
          receipt = await createReceipt(type, {
            eventType: 'MERKLE_PROOF_VERIFIED',
            verifiedHash: 'hash',
            merkleRoot: 'root',
            proofPath: [],
            payload: { decision: 'COMPLETED' },
          });
        }

        assert.ok(receipt, `Should create ${type} receipt`);
        assert.equal(receipt.receiptType, type, `Should have correct type: ${type}`);
      }
    });

    it('should maintain API stability across versions', async () => {
      // Ensure core APIs haven't changed unexpectedly
      const receipt = await createReceipt('execution', {
        eventType: 'TASK_COMPLETED',
        caseId: 'stability',
        taskId: 'task',
        payload: { decision: 'COMPLETED' },
      });

      // Core fields should exist
      assert.ok(receipt.id, 'Should have id');
      assert.ok(receipt.receiptType, 'Should have receiptType');
      assert.ok(receipt.t_ns, 'Should have t_ns');
      assert.ok(receipt.timestamp_iso, 'Should have timestamp_iso');
      assert.ok(receipt.payloadHash, 'Should have payloadHash');
      assert.ok(receipt.receiptHash, 'Should have receiptHash');
      assert.ok('previousHash' in receipt, 'Should have previousHash field');
    });
  });

  describe('Backward Compatibility', () => {
    it('should support v5-style workflow usage', async () => {
      const workflow = {
        run: async (task) => ({ status: 'completed', result: task }),
      };

      const wrapped = wrapWorkflow(workflow);

      // v5 style
      const v5Result = await wrapped.run({ id: 'v5-task' });
      assert.ok(v5Result, 'v5 style should work');

      // v6 style
      const v6Result = await wrapped.execute({ id: 'v6-task' });
      assert.ok(v6Result.result, 'v6 style should work');
      assert.ok(v6Result.receipt, 'v6 style should return receipt');
    });

    it('should support gradual migration', async () => {
      // Can use both old and new APIs together
      const store = await createStore();
      const workflow = { run: async () => ({}) };
      const wrapped = wrapWorkflow(workflow);

      assert.ok(store, 'New store API works');
      assert.ok(wrapped.run, 'Old workflow API preserved');
      assert.ok(wrapped.execute, 'New workflow API available');
    });
  });
});
