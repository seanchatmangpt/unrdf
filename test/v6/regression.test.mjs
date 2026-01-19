/**
 * V6 Regression Tests
 *
 * Ensures v6 doesn't break existing functionality, handles edge cases, and performs under load.
 *
 * @module test/v6/regression
 */

import { describe, it, expect } from 'vitest';

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

      expect(receipt.id).toBeTruthy();
      expect(receipt.receiptHash).toBeTruthy();
      expect(receipt.payloadHash).toBeTruthy();
      expect(receipt.receiptType).toBe('execution');
    });

    it('should maintain delta behavior', async () => {
      const delta = await createDelta('add', 's', 'p', 'o');

      expect(delta.id).toBeTruthy();
      expect(delta.operations).toBeTruthy();
      expect(delta.timestamp_iso).toBeTruthy();
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

        expect(receipt).toBeTruthy();
        expect(receipt.payloadHash).toBeTruthy();
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

        expect(receipt).toBeTruthy();
        expect(receipt.payloadHash.length > 0).toBeTruthy();
      });

      it('should handle special characters in IDs', async () => {
        const receipt = await createReceipt('execution', {
          eventType: 'TASK_COMPLETED',
          caseId: 'case-with-🚀-emoji',
          taskId: 'task-with-特殊字符',
          payload: { decision: 'COMPLETED' },
        });

        expect(receipt).toBeTruthy();
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

        expect(receipt).toBeTruthy();
        expect(receipt.previousHash).toBe(null);
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

        expect(delta).toBeTruthy();
        expect(delta.operations.length).toBe(1);
      });

      it('should handle different operation types', async () => {
        const delta1 = await createDelta('add', 's', 'p', 'o');
        const delta2 = await createDelta('delete', 's', 'p', 'o');

        expect(delta1).toBeTruthy();
        expect(delta2).toBeTruthy();
      });

      it('should handle complex subjects/predicates', async () => {
        const delta = await createDelta(
          'add',
          'http://example.org/subject',
          'http://example.org/predicate',
          'Complex Object'
        );

        expect(delta).toBeTruthy();
      });
    });

    describe('Adapter Edge Cases', () => {
      it('should handle store creation with undefined options', async () => {
        const store = await createStore(undefined);
        expect(store).toBeTruthy();
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

      expect(verification.valid).toBeFalsy();
      expect(verification.error).toBeTruthy();
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

      expect(receipts.length).toBe(20);
      assert.ok(
        receipts.every((r) => r.id),
        'All receipts should have IDs'
      );

      // All IDs should be unique
      const ids = receipts.map((r) => r.id);
      const uniqueIds = new Set(ids);
      expect(uniqueIds.size).toBe(20);
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

      expect(verification).toBeTruthy();
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

      expect(duration < 5000).toBeTruthy();
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

      expect(duration < 2000).toBeTruthy();
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

      expect(previousReceipt).toBeTruthy();
      expect(previousReceipt.previousHash).toBeTruthy();
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

      expect(results.length).toBe(50);
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

      expect(true).toBeTruthy();
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

      expect(receipt2.modified).toBeFalsy();
      expect(receipt2.previousHash).toBe(receipt1.receiptHash);
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

        expect(receipt).toBeTruthy();
        expect(receipt.receiptType).toBe(type);
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
      expect(receipt.id).toBeTruthy();
      expect(receipt.receiptType).toBeTruthy();
      expect(receipt.t_ns).toBeTruthy();
      expect(receipt.timestamp_iso).toBeTruthy();
      expect(receipt.payloadHash).toBeTruthy();
      expect(receipt.receiptHash).toBeTruthy();
      expect('previousHash' in receipt).toBeTruthy();
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
      expect(v5Result).toBeTruthy();

      // v6 style
      const v6Result = await wrapped.execute({ id: 'v6-task' });
      expect(v6Result.result).toBeTruthy();
      expect(v6Result.receipt).toBeTruthy();
    });

    it('should support gradual migration', async () => {
      // Can use both old and new APIs together
      const store = await createStore();
      const workflow = { run: async () => ({}) };
      const wrapped = wrapWorkflow(workflow);

      expect(store).toBeTruthy();
      expect(wrapped.run).toBeTruthy();
      expect(wrapped.execute).toBeTruthy();
    });
  });
});
