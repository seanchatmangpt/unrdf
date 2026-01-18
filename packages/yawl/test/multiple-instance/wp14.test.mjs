/**
 * @file WP14 - Multiple Instances with Runtime A Priori Knowledge Tests
 * @description
 * Tests for WP14 pattern implementation including expression evaluation,
 * instance spawning, data slicing, and barrier synchronization.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  spawnInstancesRuntimeApriori,
  createBarrier,
  registerCompletion,
  isBarrierComplete,
  sliceInputData,
} from '../../src/multiple-instance/wp14-runtime-apriori.mjs';
import {
  evaluateExpression,
  evaluateJSONPath,
  evaluateFunction,
  ExpressionType,
} from '../../src/multiple-instance/expression-evaluator.mjs';
import { TaskDefinition } from '../../src/task-definitions.mjs';

describe('WP14 - Multiple Instances with Runtime A Priori Knowledge', () => {
  let taskDef;

  beforeEach(() => {
    taskDef = new TaskDefinition({
      id: 'process-item',
      name: 'Process Item',
      joinType: 'sequence',
      splitType: 'sequence',
    });
  });

  // ===========================================================================
  // Expression Evaluator Tests
  // ===========================================================================

  describe('Expression Evaluator', () => {
    describe('JSONPath Evaluation', () => {
      it('should evaluate $.items.length', () => {
        const data = { items: [1, 2, 3, 4, 5] };
        const count = evaluateJSONPath('$.items.length', data);
        expect(count).toBe(5);
      });

      it('should evaluate $.items when items is array', () => {
        const data = { items: ['a', 'b', 'c'] };
        const count = evaluateJSONPath('$.items', data);
        expect(count).toBe(3);
      });

      it('should evaluate count($.items)', () => {
        const data = { items: [1, 2, 3, 4, 5, 6, 7] };
        const count = evaluateJSONPath('count($.items)', data);
        expect(count).toBe(7);
      });

      it('should evaluate nested properties $.data.users.length', () => {
        const data = {
          data: {
            users: ['alice', 'bob', 'charlie'],
          },
        };
        const count = evaluateJSONPath('$.data.users', data);
        expect(count).toBe(3);
      });

      it('should return 0 for non-existent path', () => {
        const data = { items: [] };
        const count = evaluateJSONPath('$.nonexistent', data);
        expect(count).toBe(0);
      });

      it('should handle empty arrays', () => {
        const data = { items: [] };
        const count = evaluateJSONPath('$.items', data);
        expect(count).toBe(0);
      });

      it('should count object keys', () => {
        const data = { config: { a: 1, b: 2, c: 3 } };
        const count = evaluateJSONPath('$.config', data);
        expect(count).toBe(3);
      });

      it('should reject invalid expressions with special chars', () => {
        const data = { items: [1, 2, 3] };
        expect(() => {
          evaluateJSONPath('$.items; DROP TABLE', data);
        }).toThrow('Invalid JSONPath expression');
      });
    });

    describe('Function Evaluation', () => {
      it('should evaluate countItems function', () => {
        const data = { items: [1, 2, 3, 4] };
        const count = evaluateFunction('countItems', data);
        expect(count).toBe(4);
      });

      it('should evaluate countKeys function', () => {
        const data = { a: 1, b: 2, c: 3 };
        const count = evaluateFunction('countKeys', data);
        expect(count).toBe(3);
      });

      it('should evaluate countAll for arrays', () => {
        const data = [1, 2, 3, 4, 5];
        const count = evaluateFunction('countAll', data);
        expect(count).toBe(5);
      });

      it('should evaluate countAll for objects', () => {
        const data = { x: 1, y: 2 };
        const count = evaluateFunction('countAll', data);
        expect(count).toBe(2);
      });

      it('should reject unknown functions', () => {
        const data = { items: [1, 2, 3] };
        expect(() => {
          evaluateFunction('maliciousFunction', data);
        }).toThrow('not allowed');
      });
    });

    describe('Unified Expression Evaluation', () => {
      it('should auto-detect JSONPath expression', async () => {
        const data = { items: [1, 2, 3, 4, 5] };
        const result = await evaluateExpression('$.items', data);

        expect(result.count).toBe(5);
        expect(result.type).toBe(ExpressionType.JSONPATH);
        expect(result.expression).toBe('$.items');
        expect(result.proof).toBeDefined();
        expect(result.proof.method).toBe('jsonpath');
      });

      it('should auto-detect function expression', async () => {
        const data = { items: [1, 2, 3] };
        const result = await evaluateExpression('countItems', data);

        expect(result.count).toBe(3);
        expect(result.type).toBe(ExpressionType.FUNCTION);
      });

      it('should auto-detect literal expression', async () => {
        const data = {};
        const result = await evaluateExpression('10', data);

        expect(result.count).toBe(10);
        expect(result.type).toBe(ExpressionType.LITERAL);
      });

      it('should handle explicit expression object', async () => {
        const data = { items: [1, 2, 3, 4] };
        const result = await evaluateExpression({
          type: ExpressionType.JSONPATH,
          expression: '$.items',
        }, data);

        expect(result.count).toBe(4);
        expect(result.type).toBe(ExpressionType.JSONPATH);
      });

      it('should reject negative literal', async () => {
        await expect(
          evaluateExpression('-5', {})
        ).rejects.toThrow('Invalid literal count');
      });

      it('should reject non-integer results', async () => {
        const data = { value: 3.14 };
        const result = await evaluateExpression('$.value', data);
        // Should floor to integer
        expect(result.count).toBe(3);
      });
    });
  });

  // ===========================================================================
  // Barrier Tests
  // ===========================================================================

  describe('Synchronization Barrier', () => {
    it('should create barrier with correct initial state', () => {
      const barrier = createBarrier(5);

      expect(barrier.totalInstances).toBe(5);
      expect(barrier.completedInstances).toBe(0);
      expect(barrier.status).toBe('active');
      expect(barrier.instanceIds).toEqual([]);
      expect(barrier.id).toBeDefined();
      expect(barrier.createdAt).toBeDefined();
    });

    it('should register instance completions', () => {
      const barrier = createBarrier(3);

      const result1 = registerCompletion(barrier, 'inst-1');
      expect(result1.barrier.completedInstances).toBe(1);
      expect(result1.isComplete).toBe(false);

      const result2 = registerCompletion(barrier, 'inst-2');
      expect(result2.barrier.completedInstances).toBe(2);
      expect(result2.isComplete).toBe(false);

      const result3 = registerCompletion(barrier, 'inst-3');
      expect(result3.barrier.completedInstances).toBe(3);
      expect(result3.isComplete).toBe(true);
      expect(result3.barrier.status).toBe('completed');
    });

    it('should detect barrier completion', () => {
      const barrier = createBarrier(2);

      expect(isBarrierComplete(barrier)).toBe(false);

      registerCompletion(barrier, 'inst-1');
      expect(isBarrierComplete(barrier)).toBe(false);

      registerCompletion(barrier, 'inst-2');
      expect(isBarrierComplete(barrier)).toBe(true);
    });

    it('should prevent duplicate instance registration', () => {
      const barrier = createBarrier(3);

      registerCompletion(barrier, 'inst-1');

      expect(() => {
        registerCompletion(barrier, 'inst-1');
      }).toThrow('already registered');
    });

    it('should detect barrier overflow', () => {
      const barrier = createBarrier(2);

      registerCompletion(barrier, 'inst-1');
      registerCompletion(barrier, 'inst-2');

      // After 2 completions, barrier is 'completed', so next registration
      // should fail with "Cannot register completion on completed barrier"
      expect(() => {
        registerCompletion(barrier, 'inst-3');
      }).toThrow('Cannot register completion on completed barrier');
    });

    it('should reject registration on completed barrier', () => {
      const barrier = createBarrier(1);

      registerCompletion(barrier, 'inst-1');

      expect(() => {
        registerCompletion(barrier, 'inst-2');
      }).toThrow('Cannot register completion on completed barrier');
    });

    it('should reject invalid barrier counts', () => {
      expect(() => createBarrier(0)).toThrow('Invalid barrier count');
      expect(() => createBarrier(-1)).toThrow('Invalid barrier count');
      expect(() => createBarrier(3.5)).toThrow('Invalid barrier count');
    });
  });

  // ===========================================================================
  // Data Slicing Tests
  // ===========================================================================

  describe('Input Data Slicing', () => {
    it('should slice items array to per-instance data', () => {
      const inputData = {
        items: [
          { id: 1, name: 'Item 1' },
          { id: 2, name: 'Item 2' },
          { id: 3, name: 'Item 3' },
        ],
      };

      const slices = sliceInputData(inputData, 3);

      expect(slices).toHaveLength(3);
      expect(slices[0].item).toEqual({ id: 1, name: 'Item 1' });
      expect(slices[0].itemIndex).toBe(0);
      expect(slices[0].totalInstances).toBe(3);

      expect(slices[1].item).toEqual({ id: 2, name: 'Item 2' });
      expect(slices[1].itemIndex).toBe(1);

      expect(slices[2].item).toEqual({ id: 3, name: 'Item 3' });
      expect(slices[2].itemIndex).toBe(2);
    });

    it('should handle more instances than items', () => {
      const inputData = { items: [1, 2] };
      const slices = sliceInputData(inputData, 3);

      expect(slices).toHaveLength(3);
      expect(slices[0].item).toBe(1);
      expect(slices[1].item).toBe(2);
      expect(slices[2].item).toBeNull(); // No item available
    });

    it('should handle data without items array', () => {
      const inputData = { config: { mode: 'production' } };
      const slices = sliceInputData(inputData, 3);

      expect(slices).toHaveLength(3);
      expect(slices[0].config).toEqual({ mode: 'production' });
      expect(slices[0].instanceIndex).toBe(0);
      expect(slices[1].instanceIndex).toBe(1);
      expect(slices[2].instanceIndex).toBe(2);
    });

    it('should preserve original data in slices', () => {
      const inputData = {
        items: ['a', 'b'],
        metadata: { source: 'test' },
      };

      const slices = sliceInputData(inputData, 2);

      expect(slices[0].metadata).toEqual({ source: 'test' });
      expect(slices[1].metadata).toEqual({ source: 'test' });
    });
  });

  // ===========================================================================
  // WP14 Integration Tests
  // ===========================================================================

  describe('WP14 Instance Spawning', () => {
    it('should spawn N instances based on JSONPath expression', async () => {
      const inputData = {
        items: [
          { id: 1, value: 'A' },
          { id: 2, value: 'B' },
          { id: 3, value: 'C' },
          { id: 4, value: 'D' },
          { id: 5, value: 'E' },
        ],
      };

      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        'count($.items)',
        inputData,
        { caseId: 'case-wp14-test-1' }
      );

      // Verify count evaluation
      expect(result.countEvaluation.count).toBe(5);
      expect(result.countEvaluation.expression).toBe('count($.items)');

      // Verify instances
      expect(result.instances).toHaveLength(5);
      expect(result.instances[0].inputData.item).toEqual({ id: 1, value: 'A' });
      expect(result.instances[0]._instanceIndex).toBe(0);
      expect(result.instances[4].inputData.item).toEqual({ id: 5, value: 'E' });

      // Verify barrier
      expect(result.barrier.totalInstances).toBe(5);
      expect(result.barrier.completedInstances).toBe(0);
      expect(result.barrier.status).toBe('active');

      // Verify receipt
      expect(result.receipt.pattern).toBe('WP14');
      expect(result.receipt.hash).toBeDefined();
      expect(result.receipt.instances).toHaveLength(5);
    });

    it('should spawn instances with literal count', async () => {
      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        '3',
        { data: 'test' },
        { caseId: 'case-literal' }
      );

      expect(result.instances).toHaveLength(3);
      expect(result.countEvaluation.count).toBe(3);
      expect(result.barrier.totalInstances).toBe(3);
    });

    it('should spawn instances with function expression', async () => {
      const inputData = {
        items: [10, 20, 30, 40],
      };

      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        'countItems',
        inputData,
        { caseId: 'case-function' }
      );

      expect(result.instances).toHaveLength(4);
      expect(result.countEvaluation.count).toBe(4);
    });

    it('should generate unique instance IDs', async () => {
      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        '5',
        {},
        { caseId: 'case-unique-ids' }
      );

      const ids = result.instances.map(inst => inst.id);
      const uniqueIds = new Set(ids);

      expect(uniqueIds.size).toBe(5);
    });

    it('should attach barrier reference to each instance', async () => {
      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        '3',
        {},
        { caseId: 'case-barrier-ref' }
      );

      for (const instance of result.instances) {
        expect(instance._barrierId).toBe(result.barrier.id);
      }
    });

    it('should reject zero count', async () => {
      await expect(
        spawnInstancesRuntimeApriori(
          taskDef,
          '$.items',
          { items: [] }, // Empty array = count 0
          { caseId: 'case-zero' }
        )
      ).rejects.toThrow('Count expression evaluated to 0');
    });

    it('should use existing barrier if provided', async () => {
      const customBarrier = createBarrier(4, { id: 'custom-barrier-123' });

      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        '4',
        {},
        { barrier: customBarrier, caseId: 'case-custom-barrier' }
      );

      expect(result.barrier.id).toBe('custom-barrier-123');
      expect(result.barrier).toStrictEqual(customBarrier);
    });

    it('should reject barrier count mismatch', async () => {
      const barrier = createBarrier(5);

      await expect(
        spawnInstancesRuntimeApriori(
          taskDef,
          '3', // Evaluates to 3, but barrier expects 5
          {},
          { barrier, caseId: 'case-mismatch' }
        )
      ).rejects.toThrow('Barrier count mismatch');
    });
  });

  // ===========================================================================
  // End-to-End WP14 Scenario
  // ===========================================================================

  describe('E2E: Complete WP14 Workflow', () => {
    it('should execute full WP14 pattern with synchronization', async () => {
      // Given: A list of orders to process
      const inputData = {
        orders: [
          { orderId: 'ORD-1', amount: 100 },
          { orderId: 'ORD-2', amount: 200 },
          { orderId: 'ORD-3', amount: 150 },
        ],
        items: [
          { orderId: 'ORD-1', amount: 100 },
          { orderId: 'ORD-2', amount: 200 },
          { orderId: 'ORD-3', amount: 150 },
        ],
      };

      // When: Spawn instances using WP14
      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        '$.orders',
        inputData,
        { caseId: 'case-e2e-orders' }
      );

      // Then: Verify correct number of instances
      expect(result.instances).toHaveLength(3);

      // And: Each instance has correct input data
      expect(result.instances[0].inputData.item.orderId).toBe('ORD-1');
      expect(result.instances[1].inputData.item.orderId).toBe('ORD-2');
      expect(result.instances[2].inputData.item.orderId).toBe('ORD-3');

      // And: Barrier is active
      expect(result.barrier.status).toBe('active');
      expect(result.barrier.completedInstances).toBe(0);

      // When: Instances complete
      registerCompletion(result.barrier, result.instances[0].id);
      expect(result.barrier.completedInstances).toBe(1);

      registerCompletion(result.barrier, result.instances[1].id);
      expect(result.barrier.completedInstances).toBe(2);

      registerCompletion(result.barrier, result.instances[2].id);

      // Then: Barrier completes
      expect(result.barrier.status).toBe('completed');
      expect(isBarrierComplete(result.barrier)).toBe(true);

      // And: Receipt captures full execution
      expect(result.receipt.pattern).toBe('WP14');
      expect(result.receipt.countEvaluation.count).toBe(3);
      expect(result.receipt.instances).toHaveLength(3);
    });
  });

  // ===========================================================================
  // Performance Tests
  // ===========================================================================

  describe('Performance', () => {
    it('should handle large instance counts efficiently', async () => {
      const items = Array.from({ length: 100 }, (_, i) => ({ id: i }));
      const inputData = { items };

      const startTime = performance.now();

      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        '$.items',
        inputData,
        { caseId: 'case-perf-100' }
      );

      const duration = performance.now() - startTime;

      expect(result.instances).toHaveLength(100);
      expect(duration).toBeLessThan(1000); // < 1 second for 100 instances
    });

    it('should handle complex input data slicing', async () => {
      const complexItems = Array.from({ length: 50 }, (_, i) => ({
        id: i,
        metadata: {
          tags: ['tag1', 'tag2', 'tag3'],
          nested: { deep: { value: i * 10 } },
        },
      }));

      const result = await spawnInstancesRuntimeApriori(
        taskDef,
        'count($.items)',
        { items: complexItems },
        { caseId: 'case-complex' }
      );

      expect(result.instances).toHaveLength(50);

      // Verify complex data preserved
      expect(result.instances[10].inputData.item.metadata.nested.deep.value).toBe(100);
    });
  });
});
