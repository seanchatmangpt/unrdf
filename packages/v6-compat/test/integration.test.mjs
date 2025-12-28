/**
 * Integration tests for P0-003: v6-compat package
 *
 * Tests all adapters, ESLint rules, and migration tracker together
 */

import { describe, it, expect } from 'vitest';
import {
  createStore,
  wrapWorkflow,
  wrapFederation,
  streamToAsync,
  withReceipt,
  validateSchema,
  MigrationTracker,
} from '../src/adapters.mjs';
import { z } from 'zod';
import { computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

describe('P0-003: v6-compat Integration', () => {
  describe('Adapters', () => {
    it('createStore adapter works', async () => {
      const store = await createStore();
      expect(store).toBeDefined();
    });

    it('wrapWorkflow adds receipt support', async () => {
      const mockWorkflow = {
        run: async (task) => ({ status: 'completed', taskId: task.id }),
      };

      const wrapped = wrapWorkflow(mockWorkflow);
      expect(wrapped.execute).toBeDefined();
      expect(wrapped.run).toBeDefined();

      const { result, receipt } = await wrapped.execute({ id: 'task-1' });
      expect(result.status).toBe('completed');
      expect(receipt).toBeDefined();
      expect(receipt.operation).toBe('workflow.execute');
    });

    it('wrapFederation adds timeout', async () => {
      const mockFederation = {
        query: async (q) => [{ s: 'subject', p: 'predicate', o: 'object' }],
      };

      const wrapped = wrapFederation(mockFederation);
      expect(wrapped.querySparql).toBeDefined();

      const results = await wrapped.querySparql('SELECT * WHERE { ?s ?p ?o }');
      expect(results).toBeDefined();
      expect(Array.isArray(results)).toBe(true);
    });

    it('withReceipt wraps function', async () => {
      const double = withReceipt((x) => x * 2, { operation: 'double' });

      const { result, receipt } = await double(5);
      expect(result).toBe(10);
      expect(receipt).toBeDefined();
      expect(receipt.operation).toBe('double');
    });

    it('validateSchema validates correctly', () => {
      const UserSchema = z.object({
        id: z.string(),
        name: z.string(),
      });

      const validate = validateSchema(UserSchema);

      expect(() => validate({ id: '123', name: 'Alice' })).not.toThrow();
      expect(() => validate({ id: 123 })).toThrow();
    });
  });

  describe('Migration Tracker (P0-003)', () => {
    it('tracks deprecation warnings', () => {
      const tracker = new MigrationTracker();

      tracker.track('old.api()', 'new.api()');
      tracker.track('old.method()', 'new.method()');

      const report = tracker.report();
      expect(report.totalWarnings).toBe(2);
      expect(report.uniqueAPIs).toBe(2);
    });

    it('analyzes source for N3 imports', () => {
      const tracker = new MigrationTracker();

      const source = `
        import { Store } from 'n3';
        import { Parser } from 'n3';

        const timestamp = Date.now();
        const random = Math.random();
      `;

      const issues = tracker.analyzeSource(source, 'test.mjs');

      expect(issues.n3Imports).toBe(2);
      expect(issues.dateNowCalls).toBe(1);
      expect(issues.mathRandomCalls).toBe(1);
      expect(tracker.staticAnalysis.n3Imports).toBe(2);
      expect(tracker.staticAnalysis.dateNowCalls).toBe(1);
      expect(tracker.staticAnalysis.mathRandomCalls).toBe(1);
      expect(tracker.staticAnalysis.filesScanned).toBe(1);
    });

    it('counts workflow.run() calls', () => {
      const tracker = new MigrationTracker();

      const source = `
        const result = await workflow.run(task);
        const result2 = await workflow.run(task2);
      `;

      const issues = tracker.analyzeSource(source, 'test.mjs');

      expect(issues.workflowRunCalls).toBe(2);
      expect(tracker.staticAnalysis.workflowRunCalls).toBe(2);
    });

    it('scans directory for migration issues', async () => {
      const tracker = new MigrationTracker();

      // Scan the v6-compat src directory itself
      const issues = await tracker.scanDirectory(
        '/home/user/unrdf/packages/v6-compat/src/*.mjs'
      );

      expect(Array.isArray(issues)).toBe(true);
      // Should have some issues (adapters.mjs uses Date.now() in MigrationTracker)
      expect(tracker.staticAnalysis.filesScanned).toBeGreaterThan(0);
    });

    it('generates summary report', () => {
      const tracker = new MigrationTracker();

      tracker.track('old.api()', 'new.api()');
      tracker.analyzeSource('import { Store } from "n3";', 'test.mjs');

      const report = tracker.report();

      expect(report.staticAnalysis).toBeDefined();
      expect(report.staticAnalysis.filesScanned).toBe(1);
      expect(report.staticAnalysis.n3Imports).toBe(1);
    });
  });

  describe('End-to-End Migration Flow', () => {
    it('completes full migration workflow', async () => {
      const tracker = new MigrationTracker();

      // 1. Analyze source
      const source = `
        import { Store } from 'n3';
        export async function oldFunction() {
          const store = new Store();
          const timestamp = Date.now();
          return { store, timestamp };
        }
      `;

      const issues = tracker.analyzeSource(source, 'legacy.mjs');

      // 2. Verify issues detected
      expect(issues.n3Imports).toBe(1);
      expect(issues.dateNowCalls).toBe(1);

      // 3. Use adapter for migration
      const store = await createStore();
      expect(store).toBeDefined();

      // 4. Generate report
      const report = tracker.report();
      expect(report.staticAnalysis.n3Imports).toBe(1);
      expect(report.staticAnalysis.dateNowCalls).toBe(1);
    });
  });

  describe('withReceipt Determinism (L3 Maturity)', () => {
    it('produces identical hashes on multiple runs with same context', async () => {
      // GIVEN: Same function, same input, same context
      const testFn = (data) => data.map((x) => x * 2);
      const testData = [1, 2, 3];
      const context = {
        t_ns: 1234567890000000000n, // Fixed timestamp
      };
      const options = {
        operation: 'testOperation',
        context,
        startTime: 100,
        endTime: 200,
      };

      // Create wrapped function
      const wrappedFn = withReceipt(testFn, options);

      // WHEN: Execute multiple times (pass array as single argument)
      const { receipt: receipt1 } = await wrappedFn(testData);
      const { receipt: receipt2 } = await wrappedFn(testData);
      const { receipt: receipt3 } = await wrappedFn(testData);

      // THEN: All receipts should be IDENTICAL
      expect(receipt1).toEqual(receipt2);
      expect(receipt2).toEqual(receipt3);

      // Verify key fields are identical
      expect(receipt1.timestamp).toBe(receipt2.timestamp);
      expect(receipt1.duration).toBe(receipt2.duration);
      expect(receipt1.args).toBe(receipt2.args);
      expect(receipt1.result).toBe(receipt2.result);
    });

    it('second execution produces identical receipt when context is fixed', async () => {
      // GIVEN: Idempotent function with fixed context
      const processData = withReceipt(
        (data) => data.map((x) => x * 2),
        {
          operation: 'processData',
          context: { t_ns: 7777777777777777777n },
          startTime: 10,
          endTime: 20,
        }
      );

      // WHEN: Execute twice
      const firstRun = await processData([10, 20, 30]);
      const secondRun = await processData([10, 20, 30]);

      // THEN: Results and receipts MUST be identical
      expect(firstRun.result).toEqual(secondRun.result);
      expect(firstRun.receipt).toEqual(secondRun.receipt);
      expect(firstRun.receipt.timestamp).toBe(secondRun.receipt.timestamp);
    });

    it('serializes arguments deterministically regardless of property order', async () => {
      // GIVEN: Function that accepts object arguments
      const testFn = (obj) => obj.a + obj.b;

      const context = { t_ns: 9999999999999999999n };
      const options = {
        operation: 'sumOp',
        context,
        startTime: 0,
        endTime: 1,
      };

      const wrappedFn = withReceipt(testFn, options);

      // WHEN: Pass objects with same data but different property definition order
      const obj1 = { a: 1, b: 2 };
      const obj2 = { b: 2, a: 1 }; // Same data, different order

      const { receipt: receipt1 } = await wrappedFn(obj1);
      const { receipt: receipt2 } = await wrappedFn(obj2);

      // THEN: Args should be IDENTICAL (deterministic serialization)
      expect(receipt1.args).toBe(receipt2.args);
      expect(receipt1.result).toBe(receipt2.result);
    });
  });
});
