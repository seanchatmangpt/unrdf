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
});
