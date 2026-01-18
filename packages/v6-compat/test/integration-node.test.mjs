/**
 * P0-003: Integration Tests (Vitest)
 */

import { describe, it, expect } from 'vitest';
import {
  createStore,
  wrapWorkflow,
  withReceipt,
  MigrationTracker,
} from '../src/adapters.mjs';

describe('P0-003: v6-compat Integration', () => {
  it('createStore adapter works', async () => {
    const store = await createStore();
    expect(store).toBeDefined();
    console.log('✅ createStore adapter works');
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

    console.log('✅ wrapWorkflow adds receipt support');
  });

  it('withReceipt wraps function', async () => {
    const double = withReceipt((x) => x * 2, { operation: 'double' });

    const { result, receipt } = await double(5);
    expect(result).toBe(10);
    expect(receipt).toBeDefined();
    expect(receipt.operation).toBe('double');

    console.log('✅ withReceipt wraps function');
  });

  it('MigrationTracker analyzes source for N3 imports', () => {
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

    console.log('✅ MigrationTracker static analysis works');
    console.log(`   N3 imports: ${issues.n3Imports}`);
    console.log(`   Date.now() calls: ${issues.dateNowCalls}`);
    console.log(`   Math.random() calls: ${issues.mathRandomCalls}`);
  });

  it('MigrationTracker counts workflow.run() calls', () => {
    const tracker = new MigrationTracker();

    const source = `
      const result = await workflow.run(task);
      const result2 = await workflow.run(task2);
    `;

    const issues = tracker.analyzeSource(source, 'test.mjs');

    expect(issues.workflowRunCalls).toBe(2);
    expect(tracker.staticAnalysis.workflowRunCalls).toBe(2);

    console.log('✅ MigrationTracker counts workflow.run() calls');
    console.log(`   workflow.run() calls: ${issues.workflowRunCalls}`);
  });

  it('MigrationTracker generates report', () => {
    const tracker = new MigrationTracker();

    tracker.track('old.api()', 'new.api()');
    tracker.analyzeSource('import { Store } from "n3";', 'test.mjs');

    const report = tracker.report();

    expect(report.staticAnalysis).toBeDefined();
    expect(report.staticAnalysis.filesScanned).toBe(1);
    expect(report.staticAnalysis.n3Imports).toBe(1);

    console.log('✅ MigrationTracker generates report');
    console.log(`   Files scanned: ${report.staticAnalysis.filesScanned}`);
    console.log(`   N3 imports: ${report.staticAnalysis.n3Imports}`);
  });
});

console.log('\n📊 P0-003 Test Summary: v6-compat Integration');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
