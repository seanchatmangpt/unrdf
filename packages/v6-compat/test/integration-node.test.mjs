/**
 * P0-003: Integration Tests (Node.js test runner)
 */

import { describe, it } from 'node:test';
import assert from 'node:assert';
import {
  createStore,
  wrapWorkflow,
  withReceipt,
  MigrationTracker,
} from '../src/adapters.mjs';

describe('P0-003: v6-compat Integration', () => {
  it('createStore adapter works', async () => {
    const store = await createStore();
    assert.ok(store);
    console.log('✅ createStore adapter works');
  });

  it('wrapWorkflow adds receipt support', async () => {
    const mockWorkflow = {
      run: async (task) => ({ status: 'completed', taskId: task.id }),
    };

    const wrapped = wrapWorkflow(mockWorkflow);
    assert.ok(wrapped.execute);
    assert.ok(wrapped.run);

    const { result, receipt } = await wrapped.execute({ id: 'task-1' });
    assert.strictEqual(result.status, 'completed');
    assert.ok(receipt);
    assert.strictEqual(receipt.operation, 'workflow.execute');

    console.log('✅ wrapWorkflow adds receipt support');
  });

  it('withReceipt wraps function', async () => {
    const double = withReceipt((x) => x * 2, { operation: 'double' });

    const { result, receipt } = await double(5);
    assert.strictEqual(result, 10);
    assert.ok(receipt);
    assert.strictEqual(receipt.operation, 'double');

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

    assert.strictEqual(issues.n3Imports, 2);
    assert.strictEqual(issues.dateNowCalls, 1);
    assert.strictEqual(issues.mathRandomCalls, 1);
    assert.strictEqual(tracker.staticAnalysis.n3Imports, 2);
    assert.strictEqual(tracker.staticAnalysis.dateNowCalls, 1);
    assert.strictEqual(tracker.staticAnalysis.mathRandomCalls, 1);

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

    assert.strictEqual(issues.workflowRunCalls, 2);
    assert.strictEqual(tracker.staticAnalysis.workflowRunCalls, 2);

    console.log('✅ MigrationTracker counts workflow.run() calls');
    console.log(`   workflow.run() calls: ${issues.workflowRunCalls}`);
  });

  it('MigrationTracker generates report', () => {
    const tracker = new MigrationTracker();

    tracker.track('old.api()', 'new.api()');
    tracker.analyzeSource('import { Store } from "n3";', 'test.mjs');

    const report = tracker.report();

    assert.ok(report.staticAnalysis);
    assert.strictEqual(report.staticAnalysis.filesScanned, 1);
    assert.strictEqual(report.staticAnalysis.n3Imports, 1);

    console.log('✅ MigrationTracker generates report');
    console.log(`   Files scanned: ${report.staticAnalysis.filesScanned}`);
    console.log(`   N3 imports: ${report.staticAnalysis.n3Imports}`);
  });
});

console.log('\n📊 P0-003 Test Summary: v6-compat Integration');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
