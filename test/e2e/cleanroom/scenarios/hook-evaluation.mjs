/**
 * @file P0 Scenario: Hook Evaluation
 * @module scenarios/hook-evaluation
 *
 * @description
 * Knowledge Hook workflow: Create → Evaluate → Track → History
 * Represents 25% of real-world usage.
 */

import { ScenarioBuilder } from '../scenario-framework.mjs';
import assert from 'assert';

/**
 * Hook Evaluation scenario - P0 (Core Workflow)
 */
export const hookEvaluationScenario = new ScenarioBuilder()
  .name('Hook Evaluation')
  .description('Create, evaluate, and track knowledge hook execution')
  .priority('P0')
  .tag('core')
  .tag('hooks')
  .tag('workflow')

  // Step 1: Create hook
  .step({
    name: 'Create SPARQL ASK hook',
    command: 'node cli/unrdf.mjs hook create health-check --type=sparql-ask --file=test/e2e/cleanroom/fixtures/health-check.rq',
    expectedExitCode: 0,
    expectedOutput: /Hook created: health-check/,
  })

  // Step 2: Evaluate hook
  .step({
    name: 'Evaluate hook with test data',
    command: 'node cli/unrdf.mjs hook eval health-check --data=test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
    expectedOutput: /Hook fired: (true|false)/,
  })

  // Step 3: View execution history
  .step({
    name: 'View hook history',
    command: 'node cli/unrdf.mjs hook history health-check --limit=5',
    expectedExitCode: 0,
    expectedOutput: /\d+ evaluations?/,
  })

  // Expected OTEL spans
  .expectSpan('cli.hook.create')
  .expectSpan('sidecar.hook.register')
  .expectSpan('cli.hook.eval')
  .expectSpan('sidecar.hook.evaluate')
  .expectSpan('sidecar.condition.evaluate')
  .expectSpan('cli.hook.history')
  .expectSpan('sidecar.storage.loadReceipts')

  // Assertions
  .assert(async (ctx) => {
    const traces = await ctx.getTraces();
    assert(traces.length > 0, 'Should have traces');

    const spans = await ctx.getSpans();
    const hookEvalSpan = spans.find(s =>
      s.operationName === 'sidecar.hook.evaluate' ||
      s.name === 'sidecar.hook.evaluate'
    );

    assert(hookEvalSpan, 'Should have hook evaluation span');

    // Hook evaluation should be fast (< 2ms)
    if (hookEvalSpan.duration) {
      assert(hookEvalSpan.duration < 2000,
        `Hook eval duration ${hookEvalSpan.duration}µs should be < 2000µs`);
    }
  })

  .build();

/**
 * Hook with Veto scenario - P0 (Error Handling)
 */
export const hookVetoScenario = new ScenarioBuilder()
  .name('Hook Veto Transaction')
  .description('Hook vetoes transaction, preventing it from committing')
  .priority('P0')
  .tag('core')
  .tag('hooks')
  .tag('veto')

  // Step 1: Create veto hook
  .step({
    name: 'Create veto hook',
    command: 'node cli/unrdf.mjs hook create veto-hook --type=sparql-ask --file=test/e2e/cleanroom/fixtures/veto-hook.rq --veto',
    expectedExitCode: 0,
  })

  // Step 2: Attempt transaction that should be vetoed
  .step({
    name: 'Attempt vetoed transaction',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/invalid-data.ttl',
    expectedExitCode: 1, // Should fail due to veto
    expectedOutput: /Transaction vetoed|Veto/,
  })

  // Step 3: Verify hook fired with veto
  .step({
    name: 'Verify veto in history',
    command: 'node cli/unrdf.mjs hook history veto-hook --limit=1',
    expectedExitCode: 0,
    expectedOutput: /veto.*true/,
  })

  .expectSpan('cli.hook.create')
  .expectSpan('cli.store.import')
  .expectSpan('sidecar.hook.evaluate')
  .expectSpan('sidecar.transaction.veto')

  .assert(async (ctx) => {
    // Verify transaction was not committed
    const stepResults = ctx.results;
    const vetoStep = stepResults.find(r => r.step === 'Attempt vetoed transaction');
    assert(vetoStep?.result.exitCode === 1, 'Transaction should have been vetoed');
  })

  .build();

/**
 * Hook Performance Benchmarking scenario - P1 (Performance)
 */
export const hookPerformanceScenario = new ScenarioBuilder()
  .name('Hook Performance Benchmark')
  .description('Evaluate hook performance with multiple iterations')
  .priority('P1')
  .tag('performance')
  .tag('hooks')
  .tag('benchmark')

  // Step 1: Create benchmark hook
  .step({
    name: 'Create benchmark hook',
    command: 'node cli/unrdf.mjs hook create perf-hook --type=sparql-ask --file=test/e2e/cleanroom/fixtures/simple-hook.rq',
    expectedExitCode: 0,
  })

  // Step 2-11: Evaluate hook 10 times
  .step({
    name: 'Evaluate hook iteration 1',
    command: 'node cli/unrdf.mjs hook eval perf-hook --data=test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })
  .step({
    name: 'Evaluate hook iteration 2',
    command: 'node cli/unrdf.mjs hook eval perf-hook --data=test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })
  .step({
    name: 'Evaluate hook iteration 3',
    command: 'node cli/unrdf.mjs hook eval perf-hook --data=test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })
  .step({
    name: 'Evaluate hook iteration 4',
    command: 'node cli/unrdf.mjs hook eval perf-hook --data=test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })
  .step({
    name: 'Evaluate hook iteration 5',
    command: 'node cli/unrdf.mjs hook eval perf-hook --data=test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })

  .expectSpan('sidecar.hook.evaluate')

  .assert(async (ctx) => {
    const spans = await ctx.getSpans();
    const hookSpans = spans.filter(s =>
      s.operationName === 'sidecar.hook.evaluate' ||
      s.name === 'sidecar.hook.evaluate'
    );

    // Calculate p99 latency
    const durations = hookSpans.map(s => s.duration || 0);
    const sorted = durations.sort((a, b) => a - b);
    const p99 = sorted[Math.floor(sorted.length * 0.99)] || 0;

    console.log(`    📊 Hook evaluation p99: ${p99}µs`);
    assert(p99 < 2000, `Hook p99 ${p99}µs should be < 2000µs`);
  })

  .build();

/**
 * Hook Chaining scenario - P1 (Advanced)
 */
export const hookChainingScenario = new ScenarioBuilder()
  .name('Hook Chaining')
  .description('Multiple hooks evaluated in sequence')
  .priority('P1')
  .tag('advanced')
  .tag('hooks')
  .tag('chaining')

  // Create multiple hooks
  .step({
    name: 'Create pre-hook 1',
    command: 'node cli/unrdf.mjs hook create pre-1 --type=sparql-ask --file=test/e2e/cleanroom/fixtures/pre-hook-1.rq --phase=pre',
    expectedExitCode: 0,
  })

  .step({
    name: 'Create pre-hook 2',
    command: 'node cli/unrdf.mjs hook create pre-2 --type=sparql-ask --file=test/e2e/cleanroom/fixtures/pre-hook-2.rq --phase=pre',
    expectedExitCode: 0,
  })

  .step({
    name: 'Create post-hook',
    command: 'node cli/unrdf.mjs hook create post-1 --type=sparql-ask --file=test/e2e/cleanroom/fixtures/post-hook-1.rq --phase=post',
    expectedExitCode: 0,
  })

  // Trigger transaction that runs all hooks
  .step({
    name: 'Execute transaction with chained hooks',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })

  .expectSpan('sidecar.hook.evaluate')

  .assert(async (ctx) => {
    const spans = await ctx.getSpans();
    const hookSpans = spans.filter(s =>
      s.operationName === 'sidecar.hook.evaluate' ||
      s.name === 'sidecar.hook.evaluate'
    );

    // Should have at least 3 hook evaluations (pre-1, pre-2, post-1)
    assert(hookSpans.length >= 3,
      `Should have >= 3 hook evaluations, found ${hookSpans.length}`);
  })

  .build();

export default {
  hookEvaluationScenario,
  hookVetoScenario,
  hookPerformanceScenario,
  hookChainingScenario,
};
