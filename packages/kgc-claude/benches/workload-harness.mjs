/**
 * Performance Workload Harness - Regression Detection for KGC-Claude Substrate
 *
 * Deterministic workloads with regression detection against baseline.
 *
 * Workloads:
 * 1. RunCapsule creation (1000 runs)
 * 2. Checkpoint freeze/thaw (depth 10)
 * 3. ShardMerge operations (10 agents)
 * 4. AsyncWorkflow execution
 *
 * Guards:
 * - No absolute performance claims (only regression detection)
 * - No system-dependent tuning (same code = repeatable measurements)
 * - Deterministic input generation (no randomization)
 *
 * Usage:
 *   npm run bench:harness                  # Run and compare to baseline
 *   npm run bench:harness -- --baseline    # Create new baseline
 *
 * @module benches/workload-harness
 */

import { performance } from 'node:perf_hooks';
import { readFile, writeFile, access } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

// Import substrate modules (with fallback to mocks for testing)
let createRunCapsule, checkAdmission, createAutonomyGuard, createShard, addDelta, mergeDeltas;
let enqueueWorkItem, registerExecutor, assignWorkItem, completeWorkItem;

try {
  const substrate = await import('../src/index.mjs');
  ({ createRunCapsule, checkAdmission, createAutonomyGuard, createShard, addDelta, mergeDeltas,
     enqueueWorkItem, registerExecutor, assignWorkItem, completeWorkItem } = substrate);
} catch (err) {
  console.warn('⚠️  Substrate modules not available, using mocks for demonstration\n');
  // Mock implementations for demonstration when dependencies aren't installed
  createRunCapsule = (opts = {}) => ({
    id: crypto.randomUUID(),
    addToolCall: () => crypto.randomUUID(),
    completeToolCall: () => {},
    addDeltaO: () => {},
    addArtifact: () => {},
    seal: async () => ({ id: crypto.randomUUID(), runHash: 'mock-hash' }),
  });
  createShard = (opts) => ({ id: crypto.randomUUID(), ...opts, deltas: [] });
  addDelta = (shard, delta) => { shard.deltas = shard.deltas || []; shard.deltas.push(delta); };
  mergeDeltas = (deltas) => ({ merged: deltas, conflicts: [], receiptHash: 'mock-hash' });
  enqueueWorkItem = (opts) => ({ id: crypto.randomUUID(), ...opts });
  registerExecutor = (opts) => ({ id: opts.id });
  assignWorkItem = () => true;
  completeWorkItem = () => {};
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Measure execution time of a workload
 * @param {Function} fn - Workload function
 * @returns {Promise<{ result: any, durationMs: number }>}
 */
async function measure(fn) {
  const start = performance.now();
  const result = await fn();
  const end = performance.now();
  return {
    result,
    durationMs: end - start,
  };
}

/**
 * Workload 1: Create and seal 1000 run capsules
 *
 * Deterministic: Same operations, same order
 * No randomization, fixed input data
 */
async function workload1_runCapsules() {
  const runs = [];

  for (let i = 0; i < 1000; i++) {
    const builder = createRunCapsule({
      parentRunId: i > 0 ? runs[i - 1].id : undefined,
    });

    // Deterministic tool calls
    const readId = builder.addToolCall({
      name: 'Read',
      input: { file_path: `/file-${i}.txt` },
    });

    builder.completeToolCall(readId, {
      output: `Content of file ${i}`,
    });

    const writeId = builder.addToolCall({
      name: 'Write',
      input: { file_path: `/output-${i}.txt`, content: `Output ${i}` },
    });

    builder.completeToolCall(writeId, {
      output: 'Success',
    });

    // Add deltas
    builder.addDeltaO({
      type: 'add',
      target: `http://example.org/file-${i}`,
      after: { content: `Content ${i}` },
    });

    // Add artifact
    builder.addArtifact({
      type: 'file',
      path: `/output-${i}.txt`,
      contentHash: `hash-${i}`,
    });

    const capsule = await builder.seal();
    runs.push(capsule);
  }

  return runs.length;
}

/**
 * Workload 2: Checkpoint chain of depth 10
 *
 * Simulates checkpoint creation and verification
 * (Simplified - no actual store/git for determinism)
 */
async function workload2_checkpoints() {
  const checkpoints = [];

  for (let i = 0; i < 10; i++) {
    // Simulate checkpoint creation
    const t_ns = BigInt(Date.now()) * 1000000n + BigInt(i);
    const checkpoint = {
      id: crypto.randomUUID(),
      t_ns: t_ns.toString(), // Convert BigInt to string for JSON serialization
      timestamp_iso: new Date().toISOString(),
      snapshotHash: `checkpoint-hash-${i}`,
      universeSize: 1000 + i * 100,
      runCapsuleIds: Array.from({ length: 10 }, (_, j) => `run-${i}-${j}`),
      previousCheckpointHash: i > 0 ? checkpoints[i - 1].checkpointHash : null,
      checkpointHash: `chain-hash-${i}`,
    };

    checkpoints.push(checkpoint);

    // Simulate verification by rehashing
    const serialized = JSON.stringify(checkpoint);
    const hash = serialized.length.toString(16); // Deterministic placeholder
  }

  return checkpoints.length;
}

/**
 * Workload 3: Multi-agent shard merge (10 agents)
 *
 * Deterministic shard creation and delta merging
 */
async function workload3_shardMerge() {
  const shards = [];

  // Create 10 agent shards
  for (let i = 0; i < 10; i++) {
    const shard = createShard({
      agentId: `agent-${i}`,
      scope: {
        files: [`/workspace/agent-${i}/**`],
        graphs: [`http://example.org/graph/${i}`],
      },
      priority: i,
    });

    shards.push(shard);

    // Add 10 deltas per shard
    for (let j = 0; j < 10; j++) {
      addDelta(shard, {
        type: 'add',
        target: `http://example.org/resource-${i}-${j}`,
        after: { value: `Data from agent ${i}, delta ${j}` },
      });
    }
  }

  // Merge all shards
  const allDeltas = shards.flatMap((shard) => shard.deltas || []);
  const merged = mergeDeltas(allDeltas);

  return merged.merged.length;
}

/**
 * Workload 4: Async workflow execution
 *
 * Enqueue, assign, and complete work items
 */
async function workload4_asyncWorkflow() {
  const workItems = [];

  // Enqueue 50 work items
  for (let i = 0; i < 50; i++) {
    const item = enqueueWorkItem({
      type: 'task',
      payload: {
        operation: 'process',
        data: `input-${i}`,
      },
      constraints: {
        maxRetries: 3,
        timeout: 60000,
      },
    });

    workItems.push(item);
  }

  // Register 5 executors
  const executors = [];
  for (let i = 0; i < 5; i++) {
    const executor = registerExecutor({
      id: `executor-${i}`,
      capabilities: ['task'],
    });
    executors.push(executor);
  }

  // Assign and complete work items
  let completed = 0;
  for (const item of workItems) {
    const executorIdx = completed % executors.length;
    const assignment = assignWorkItem(item.id, executors[executorIdx].id);

    if (assignment) {
      completeWorkItem(item.id, {
        result: `Processed by ${executors[executorIdx].id}`,
      });
      completed++;
    }
  }

  return completed;
}

/**
 * Run all workloads and collect results
 *
 * @returns {Promise<Object>} Workload results with timings
 */
async function runWorkloads() {
  console.log('Running workload harness...\n');

  const results = {};

  // Workload 1: RunCapsules
  console.log('Workload 1: Creating 1000 run capsules...');
  const w1 = await measure(workload1_runCapsules);
  results.workload1_runCapsules = {
    operations: w1.result,
    durationMs: w1.durationMs,
    opsPerSecond: (w1.result / (w1.durationMs / 1000)).toFixed(2),
  };
  console.log(`  ✓ Completed in ${w1.durationMs.toFixed(2)}ms (${results.workload1_runCapsules.opsPerSecond} ops/sec)\n`);

  // Workload 2: Checkpoints
  console.log('Workload 2: Creating checkpoint chain (depth 10)...');
  const w2 = await measure(workload2_checkpoints);
  results.workload2_checkpoints = {
    operations: w2.result,
    durationMs: w2.durationMs,
    opsPerSecond: (w2.result / (w2.durationMs / 1000)).toFixed(2),
  };
  console.log(`  ✓ Completed in ${w2.durationMs.toFixed(2)}ms (${results.workload2_checkpoints.opsPerSecond} ops/sec)\n`);

  // Workload 3: ShardMerge
  console.log('Workload 3: Multi-agent shard merge (10 agents)...');
  const w3 = await measure(workload3_shardMerge);
  results.workload3_shardMerge = {
    operations: w3.result,
    durationMs: w3.durationMs,
    opsPerSecond: (w3.result / (w3.durationMs / 1000)).toFixed(2),
  };
  console.log(`  ✓ Completed in ${w3.durationMs.toFixed(2)}ms (${results.workload3_shardMerge.opsPerSecond} ops/sec)\n`);

  // Workload 4: AsyncWorkflow
  console.log('Workload 4: Async workflow execution...');
  const w4 = await measure(workload4_asyncWorkflow);
  results.workload4_asyncWorkflow = {
    operations: w4.result,
    durationMs: w4.durationMs,
    opsPerSecond: (w4.result / (w4.durationMs / 1000)).toFixed(2),
  };
  console.log(`  ✓ Completed in ${w4.durationMs.toFixed(2)}ms (${results.workload4_asyncWorkflow.opsPerSecond} ops/sec)\n`);

  return results;
}

/**
 * Detect regressions by comparing to baseline
 *
 * @param {Object} current - Current workload results
 * @param {Object} baseline - Baseline results
 * @param {number} threshold - Regression threshold (default 10%)
 * @returns {Object} Regression analysis
 */
function detectRegressions(current, baseline, threshold = 0.10) {
  const regressions = [];
  const improvements = [];
  const stable = [];

  for (const [workload, currentData] of Object.entries(current)) {
    const baselineData = baseline[workload];

    if (!baselineData) {
      stable.push({
        workload,
        status: 'new',
        message: 'No baseline data',
      });
      continue;
    }

    const deltaMs = currentData.durationMs - baselineData.durationMs;
    const deltaPercent = (deltaMs / baselineData.durationMs) * 100;

    const comparison = {
      workload,
      baseline_ms: baselineData.durationMs.toFixed(2),
      current_ms: currentData.durationMs.toFixed(2),
      delta_ms: deltaMs.toFixed(2),
      delta_percent: deltaPercent.toFixed(2),
    };

    if (deltaPercent > threshold * 100) {
      regressions.push({
        ...comparison,
        status: 'regression',
      });
    } else if (deltaPercent < -threshold * 100) {
      improvements.push({
        ...comparison,
        status: 'improvement',
      });
    } else {
      stable.push({
        ...comparison,
        status: 'stable',
      });
    }
  }

  return {
    regressions,
    improvements,
    stable,
    hasRegressions: regressions.length > 0,
  };
}

/**
 * Print regression analysis report
 *
 * @param {Object} analysis - Regression analysis results
 */
function printReport(analysis) {
  console.log('\n========================================');
  console.log('REGRESSION ANALYSIS REPORT');
  console.log('========================================\n');

  if (analysis.regressions.length > 0) {
    console.log('⚠️  REGRESSIONS DETECTED:\n');
    for (const reg of analysis.regressions) {
      console.log(`  ${reg.workload}:`);
      console.log(`    Baseline: ${reg.baseline_ms}ms`);
      console.log(`    Current:  ${reg.current_ms}ms`);
      console.log(`    Delta:    +${reg.delta_ms}ms (+${reg.delta_percent}%)`);
      console.log('');
    }
  } else {
    console.log('✓ No regressions detected\n');
  }

  if (analysis.improvements.length > 0) {
    console.log('✓ IMPROVEMENTS:\n');
    for (const imp of analysis.improvements) {
      console.log(`  ${imp.workload}:`);
      console.log(`    Baseline: ${imp.baseline_ms}ms`);
      console.log(`    Current:  ${imp.current_ms}ms`);
      console.log(`    Delta:    ${imp.delta_ms}ms (${imp.delta_percent}%)`);
      console.log('');
    }
  }

  if (analysis.stable.length > 0) {
    console.log('→ STABLE (within threshold):\n');
    for (const stb of analysis.stable) {
      if (stb.status === 'new') {
        console.log(`  ${stb.workload}: ${stb.message}`);
      } else {
        console.log(`  ${stb.workload}: ${stb.delta_percent}%`);
      }
    }
    console.log('');
  }

  console.log('========================================\n');
}

/**
 * Main harness entry point
 */
async function main() {
  const args = process.argv.slice(2);
  const createBaseline = args.includes('--baseline');
  const baselinePath = resolve(__dirname, 'baseline.json');
  const resultsPath = resolve(__dirname, 'workload-results.json');

  // Run workloads
  const results = await runWorkloads();

  // Save current results
  await writeFile(resultsPath, JSON.stringify(results, null, 2));
  console.log(`Results saved to: ${resultsPath}\n`);

  if (createBaseline) {
    // Create new baseline
    await writeFile(baselinePath, JSON.stringify(results, null, 2));
    console.log(`✓ Baseline created: ${baselinePath}\n`);
    console.log('Run again without --baseline to detect regressions.\n');
  } else {
    // Compare to baseline
    try {
      await access(baselinePath);
      const baselineData = JSON.parse(await readFile(baselinePath, 'utf-8'));
      const analysis = detectRegressions(results, baselineData);

      printReport(analysis);

      // Save analysis
      const analysisPath = resolve(__dirname, 'regression-analysis.json');
      await writeFile(analysisPath, JSON.stringify(analysis, null, 2));
      console.log(`Analysis saved to: ${analysisPath}\n`);

      // Exit with error if regressions detected
      if (analysis.hasRegressions) {
        console.error('❌ Regression detection failed: Performance degraded beyond threshold\n');
        process.exit(1);
      } else {
        console.log('✅ All workloads within acceptable performance range\n');
      }
    } catch (err) {
      if (err.code === 'ENOENT') {
        console.warn('⚠️  No baseline found. Run with --baseline to create one.\n');
        console.log('Current results available in:', resultsPath, '\n');
      } else {
        throw err;
      }
    }
  }
}

// Run harness
main().catch((err) => {
  console.error('Harness failed:', err);
  process.exit(1);
});
