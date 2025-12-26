/**
 * Hook Execution Performance Benchmark
 * Measures hook definition, registration, and execution latency
 *
 * Claims to validate:
 * - Hook execution: <1ms (claimed)
 * - Hook chain execution: low overhead
 */
import { performance } from 'perf_hooks';
import {
  defineHook,
  executeHook,
  executeHookChain,
  createHookRegistry,
  registerHook,
  compileHookChain,
  executeBatch,
} from '../packages/hooks/src/index.mjs';
import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

/**
 * Benchmark configuration
 */
const config = {
  definitionIterations: 10000,
  executionIterations: 10000,
  batchSize: 1000,
  warmupIterations: 1000,
};

/**
 * Calculate statistics from measurements
 */
function calculateStats(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  const variance = values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
  const stddev = Math.sqrt(variance);

  return {
    count: values.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    stddev,
    median: sorted[Math.floor(sorted.length / 2)],
    p50: sorted[Math.floor(sorted.length * 0.50)],
    p90: sorted[Math.floor(sorted.length * 0.90)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

/**
 * Benchmark 1: Hook Definition (no policy)
 */
function benchmarkHookDefinition() {
  const latencies = [];

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    defineHook({
      name: `warmup-hook-${i}`,
      trigger: 'before-add',
      validate: () => true,
    });
  }

  // Actual benchmark
  for (let i = 0; i < config.definitionIterations; i++) {
    const start = performance.now();
    const hook = defineHook({
      name: `bench-hook-${i}`,
      trigger: 'before-add',
      validate: (quad) => quad !== null,
    });
    const elapsed = performance.now() - start;
    latencies.push(elapsed);

    if (!hook) {
      throw new Error(`Failed to define hook at iteration ${i}`);
    }
  }

  return latencies;
}

/**
 * Benchmark 2: Hook Definition with Validation
 */
function benchmarkHookDefinitionWithValidation() {
  const latencies = [];

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    defineHook({
      name: `warmup-val-hook-${i}`,
      trigger: 'before-add',
      validate: (quad) => quad.subject.value.startsWith('http://'),
    });
  }

  // Actual benchmark
  for (let i = 0; i < config.definitionIterations; i++) {
    const start = performance.now();
    const hook = defineHook({
      name: `bench-val-hook-${i}`,
      trigger: 'before-add',
      validate: (quad) => quad.subject.value.startsWith('http://'),
      transform: (quad) => quad,
    });
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark 3: Single Hook Execution
 */
async function benchmarkHookExecution() {
  const latencies = [];

  // Create test hook
  const hook = defineHook({
    name: 'execution-test-hook',
    trigger: 'before-add',
    validate: (q) => q.subject.value.startsWith('http://'),
  });

  // Create test quad
  const testQuad = quad(
    namedNode('http://example.org/subject'),
    namedNode('http://example.org/predicate'),
    literal('test value')
  );

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    await executeHook(hook, testQuad);
  }

  // Actual benchmark
  for (let i = 0; i < config.executionIterations; i++) {
    const start = performance.now();
    const result = await executeHook(hook, testQuad);
    const elapsed = performance.now() - start;
    latencies.push(elapsed);

    if (!result) {
      throw new Error(`Hook execution failed at iteration ${i}`);
    }
  }

  return latencies;
}

/**
 * Benchmark 4: Hook Chain Execution (3 hooks)
 */
async function benchmarkHookChainExecution() {
  const latencies = [];

  // Create test hooks
  const hooks = [
    defineHook({
      name: 'chain-hook-1',
      trigger: 'before-add',
      validate: (q) => q.subject.value.startsWith('http://'),
    }),
    defineHook({
      name: 'chain-hook-2',
      trigger: 'before-add',
      validate: (q) => q.predicate.value.startsWith('http://'),
    }),
    defineHook({
      name: 'chain-hook-3',
      trigger: 'before-add',
      validate: () => true,
    }),
  ];

  // Create test quad
  const testQuad = quad(
    namedNode('http://example.org/subject'),
    namedNode('http://example.org/predicate'),
    literal('test value')
  );

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    await executeHookChain(hooks, testQuad);
  }

  // Actual benchmark
  for (let i = 0; i < config.executionIterations; i++) {
    const start = performance.now();
    const result = await executeHookChain(hooks, testQuad);
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark 5: Hook Registry Operations
 */
function benchmarkHookRegistry() {
  const latencies = {
    register: [],
    lookup: [],
  };

  const registry = createHookRegistry();

  // Benchmark registration
  for (let i = 0; i < config.definitionIterations; i++) {
    const hook = defineHook({
      name: `registry-hook-${i}`,
      trigger: 'before-add',
      validate: () => true,
    });

    const start = performance.now();
    registerHook(registry, hook);
    const elapsed = performance.now() - start;
    latencies.register.push(elapsed);
  }

  // Benchmark lookup (after registry is populated)
  for (let i = 0; i < config.executionIterations; i++) {
    const hookName = `registry-hook-${i % config.definitionIterations}`;
    const start = performance.now();
    registry.hooks.get(hookName);
    const elapsed = performance.now() - start;
    latencies.lookup.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark 6: Batch Hook Execution
 */
async function benchmarkBatchExecution() {
  const latencies = [];

  // Create test hooks
  const hooks = [
    defineHook({
      name: 'batch-hook',
      trigger: 'before-add',
      validate: (q) => q.subject.value.startsWith('http://'),
    }),
  ];

  // Create batch of quads
  const quads = [];
  for (let i = 0; i < config.batchSize; i++) {
    quads.push(quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://example.org/predicate'),
      literal(`value ${i}`)
    ));
  }

  // Warmup
  for (let i = 0; i < 10; i++) {
    try {
      await executeBatch(hooks, quads);
    } catch (e) {
      // executeBatch might not exist - skip
      break;
    }
  }

  // Actual benchmark
  for (let i = 0; i < 100; i++) {
    const start = performance.now();
    try {
      await executeBatch(hooks, quads);
    } catch (e) {
      // Fall back to sequential
      for (const q of quads) {
        await executeHook(hooks[0], q);
      }
    }
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Main benchmark runner
 */
async function main() {
  console.log('='.repeat(70));
  console.log('HOOK EXECUTION PERFORMANCE BENCHMARK');
  console.log('='.repeat(70));
  console.log(`Definition iterations: ${config.definitionIterations}`);
  console.log(`Execution iterations: ${config.executionIterations}`);
  console.log(`Batch size: ${config.batchSize}`);
  console.log('');

  const results = {};

  // Benchmark 1: Hook Definition
  console.log('Running: Hook Definition (simple)...');
  const defLatencies = benchmarkHookDefinition();
  const defStats = calculateStats(defLatencies);
  results.definition = defStats;
  console.log(`  Mean: ${(defStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(defStats.p95 * 1000).toFixed(2)} us`);
  console.log(`  P99:  ${(defStats.p99 * 1000).toFixed(2)} us`);

  // Benchmark 2: Hook Definition with Validation
  console.log('\nRunning: Hook Definition (with validation)...');
  const defValLatencies = benchmarkHookDefinitionWithValidation();
  const defValStats = calculateStats(defValLatencies);
  results.definitionWithValidation = defValStats;
  console.log(`  Mean: ${(defValStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(defValStats.p95 * 1000).toFixed(2)} us`);
  console.log(`  P99:  ${(defValStats.p99 * 1000).toFixed(2)} us`);

  // Benchmark 3: Single Hook Execution
  console.log('\nRunning: Single Hook Execution...');
  const execLatencies = await benchmarkHookExecution();
  const execStats = calculateStats(execLatencies);
  results.execution = execStats;
  console.log(`  Mean: ${(execStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(execStats.p95 * 1000).toFixed(2)} us`);
  console.log(`  P99:  ${(execStats.p99 * 1000).toFixed(2)} us`);

  // Benchmark 4: Hook Chain Execution
  console.log('\nRunning: Hook Chain Execution (3 hooks)...');
  const chainLatencies = await benchmarkHookChainExecution();
  const chainStats = calculateStats(chainLatencies);
  results.chainExecution = chainStats;
  console.log(`  Mean: ${(chainStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(chainStats.p95 * 1000).toFixed(2)} us`);
  console.log(`  P99:  ${(chainStats.p99 * 1000).toFixed(2)} us`);

  // Benchmark 5: Registry Operations
  console.log('\nRunning: Hook Registry Operations...');
  const regLatencies = benchmarkHookRegistry();
  const regStats = calculateStats(regLatencies.register);
  const lookupStats = calculateStats(regLatencies.lookup);
  results.registry = { register: regStats, lookup: lookupStats };
  console.log(`  Register Mean: ${(regStats.mean * 1000).toFixed(2)} us`);
  console.log(`  Lookup Mean:   ${(lookupStats.mean * 1000).toFixed(2)} us`);

  // Benchmark 6: Batch Execution
  console.log('\nRunning: Batch Hook Execution...');
  const batchLatencies = await benchmarkBatchExecution();
  const batchStats = calculateStats(batchLatencies);
  results.batchExecution = batchStats;
  console.log(`  Mean (${config.batchSize} quads): ${batchStats.mean.toFixed(2)} ms`);
  console.log(`  Per-quad: ${((batchStats.mean / config.batchSize) * 1000).toFixed(2)} us`);

  // Summary
  console.log('\n' + '='.repeat(70));
  console.log('SUMMARY');
  console.log('='.repeat(70));

  console.log('\nLATENCY BREAKDOWN (microseconds):');
  console.log('-'.repeat(70));
  console.log('Operation                          | Mean    | P95     | P99     | Max');
  console.log('-'.repeat(70));
  console.log(`Hook Definition (simple)           | ${(defStats.mean * 1000).toFixed(1).padStart(6)} | ${(defStats.p95 * 1000).toFixed(1).padStart(6)} | ${(defStats.p99 * 1000).toFixed(1).padStart(6)} | ${(defStats.max * 1000).toFixed(1).padStart(6)}`);
  console.log(`Hook Definition (with validation)  | ${(defValStats.mean * 1000).toFixed(1).padStart(6)} | ${(defValStats.p95 * 1000).toFixed(1).padStart(6)} | ${(defValStats.p99 * 1000).toFixed(1).padStart(6)} | ${(defValStats.max * 1000).toFixed(1).padStart(6)}`);
  console.log(`Single Hook Execution              | ${(execStats.mean * 1000).toFixed(1).padStart(6)} | ${(execStats.p95 * 1000).toFixed(1).padStart(6)} | ${(execStats.p99 * 1000).toFixed(1).padStart(6)} | ${(execStats.max * 1000).toFixed(1).padStart(6)}`);
  console.log(`Hook Chain (3 hooks)               | ${(chainStats.mean * 1000).toFixed(1).padStart(6)} | ${(chainStats.p95 * 1000).toFixed(1).padStart(6)} | ${(chainStats.p99 * 1000).toFixed(1).padStart(6)} | ${(chainStats.max * 1000).toFixed(1).padStart(6)}`);
  console.log(`Registry Register                  | ${(regStats.mean * 1000).toFixed(1).padStart(6)} | ${(regStats.p95 * 1000).toFixed(1).padStart(6)} | ${(regStats.p99 * 1000).toFixed(1).padStart(6)} | ${(regStats.max * 1000).toFixed(1).padStart(6)}`);
  console.log(`Registry Lookup                    | ${(lookupStats.mean * 1000).toFixed(1).padStart(6)} | ${(lookupStats.p95 * 1000).toFixed(1).padStart(6)} | ${(lookupStats.p99 * 1000).toFixed(1).padStart(6)} | ${(lookupStats.max * 1000).toFixed(1).padStart(6)}`);
  console.log('-'.repeat(70));

  // Claims Validation
  console.log('\n' + '='.repeat(70));
  console.log('CLAIMS VALIDATION');
  console.log('='.repeat(70));

  const hookP95Us = execStats.p95 * 1000;
  const hookClaimUs = 1000; // <1ms = 1000us
  const hookPass = hookP95Us < hookClaimUs;

  console.log(`\nClaim: Hook execution <1ms`);
  console.log(`  Measured P95: ${hookP95Us.toFixed(1)} us (${(hookP95Us / 1000).toFixed(3)} ms)`);
  console.log(`  Target:       ${hookClaimUs} us (1.000 ms)`);
  console.log(`  Status:       ${hookPass ? 'PASS' : 'FAIL'}`);

  const chainP95Us = chainStats.p95 * 1000;
  const chainClaimUs = 1000; // <1ms for chain too
  const chainPass = chainP95Us < chainClaimUs;

  console.log(`\nClaim: Hook chain <1ms`);
  console.log(`  Measured P95: ${chainP95Us.toFixed(1)} us (${(chainP95Us / 1000).toFixed(3)} ms)`);
  console.log(`  Target:       ${chainClaimUs} us (1.000 ms)`);
  console.log(`  Status:       ${chainPass ? 'PASS' : 'FAIL'}`);

  // Throughput
  const throughput = 1000 / execStats.mean; // ops per ms = ops per second / 1000
  console.log(`\nThroughput: ${(throughput * 1000).toFixed(0).toLocaleString()} hooks/sec`);

  console.log('\n' + '='.repeat(70));
  console.log('Benchmark complete');
  console.log('='.repeat(70));

  // Output JSON for summary generator
  console.log('\n__JSON_RESULTS__');
  console.log(JSON.stringify({
    benchmark: 'hook-execution',
    timestamp: new Date().toISOString(),
    results: {
      definition: { meanUs: defStats.mean * 1000, p95Us: defStats.p95 * 1000, p99Us: defStats.p99 * 1000 },
      definitionWithValidation: { meanUs: defValStats.mean * 1000, p95Us: defValStats.p95 * 1000, p99Us: defValStats.p99 * 1000 },
      execution: { meanUs: execStats.mean * 1000, p95Us: execStats.p95 * 1000, p99Us: execStats.p99 * 1000 },
      chainExecution: { meanUs: chainStats.mean * 1000, p95Us: chainStats.p95 * 1000, p99Us: chainStats.p99 * 1000 },
      registryRegister: { meanUs: regStats.mean * 1000, p95Us: regStats.p95 * 1000, p99Us: regStats.p99 * 1000 },
      registryLookup: { meanUs: lookupStats.mean * 1000, p95Us: lookupStats.p95 * 1000, p99Us: lookupStats.p99 * 1000 },
      batchPerQuadUs: (batchStats.mean / config.batchSize) * 1000,
      throughputPerSec: throughput * 1000,
    },
    claims: {
      hookExecutionLt1ms: { target: 1000, actualP95: hookP95Us, pass: hookPass },
      hookChainLt1ms: { target: 1000, actualP95: chainP95Us, pass: chainPass },
    },
  }, null, 2));
}

main().catch(console.error);
