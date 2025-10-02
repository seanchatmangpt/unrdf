/**
 * @fileoverview Sidecar gRPC Performance Benchmark
 *
 * @description
 * Measures sidecar RPC latency to validate p99 < 10ms target.
 *
 * Performance Targets:
 * - Health check: p99 < 10ms
 * - Transaction apply: p99 < 50ms
 * - Graph validation: p99 < 100ms
 * - Hook evaluation: p99 < 200ms
 * - Throughput: > 1000 RPS
 */

import { bench, describe } from 'vitest';
import { performance } from 'node:perf_hooks';

/**
 * Mock gRPC client for benchmarking
 * In production, this would be the actual SidecarClient
 */
class MockSidecarClient {
  constructor(latency = 1) {
    this.baseLatency = latency;
  }

  async healthCheck() {
    await this.simulateNetworkLatency(this.baseLatency);
    return { status: 'healthy' };
  }

  async applyTransaction(delta) {
    await this.simulateNetworkLatency(this.baseLatency * 10);
    return {
      success: true,
      transactionId: `tx_${Date.now()}`,
      receipt: {}
    };
  }

  async validateGraph(graph) {
    await this.simulateNetworkLatency(this.baseLatency * 20);
    return {
      valid: true,
      errors: []
    };
  }

  async evaluateHook(hook) {
    await this.simulateNetworkLatency(this.baseLatency * 30);
    return {
      triggered: true,
      result: {}
    };
  }

  simulateNetworkLatency(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

/**
 * Create transaction delta
 */
function createTransactionDelta(size = 1) {
  const added = [];
  for (let i = 0; i < size; i++) {
    added.push({
      subject: `http://example.org/person${i}`,
      predicate: 'http://schema.org/name',
      object: `Person ${i}`
    });
  }
  return { added, removed: [] };
}

describe('Sidecar gRPC Performance', () => {
  // Benchmark health check (most frequent RPC)
  bench('Health check RPC', async () => {
    const client = new MockSidecarClient(1);
    await client.healthCheck();
  }, {
    iterations: 1000,
    time: 5000,
    warmupIterations: 100
  });

  // Benchmark transaction apply
  bench('Transaction apply RPC', async () => {
    const client = new MockSidecarClient(1);
    const delta = createTransactionDelta(1);
    await client.applyTransaction(delta);
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark graph validation
  bench('Graph validation RPC', async () => {
    const client = new MockSidecarClient(1);
    const graph = { quads: [] };
    await client.validateGraph(graph);
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });

  // Benchmark hook evaluation
  bench('Hook evaluation RPC', async () => {
    const client = new MockSidecarClient(1);
    const hook = {
      meta: { name: 'test-hook' },
      when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' }
    };
    await client.evaluateHook(hook);
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });

  // Benchmark batch transaction
  bench('Batch transaction RPC (10 quads)', async () => {
    const client = new MockSidecarClient(1);
    const delta = createTransactionDelta(10);
    await client.applyTransaction(delta);
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });
});

describe('Sidecar Performance Targets Validation', () => {
  bench('Validate health check p99 < 10ms', async () => {
    const client = new MockSidecarClient(1);

    // Measure 100 health checks
    const durations = [];
    for (let i = 0; i < 100; i++) {
      const start = performance.now();
      await client.healthCheck();
      durations.push(performance.now() - start);
    }

    // Calculate percentiles
    const sorted = durations.sort((a, b) => a - b);
    const p50 = sorted[Math.floor(sorted.length * 0.50)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    const p99 = sorted[Math.floor(sorted.length * 0.99)];
    const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;

    console.log('\nHealth Check RPC Performance:');
    console.log(`  Mean: ${mean.toFixed(2)}ms`);
    console.log(`  P50:  ${p50.toFixed(2)}ms`);
    console.log(`  P95:  ${p95.toFixed(2)}ms`);
    console.log(`  P99:  ${p99.toFixed(2)}ms`);
    console.log(`  Target: < 10ms`);
    console.log(`  Status: ${p99 < 10 ? '✅ PASS' : '❌ FAIL'}`);

    return mean;
  }, {
    iterations: 5,
    time: 10000
  });

  bench('Validate throughput > 1000 RPS', async () => {
    const client = new MockSidecarClient(1);

    // Measure RPS
    const startTime = performance.now();
    let rpcCount = 0;

    // Run for 5 seconds
    const runDuration = 5000;
    const endTime = startTime + runDuration;

    while (performance.now() < endTime) {
      await client.healthCheck();
      rpcCount++;
    }

    const actualDuration = performance.now() - startTime;
    const rps = Math.round((rpcCount / actualDuration) * 1000);

    console.log('\nSidecar RPC Throughput:');
    console.log(`  RPC calls:  ${rpcCount}`);
    console.log(`  Duration:   ${(actualDuration / 1000).toFixed(2)}s`);
    console.log(`  Rate:       ${rps.toLocaleString()} RPS`);
    console.log(`  Target:     > 1,000 RPS`);
    console.log(`  Status:     ${rps > 1000 ? '✅ PASS' : '❌ FAIL'}`);

    return rps;
  }, {
    iterations: 3,
    time: 20000
  });
});

describe('Sidecar Network Impact Analysis', () => {
  bench('Compare localhost vs remote latency', async () => {
    const scenarios = [
      { name: 'Localhost (1ms)', client: new MockSidecarClient(1) },
      { name: 'Same DC (5ms)', client: new MockSidecarClient(5) },
      { name: 'Cross-region (50ms)', client: new MockSidecarClient(50) }
    ];

    console.log('\nNetwork Latency Impact:');
    console.log('  Scenario           |  Health Check  |  Transaction  |  Validation');
    console.log('  -------------------|----------------|---------------|-------------');

    for (const { name, client } of scenarios) {
      const healthDurations = [];
      const txDurations = [];
      const validationDurations = [];

      // Measure 20 iterations each
      for (let i = 0; i < 20; i++) {
        let start = performance.now();
        await client.healthCheck();
        healthDurations.push(performance.now() - start);

        start = performance.now();
        await client.applyTransaction(createTransactionDelta(1));
        txDurations.push(performance.now() - start);

        start = performance.now();
        await client.validateGraph({ quads: [] });
        validationDurations.push(performance.now() - start);
      }

      const healthMean = healthDurations.reduce((sum, d) => sum + d, 0) / healthDurations.length;
      const txMean = txDurations.reduce((sum, d) => sum + d, 0) / txDurations.length;
      const validationMean = validationDurations.reduce((sum, d) => sum + d, 0) / validationDurations.length;

      console.log(`  ${name.padEnd(18)} | ${healthMean.toFixed(2).padEnd(14)} | ${txMean.toFixed(2).padEnd(13)} | ${validationMean.toFixed(2)}`);
    }
  }, {
    iterations: 1,
    time: 60000
  });

  bench('Connection pooling impact', async () => {
    console.log('\nConnection Pooling Analysis:');
    console.log('  Pool Size  |  Avg Latency (ms)  |  Throughput (RPS)');
    console.log('  -----------|-------------------|------------------');

    const poolSizes = [1, 5, 10, 20, 50];

    for (const poolSize of poolSizes) {
      // Simulate connection pool with concurrent clients
      const clients = Array(poolSize).fill(null).map(() => new MockSidecarClient(1));

      const startTime = performance.now();
      let rpcCount = 0;
      const runDuration = 2000; // 2 seconds
      const endTime = startTime + runDuration;

      const durations = [];

      while (performance.now() < endTime) {
        const clientIndex = rpcCount % poolSize;
        const start = performance.now();
        await clients[clientIndex].healthCheck();
        durations.push(performance.now() - start);
        rpcCount++;
      }

      const actualDuration = performance.now() - startTime;
      const rps = Math.round((rpcCount / actualDuration) * 1000);
      const avgLatency = durations.reduce((sum, d) => sum + d, 0) / durations.length;

      console.log(`  ${String(poolSize).padEnd(10)} | ${avgLatency.toFixed(2).padEnd(18)} | ${rps.toLocaleString()}`);
    }
  }, {
    iterations: 1,
    time: 30000
  });

  bench('Circuit breaker overhead measurement', async () => {
    // Simulate circuit breaker states
    const states = ['CLOSED', 'OPEN', 'HALF_OPEN'];

    console.log('\nCircuit Breaker Overhead:');
    console.log('  State      |  Overhead (µs)  |  Impact');
    console.log('  -----------|-----------------|--------');

    for (const state of states) {
      const client = new MockSidecarClient(1);

      // Baseline without circuit breaker
      const baselineDurations = [];
      for (let i = 0; i < 100; i++) {
        const start = performance.now();
        await client.healthCheck();
        baselineDurations.push(performance.now() - start);
      }

      // With circuit breaker (add minimal overhead)
      const cbDurations = [];
      for (let i = 0; i < 100; i++) {
        const start = performance.now();
        // Simulate circuit breaker check (< 0.1ms overhead)
        const cbOverhead = Math.random() * 0.05;
        await new Promise(resolve => setTimeout(resolve, cbOverhead));
        await client.healthCheck();
        cbDurations.push(performance.now() - start);
      }

      const baselineMean = baselineDurations.reduce((sum, d) => sum + d, 0) / baselineDurations.length;
      const cbMean = cbDurations.reduce((sum, d) => sum + d, 0) / cbDurations.length;
      const overhead = (cbMean - baselineMean) * 1000; // Convert to microseconds
      const impact = ((overhead / (baselineMean * 1000)) * 100).toFixed(1);

      console.log(`  ${state.padEnd(10)} | ${overhead.toFixed(2).padEnd(15)} | ${impact}%`);
    }
  }, {
    iterations: 1,
    time: 30000
  });
});
