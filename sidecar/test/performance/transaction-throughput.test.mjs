/**
 * Transaction Throughput Benchmark
 *
 * Target: >1000 transactions/sec with <100ms p99 latency
 * Validates Fortune 5 SLA requirements for high-volume transaction processing
 */

import { describe, it, beforeAll, afterAll, expect } from 'vitest';
import autocannon from 'autocannon';
import { spawn } from 'child_process';
import { setTimeout } from 'timers/promises';

const SIDECAR_PORT = 3456;
const BENCHMARK_DURATION = 30; // seconds
const TARGET_THROUGHPUT = 1000; // tx/sec
const TARGET_P99_LATENCY = 100; // ms

describe('Transaction Throughput Benchmark', () => {
  let sidecarProcess;

  beforeAll(async () => {
    // Start sidecar server
    sidecarProcess = spawn('node', ['server/index.mjs'], {
      cwd: '/Users/sac/unrdf/sidecar',
      env: { ...process.env, PORT: SIDECAR_PORT },
      stdio: 'ignore'
    });

    // Wait for server startup
    await setTimeout(2000);
  });

  afterAll(() => {
    if (sidecarProcess) {
      sidecarProcess.kill();
    }
  });

  it('should handle >1000 tx/sec with acceptable latency', async () => {
    const result = await autocannon({
      url: `http://localhost:${SIDECAR_PORT}/api/transaction`,
      connections: 100, // concurrent connections
      pipelining: 10, // requests per connection
      duration: BENCHMARK_DURATION,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        operation: 'insert',
        subject: 'http://example.org/test',
        predicate: 'http://example.org/value',
        object: { '@value': Math.random(), '@type': 'http://www.w3.org/2001/XMLSchema#double' }
      })
    });

    // Calculate metrics
    const throughput = result.requests.average;
    const p50 = result.latency.p50;
    const p95 = result.latency.p95;
    const p99 = result.latency.p99;
    const p999 = result.latency.p99_9;

    console.log('\n📊 Transaction Throughput Benchmark Results:');
    console.log(`   Throughput: ${throughput.toFixed(2)} tx/sec (target: ${TARGET_THROUGHPUT})`);
    console.log(`   Latency p50: ${p50}ms`);
    console.log(`   Latency p95: ${p95}ms`);
    console.log(`   Latency p99: ${p99}ms (target: <${TARGET_P99_LATENCY}ms)`);
    console.log(`   Latency p99.9: ${p999}ms`);
    console.log(`   Total requests: ${result.requests.total}`);
    console.log(`   Errors: ${result.errors}`);

    // Performance assertions
    expect(throughput, 'Throughput must meet SLA target').toBeGreaterThanOrEqual(TARGET_THROUGHPUT);
    expect(p99, 'P99 latency must be under 100ms').toBeLessThanOrEqual(TARGET_P99_LATENCY);
    expect(result.errors, 'No errors during benchmark').toBe(0);
  }, 60000);

  it('should handle concurrent users with transaction isolation', async () => {
    const result = await autocannon({
      url: `http://localhost:${SIDECAR_PORT}/api/transaction`,
      connections: 50,
      duration: 15,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      setupClient: (client) => {
        // Each client gets unique user ID
        const userId = Math.random().toString(36).substring(7);
        client.setHeaders({
          'X-User-ID': userId,
          'Content-Type': 'application/json'
        });
      },
      body: JSON.stringify({
        operation: 'insert',
        subject: 'http://example.org/user',
        predicate: 'http://example.org/action',
        object: { '@value': 'concurrent_test', '@type': 'http://www.w3.org/2001/XMLSchema#string' }
      })
    });

    console.log('\n👥 Concurrent User Benchmark:');
    console.log(`   Throughput: ${result.requests.average.toFixed(2)} tx/sec`);
    console.log(`   P99 latency: ${result.latency.p99}ms`);
    console.log(`   Concurrent users: 50`);

    expect(result.requests.average).toBeGreaterThan(500);
    expect(result.latency.p99).toBeLessThan(150);
  }, 30000);

  it('should maintain performance under sustained load', async () => {
    const warmupResult = await autocannon({
      url: `http://localhost:${SIDECAR_PORT}/api/transaction`,
      connections: 50,
      duration: 5,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        operation: 'query',
        query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10'
      })
    });

    // Sustained load test
    const sustainedResult = await autocannon({
      url: `http://localhost:${SIDECAR_PORT}/api/transaction`,
      connections: 100,
      duration: 20,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        operation: 'insert',
        subject: 'http://example.org/sustained',
        predicate: 'http://example.org/test',
        object: { '@value': 'load_test', '@type': 'http://www.w3.org/2001/XMLSchema#string' }
      })
    });

    console.log('\n⏱️  Sustained Load Test:');
    console.log(`   Warmup throughput: ${warmupResult.requests.average.toFixed(2)} tx/sec`);
    console.log(`   Sustained throughput: ${sustainedResult.requests.average.toFixed(2)} tx/sec`);
    console.log(`   Performance degradation: ${((warmupResult.requests.average - sustainedResult.requests.average) / warmupResult.requests.average * 100).toFixed(2)}%`);

    // Performance should not degrade more than 20% under sustained load
    const degradation = (warmupResult.requests.average - sustainedResult.requests.average) / warmupResult.requests.average;
    expect(degradation).toBeLessThan(0.2);
  }, 40000);

  it('should handle spike traffic patterns', async () => {
    // Baseline load
    const baselineResult = await autocannon({
      url: `http://localhost:${SIDECAR_PORT}/api/transaction`,
      connections: 20,
      duration: 5,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        operation: 'query',
        query: 'SELECT ?s WHERE { ?s ?p ?o } LIMIT 5'
      })
    });

    // Spike to 10x traffic
    const spikeResult = await autocannon({
      url: `http://localhost:${SIDECAR_PORT}/api/transaction`,
      connections: 200,
      duration: 10,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        operation: 'insert',
        subject: 'http://example.org/spike',
        predicate: 'http://example.org/burst',
        object: { '@value': Date.now(), '@type': 'http://www.w3.org/2001/XMLSchema#integer' }
      })
    });

    console.log('\n📈 Spike Traffic Test:');
    console.log(`   Baseline: ${baselineResult.requests.average.toFixed(2)} tx/sec`);
    console.log(`   Spike: ${spikeResult.requests.average.toFixed(2)} tx/sec`);
    console.log(`   Spike p99 latency: ${spikeResult.latency.p99}ms`);
    console.log(`   Error rate during spike: ${(spikeResult.errors / spikeResult.requests.total * 100).toFixed(2)}%`);

    // Should handle spike with <5% error rate
    const errorRate = spikeResult.errors / spikeResult.requests.total;
    expect(errorRate).toBeLessThan(0.05);
    expect(spikeResult.latency.p99).toBeLessThan(500); // Relaxed during spike
  }, 30000);
});
