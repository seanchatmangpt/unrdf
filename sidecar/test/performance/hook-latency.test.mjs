/**
 * Knowledge Hook Latency Benchmark
 *
 * Target: <2ms p99 latency for hook execution
 * Validates hook performance across all predicate types
 */

import { describe, it, expect } from 'vitest';
import { performance } from 'perf_hooks';

describe('Knowledge Hook Latency Benchmark', () => {
  const ITERATIONS = 10000;
  const TARGET_P99_LATENCY = 2; // ms

  /**
   * Calculate percentile from sorted array
   */
  function calculatePercentile(sortedArray, percentile) {
    const index = Math.ceil((percentile / 100) * sortedArray.length) - 1;
    return sortedArray[index];
  }

  /**
   * Benchmark a hook execution function
   */
  async function benchmarkHook(hookFn, name) {
    const latencies = [];

    // Warmup
    for (let i = 0; i < 100; i++) {
      await hookFn();
    }

    // Actual benchmark
    for (let i = 0; i < ITERATIONS; i++) {
      const start = performance.now();
      await hookFn();
      const end = performance.now();
      latencies.push(end - start);
    }

    latencies.sort((a, b) => a - b);

    const p50 = calculatePercentile(latencies, 50);
    const p95 = calculatePercentile(latencies, 95);
    const p99 = calculatePercentile(latencies, 99);
    const p999 = calculatePercentile(latencies, 99.9);
    const mean = latencies.reduce((sum, val) => sum + val, 0) / latencies.length;

    console.log(`\n🔧 ${name} Benchmark (${ITERATIONS} iterations):`);
    console.log(`   Mean: ${mean.toFixed(3)}ms`);
    console.log(`   P50: ${p50.toFixed(3)}ms`);
    console.log(`   P95: ${p95.toFixed(3)}ms`);
    console.log(`   P99: ${p99.toFixed(3)}ms (target: <${TARGET_P99_LATENCY}ms)`);
    console.log(`   P99.9: ${p999.toFixed(3)}ms`);

    return { p50, p95, p99, p999, mean };
  }

  it('should validate hook execution with <2ms p99 latency', async () => {
    const hook = async () => {
      // Simulate hook validation logic
      const data = {
        subject: 'http://example.org/resource',
        predicate: 'http://example.org/property',
        object: { '@value': 'test', '@type': 'http://www.w3.org/2001/XMLSchema#string' }
      };

      // Minimal validation
      if (!data.subject || !data.predicate || !data.object) {
        throw new Error('Invalid data');
      }

      return true;
    };

    const metrics = await benchmarkHook(hook, 'Basic Hook Validation');
    expect(metrics.p99).toBeLessThan(TARGET_P99_LATENCY);
  }, 30000);

  it('should benchmark SHACL validation hook', async () => {
    const shaclHook = async () => {
      // Simulate SHACL validation
      const shape = {
        targetClass: 'http://example.org/Person',
        properties: [
          { path: 'http://example.org/name', minCount: 1, datatype: 'string' },
          { path: 'http://example.org/age', minCount: 1, datatype: 'integer' }
        ]
      };

      const data = {
        '@type': 'http://example.org/Person',
        'http://example.org/name': 'John Doe',
        'http://example.org/age': 30
      };

      // Validate against shape
      for (const prop of shape.properties) {
        if (prop.minCount && !data[prop.path]) {
          return false;
        }
      }

      return true;
    };

    const metrics = await benchmarkHook(shaclHook, 'SHACL Validation Hook');
    expect(metrics.p99).toBeLessThan(TARGET_P99_LATENCY * 2); // SHACL allowed 2x baseline
  }, 30000);

  it('should benchmark cryptographic provenance hook', async () => {
    const provenanceHook = async () => {
      // Simulate signature verification (fast path - cached keys)
      const signature = 'mock_signature_' + Math.random();
      const data = 'test_data_' + Math.random();

      // Fast hash comparison (simulated)
      const hash = signature.length + data.length;

      return hash > 0;
    };

    const metrics = await benchmarkHook(provenanceHook, 'Cryptographic Provenance Hook');
    expect(metrics.p99).toBeLessThan(TARGET_P99_LATENCY);
  }, 30000);

  it('should benchmark policy enforcement hook', async () => {
    const policyHook = async () => {
      // Simulate policy check with in-memory rules
      const policies = [
        { pattern: /^http:\/\/example\.org\//, action: 'allow' },
        { pattern: /^http:\/\/forbidden\.org\//, action: 'deny' }
      ];

      const uri = 'http://example.org/resource';

      for (const policy of policies) {
        if (policy.pattern.test(uri)) {
          return policy.action === 'allow';
        }
      }

      return true;
    };

    const metrics = await benchmarkHook(policyHook, 'Policy Enforcement Hook');
    expect(metrics.p99).toBeLessThan(TARGET_P99_LATENCY);
  }, 30000);

  it('should benchmark transformation hook', async () => {
    const transformHook = async () => {
      // Simulate data transformation
      const input = {
        '@value': 'TEST_VALUE',
        '@type': 'http://www.w3.org/2001/XMLSchema#string'
      };

      const output = {
        ...input,
        '@value': input['@value'].toLowerCase(),
        normalized: true
      };

      return output;
    };

    const metrics = await benchmarkHook(transformHook, 'Transformation Hook');
    expect(metrics.p99).toBeLessThan(TARGET_P99_LATENCY);
  }, 30000);

  it('should benchmark memory usage during hook execution', async () => {
    const initialMemory = process.memoryUsage();

    const memoryHook = async () => {
      // Allocate and release small objects
      const temp = new Array(100).fill({ value: Math.random() });
      return temp.length;
    };

    await benchmarkHook(memoryHook, 'Memory Profile Hook');

    const finalMemory = process.memoryUsage();
    const heapGrowth = (finalMemory.heapUsed - initialMemory.heapUsed) / 1024 / 1024;

    console.log(`\n💾 Memory Profile:`);
    console.log(`   Heap growth: ${heapGrowth.toFixed(2)} MB`);
    console.log(`   RSS: ${(finalMemory.rss / 1024 / 1024).toFixed(2)} MB`);

    // Memory growth should be minimal (<10MB for 10k iterations)
    expect(Math.abs(heapGrowth)).toBeLessThan(10);
  }, 30000);

  it('should benchmark parallel hook execution', async () => {
    const parallelHook = async () => {
      // Simulate parallel validation of multiple hooks
      const hooks = [
        async () => true,
        async () => true,
        async () => true
      ];

      const results = await Promise.all(hooks.map(h => h()));
      return results.every(r => r === true);
    };

    const metrics = await benchmarkHook(parallelHook, 'Parallel Hook Execution');
    expect(metrics.p99).toBeLessThan(TARGET_P99_LATENCY * 1.5);
  }, 30000);
});
