#!/usr/bin/env node
/**
 * @file Knowledge Hook Performance Benchmarking
 * @description Comprehensive benchmark suite for all hook configurations
 */

import { dataFactory } from '@unrdf/oxigraph';
import { defineHook } from '../packages/hooks/src/hooks/define-hook.mjs';
import { executeHook } from '../packages/hooks/src/hooks/hook-executor.mjs';
import fs from 'fs';
import path from 'path';

const { namedNode, literal, quad } = dataFactory;

/**
 * Generate test data of various sizes
 */
function generateTestData(size) {
  const quads = [];
  for (let i = 0; i < size; i++) {
    quads.push(
      quad(
        namedNode(`http://example.org/subject${i}`),
        namedNode('http://example.org/predicate'),
        literal(`value${i}`)
      )
    );
  }
  return quads;
}

/**
 * Measure memory usage
 */
function measureMemory() {
  const usage = process.memoryUsage();
  return {
    heapUsed: usage.heapUsed / 1024 / 1024, // MB
    heapTotal: usage.heapTotal / 1024 / 1024, // MB
    external: usage.external / 1024 / 1024, // MB
    rss: usage.rss / 1024 / 1024 // MB
  };
}

/**
 * Calculate statistics
 */
function calculateStats(arr) {
  if (arr.length === 0) return { min: 0, max: 0, mean: 0, median: 0, p95: 0, p99: 0 };

  const sorted = [...arr].sort((a, b) => a - b);
  const sum = arr.reduce((acc, val) => acc + val, 0);

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean: sum / arr.length,
    median: sorted[Math.floor(sorted.length / 2)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)]
  };
}

/**
 * Hook configurations to benchmark
 */
const hookConfigs = {
  'minimal': {
    name: 'MinimalHook',
    description: 'Minimal hook with validation only',
    trigger: 'before-add',
    validate: (_quad) => true
  },
  'validation-only': {
    name: 'ValidationHook',
    description: 'Validation hook with type checking',
    trigger: 'before-add',
    validate: (quad) => {
      return quad && quad.subject && quad.predicate && quad.object;
    }
  },
  'transformation': {
    name: 'TransformHook',
    description: 'Hook that transforms data',
    trigger: 'before-add',
    transform: (q) => {
      if (q.object.termType === 'Literal') {
        return quad(q.subject, q.predicate, literal(q.object.value.toUpperCase()));
      }
      return q;
    }
  },
  'complex-validation': {
    name: 'ComplexValidationHook',
    description: 'Hook with complex validation logic',
    trigger: 'before-add',
    validate: (quad) => {
      // Simulate complex validation
      let valid = true;
      for (let i = 0; i < 100; i++) {
        valid = valid && Math.sqrt(i) > 0;
      }
      return valid && quad.subject.termType === 'NamedNode';
    }
  },
  'transformation-chain': {
    name: 'TransformChainHook',
    description: 'Hook with validation and transformation',
    trigger: 'before-add',
    validate: (quad) => quad.subject.termType === 'NamedNode',
    transform: (q) => {
      if (q.object.termType === 'Literal') {
        return quad(
          q.subject,
          q.predicate,
          literal(q.object.value.trim())
        );
      }
      return q;
    }
  },
  'intensive-compute': {
    name: 'IntensiveComputeHook',
    description: 'Hook with CPU-intensive operations',
    trigger: 'before-add',
    validate: (quad) => {
      // Simulate CPU-intensive work
      let result = 0;
      for (let i = 0; i < 1000; i++) {
        result += Math.sqrt(i) * Math.cos(i);
      }
      return result > 0 && quad.subject.termType === 'NamedNode';
    }
  }
};

/**
 * Benchmark a single hook configuration
 */
async function benchmarkHook(configName, config, testSizes, iterations) {
  console.log(`\n📊 Benchmarking: ${config.name}`);
  console.log(`   Description: ${config.description}`);
  console.log('─'.repeat(60));

  const hook = defineHook(config);
  const results = [];

  for (const size of testSizes) {
    console.log(`   Testing with ${size} quads...`);

    const testData = generateTestData(size);
    const latencies = [];
    const memoryBefore = measureMemory();

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();

      // Execute hook on all quads
      for (const quad of testData) {
        executeHook(hook, quad);
      }

      const duration = performance.now() - start;
      latencies.push(duration);
    }

    const memoryAfter = measureMemory();
    const stats = calculateStats(latencies);
    const throughput = (size * iterations) / (stats.mean / 1000); // quads/sec
    const overhead = ((stats.mean / size)).toFixed(3); // ms per quad

    results.push({
      hookConfig: configName,
      hookName: config.name,
      dataSize: size,
      iterations,
      latency: {
        min: stats.min.toFixed(3),
        max: stats.max.toFixed(3),
        mean: stats.mean.toFixed(3),
        median: stats.median.toFixed(3),
        p95: stats.p95.toFixed(3),
        p99: stats.p99.toFixed(3)
      },
      throughput: throughput.toFixed(2),
      memory: {
        before: memoryBefore.heapUsed.toFixed(2),
        after: memoryAfter.heapUsed.toFixed(2),
        delta: (memoryAfter.heapUsed - memoryBefore.heapUsed).toFixed(2)
      },
      overhead: overhead
    });

    console.log(`      Mean: ${stats.mean.toFixed(3)}ms | Throughput: ${throughput.toFixed(2)} quads/sec | Overhead: ${overhead}ms/quad`);
  }

  return results;
}

/**
 * Compare hook performance across trigger types
 */
async function benchmarkTriggerTypes() {
  console.log('\n📊 Hook Performance by Trigger Type');
  console.log('─'.repeat(60));

  const triggers = ['before-add', 'after-add', 'before-query', 'after-query'];
  const results = [];

  for (const trigger of triggers) {
    const hook = defineHook({
      name: `${trigger}Hook`,
      trigger: trigger,
      validate: (quad) => quad.subject.termType === 'NamedNode'
    });

    const testData = generateTestData(100);
    const latencies = [];

    for (let i = 0; i < 50; i++) {
      const start = performance.now();
      for (const quad of testData) {
        executeHook(hook, quad);
      }
      latencies.push(performance.now() - start);
    }

    const stats = calculateStats(latencies);
    results.push({
      trigger: trigger,
      latency: {
        mean: stats.mean.toFixed(3),
        p95: stats.p95.toFixed(3),
        p99: stats.p99.toFixed(3)
      }
    });

    console.log(`   ${trigger.padEnd(15)}: Mean=${stats.mean.toFixed(3)}ms | P95=${stats.p95.toFixed(3)}ms | P99=${stats.p99.toFixed(3)}ms`);
  }

  return results;
}

/**
 * Main benchmark runner
 */
async function runBenchmarks() {
  console.log('🚀 Knowledge Hook Performance Benchmarking');
  console.log('='.repeat(60));
  console.log(`Start time: ${new Date().toISOString()}`);
  console.log(`Node version: ${process.version}`);
  console.log(`Platform: ${process.platform} ${process.arch}`);

  const testSizes = [10, 50, 100, 500, 1000];
  const iterations = 100;

  const allResults = [];

  // Benchmark each hook configuration
  for (const [configName, config] of Object.entries(hookConfigs)) {
    const results = await benchmarkHook(configName, config, testSizes, iterations);
    allResults.push(...results);
  }

  // Benchmark trigger types
  const triggerResults = await benchmarkTriggerTypes();

  console.log('\n' + '='.repeat(60));
  console.log('✅ Benchmarks Complete!');
  console.log('='.repeat(60));

  // Summary statistics
  console.log('\n📈 Summary Statistics:');
  const allLatencies = allResults.map(r => parseFloat(r.latency.mean));
  const summaryStats = calculateStats(allLatencies);
  console.log(`   Overall Mean Latency: ${summaryStats.mean.toFixed(3)}ms`);
  console.log(`   Overall P95 Latency:  ${summaryStats.p95.toFixed(3)}ms`);
  console.log(`   Overall P99 Latency:  ${summaryStats.p99.toFixed(3)}ms`);

  // Save results
  const output = {
    metadata: {
      timestamp: new Date().toISOString(),
      nodeVersion: process.version,
      platform: `${process.platform} ${process.arch}`,
      testSizes,
      iterations
    },
    hookConfigurations: Object.keys(hookConfigs).map(key => ({
      name: key,
      description: hookConfigs[key].description
    })),
    results: allResults,
    triggerResults,
    summary: {
      totalBenchmarks: allResults.length,
      meanLatency: summaryStats.mean,
      p95Latency: summaryStats.p95,
      p99Latency: summaryStats.p99
    }
  };

  const outputPath = path.join(process.cwd(), 'reports', 'hook-performance-benchmarks.json');
  fs.mkdirSync(path.dirname(outputPath), { recursive: true });
  fs.writeFileSync(outputPath, JSON.stringify(output, null, 2));

  console.log(`\n📁 Results saved to: ${outputPath}`);

  return output;
}

// Run benchmarks
runBenchmarks()
  .then(() => {
    console.log('\n✅ Benchmark suite complete!');
    process.exit(0);
  })
  .catch(error => {
    console.error('\n❌ Benchmark failed:', error);
    process.exit(1);
  });
