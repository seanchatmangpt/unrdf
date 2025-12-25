#!/usr/bin/env node
/**
 * Load Tester
 * Performs load testing on core functionality
 */

import { writeFileSync } from 'fs';

/**
 * Simulate concurrent operations
 * @param {number} concurrency - Number of concurrent operations
 * @param {Function} operation - Operation to execute
 * @returns {Object} Load test results
 */
async function loadTest(concurrency, operation) {
  const results = {
    concurrency,
    operations: [],
    errors: []
  };

  const startTime = Date.now();

  // Execute operations concurrently
  const promises = Array.from({ length: concurrency }, async (_, i) => {
    const opStart = Date.now();
    try {
      await operation(i);
      results.operations.push({
        id: i,
        duration: Date.now() - opStart,
        status: 'success'
      });
    } catch (error) {
      results.errors.push({
        id: i,
        error: error.message,
        duration: Date.now() - opStart
      });
    }
  });

  await Promise.all(promises);

  results.total_duration = Date.now() - startTime;

  // Calculate statistics
  const durations = results.operations.map(op => op.duration);
  results.stats = {
    total_operations: results.operations.length,
    successful: results.operations.filter(op => op.status === 'success').length,
    failed: results.errors.length,
    avg_latency: Math.round(durations.reduce((a, b) => a + b, 0) / durations.length),
    min_latency: Math.min(...durations),
    max_latency: Math.max(...durations),
    throughput: Math.round((results.operations.length / results.total_duration) * 1000) // ops/sec
  };

  return results;
}

/**
 * Sample load test operation
 * @param {number} id - Operation ID
 */
async function sampleOperation(id) {
  // Simulate work
  await new Promise(resolve => setTimeout(resolve, Math.random() * 100));

  // Simulate some data processing
  const data = Array.from({ length: 1000 }, (_, i) => ({
    id: i,
    value: Math.random()
  }));

  return data.filter(d => d.value > 0.5);
}

/**
 * Main execution
 */
async function main() {
  console.log('Running load tests...\n');

  const testConfigs = [
    { name: 'Low Load', concurrency: 10 },
    { name: 'Medium Load', concurrency: 50 },
    { name: 'High Load', concurrency: 100 }
  ];

  const allResults = {
    timestamp: new Date().toISOString(),
    tests: []
  };

  for (const config of testConfigs) {
    console.log(`Running ${config.name} (${config.concurrency} concurrent ops)...`);

    const results = await loadTest(config.concurrency, sampleOperation);
    results.name = config.name;
    allResults.tests.push(results);

    console.log(`  Throughput: ${results.stats.throughput} ops/sec`);
    console.log(`  Avg Latency: ${results.stats.avg_latency}ms`);
    console.log(`  Success Rate: ${(results.stats.successful / config.concurrency * 100).toFixed(2)}%\n`);
  }

  // Write results
  writeFileSync('load-test-results.json', JSON.stringify(allResults, null, 2));

  // Write log
  const log = `Load Testing Results
===================

${allResults.tests.map(t => `
${t.name}:
  Concurrency: ${t.concurrency}
  Throughput: ${t.stats.throughput} ops/sec
  Avg Latency: ${t.stats.avg_latency}ms
  Min Latency: ${t.stats.min_latency}ms
  Max Latency: ${t.stats.max_latency}ms
  Success Rate: ${(t.stats.successful / t.concurrency * 100).toFixed(2)}%
`).join('\n')}
`;

  console.log(log);
  writeFileSync('load-test.log', log);

  console.log('✅ Load testing completed');
  console.log('Results saved to: load-test-results.json');
}

main().catch(error => {
  console.error('Load testing error:', error);
  process.exit(1);
});
