#!/usr/bin/env node
/**
 * Collect benchmark metrics from test output
 * Aggregates performance data for tracking
 */

import { readFileSync, writeFileSync, readdirSync } from 'fs';
import { join } from 'path';

/**
 * Parse benchmark log file
 * @param {string} filepath - Path to log file
 * @returns {Object[]} Benchmark results
 */
function parseBenchmarkLog(filepath) {
  try {
    const content = readFileSync(filepath, 'utf8');
    const results = [];

    // Extract timing information from test output
    const testPattern = /Test:\s+(.+?)\s+(\d+)ms/g;
    let match;

    while ((match = testPattern.exec(content)) !== null) {
      results.push({
        name: match[1],
        duration: parseInt(match[2]),
        timestamp: new Date().toISOString()
      });
    }

    // Extract Vitest benchmark results if present
    const vitestPattern = /✓\s+(.+?)\s+\((\d+)\s*ms\)/g;
    while ((match = vitestPattern.exec(content)) !== null) {
      results.push({
        name: match[1],
        duration: parseInt(match[2]),
        timestamp: new Date().toISOString()
      });
    }

    return results;
  } catch (error) {
    console.warn(`Failed to parse ${filepath}:`, error.message);
    return [];
  }
}

/**
 * Collect all benchmark results
 */
function collectBenchmarks() {
  const benchmarkFiles = [
    'benchmark-core.log',
    'benchmark-streaming.log',
    'benchmark-federation.log',
    'benchmark-yawl.log'
  ];

  const allResults = {
    timestamp: new Date().toISOString(),
    commit: process.env.GITHUB_SHA || 'unknown',
    branch: process.env.GITHUB_REF_NAME || 'unknown',
    benchmarks: {}
  };

  for (const file of benchmarkFiles) {
    if (!require('fs').existsSync(file)) {
      console.warn(`Benchmark file not found: ${file}`);
      continue;
    }

    const packageName = file.replace('benchmark-', '').replace('.log', '');
    allResults.benchmarks[packageName] = parseBenchmarkLog(file);
  }

  // Calculate summary statistics
  const allBenchmarks = Object.values(allResults.benchmarks).flat();
  if (allBenchmarks.length > 0) {
    const durations = allBenchmarks.map(b => b.duration);
    allResults.summary = {
      total_benchmarks: allBenchmarks.length,
      avg_duration: Math.round(durations.reduce((a, b) => a + b, 0) / durations.length),
      min_duration: Math.min(...durations),
      max_duration: Math.max(...durations),
      total_duration: durations.reduce((a, b) => a + b, 0)
    };
  }

  return allResults;
}

/**
 * Main execution
 */
function main() {
  console.log('Collecting benchmark metrics...');

  const results = collectBenchmarks();

  // Write results to JSON
  writeFileSync('benchmark-results.json', JSON.stringify(results, null, 2));

  console.log('\nBenchmark Summary:');
  if (results.summary) {
    console.log(`  Total benchmarks: ${results.summary.total_benchmarks}`);
    console.log(`  Average duration: ${results.summary.avg_duration}ms`);
    console.log(`  Min duration: ${results.summary.min_duration}ms`);
    console.log(`  Max duration: ${results.summary.max_duration}ms`);
    console.log(`  Total duration: ${results.summary.total_duration}ms`);
  } else {
    console.log('  No benchmark results found');
  }

  console.log('\n✅ Benchmark metrics collected');
  console.log('Results saved to: benchmark-results.json');
}

main();
