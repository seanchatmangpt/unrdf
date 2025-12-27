#!/usr/bin/env node
/**
 * V6 Performance Benchmark Suite Runner
 *
 * Executes all v6 performance benchmarks and generates comprehensive summary.
 *
 * Usage:
 *   node benchmarks/v6/run-all.mjs
 *   node --expose-gc benchmarks/v6/run-all.mjs  (for memory benchmark)
 *
 * @module benchmarks/v6/run-all
 */

import { execSync } from 'node:child_process';
import { writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// =============================================================================
// Benchmark Configuration
// =============================================================================

const BENCHMARKS = [
  {
    id: 'receipt-overhead',
    name: 'Receipt Overhead',
    file: '1-receipt-overhead.mjs',
    target: '<1% overhead',
    description: 'Receipt generation overhead vs bare function',
  },
  {
    id: 'delta-compression',
    name: 'Delta Compression',
    file: '2-delta-compression.mjs',
    target: '<10% of state size',
    description: 'Delta proposal size vs full RDF state',
  },
  {
    id: 'query-performance',
    name: 'Query Performance',
    file: '3-query-performance.mjs',
    target: '<5% overhead',
    description: 'SPARQL query overhead with receipts',
  },
  {
    id: 'memory-usage',
    name: 'Memory Usage',
    file: '4-memory-usage.mjs',
    target: '<2% overhead',
    description: 'Receipt chain memory overhead',
    requiresGC: true,
  },
  {
    id: 'composition-latency',
    name: 'Composition Latency',
    file: '5-composition-latency.mjs',
    target: '<10% slowdown',
    description: 'Multi-module composition overhead',
  },
];

// =============================================================================
// Utilities
// =============================================================================

/**
 * Extract JSON results from benchmark output
 * @param {string} output - Benchmark output
 * @returns {Object|null} Parsed JSON results
 */
function extractJSONResults(output) {
  const match = output.match(/__JSON_RESULTS__\s*(\{[\s\S]*\})/);
  if (match) {
    try {
      return JSON.parse(match[1]);
    } catch (error) {
      console.error(`Failed to parse JSON results: ${error.message}`);
      return null;
    }
  }
  return null;
}

/**
 * Run a single benchmark
 * @param {Object} benchmark - Benchmark configuration
 * @returns {Object} Benchmark result
 */
function runBenchmark(benchmark) {
  console.log('\n' + '='.repeat(80));
  console.log(`Running: ${benchmark.name}`);
  console.log('='.repeat(80));
  console.log(`Description: ${benchmark.description}`);
  console.log(`Target: ${benchmark.target}`);
  console.log('');

  const benchmarkPath = join(__dirname, benchmark.file);
  const startTime = Date.now();

  try {
    // Build command
    let command = 'node';
    if (benchmark.requiresGC) {
      command += ' --expose-gc';
    }
    command += ` "${benchmarkPath}"`;

    // Execute benchmark
    const output = execSync(command, {
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe'],
      maxBuffer: 10 * 1024 * 1024, // 10MB buffer
    });

    const elapsed = Date.now() - startTime;

    // Extract results
    const jsonResults = extractJSONResults(output);

    // Print output
    console.log(output);

    return {
      benchmark: benchmark.id,
      name: benchmark.name,
      success: true,
      elapsed,
      results: jsonResults,
      output,
    };
  } catch (error) {
    const elapsed = Date.now() - startTime;

    console.error(`❌ Benchmark failed: ${error.message}`);
    if (error.stdout) console.log(error.stdout);
    if (error.stderr) console.error(error.stderr);

    return {
      benchmark: benchmark.id,
      name: benchmark.name,
      success: false,
      elapsed,
      error: error.message,
      output: error.stdout || '',
    };
  }
}

/**
 * Generate summary report
 * @param {Array<Object>} results - Benchmark results
 * @returns {Object} Summary report
 */
function generateSummary(results) {
  const summary = {
    timestamp: new Date().toISOString(),
    totalBenchmarks: results.length,
    successful: results.filter((r) => r.success).length,
    failed: results.filter((r) => !r.success).length,
    totalTime: results.reduce((sum, r) => sum + r.elapsed, 0),
    benchmarks: {},
    overallStatus: 'PASS',
  };

  for (const result of results) {
    if (!result.success) {
      summary.overallStatus = 'FAIL';
      summary.benchmarks[result.benchmark] = {
        name: result.name,
        status: 'FAIL',
        error: result.error,
      };
      continue;
    }

    const benchmarkSummary = {
      name: result.name,
      status: result.results?.target?.pass ? 'PASS' : 'FAIL',
      elapsed: result.elapsed,
    };

    // Add benchmark-specific metrics
    if (result.benchmark === 'receipt-overhead') {
      benchmarkSummary.overhead = result.results?.overhead?.percentRelative;
      benchmarkSummary.target = '<1%';
    } else if (result.benchmark === 'delta-compression') {
      benchmarkSummary.compressionRatio = result.results?.results?.compressionRatioPerOperationPercent;
      benchmarkSummary.target = '<10%';
    } else if (result.benchmark === 'query-performance') {
      benchmarkSummary.avgOverhead = result.results?.results?.avgOverheadPercent;
      benchmarkSummary.maxOverhead = result.results?.results?.maxOverheadPercent;
      benchmarkSummary.target = '<5%';
    } else if (result.benchmark === 'memory-usage') {
      benchmarkSummary.memoryOverhead = result.results?.results?.overhead?.percentRelative;
      benchmarkSummary.target = '<2%';
    } else if (result.benchmark === 'composition-latency') {
      benchmarkSummary.twoHopOverhead = result.results?.results?.twoHop?.overheadPercent;
      benchmarkSummary.threeHopOverhead = result.results?.results?.threeHop?.overheadPercent;
      benchmarkSummary.target = '<10%';
    }

    if (benchmarkSummary.status === 'FAIL') {
      summary.overallStatus = 'FAIL';
    }

    summary.benchmarks[result.benchmark] = benchmarkSummary;
  }

  return summary;
}

/**
 * Print summary table
 * @param {Object} summary - Summary report
 */
function printSummary(summary) {
  console.log('\n\n');
  console.log('='.repeat(100));
  console.log('V6 PERFORMANCE BENCHMARK SUMMARY');
  console.log('='.repeat(100));
  console.log(`Timestamp: ${summary.timestamp}`);
  console.log(`Total Benchmarks: ${summary.totalBenchmarks}`);
  console.log(`Successful: ${summary.successful}`);
  console.log(`Failed: ${summary.failed}`);
  console.log(`Total Time: ${(summary.totalTime / 1000).toFixed(2)}s`);
  console.log(`Overall Status: ${summary.overallStatus === 'PASS' ? '✅ PASS' : '❌ FAIL'}`);

  console.log('\n' + '-'.repeat(100));
  console.log('BENCHMARK RESULTS');
  console.log('-'.repeat(100));

  const tableHeader = String.prototype.padEnd.call('Benchmark', 25) +
    String.prototype.padEnd.call('Status', 10) +
    String.prototype.padEnd.call('Target', 15) +
    String.prototype.padEnd.call('Actual', 20) +
    String.prototype.padEnd.call('Time', 10);
  console.log(tableHeader);
  console.log('-'.repeat(100));

  for (const [id, bench] of Object.entries(summary.benchmarks)) {
    const status = bench.status === 'PASS' ? '✅ PASS' : '❌ FAIL';
    const name = String.prototype.padEnd.call(bench.name, 25);
    const statusStr = String.prototype.padEnd.call(status, 10);
    const target = String.prototype.padEnd.call(bench.target || 'N/A', 15);

    let actual = 'N/A';
    if (id === 'receipt-overhead' && bench.overhead !== undefined) {
      actual = `${bench.overhead.toFixed(2)}%`;
    } else if (id === 'delta-compression' && bench.compressionRatio !== undefined) {
      actual = `${bench.compressionRatio.toFixed(2)}%`;
    } else if (id === 'query-performance' && bench.avgOverhead !== undefined) {
      actual = `avg:${bench.avgOverhead.toFixed(2)}% max:${bench.maxOverhead.toFixed(2)}%`;
    } else if (id === 'memory-usage' && bench.memoryOverhead !== undefined) {
      actual = `${bench.memoryOverhead.toFixed(2)}%`;
    } else if (id === 'composition-latency' && bench.threeHopOverhead !== undefined) {
      actual = `3-hop:${bench.threeHopOverhead.toFixed(2)}%`;
    }

    const actualStr = String.prototype.padEnd.call(actual, 20);
    const time = String.prototype.padEnd.call(`${(bench.elapsed / 1000).toFixed(2)}s`, 10);

    console.log(name + statusStr + target + actualStr + time);
  }

  console.log('-'.repeat(100));
  console.log(`\nSummary: [Receipt: ${summary.benchmarks['receipt-overhead']?.status}] ` +
    `[Delta: ${summary.benchmarks['delta-compression']?.status}] ` +
    `[Query: ${summary.benchmarks['query-performance']?.status}] ` +
    `[Memory: ${summary.benchmarks['memory-usage']?.status}] ` +
    `[Composition: ${summary.benchmarks['composition-latency']?.status}]`);
  console.log('='.repeat(100));
}

// =============================================================================
// Main Runner
// =============================================================================

async function main() {
  console.log('='.repeat(100));
  console.log('V6 PERFORMANCE BENCHMARK SUITE');
  console.log('='.repeat(100));
  console.log(`Starting ${BENCHMARKS.length} benchmarks...`);
  console.log('');

  const results = [];

  // Run all benchmarks
  for (const benchmark of BENCHMARKS) {
    const result = runBenchmark(benchmark);
    results.push(result);
  }

  // Generate summary
  const summary = generateSummary(results);

  // Print summary
  printSummary(summary);

  // Save results to file
  const resultsFile = join(__dirname, 'results.json');
  writeFileSync(resultsFile, JSON.stringify(summary, null, 2));
  console.log(`\nDetailed results saved to: ${resultsFile}`);

  // Exit with appropriate code
  process.exit(summary.overallStatus === 'PASS' ? 0 : 1);
}

main().catch((error) => {
  console.error('Benchmark suite failed:', error);
  process.exit(1);
});
