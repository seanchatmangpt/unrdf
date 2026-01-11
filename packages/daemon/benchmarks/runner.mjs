/**
 * @file Daemon Benchmark Runner
 * @module @unrdf/daemon/benchmarks/runner
 * @description Orchestrates execution of all benchmark suites with reporting and regression detection
 */

import { performance } from 'perf_hooks';
import { generateReport, loadBaseline, saveReport, formatReportForConsole, saveBaseline } from './suite.mjs';
import { benchmarkOperationScheduling, benchmarkSchedulingThroughput, benchmarkQueueImpact } from './01-operation-scheduling.bench.mjs';
import { benchmarkConcurrentThroughput, benchmarkExecutionLatencyPercentiles, benchmarkConcurrencyImpact } from './02-concurrent-throughput.bench.mjs';
import { benchmarkMemoryWithLoad, benchmarkExecutionMemory, benchmarkMemoryStability } from './03-memory-load.bench.mjs';
import { benchmarkLeaderReplication, benchmarkConsensusCommit, benchmarkReplicationThroughput } from './04-raft-replication.bench.mjs';
import { benchmarkSequentialWorkflow, benchmarkParallelWorkflow, benchmarkConditionalWorkflow, benchmarkMixedWorkflow } from './05-yawl-execution.bench.mjs';

/**
 * Parse command line arguments
 * @returns {Object} Parsed arguments
 */
function parseArguments() {
  const args = process.argv.slice(2);
  const options = {
    verbose: false,
    saveBaseline: false,
    benchmark: null,
    runs: null,
    suite: 'all',
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--verbose' || arg === '-v') {
      options.verbose = true;
    } else if (arg === '--save-baseline') {
      options.saveBaseline = true;
    } else if (arg.startsWith('--benchmark=')) {
      options.benchmark = arg.split('=')[1];
    } else if (arg.startsWith('--suite=')) {
      options.suite = arg.split('=')[1];
    } else if (arg.startsWith('--runs=')) {
      options.runs = parseInt(arg.split('=')[1], 10);
    }
  }

  return options;
}

/**
 * Run specific benchmark suite
 * @param {string} name - Benchmark suite name
 * @param {Object} options - Runner options
 * @returns {Promise<Object>} Benchmark result
 */
async function runBenchmarkSuite(name, options) {
  if (options.verbose) {
    console.log(`\n▶ Running: ${name}`);
  }

  const startTime = performance.now();
  let result;

  try {
    switch (name) {
      case 'operation-scheduling-latency':
        result = await benchmarkOperationScheduling({ runs: options.runs || 10 });
        break;
      case 'operation-scheduling-throughput':
        result = await benchmarkSchedulingThroughput({ runs: options.runs || 5 });
        break;
      case 'operation-queue-impact':
        result = await benchmarkQueueImpact({ runs: options.runs || 5 });
        break;
      case 'concurrent-execution-throughput':
        result = await benchmarkConcurrentThroughput({ operationCount: 100, runs: options.runs || 5 });
        break;
      case 'execution-latency-percentiles':
        result = await benchmarkExecutionLatencyPercentiles({ operationCount: 50, runs: options.runs || 5 });
        break;
      case 'concurrency-impact':
        result = await benchmarkConcurrencyImpact({ runs: options.runs || 3 });
        break;
      case 'memory-usage-with-load':
        result = await benchmarkMemoryWithLoad({ maxOperations: 10000, steps: 5 });
        break;
      case 'execution-memory-consumption':
        result = await benchmarkExecutionMemory({ operationCount: 1000, runs: options.runs || 3 });
        break;
      case 'memory-stability-under-load':
        result = await benchmarkMemoryStability({ duration: 5000, opsPerSecond: 100 });
        break;
      case 'raft-leader-replication':
        result = await benchmarkLeaderReplication({ nodeCount: 3, operationCount: 100, runs: options.runs || 5 });
        break;
      case 'raft-consensus-commit':
        result = await benchmarkConsensusCommit({ quorumSize: 3, operationCount: 100, runs: options.runs || 5 });
        break;
      case 'raft-replication-throughput':
        result = await benchmarkReplicationThroughput({ nodeCount: 3, duration: 5000, runs: options.runs || 3 });
        break;
      case 'yawl-sequential-workflow':
        result = await benchmarkSequentialWorkflow({ stepsPerWorkflow: 5, workflowCount: 20, runs: options.runs || 5 });
        break;
      case 'yawl-parallel-workflow':
        result = await benchmarkParallelWorkflow({ parallelTasks: 5, workflowCount: 20, runs: options.runs || 5 });
        break;
      case 'yawl-conditional-workflow':
        result = await benchmarkConditionalWorkflow({ branchingFactor: 3, workflowCount: 20, runs: options.runs || 5 });
        break;
      case 'yawl-mixed-workflow-throughput':
        result = await benchmarkMixedWorkflow({ workflowCount: 30, runs: options.runs || 3 });
        break;
      default:
        throw new Error(`Unknown benchmark: ${name}`);
    }

    const duration = performance.now() - startTime;

    if (options.verbose) {
      console.log(`✓ Completed in ${duration.toFixed(2)}ms`);
      console.log(`  Value: ${result.value.toFixed(4)} ${result.unit}`);
      if (result.variance) {
        console.log(`  Variance: ${result.variance.toFixed(2)}%`);
      }
    }

    return result;
  } catch (error) {
    console.error(`✗ Failed: ${error.message}`);
    throw error;
  }
}

/**
 * Run all benchmarks or specific benchmark
 * @param {Object} options - Runner options
 * @returns {Promise<Object>} All benchmark results
 */
async function runAllBenchmarks(options) {
  const benchmarkNames = [
    'operation-scheduling-latency',
    'operation-scheduling-throughput',
    'operation-queue-impact',
    'concurrent-execution-throughput',
    'execution-latency-percentiles',
    'concurrency-impact',
    'memory-usage-with-load',
    'execution-memory-consumption',
    'memory-stability-under-load',
    'raft-leader-replication',
    'raft-consensus-commit',
    'raft-replication-throughput',
    'yawl-sequential-workflow',
    'yawl-parallel-workflow',
    'yawl-conditional-workflow',
    'yawl-mixed-workflow-throughput',
  ];

  const results = {};

  if (options.benchmark) {
    // Run specific benchmark
    const benchmarks = benchmarkNames.filter(name =>
      name.includes(options.benchmark) || options.benchmark.includes(name)
    );

    if (benchmarks.length === 0) {
      console.error(`No benchmarks match: ${options.benchmark}`);
      process.exit(1);
    }

    for (const name of benchmarks) {
      results[name] = await runBenchmarkSuite(name, options);
    }
  } else {
    // Run all benchmarks
    for (const name of benchmarkNames) {
      results[name] = await runBenchmarkSuite(name, options);
    }
  }

  return results;
}

/**
 * Main entry point
 */
async function main() {
  const options = parseArguments();

  console.log('═══════════════════════════════════════════════════════════════');
  console.log('         DAEMON PERFORMANCE BENCHMARK SUITE');
  console.log('═══════════════════════════════════════════════════════════════\n');

  const startTime = performance.now();

  try {
    // Run benchmarks
    const results = await runAllBenchmarks(options);

    // Load baseline
    const baseline = loadBaseline();

    // Generate report
    const report = generateReport(results, baseline);

    // Display formatted report
    console.log(formatReportForConsole(report));

    // Save report
    const saved = saveReport(report);
    if (saved) {
      console.log('Report saved to: benchmarks/benchmarks-*.json\n');
    }

    // Save baseline if requested
    if (options.saveBaseline) {
      const newBaseline = { ...baseline };
      for (const [name, result] of Object.entries(results)) {
        newBaseline[name] = result;
      }
      const baselineSaved = saveBaseline(newBaseline);
      if (baselineSaved) {
        console.log('Baseline updated: benchmarks/baselines/baseline.json\n');
      }
    }

    // Report total time
    const totalTime = performance.now() - startTime;
    console.log(`Total execution time: ${(totalTime / 1000).toFixed(2)}s\n`);

    // Exit with error code if regressions detected
    if (report.summary.totalRegressions > 0) {
      console.error('❌ Regressions detected!');
      process.exit(1);
    } else {
      console.log('✅ All benchmarks passed!\n');
      process.exit(0);
    }
  } catch (error) {
    console.error('\n❌ Benchmark suite failed:');
    console.error(error.message);
    if (options.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

// Run main
main();
