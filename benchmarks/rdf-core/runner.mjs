/**
 * @file RDF Core Benchmark Runner
 * @module benchmarks/rdf-core/runner
 * @description Orchestrates execution of all RDF core benchmark suites
 */

import { performance } from 'perf_hooks';
import { generateReport, loadBaseline, saveReport, formatReportForConsole, saveBaseline } from './suite.mjs';
import { runParsingBenchmarks } from './01-parsing.bench.mjs';
import { runSerializationBenchmarks } from './02-serialization.bench.mjs';
import { runSparqlBenchmarks } from './03-sparql-queries.bench.mjs';
import { runGraphOperationsBenchmarks } from './04-graph-operations.bench.mjs';
import { runValidationBenchmarks } from './05-validation.bench.mjs';

/**
 * Parse command line arguments
 * @returns {Object} Parsed arguments
 */
function parseArguments() {
  const args = process.argv.slice(2);
  const options = {
    verbose: false,
    saveBaseline: false,
    suite: 'all',
    quick: false,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--verbose' || arg === '-v') {
      options.verbose = true;
    } else if (arg === '--save-baseline') {
      options.saveBaseline = true;
    } else if (arg === '--quick') {
      options.quick = true;
    } else if (arg.startsWith('--suite=')) {
      options.suite = arg.split('=')[1];
    }
  }

  return options;
}

/**
 * Run specific benchmark suite
 * @param {string} name - Benchmark suite name
 * @param {Function} runFn - Suite runner function
 * @param {Object} options - Runner options
 * @returns {Promise<Object>} Benchmark result
 */
async function runBenchmarkSuite(name, runFn, options) {
  if (options.verbose) {
    console.log(`\n▶ Running: ${name}`);
  }

  const startTime = performance.now();
  let result;

  try {
    result = await runFn();
    const duration = performance.now() - startTime;

    if (options.verbose) {
      console.log(`✓ Completed in ${(duration / 1000).toFixed(2)}s`);
      console.log(`  Passed: ${result.summary.passed}/${result.summary.total}`);
      if (result.summary.failed > 0) {
        console.log(`  Failed: ${result.summary.failed}`);
      }
    }

    return result;
  } catch (error) {
    console.error(`✗ Failed: ${error.message}`);
    if (options.verbose) {
      console.error(error.stack);
    }
    throw error;
  }
}

/**
 * Run all benchmarks
 * @param {Object} options - Runner options
 * @returns {Promise<Object>} All benchmark results
 */
async function runAllBenchmarks(options) {
  const suites = [
    { name: 'parsing', fn: runParsingBenchmarks },
    { name: 'serialization', fn: runSerializationBenchmarks },
    { name: 'sparql', fn: runSparqlBenchmarks },
    { name: 'graph-operations', fn: runGraphOperationsBenchmarks },
    { name: 'validation', fn: runValidationBenchmarks },
  ];

  const results = {};

  for (const suite of suites) {
    if (options.suite === 'all' || options.suite === suite.name) {
      results[suite.name] = await runBenchmarkSuite(
        suite.name,
        suite.fn,
        options
      );
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
  console.log('         RDF CORE PERFORMANCE BENCHMARK SUITE');
  console.log('═══════════════════════════════════════════════════════════════');

  if (options.quick) {
    console.log('\nRunning in QUICK mode (reduced iterations)\n');
  }

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
    if (!saved) {
      console.warn('Warning: Failed to save report');
    }

    // Save baseline if requested
    if (options.saveBaseline) {
      const newBaseline = {};
      for (const [suiteName, suiteResults] of Object.entries(results)) {
        if (suiteResults && suiteResults.results) {
          for (const [benchName, benchResult] of Object.entries(suiteResults.results)) {
            const fullName = `${suiteName}.${benchName}`;
            newBaseline[fullName] = benchResult;
          }
        }
      }
      const baselineSaved = saveBaseline(newBaseline);
      if (baselineSaved) {
        console.log('✓ Baseline updated: benchmarks/rdf-core/baselines/baseline.json\n');
      }
    }

    // Report total time
    const totalTime = performance.now() - startTime;
    console.log(`\nTotal execution time: ${(totalTime / 1000).toFixed(2)}s\n`);

    // Performance summary
    console.log('── Performance Targets ──────────────────────────────────────');
    console.log('Parsing (Turtle 10K):  <1000ms    (P95)');
    console.log('SPARQL SELECT:         <10ms      (P95)');
    console.log('Graph add (1K):        <50ms      (P95)');
    console.log('Graph find:            <5ms       (P95)');
    console.log('Quad validation:       <0.1ms     (P95)');
    console.log('═══════════════════════════════════════════════════════════════\n');

    // Exit with error code if regressions detected or failures
    if (report.summary.totalRegressions > 0) {
      console.error('❌ Performance regressions detected!');
      process.exit(1);
    } else if (report.summary.failed > 0) {
      console.error('❌ Some benchmarks failed!');
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
