/**
 * @file Comprehensive Benchmark Runner
 * @module narrative-state-chain/bench/run-all
 * @description Executes all benchmarks and generates comprehensive report
 */

import { benchmarkReconciliationLatency } from './reconcile.mjs';
import { benchmarkGuardEvaluation } from './guards.mjs';
import { benchmarkReceiptVerification } from './receipts.mjs';
import { benchmarkBridgeProofVerification } from './bridges.mjs';
import { benchmarkThroughput } from './throughput.mjs';
import { writeFileSync, readFileSync } from 'fs';

/**
 * Format benchmark results for JSON output
 * @param {object} results - Benchmark results
 * @returns {object} Formatted results
 */
function formatResultsForJSON(allResults) {
  return {
    timestamp: new Date().toISOString(),
    benchmarks: allResults,
    summary: generateSummary(allResults)
  };
}

/**
 * Generate summary and SLA status
 * @param {Array} allResults - All benchmark results
 * @returns {object} Summary object
 */
function generateSummary(allResults) {
  const summary = {
    totalBenchmarks: allResults.length,
    slaStatus: {},
    recommendations: []
  };

  // Check each benchmark against SLAs
  for (const result of allResults) {
    if (result.name === 'Reconciliation Latency') {
      const oneQuad = result.results['1 quad'];
      const thousandQuads = result.results['1000 quads'];

      summary.slaStatus['Reconciliation'] = {
        '1 quad': {
          p99: oneQuad.p99,
          slaMs: 5,
          status: oneQuad.p99 <= 5 ? 'PASS ✅' : 'FAIL ❌'
        },
        '1000 quads': {
          p99: thousandQuads.p99,
          slaMs: 100,
          status: thousandQuads.p99 <= 100 ? 'PASS ✅' : 'FAIL ❌'
        }
      };

      if (thousandQuads.p99 > 100) {
        summary.recommendations.push(
          'Reconciliation latency for 1000 quads exceeds SLA. ' +
          'Consider optimizing DeltaCapsule processing or RDF library calls.'
        );
      }
    } else if (result.name === 'Guard Evaluation') {
      const guards10 = result.results['10 guards'];
      summary.slaStatus['Guards'] = {
        '10 guards': {
          p99: guards10.p99,
          slaMs: 30,
          status: guards10.p99 <= 30 ? 'PASS ✅' : 'FAIL ❌'
        }
      };

      if (guards10.p99 > 30) {
        summary.recommendations.push(
          'Guard evaluation is slow. Consider parallelizing guard checks ' +
          'or implementing early termination on first failure.'
        );
      }
    } else if (result.name === 'Receipt Verification') {
      const complex = result.results['Complex'];
      summary.slaStatus['Receipts'] = {
        'Complex': {
          p99: complex.p99,
          slaMs: 10,
          status: complex.p99 <= 10 ? 'PASS ✅' : 'FAIL ❌'
        }
      };

      if (complex.p99 > 10) {
        summary.recommendations.push(
          'Complex receipt verification is slow. Consider caching hash computations ' +
          'or using hardware acceleration.'
        );
      }
    } else if (result.name === 'Bridge Proof Verification') {
      const complex = result.results['Type Coercion (depth 10)'] ||
                      result.results['Invariant Preservation (depth 10)'];

      summary.slaStatus['BridgeProofs'] = {
        'depth 10': {
          p99: complex.p99,
          slaMs: 500,
          status: complex.p99 <= 500 ? 'PASS ✅' : 'FAIL ❌'
        }
      };

      if (complex.p99 > 500) {
        summary.recommendations.push(
          'Bridge proof verification for depth 10 exceeds SLA. ' +
          'Consider optimizing hash chain validation or implementing memoization.'
        );
      }
    } else if (result.name === 'Throughput') {
      summary.slaStatus['Throughput'] = {
        individual: {
          scenesPerSec: result.individual.scenesPerSec,
          slaMin: 10,
          status: result.individual.scenesPerSec >= 10 ? 'PASS ✅' : 'FAIL ❌'
        }
      };

      if (result.individual.scenesPerSec < 10) {
        summary.recommendations.push(
          'Throughput is below SLA of 10 scenes/sec. ' +
          'Profile the admission engine to identify bottlenecks.'
        );
      }
    }
  }

  return summary;
}

/**
 * Display comprehensive report
 * @param {object} summary - Summary object
 */
function displayReport(summary) {
  console.log('\n\n========================================');
  console.log('         SLA COMPLIANCE SUMMARY');
  console.log('========================================\n');

  for (const [category, details] of Object.entries(summary.slaStatus)) {
    console.log(`${category}:`);
    for (const [key, value] of Object.entries(details)) {
      const slaField = value.slaMs || value.slaMin;
      const actualField = value.p99 || value.scenesPerSec;
      const status = value.status;

      if (value.slaMs) {
        console.log(`  ${key}: ${actualField.toFixed(2)}ms (SLA: ${slaField}ms) ${status}`);
      } else {
        console.log(`  ${key}: ${actualField.toFixed(2)} scenes/sec (SLA: ${slaField}) ${status}`);
      }
    }
    console.log();
  }

  if (summary.recommendations.length > 0) {
    console.log('========================================');
    console.log('       OPTIMIZATION RECOMMENDATIONS');
    console.log('========================================\n');

    for (let i = 0; i < summary.recommendations.length; i++) {
      console.log(`${i + 1}. ${summary.recommendations[i]}\n`);
    }
  }

  const passCount = Object.values(summary.slaStatus)
    .flatMap(details => Object.values(details))
    .filter(v => v.status && v.status.includes('PASS')).length;

  const totalCount = Object.values(summary.slaStatus)
    .flatMap(details => Object.values(details)).length;

  console.log('========================================');
  console.log(`Overall: ${passCount}/${totalCount} SLAs met`);
  console.log('========================================\n');
}

/**
 * Run all benchmarks
 * @async
 */
async function runAllBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║   NARRATIVE-STATE-CHAIN PERFORMANCE BENCHMARK SUITE       ║');
  console.log('║              Comprehensive SLA Baseline Measurement        ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  const results = [];

  try {
    // Run each benchmark
    console.log('▶ Running Reconciliation Latency Benchmark...');
    const reconcile = await benchmarkReconciliationLatency();
    results.push(reconcile);

    console.log('\n▶ Running Guard Evaluation Benchmark...');
    const guards = await benchmarkGuardEvaluation();
    results.push(guards);

    console.log('\n▶ Running Receipt Verification Benchmark...');
    const receipts = await benchmarkReceiptVerification();
    results.push(receipts);

    console.log('\n▶ Running Bridge Proof Verification Benchmark...');
    const bridges = await benchmarkBridgeProofVerification();
    results.push(bridges);

    console.log('\n▶ Running Throughput Benchmark...');
    const throughput = await benchmarkThroughput();
    results.push(throughput);

    // Format and save results
    const output = formatResultsForJSON(results);

    // Save to JSON file
    const outputPath = '/home/user/unrdf/benchmark-results.json';
    writeFileSync(outputPath, JSON.stringify(output, null, 2));
    console.log(`\n✅ Results saved to ${outputPath}`);

    // Display summary and recommendations
    displayReport(output.summary);

  } catch (error) {
    console.error('❌ Benchmark execution failed:', error);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  await runAllBenchmarks();
}
