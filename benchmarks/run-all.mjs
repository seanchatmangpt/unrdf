#!/usr/bin/env node
/**
 * @file Run All Benchmarks
 * @module benchmarks/run-all
 *
 * @description
 * Main benchmark runner that executes all benchmark suites and generates reports
 */

import { writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { formatMarkdownTable, formatDetailedReport } from './framework.mjs';

// Import all benchmark suites
import { workflowBenchmarks } from './core/workflow-performance.mjs';
import { engineBenchmarks } from './core/engine-performance.mjs';
import { sparqlBenchmarks } from './core/sparql-performance.mjs';
import { federationBenchmarks } from './integration/federation-benchmark.mjs';
import { streamingBenchmarks } from './integration/streaming-benchmark.mjs';
import { knowledgeEngineBenchmarks } from './integration/knowledge-engine-benchmark.mjs';
import { blockchainReceiptBenchmarks } from './advanced/blockchain-receipt-benchmark.mjs';
import { visualizationBenchmarks } from './advanced/visualization-benchmark.mjs';
import { baselineTestBenchmarks, loadBaseline, saveBaseline, compareToBaseline, formatComparisonReport } from './regression/baseline-comparison.mjs';

// =============================================================================
// Benchmark Suite Registry
// =============================================================================

const BENCHMARK_SUITES = {
  core: [
    { name: 'Workflow Performance', fn: workflowBenchmarks },
    { name: 'Engine Performance', fn: engineBenchmarks },
    { name: 'SPARQL Performance', fn: sparqlBenchmarks }
  ],
  integration: [
    { name: 'Federation Performance', fn: federationBenchmarks },
    { name: 'Streaming Performance', fn: streamingBenchmarks },
    { name: 'Knowledge Engine Performance', fn: knowledgeEngineBenchmarks }
  ],
  advanced: [
    { name: 'Blockchain Receipt Performance', fn: blockchainReceiptBenchmarks },
    { name: 'Visualization Performance', fn: visualizationBenchmarks }
  ],
  regression: [
    { name: 'Baseline Test Suite', fn: baselineTestBenchmarks }
  ]
};

// =============================================================================
// Main Runner
// =============================================================================

async function runAllBenchmarks(category = 'all') {
  console.log('╔═══════════════════════════════════════════════════════════════════════╗');
  console.log('║         UNRDF Comprehensive Benchmark Suite                          ║');
  console.log('╚═══════════════════════════════════════════════════════════════════════╝\n');

  const results = [];
  const startTime = Date.now();

  // Determine which suites to run
  let suitesToRun = [];
  if (category === 'all') {
    for (const suites of Object.values(BENCHMARK_SUITES)) {
      suitesToRun.push(...suites);
    }
  } else if (BENCHMARK_SUITES[category]) {
    suitesToRun = BENCHMARK_SUITES[category];
  } else {
    console.error(`Unknown category: ${category}`);
    console.log(`Available categories: ${Object.keys(BENCHMARK_SUITES).join(', ')}, all`);
    process.exit(1);
  }

  console.log(`Running ${suitesToRun.length} benchmark suite(s)...\n`);

  // Run each suite
  for (const suite of suitesToRun) {
    console.log(`\n${'─'.repeat(80)}`);
    console.log(`Running: ${suite.name}`);
    console.log('─'.repeat(80));

    try {
      const result = await suite.fn();
      results.push(result);

      // Print summary
      const successCount = result.results.filter(r => !r.failed).length;
      const failCount = result.results.filter(r => r.failed).length;
      console.log(`\n✓ Completed: ${successCount} passed, ${failCount} failed`);
    } catch (error) {
      console.error(`\n✗ Suite failed: ${error.message}`);
      results.push({
        suite: suite.name,
        error: error.message,
        failed: true
      });
    }
  }

  const endTime = Date.now();
  const totalTime = ((endTime - startTime) / 1000).toFixed(2);

  console.log(`\n${'═'.repeat(80)}`);
  console.log(`All benchmarks completed in ${totalTime}s`);
  console.log('═'.repeat(80));

  return results;
}

// =============================================================================
// Report Generation
// =============================================================================

async function generateReport(results, outputPath) {
  const lines = [];

  lines.push('# UNRDF Benchmark Report\n');
  lines.push(`**Generated**: ${new Date().toISOString()}\n`);

  // Executive Summary
  const totalBenchmarks = results.reduce((sum, r) => sum + (r.results?.length || 0), 0);
  const failedBenchmarks = results.reduce((sum, r) =>
    sum + (r.results?.filter(b => b.failed).length || 0), 0
  );

  lines.push('## Executive Summary\n');
  lines.push(`- **Total benchmark suites**: ${results.length}`);
  lines.push(`- **Total benchmarks**: ${totalBenchmarks}`);
  lines.push(`- **Passed**: ${totalBenchmarks - failedBenchmarks}`);
  lines.push(`- **Failed**: ${failedBenchmarks}\n`);

  // Individual suite results
  for (const result of results) {
    if (result.failed) {
      lines.push(`## ${result.suite} (FAILED)\n`);
      lines.push(`**Error**: ${result.error}\n`);
      continue;
    }

    lines.push(formatMarkdownTable(result));
  }

  // Write report
  const reportContent = lines.join('\n');
  await writeFile(outputPath, reportContent, 'utf-8');
  console.log(`\n📊 Report written to: ${outputPath}`);

  return reportContent;
}

async function generateDetailedReport(results, outputPath) {
  const lines = [];

  lines.push('# UNRDF Detailed Benchmark Report\n');
  lines.push(`**Generated**: ${new Date().toISOString()}\n`);

  for (const result of results) {
    if (result.failed) {
      lines.push(`## ${result.suite} (FAILED)\n`);
      lines.push(`**Error**: ${result.error}\n`);
      continue;
    }

    lines.push(formatDetailedReport(result));
    lines.push('\n---\n');
  }

  const reportContent = lines.join('\n');
  await writeFile(outputPath, reportContent, 'utf-8');
  console.log(`📊 Detailed report written to: ${outputPath}`);

  return reportContent;
}

// =============================================================================
// Baseline Management
// =============================================================================

async function handleBaseline(results, mode) {
  // Use the regression suite results as baseline
  const baselineResults = results.find(r => r.suite === 'Baseline Test Suite');

  if (!baselineResults) {
    console.warn('⚠️  No baseline test suite results found');
    return null;
  }

  if (mode === 'save') {
    await saveBaseline(baselineResults);
    return null;
  }

  if (mode === 'compare') {
    const baseline = await loadBaseline();
    const comparison = compareToBaseline(baselineResults, baseline);
    const report = formatComparisonReport(comparison);

    // Write comparison report
    const reportPath = join(process.cwd(), 'benchmarks', 'reports', 'baseline-comparison.md');
    await writeFile(reportPath, report, 'utf-8');
    console.log(`\n📊 Baseline comparison written to: ${reportPath}`);

    // Print to console
    console.log('\n' + report);

    return comparison;
  }

  return null;
}

// =============================================================================
// CLI
// =============================================================================

async function main() {
  const args = process.argv.slice(2);
  const category = args[0] || 'all';
  const saveBaselines = args.includes('--save-baseline');
  const compareBaselines = args.includes('--compare-baseline');

  // Ensure reports directory exists
  const reportsDir = join(process.cwd(), 'benchmarks', 'reports');
  await mkdir(reportsDir, { recursive: true });

  const baselinesDir = join(process.cwd(), 'benchmarks', 'baselines');
  await mkdir(baselinesDir, { recursive: true });

  // Run benchmarks
  const results = await runAllBenchmarks(category);

  // Generate reports
  const summaryPath = join(reportsDir, 'benchmark-summary.md');
  const detailedPath = join(reportsDir, 'benchmark-detailed.md');

  await generateReport(results, summaryPath);
  await generateDetailedReport(results, detailedPath);

  // Handle baseline operations
  if (saveBaselines) {
    await handleBaseline(results, 'save');
  }

  if (compareBaselines) {
    const comparison = await handleBaseline(results, 'compare');
    if (comparison?.regressions?.length > 0) {
      console.error(`\n⚠️  ${comparison.regressions.length} regression(s) detected!`);
      process.exit(1);
    }
  }

  // Exit with error if any benchmarks failed
  const failedCount = results.reduce((sum, r) =>
    sum + (r.results?.filter(b => b.failed).length || 0), 0
  );

  if (failedCount > 0) {
    console.error(`\n⚠️  ${failedCount} benchmark(s) failed!`);
    process.exit(1);
  }

  console.log('\n✅ All benchmarks passed!');
  process.exit(0);
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

export { runAllBenchmarks, generateReport, generateDetailedReport };
