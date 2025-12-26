/**
 * @file Baseline Comparison Benchmarks
 * @module benchmarks/regression/baseline-comparison
 *
 * @description
 * Compare current performance against baseline metrics:
 * - Load baseline from file
 * - Run current benchmarks
 * - Compare metrics and flag regressions >5%
 * - Generate comparison report
 */

import { suite, runBenchmark } from '../framework.mjs';
import { readFile, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

// =============================================================================
// Baseline Management
// =============================================================================

const BASELINE_DIR = join(process.cwd(), 'benchmarks', 'baselines');
const BASELINE_FILE = join(BASELINE_DIR, 'baseline.json');

/**
 * Load baseline from file
 * @returns {Promise<object|null>} Baseline data or null
 */
export async function loadBaseline() {
  try {
    if (!existsSync(BASELINE_FILE)) {
      return null;
    }

    const data = await readFile(BASELINE_FILE, 'utf-8');
    return JSON.parse(data);
  } catch (error) {
    console.warn(`Failed to load baseline: ${error.message}`);
    return null;
  }
}

/**
 * Save baseline to file
 * @param {object} baseline - Baseline data
 */
export async function saveBaseline(baseline) {
  try {
    const data = JSON.stringify(baseline, null, 2);
    await writeFile(BASELINE_FILE, data, 'utf-8');
    console.log(`Baseline saved to ${BASELINE_FILE}`);
  } catch (error) {
    console.error(`Failed to save baseline: ${error.message}`);
  }
}

/**
 * Compare current results against baseline
 * @param {object} current - Current benchmark results
 * @param {object} baseline - Baseline benchmark results
 * @returns {object} Comparison report
 */
export function compareToBaseline(current, baseline) {
  if (!baseline) {
    return {
      hasBaseline: false,
      message: 'No baseline available for comparison'
    };
  }

  const regressions = [];
  const improvements = [];
  const unchanged = [];

  for (const currentResult of current.results) {
    const baselineResult = baseline.results.find(r => r.name === currentResult.name);

    if (!baselineResult) {
      unchanged.push({
        name: currentResult.name,
        status: 'new',
        message: 'New benchmark (no baseline)'
      });
      continue;
    }

    if (currentResult.failed || baselineResult.failed) {
      unchanged.push({
        name: currentResult.name,
        status: 'error',
        message: 'Benchmark failed (current or baseline)'
      });
      continue;
    }

    // Compare throughput
    const throughputChange = ((currentResult.throughput - baselineResult.throughput) / baselineResult.throughput) * 100;

    // Compare P95 latency
    const latencyChange = ((currentResult.latency.p95 - baselineResult.latency.p95) / baselineResult.latency.p95) * 100;

    // Compare memory usage
    const memoryChange = ((currentResult.memory.heapUsed - baselineResult.memory.heapUsed) / Math.abs(baselineResult.memory.heapUsed || 1)) * 100;

    const comparison = {
      name: currentResult.name,
      throughput: {
        current: currentResult.throughput,
        baseline: baselineResult.throughput,
        change: throughputChange
      },
      latency: {
        current: currentResult.latency.p95,
        baseline: baselineResult.latency.p95,
        change: latencyChange
      },
      memory: {
        current: currentResult.memory.heapUsed,
        baseline: baselineResult.memory.heapUsed,
        change: memoryChange
      }
    };

    // Flag regression if throughput decreased >5% OR latency increased >5% OR memory increased >10%
    if (throughputChange < -5 || latencyChange > 5 || memoryChange > 10) {
      comparison.status = 'regression';
      regressions.push(comparison);
    } else if (throughputChange > 5 || latencyChange < -5 || memoryChange < -10) {
      comparison.status = 'improvement';
      improvements.push(comparison);
    } else {
      comparison.status = 'unchanged';
      unchanged.push(comparison);
    }
  }

  return {
    hasBaseline: true,
    timestamp: new Date().toISOString(),
    summary: {
      total: current.results.length,
      regressions: regressions.length,
      improvements: improvements.length,
      unchanged: unchanged.length
    },
    regressions,
    improvements,
    unchanged
  };
}

/**
 * Format comparison report as markdown
 * @param {object} comparison - Comparison results
 * @returns {string} Markdown report
 */
export function formatComparisonReport(comparison) {
  if (!comparison.hasBaseline) {
    return `# Baseline Comparison\n\n${comparison.message}\n`;
  }

  const lines = [];

  lines.push('# Baseline Comparison Report\n');
  lines.push(`**Generated**: ${comparison.timestamp}\n`);

  // Summary
  lines.push('## Summary\n');
  lines.push(`- **Total benchmarks**: ${comparison.summary.total}`);
  lines.push(`- **Regressions**: ${comparison.summary.regressions} ⚠️`);
  lines.push(`- **Improvements**: ${comparison.summary.improvements} ✓`);
  lines.push(`- **Unchanged**: ${comparison.summary.unchanged}\n`);

  // Regressions
  if (comparison.regressions.length > 0) {
    lines.push('## ⚠️ Regressions\n');
    lines.push('| Benchmark | Metric | Baseline | Current | Change |');
    lines.push('|-----------|--------|----------|---------|--------|');

    for (const reg of comparison.regressions) {
      if (reg.throughput.change < -5) {
        lines.push(`| ${reg.name} | Throughput | ${reg.throughput.baseline.toFixed(2)}/s | ${reg.throughput.current.toFixed(2)}/s | ${reg.throughput.change.toFixed(1)}% |`);
      }
      if (reg.latency.change > 5) {
        lines.push(`| ${reg.name} | Latency P95 | ${reg.latency.baseline.toFixed(3)}ms | ${reg.latency.current.toFixed(3)}ms | +${reg.latency.change.toFixed(1)}% |`);
      }
      if (reg.memory.change > 10) {
        lines.push(`| ${reg.name} | Memory | ${reg.memory.baseline} B | ${reg.memory.current} B | +${reg.memory.change.toFixed(1)}% |`);
      }
    }
    lines.push('');
  }

  // Improvements
  if (comparison.improvements.length > 0) {
    lines.push('## ✓ Improvements\n');
    lines.push('| Benchmark | Metric | Baseline | Current | Change |');
    lines.push('|-----------|--------|----------|---------|--------|');

    for (const imp of comparison.improvements) {
      if (imp.throughput.change > 5) {
        lines.push(`| ${imp.name} | Throughput | ${imp.throughput.baseline.toFixed(2)}/s | ${imp.throughput.current.toFixed(2)}/s | +${imp.throughput.change.toFixed(1)}% |`);
      }
      if (imp.latency.change < -5) {
        lines.push(`| ${imp.name} | Latency P95 | ${imp.latency.baseline.toFixed(3)}ms | ${imp.latency.current.toFixed(3)}ms | ${imp.latency.change.toFixed(1)}% |`);
      }
      if (imp.memory.change < -10) {
        lines.push(`| ${imp.name} | Memory | ${imp.memory.baseline} B | ${imp.memory.current} B | ${imp.memory.change.toFixed(1)}% |`);
      }
    }
    lines.push('');
  }

  return lines.join('\n');
}

// =============================================================================
// Simple Test Benchmarks
// =============================================================================

export const baselineTestBenchmarks = suite('Baseline Test Suite', {
  'simple computation': {
    fn: () => {
      let sum = 0;
      for (let i = 0; i < 1000; i++) {
        sum += i;
      }
      return sum;
    },
    iterations: 10000,
    warmup: 1000
  },

  'object creation': {
    fn: () => {
      return {
        id: Math.random(),
        name: 'test',
        timestamp: Date.now(),
        data: { value: Math.random() }
      };
    },
    iterations: 50000,
    warmup: 5000
  },

  'array operations': {
    fn: () => {
      const arr = Array.from({ length: 100 }, (_, i) => i);
      return arr.map(x => x * 2).filter(x => x > 50).reduce((sum, x) => sum + x, 0);
    },
    iterations: 10000,
    warmup: 1000
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('Running baseline comparison...\n');

  // Run current benchmarks
  const currentResults = await baselineTestBenchmarks();

  // Load baseline
  const baseline = await loadBaseline();

  // Compare
  const comparison = compareToBaseline(currentResults, baseline);

  // Print report
  console.log('\n' + formatComparisonReport(comparison));

  // Optionally save as new baseline
  if (process.argv.includes('--save-baseline')) {
    await saveBaseline(currentResults);
  }

  // Exit with error code if regressions detected
  if (comparison.regressions && comparison.regressions.length > 0) {
    console.error(`\n⚠️  ${comparison.regressions.length} regression(s) detected!`);
    process.exit(1);
  }

  process.exit(0);
}
