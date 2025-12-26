#!/usr/bin/env node
/**
 * Comprehensive Benchmark Suite Runner
 * Runs all benchmarks, compares with baseline, detects regressions
 *
 * Usage:
 *   node scripts/bench-all.mjs                    # Run all benchmarks
 *   node scripts/bench-all.mjs --baseline         # Update baseline
 *   node scripts/bench-all.mjs --threshold 15     # Custom regression threshold (default: 10%)
 *   node scripts/bench-all.mjs --format markdown  # Output markdown report
 */

import { spawn } from 'child_process';
import { readFileSync, writeFileSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = resolve(__dirname, '..');
const baselineFile = resolve(projectRoot, 'benchmarks/baseline.json');
const budgetsFile = resolve(projectRoot, 'performance-budgets.yml');

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  return {
    updateBaseline: args.includes('--baseline'),
    threshold: parseInt(args.find(a => a.startsWith('--threshold='))?.split('=')[1] || '10'),
    format: args.find(a => a.startsWith('--format='))?.split('=')[1] || 'console',
    filter: args.find(a => !a.startsWith('--')) || null,
  };
}

/**
 * Discover all benchmark files
 */
function discoverBenchmarks(filter = null) {
  const benchmarkDir = resolve(projectRoot, 'benchmarks');
  const benchmarks = [
    { name: 'hook-execution', file: 'hook-execution-bench.mjs', priority: 1 },
    { name: 'receipt-generation', file: 'receipt-generation-bench.mjs', priority: 1 },
    { name: 'sparql-query', file: 'sparql-query-bench.mjs', priority: 1 },
    { name: 'task-activation', file: 'task-activation-bench.mjs', priority: 2 },
    { name: 'workflow-e2e', file: 'workflow-e2e-bench.mjs', priority: 2 },
    { name: 'optimization-suite', file: 'optimization-suite.mjs', priority: 3 },
  ];

  return benchmarks
    .filter(b => !filter || b.name.includes(filter))
    .filter(b => existsSync(resolve(benchmarkDir, b.file)))
    .sort((a, b) => a.priority - b.priority);
}

/**
 * Run a single benchmark
 */
async function runBenchmark(benchmark) {
  const benchmarkPath = resolve(projectRoot, 'benchmarks', benchmark.file);

  return new Promise((resolve, reject) => {
    const startTime = Date.now();
    let stdout = '';
    let stderr = '';

    const proc = spawn('node', [benchmarkPath], {
      cwd: projectRoot,
      env: { ...process.env, NODE_ENV: 'production' },
      timeout: 60000, // 60 second timeout
    });

    proc.stdout.on('data', (data) => {
      const output = data.toString();
      stdout += output;
      // Stream output but don't clutter with JSON
      if (!output.includes('__JSON_RESULTS__')) {
        process.stdout.write(output);
      }
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();
      process.stderr.write(data);
    });

    proc.on('close', (code) => {
      const elapsed = Date.now() - startTime;

      if (code !== 0) {
        reject(new Error(`Benchmark failed with code ${code}\n${stderr}`));
        return;
      }

      // Extract JSON results
      const jsonMatch = stdout.match(/__JSON_RESULTS__\s*\n([\s\S]*?)(?:\n\n|$)/);
      let results = null;

      if (jsonMatch) {
        try {
          results = JSON.parse(jsonMatch[1]);
        } catch (e) {
          console.warn(`Failed to parse JSON results from ${benchmark.name}:`, e.message);
        }
      }

      resolve({
        name: benchmark.name,
        file: benchmark.file,
        duration: elapsed,
        results,
        success: true,
      });
    });

    proc.on('error', (error) => {
      reject(error);
    });
  });
}

/**
 * Load baseline data
 */
function loadBaseline() {
  if (!existsSync(baselineFile)) {
    return null;
  }

  try {
    return JSON.parse(readFileSync(baselineFile, 'utf8'));
  } catch (e) {
    console.warn('Failed to load baseline:', e.message);
    return null;
  }
}

/**
 * Load performance budgets
 */
function loadBudgets() {
  if (!existsSync(budgetsFile)) {
    return null;
  }

  // Simple YAML parser for budgets (80/20: only parse what we need)
  try {
    const content = readFileSync(budgetsFile, 'utf8');
    const budgets = {};

    let currentBenchmark = null;
    for (const line of content.split('\n')) {
      const trimmed = line.trim();
      if (trimmed.endsWith(':') && !trimmed.startsWith('-')) {
        currentBenchmark = trimmed.slice(0, -1);
        budgets[currentBenchmark] = {};
      } else if (trimmed.startsWith('-')) {
        const [metric, value] = trimmed.slice(1).trim().split(':');
        if (currentBenchmark && metric && value) {
          budgets[currentBenchmark][metric.trim()] = parseFloat(value.trim());
        }
      }
    }

    return budgets;
  } catch (e) {
    console.warn('Failed to load budgets:', e.message);
    return null;
  }
}

/**
 * Compare results with baseline
 */
function compareWithBaseline(current, baseline, threshold = 10) {
  if (!baseline || !baseline.benchmarks) {
    return { regressions: [], improvements: [], missing: [] };
  }

  const regressions = [];
  const improvements = [];
  const missing = [];

  for (const result of current) {
    const baselineResult = baseline.benchmarks.find(b => b.name === result.name);

    if (!baselineResult) {
      missing.push(result.name);
      continue;
    }

    // Compare key metrics
    if (result.results?.results) {
      const currentMetrics = result.results.results;
      const baselineMetrics = baselineResult.results?.results || {};

      for (const [key, value] of Object.entries(currentMetrics)) {
        const baselineValue = baselineMetrics[key];

        if (!baselineValue || typeof value !== 'object') continue;

        // Compare P95 latency (primary metric)
        if (value.p95Us !== undefined && baselineValue.p95Us !== undefined) {
          const change = ((value.p95Us - baselineValue.p95Us) / baselineValue.p95Us) * 100;

          if (change > threshold) {
            regressions.push({
              benchmark: result.name,
              metric: `${key}.p95Us`,
              baseline: baselineValue.p95Us,
              current: value.p95Us,
              change: change.toFixed(1),
            });
          } else if (change < -threshold) {
            improvements.push({
              benchmark: result.name,
              metric: `${key}.p95Us`,
              baseline: baselineValue.p95Us,
              current: value.p95Us,
              change: change.toFixed(1),
            });
          }
        }

        // Compare P95 milliseconds (for slower operations)
        if (value.p95Ms !== undefined && baselineValue.p95Ms !== undefined) {
          const change = ((value.p95Ms - baselineValue.p95Ms) / baselineValue.p95Ms) * 100;

          if (change > threshold) {
            regressions.push({
              benchmark: result.name,
              metric: `${key}.p95Ms`,
              baseline: baselineValue.p95Ms,
              current: value.p95Ms,
              change: change.toFixed(1),
            });
          } else if (change < -threshold) {
            improvements.push({
              benchmark: result.name,
              metric: `${key}.p95Ms`,
              baseline: baselineValue.p95Ms,
              current: value.p95Ms,
              change: change.toFixed(1),
            });
          }
        }
      }
    }
  }

  return { regressions, improvements, missing };
}

/**
 * Check against performance budgets
 */
function checkBudgets(results, budgets) {
  if (!budgets) return [];

  const violations = [];

  for (const result of results) {
    const budget = budgets[result.name];
    if (!budget || !result.results?.results) continue;

    const metrics = result.results.results;

    for (const [metricName, value] of Object.entries(metrics)) {
      if (typeof value !== 'object') continue;

      // Check P95 budgets
      if (value.p95Us !== undefined && budget[`${metricName}_p95_us`] !== undefined) {
        const budgetValue = budget[`${metricName}_p95_us`];
        if (value.p95Us > budgetValue) {
          violations.push({
            benchmark: result.name,
            metric: `${metricName}.p95Us`,
            budget: budgetValue,
            actual: value.p95Us,
            excess: ((value.p95Us - budgetValue) / budgetValue * 100).toFixed(1),
          });
        }
      }

      if (value.p95Ms !== undefined && budget[`${metricName}_p95_ms`] !== undefined) {
        const budgetValue = budget[`${metricName}_p95_ms`];
        if (value.p95Ms > budgetValue) {
          violations.push({
            benchmark: result.name,
            metric: `${metricName}.p95Ms`,
            budget: budgetValue,
            actual: value.p95Ms,
            excess: ((value.p95Ms - budgetValue) / budgetValue * 100).toFixed(1),
          });
        }
      }
    }
  }

  return violations;
}

/**
 * Generate console report
 */
function generateConsoleReport(results, comparison, budgetViolations) {
  console.log('\n' + '='.repeat(70));
  console.log('BENCHMARK SUITE SUMMARY');
  console.log('='.repeat(70));

  console.log(`\nTotal benchmarks: ${results.length}`);
  console.log(`Successful: ${results.filter(r => r.success).length}`);
  console.log(`Failed: ${results.filter(r => !r.success).length}`);

  const totalTime = results.reduce((sum, r) => sum + r.duration, 0);
  console.log(`Total time: ${(totalTime / 1000).toFixed(1)}s`);

  // Regressions
  if (comparison.regressions.length > 0) {
    console.log('\n' + '⚠️  PERFORMANCE REGRESSIONS DETECTED'.padEnd(70));
    console.log('-'.repeat(70));
    for (const reg of comparison.regressions) {
      console.log(`${reg.benchmark} / ${reg.metric}:`);
      console.log(`  Baseline: ${reg.baseline.toFixed(2)}`);
      console.log(`  Current:  ${reg.current.toFixed(2)}`);
      console.log(`  Change:   +${reg.change}% ⚠️`);
    }
  }

  // Improvements
  if (comparison.improvements.length > 0) {
    console.log('\n' + '✅ PERFORMANCE IMPROVEMENTS'.padEnd(70));
    console.log('-'.repeat(70));
    for (const imp of comparison.improvements) {
      console.log(`${imp.benchmark} / ${imp.metric}: ${imp.change}%`);
    }
  }

  // Budget violations
  if (budgetViolations.length > 0) {
    console.log('\n' + '❌ PERFORMANCE BUDGET VIOLATIONS'.padEnd(70));
    console.log('-'.repeat(70));
    for (const viol of budgetViolations) {
      console.log(`${viol.benchmark} / ${viol.metric}:`);
      console.log(`  Budget: ${viol.budget.toFixed(2)}`);
      console.log(`  Actual: ${viol.actual.toFixed(2)}`);
      console.log(`  Excess: +${viol.excess}%`);
    }
  }

  console.log('\n' + '='.repeat(70));

  // Exit code based on results
  const hasRegressions = comparison.regressions.length > 0;
  const hasBudgetViolations = budgetViolations.length > 0;

  if (hasRegressions || hasBudgetViolations) {
    console.log('Status: FAILED (regressions or budget violations detected)');
    return 1;
  } else {
    console.log('Status: PASSED (all benchmarks within acceptable range)');
    return 0;
  }
}

/**
 * Generate markdown report
 */
function generateMarkdownReport(results, comparison, budgetViolations) {
  let md = '# Performance Benchmark Report\n\n';
  md += `**Generated:** ${new Date().toISOString()}\n\n`;

  md += '## Summary\n\n';
  md += `- Total benchmarks: ${results.length}\n`;
  md += `- Successful: ${results.filter(r => r.success).length}\n`;
  md += `- Failed: ${results.filter(r => !r.success).length}\n`;

  const totalTime = results.reduce((sum, r) => sum + r.duration, 0);
  md += `- Total time: ${(totalTime / 1000).toFixed(1)}s\n\n`;

  // Regressions
  if (comparison.regressions.length > 0) {
    md += '## ⚠️  Performance Regressions\n\n';
    md += '| Benchmark | Metric | Baseline | Current | Change |\n';
    md += '|-----------|--------|----------|---------|--------|\n';
    for (const reg of comparison.regressions) {
      md += `| ${reg.benchmark} | ${reg.metric} | ${reg.baseline.toFixed(2)} | ${reg.current.toFixed(2)} | +${reg.change}% |\n`;
    }
    md += '\n';
  }

  // Improvements
  if (comparison.improvements.length > 0) {
    md += '## ✅ Performance Improvements\n\n';
    md += '| Benchmark | Metric | Baseline | Current | Change |\n';
    md += '|-----------|--------|----------|---------|--------|\n';
    for (const imp of comparison.improvements) {
      md += `| ${imp.benchmark} | ${imp.metric} | ${imp.baseline.toFixed(2)} | ${imp.current.toFixed(2)} | ${imp.change}% |\n`;
    }
    md += '\n';
  }

  // Budget violations
  if (budgetViolations.length > 0) {
    md += '## ❌ Performance Budget Violations\n\n';
    md += '| Benchmark | Metric | Budget | Actual | Excess |\n';
    md += '|-----------|--------|--------|--------|--------|\n';
    for (const viol of budgetViolations) {
      md += `| ${viol.benchmark} | ${viol.metric} | ${viol.budget.toFixed(2)} | ${viol.actual.toFixed(2)} | +${viol.excess}% |\n`;
    }
    md += '\n';
  }

  // Detailed results
  md += '## Detailed Results\n\n';
  for (const result of results) {
    md += `### ${result.name}\n\n`;
    if (result.results?.results) {
      md += '```json\n';
      md += JSON.stringify(result.results.results, null, 2);
      md += '\n```\n\n';
    }
  }

  return md;
}

/**
 * Main execution
 */
async function main() {
  const opts = parseArgs();

  console.log('='.repeat(70));
  console.log('RUNNING BENCHMARK SUITE');
  console.log('='.repeat(70));
  console.log(`Regression threshold: ${opts.threshold}%`);
  console.log(`Output format: ${opts.format}`);
  console.log('');

  // Discover benchmarks
  const benchmarks = discoverBenchmarks(opts.filter);
  console.log(`Found ${benchmarks.length} benchmarks\n`);

  // Run benchmarks
  const results = [];
  for (const benchmark of benchmarks) {
    console.log(`\nRunning ${benchmark.name}...`);
    console.log('-'.repeat(70));

    try {
      const result = await runBenchmark(benchmark);
      results.push(result);
      console.log(`✅ ${benchmark.name} completed in ${(result.duration / 1000).toFixed(1)}s\n`);
    } catch (error) {
      console.error(`❌ ${benchmark.name} failed:`, error.message);
      results.push({
        name: benchmark.name,
        file: benchmark.file,
        success: false,
        error: error.message,
      });
    }
  }

  // Load baseline and budgets
  const baseline = loadBaseline();
  const budgets = loadBudgets();

  // Compare results
  const comparison = compareWithBaseline(results, baseline, opts.threshold);
  const budgetViolations = checkBudgets(results, budgets);

  // Generate report
  if (opts.format === 'markdown') {
    const md = generateMarkdownReport(results, comparison, budgetViolations);
    console.log('\n' + md);
  } else {
    const exitCode = generateConsoleReport(results, comparison, budgetViolations);

    // Update baseline if requested
    if (opts.updateBaseline) {
      const baselineData = {
        timestamp: new Date().toISOString(),
        benchmarks: results.filter(r => r.success),
      };

      writeFileSync(baselineFile, JSON.stringify(baselineData, null, 2));
      console.log(`\n✅ Baseline updated: ${baselineFile}`);
    }

    process.exit(exitCode);
  }
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
