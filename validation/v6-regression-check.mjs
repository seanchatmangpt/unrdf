#!/usr/bin/env node
/**
 * @file UNRDF v6 Regression Detection
 * @module validation/v6-regression-check
 *
 * @description
 * Compares current validation metrics against baseline to detect regressions.
 * Alerts when metrics fall below acceptable thresholds.
 *
 * Usage:
 *   node validation/v6-regression-check.mjs
 *   node validation/v6-regression-check.mjs --strict
 */

import { readFile } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = join(__dirname, '..');

/**
 * Load baseline metrics
 * @returns {Promise<Object>} Baseline metrics
 */
async function loadBaseline() {
  const baselinePath = join(__dirname, 'v6-baseline-metrics.json');
  const content = await readFile(baselinePath, 'utf-8');
  return JSON.parse(content);
}

/**
 * Load current validation report
 * @returns {Promise<Object>} Current validation report
 */
async function loadCurrentReport() {
  const reportPath = join(ROOT_DIR, 'coverage', 'v6-validation-report.json');
  const content = await readFile(reportPath, 'utf-8');
  return JSON.parse(content);
}

/**
 * Extract metrics from validation report
 * @param {Object} report - Validation report
 * @returns {Object} Extracted metrics
 */
function extractMetrics(report) {
  const metrics = {};

  for (const check of report.checks) {
    if (check.checkName === 'zod-validated-apis' && check.metrics) {
      metrics.zod_coverage_percent = check.metrics.coveragePercent;
      metrics.packages_with_zod = check.metrics.packagesWithZod;
    }

    if (check.checkName === 'receipt-generation' && check.metrics) {
      metrics.receipt_call_count = check.metrics.receiptCallCount;
      metrics.receipt_modules = check.metrics.receiptModules;
    }

    if (check.checkName === 'determinism' && check.metrics) {
      metrics.business_logic_violations = check.metrics.businessLogicViolations;
      metrics.time_abstraction_usage = check.metrics.timeAbstractionUsage;
    }

    if (check.checkName === 'timeout-guards' && check.metrics) {
      metrics.timeout_coverage_percent = check.metrics.coveragePercent;
    }

    if (check.checkName === 'performance-baseline' && check.metrics) {
      metrics.receipt_avg_ms = check.metrics.receiptAvgMs;
      metrics.receipt_p95_ms = check.metrics.receiptP95Ms;
    }

    if (check.checkName === 'receipt-security' && check.metrics) {
      metrics.security_violations = check.metrics.violationsFound;
    }
  }

  return metrics;
}

/**
 * Compare metrics and detect regressions
 * @param {Object} baseline - Baseline metrics
 * @param {Object} current - Current metrics
 * @param {boolean} strict - Use strict thresholds
 * @returns {Object} Regression analysis
 */
function detectRegressions(baseline, current, strict = false) {
  const regressions = [];
  const warnings = [];
  const improvements = [];

  const thresholds = baseline.regression_thresholds;

  // 1. Zod coverage regression
  if (current.zod_coverage_percent !== undefined) {
    const baselineZod = baseline.metrics.packages.zod_coverage_percent;
    const delta = current.zod_coverage_percent - baselineZod;

    if (delta < thresholds.zod_coverage_drop_percent) {
      regressions.push({
        metric: 'Zod Coverage',
        baseline: baselineZod,
        current: current.zod_coverage_percent,
        delta: delta.toFixed(2),
        threshold: thresholds.zod_coverage_drop_percent,
        severity: 'HIGH',
      });
    } else if (delta > 0) {
      improvements.push({
        metric: 'Zod Coverage',
        baseline: baselineZod,
        current: current.zod_coverage_percent,
        delta: delta.toFixed(2),
      });
    }
  }

  // 2. Receipt coverage regression
  if (current.receipt_call_count !== undefined) {
    const baselineReceipts = baseline.metrics.receipts.generation_calls;
    const delta = current.receipt_call_count - baselineReceipts;
    const deltaPercent = (delta / baselineReceipts) * 100;

    if (deltaPercent < thresholds.receipt_coverage_drop_percent) {
      regressions.push({
        metric: 'Receipt Generation Calls',
        baseline: baselineReceipts,
        current: current.receipt_call_count,
        delta: deltaPercent.toFixed(2) + '%',
        threshold: thresholds.receipt_coverage_drop_percent + '%',
        severity: 'HIGH',
      });
    } else if (delta > 0) {
      improvements.push({
        metric: 'Receipt Generation Calls',
        baseline: baselineReceipts,
        current: current.receipt_call_count,
        delta: `+${delta}`,
      });
    }
  }

  // 3. Determinism violations (new violations)
  if (current.business_logic_violations !== undefined) {
    const baselineViolations = baseline.metrics.determinism.business_logic_violations;
    const newViolations = current.business_logic_violations - baselineViolations;

    if (newViolations > thresholds.new_determinism_violations) {
      regressions.push({
        metric: 'Determinism Violations',
        baseline: baselineViolations,
        current: current.business_logic_violations,
        delta: `+${newViolations}`,
        threshold: '0 new violations',
        severity: 'CRITICAL',
      });
    } else if (newViolations < 0) {
      improvements.push({
        metric: 'Determinism Violations',
        baseline: baselineViolations,
        current: current.business_logic_violations,
        delta: newViolations,
      });
    }
  }

  // 4. Performance regression
  if (current.receipt_avg_ms !== undefined) {
    const baselinePerf = baseline.metrics.performance.receipt_generation.avg_ms;
    const delta = current.receipt_avg_ms - baselinePerf;
    const deltaPercent = (delta / baselinePerf) * 100;

    if (deltaPercent > thresholds.performance_regression_percent) {
      regressions.push({
        metric: 'Receipt Generation Latency',
        baseline: `${baselinePerf}ms`,
        current: `${current.receipt_avg_ms.toFixed(2)}ms`,
        delta: `+${deltaPercent.toFixed(2)}%`,
        threshold: `≤${thresholds.performance_regression_percent}%`,
        severity: 'MEDIUM',
      });
    } else if (delta < 0) {
      improvements.push({
        metric: 'Receipt Generation Latency',
        baseline: `${baselinePerf}ms`,
        current: `${current.receipt_avg_ms.toFixed(2)}ms`,
        delta: `${deltaPercent.toFixed(2)}%`,
      });
    }
  }

  // 5. Security violations (new violations)
  if (current.security_violations !== undefined) {
    const baselineSecurityViolations = baseline.metrics.security.violations_found;
    const newSecurityViolations = current.security_violations - baselineSecurityViolations;

    if (newSecurityViolations > thresholds.new_security_violations) {
      regressions.push({
        metric: 'Security Violations',
        baseline: baselineSecurityViolations,
        current: current.security_violations,
        delta: `+${newSecurityViolations}`,
        threshold: '0 new violations',
        severity: 'CRITICAL',
      });
    } else if (newSecurityViolations < 0) {
      improvements.push({
        metric: 'Security Violations',
        baseline: baselineSecurityViolations,
        current: current.security_violations,
        delta: newSecurityViolations,
      });
    }
  }

  // 6. Timeout coverage
  if (current.timeout_coverage_percent !== undefined) {
    const baselineCoverage = baseline.metrics.timeouts.coverage_percent;
    const delta = current.timeout_coverage_percent - baselineCoverage;

    if (delta < -5.0) { // More than 5% drop
      warnings.push({
        metric: 'Timeout Coverage',
        baseline: `${baselineCoverage}%`,
        current: `${current.timeout_coverage_percent.toFixed(2)}%`,
        delta: `${delta.toFixed(2)}%`,
        severity: 'LOW',
      });
    } else if (delta > 5.0) {
      improvements.push({
        metric: 'Timeout Coverage',
        baseline: `${baselineCoverage}%`,
        current: `${current.timeout_coverage_percent.toFixed(2)}%`,
        delta: `+${delta.toFixed(2)}%`,
      });
    }
  }

  return {
    regressions,
    warnings,
    improvements,
    hasCriticalRegressions: regressions.some(r => r.severity === 'CRITICAL'),
    hasHighRegressions: regressions.some(r => r.severity === 'HIGH'),
  };
}

/**
 * Print regression report
 * @param {Object} analysis - Regression analysis
 */
function printReport(analysis) {
  console.log('📊 Regression Detection Report\n');
  console.log('═'.repeat(80) + '\n');

  if (analysis.regressions.length > 0) {
    console.log('❌ REGRESSIONS DETECTED:\n');
    for (const reg of analysis.regressions) {
      const icon = reg.severity === 'CRITICAL' ? '🚨' : reg.severity === 'HIGH' ? '⚠️' : '⚡';
      console.log(`${icon} ${reg.metric} (${reg.severity})`);
      console.log(`   Baseline: ${reg.baseline}`);
      console.log(`   Current:  ${reg.current}`);
      console.log(`   Delta:    ${reg.delta} (threshold: ${reg.threshold})`);
      console.log('');
    }
  }

  if (analysis.warnings.length > 0) {
    console.log('⚠️  WARNINGS:\n');
    for (const warn of analysis.warnings) {
      console.log(`   ${warn.metric}: ${warn.baseline} → ${warn.current} (${warn.delta})`);
    }
    console.log('');
  }

  if (analysis.improvements.length > 0) {
    console.log('✅ IMPROVEMENTS:\n');
    for (const imp of analysis.improvements) {
      console.log(`   ${imp.metric}: ${imp.baseline} → ${imp.current} (${imp.delta})`);
    }
    console.log('');
  }

  if (analysis.regressions.length === 0 && analysis.warnings.length === 0) {
    console.log('✅ No regressions detected!\n');
  }

  console.log('═'.repeat(80) + '\n');

  // Summary
  console.log('Summary:');
  console.log(`   Regressions: ${analysis.regressions.length}`);
  console.log(`   Warnings: ${analysis.warnings.length}`);
  console.log(`   Improvements: ${analysis.improvements.length}`);
  console.log('');

  if (analysis.hasCriticalRegressions) {
    console.log('🚨 CRITICAL: Build should FAIL - critical regressions detected');
    return 2;
  } else if (analysis.hasHighRegressions) {
    console.log('⚠️  HIGH: Build should WARN - high severity regressions detected');
    return 1;
  } else if (analysis.regressions.length > 0) {
    console.log('⚡ MEDIUM: Some regressions detected');
    return 0; // Non-blocking for now
  } else {
    console.log('✅ PASS: No regressions detected');
    return 0;
  }
}

/**
 * Main entry point
 */
async function main() {
  const strict = process.argv.includes('--strict');

  try {
    console.log('🔍 Loading baseline metrics...');
    const baseline = await loadBaseline();
    console.log(`   ✓ Loaded baseline from ${baseline.baseline_date}\n`);

    console.log('📈 Loading current validation report...');
    const report = await loadCurrentReport();
    console.log(`   ✓ Loaded report from ${report.timestamp}\n`);

    console.log('🔎 Extracting metrics...');
    const currentMetrics = extractMetrics(report);
    console.log(`   ✓ Extracted ${Object.keys(currentMetrics).length} metrics\n`);

    console.log('⚖️  Comparing against baseline...\n');
    const analysis = detectRegressions(baseline, currentMetrics, strict);

    const exitCode = printReport(analysis);
    process.exit(exitCode);
  } catch (error) {
    console.error('❌ Regression check failed:', error.message);
    console.error('\n   This is expected if v6-validate.mjs has not been run yet.');
    console.error('   Run: node validation/v6-validate.mjs\n');
    process.exit(1);
  }
}

main();
