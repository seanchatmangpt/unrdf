#!/usr/bin/env node
/**
 * Baseline Metrics Storage and Comparison
 *
 * Compares current performance metrics against baseline to detect regressions.
 *
 * Usage:
 *   node .github/scripts/baseline-metrics.mjs compare \
 *     <baseline.json> \
 *     <current.json> \
 *     --threshold <percent>
 */

import { readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';

const [command, baselinePath, currentPath, ...args] = process.argv.slice(2);

/**
 * Get command-line argument value
 * @param {string} flag - Argument flag
 * @param {string} defaultValue - Default value
 * @returns {string} Argument value
 */
function getArg(flag, defaultValue = '10') {
  const index = args.indexOf(flag);
  return index !== -1 ? args[index + 1] : defaultValue;
}

const threshold = parseFloat(getArg('--threshold', '10'));

/**
 * Read JSON file safely
 * @param {string} path - File path
 * @returns {object|null} Parsed JSON or null
 */
function readJSON(path) {
  try {
    const content = readFileSync(resolve(path), 'utf-8');
    return JSON.parse(content);
  } catch (error) {
    console.error(`❌ Could not read ${path}: ${error.message}`);
    return null;
  }
}

/**
 * Calculate percentage change
 * @param {number} baseline - Baseline value
 * @param {number} current - Current value
 * @returns {number} Percentage change
 */
function percentageChange(baseline, current) {
  if (baseline === 0) return current === 0 ? 0 : 100;
  return ((current - baseline) / baseline) * 100;
}

/**
 * Compare metrics and detect regressions
 * @param {object} baseline - Baseline metrics
 * @param {object} current - Current metrics
 * @param {number} threshold - Regression threshold (%)
 * @returns {object} Comparison report
 */
function compareMetrics(baseline, current, threshold) {
  const regressions = [];
  const improvements = [];
  const unchanged = [];

  // Extract metrics
  const baselineMetrics = baseline.metrics || baseline;
  const currentMetrics = current.metrics || current;

  // Compare each metric
  for (const [key, baselineValue] of Object.entries(baselineMetrics)) {
    const currentValue = currentMetrics[key];

    if (currentValue === undefined) {
      console.warn(`⚠️ Metric "${key}" not found in current metrics`);
      continue;
    }

    const change = percentageChange(
      parseFloat(baselineValue),
      parseFloat(currentValue)
    );

    const comparison = {
      metric: key,
      baseline: baselineValue,
      current: currentValue,
      change: change.toFixed(2),
      change_abs: Math.abs(change).toFixed(2),
    };

    // Determine if regression (higher is worse for durations)
    if (key.includes('duration') || key.includes('time')) {
      // For time metrics, increase is bad
      if (change > threshold) {
        regressions.push({ ...comparison, type: 'performance' });
      } else if (change < -threshold) {
        improvements.push({ ...comparison, type: 'performance' });
      } else {
        unchanged.push(comparison);
      }
    } else if (key.includes('coverage')) {
      // For coverage, decrease is bad
      if (change < -threshold) {
        regressions.push({ ...comparison, type: 'coverage' });
      } else if (change > threshold) {
        improvements.push({ ...comparison, type: 'coverage' });
      } else {
        unchanged.push(comparison);
      }
    } else {
      // Generic comparison
      if (Math.abs(change) > threshold) {
        if (change > 0) {
          improvements.push({ ...comparison, type: 'metric' });
        } else {
          regressions.push({ ...comparison, type: 'metric' });
        }
      } else {
        unchanged.push(comparison);
      }
    }
  }

  return {
    timestamp: new Date().toISOString(),
    baseline: {
      timestamp: baseline.timestamp,
      commit: baseline.commit,
    },
    current: {
      timestamp: current.timestamp,
      commit: current.commit,
    },
    threshold: `${threshold}%`,
    summary: {
      total_metrics: Object.keys(baselineMetrics).length,
      regressions: regressions.length,
      improvements: improvements.length,
      unchanged: unchanged.length,
    },
    regressions,
    improvements,
    unchanged,
  };
}

/**
 * Format comparison report for console output
 * @param {object} report - Comparison report
 */
function printReport(report) {
  console.log('\n═══════════════════════════════════════');
  console.log('Performance Regression Analysis');
  console.log('═══════════════════════════════════════\n');

  console.log(`Threshold: ${report.threshold}`);
  console.log(`Baseline: ${report.baseline.commit || 'unknown'} (${report.baseline.timestamp})`);
  console.log(`Current:  ${report.current.commit || 'unknown'} (${report.current.timestamp})`);
  console.log('');

  console.log('Summary:');
  console.log(`  Total Metrics: ${report.summary.total_metrics}`);
  console.log(`  Regressions:   ${report.summary.regressions}`);
  console.log(`  Improvements:  ${report.summary.improvements}`);
  console.log(`  Unchanged:     ${report.summary.unchanged}`);
  console.log('');

  if (report.regressions.length > 0) {
    console.log('❌ Regressions Detected:\n');
    for (const reg of report.regressions) {
      console.log(`  ${reg.metric}:`);
      console.log(`    Baseline: ${reg.baseline}`);
      console.log(`    Current:  ${reg.current}`);
      console.log(`    Change:   ${reg.change}% (threshold: ${report.threshold})`);
      console.log('');
    }
  } else {
    console.log('✅ No regressions detected\n');
  }

  if (report.improvements.length > 0) {
    console.log('✨ Improvements:\n');
    for (const imp of report.improvements) {
      console.log(`  ${imp.metric}: ${imp.change}%`);
    }
    console.log('');
  }
}

// Main command handler
function main() {
  if (command === 'compare') {
    if (!baselinePath || !currentPath) {
      console.error('❌ Usage: node baseline-metrics.mjs compare <baseline.json> <current.json> [--threshold N]');
      process.exit(1);
    }

    const baseline = readJSON(baselinePath);
    const current = readJSON(currentPath);

    if (!baseline || !current) {
      console.error('❌ Failed to load baseline or current metrics');
      process.exit(1);
    }

    const report = compareMetrics(baseline, current, threshold);

    // Print to console
    printReport(report);

    // Write to file
    const outputPath = 'regression-report.json';
    writeFileSync(outputPath, JSON.stringify(report, null, 2), 'utf-8');
    console.log(`✅ Report saved to ${outputPath}`);

    // Exit with error if regressions found
    if (report.regressions.length > 0) {
      process.exit(1);
    }
  } else {
    console.error('❌ Unknown command:', command);
    console.error('Usage: node baseline-metrics.mjs compare <baseline.json> <current.json> [--threshold N]');
    process.exit(1);
  }
}

// Run
main();
