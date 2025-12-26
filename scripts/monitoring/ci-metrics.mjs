#!/usr/bin/env node
/**
 * CI Metrics Tracking
 * Collects and analyzes CI/CD pipeline metrics
 *
 * Tracks:
 * - Build duration
 * - Test pass rates
 * - Coverage trends
 * - Failure patterns
 */

import { execSync } from 'child_process';
import { writeFileSync, readFileSync, existsSync } from 'fs';

const METRICS_FILE = 'ci-metrics.json';
const HISTORY_LIMIT = 100;

/**
 * Execute command and return output
 * @param {string} cmd - Command to execute
 * @returns {string} Command output
 */
function exec(cmd) {
  try {
    return execSync(cmd, { encoding: 'utf8', stdio: ['pipe', 'pipe', 'ignore'] });
  } catch (error) {
    return '';
  }
}

/**
 * Get current CI run information
 * @returns {Object} CI run data
 */
function getCIRunData() {
  const data = {
    timestamp: new Date().toISOString(),
    commit: process.env.GITHUB_SHA || exec('git rev-parse HEAD').trim(),
    branch: process.env.GITHUB_REF_NAME || exec('git rev-parse --abbrev-ref HEAD').trim(),
    workflow: process.env.GITHUB_WORKFLOW || 'local',
    run_id: process.env.GITHUB_RUN_ID || 'local',
    event: process.env.GITHUB_EVENT_NAME || 'manual'
  };

  return data;
}

/**
 * Calculate test metrics
 * @returns {Object} Test metrics
 */
function getTestMetrics() {
  // This would parse test output or coverage files
  const metrics = {
    total_tests: 0,
    passed: 0,
    failed: 0,
    skipped: 0,
    duration: 0
  };

  // Try to read from test output if available
  if (existsSync('./test-results.json')) {
    try {
      const results = JSON.parse(readFileSync('./test-results.json', 'utf8'));
      metrics.total_tests = results.numTotalTests || 0;
      metrics.passed = results.numPassedTests || 0;
      metrics.failed = results.numFailedTests || 0;
      metrics.skipped = results.numPendingTests || 0;
    } catch (error) {
      console.warn('Could not parse test results');
    }
  }

  return metrics;
}

/**
 * Calculate coverage metrics
 * @returns {Object} Coverage metrics
 */
function getCoverageMetrics() {
  const metrics = {
    lines: 0,
    statements: 0,
    functions: 0,
    branches: 0
  };

  if (existsSync('./coverage/coverage-summary.json')) {
    try {
      const coverage = JSON.parse(readFileSync('./coverage/coverage-summary.json', 'utf8'));
      const total = coverage.total;

      metrics.lines = total.lines.pct;
      metrics.statements = total.statements.pct;
      metrics.functions = total.functions.pct;
      metrics.branches = total.branches.pct;
    } catch (error) {
      console.warn('Could not parse coverage summary');
    }
  }

  return metrics;
}

/**
 * Load metrics history
 * @returns {Array} Metrics history
 */
function loadHistory() {
  if (existsSync(METRICS_FILE)) {
    try {
      return JSON.parse(readFileSync(METRICS_FILE, 'utf8'));
    } catch (error) {
      console.warn('Could not load metrics history');
    }
  }
  return [];
}

/**
 * Calculate trends from history
 * @param {Array} history - Metrics history
 * @returns {Object} Trend analysis
 */
function calculateTrends(history) {
  if (history.length < 2) {
    return { message: 'Insufficient data for trends' };
  }

  const recent = history.slice(-10);
  const older = history.slice(-20, -10);

  // Calculate average coverage
  const recentAvgCoverage = recent.reduce((sum, m) => sum + (m.coverage?.lines || 0), 0) / recent.length;
  const olderAvgCoverage = older.length > 0
    ? older.reduce((sum, m) => sum + (m.coverage?.lines || 0), 0) / older.length
    : recentAvgCoverage;

  // Calculate average duration
  const recentAvgDuration = recent.reduce((sum, m) => sum + (m.tests?.duration || 0), 0) / recent.length;
  const olderAvgDuration = older.length > 0
    ? older.reduce((sum, m) => sum + (m.tests?.duration || 0), 0) / older.length
    : recentAvgDuration;

  return {
    coverage: {
      current: recentAvgCoverage.toFixed(2),
      previous: olderAvgCoverage.toFixed(2),
      trend: recentAvgCoverage > olderAvgCoverage ? 'improving' : 'declining'
    },
    duration: {
      current: Math.round(recentAvgDuration),
      previous: Math.round(olderAvgDuration),
      trend: recentAvgDuration < olderAvgDuration ? 'improving' : 'declining'
    }
  };
}

/**
 * Main execution
 */
function main() {
  console.log('Collecting CI metrics...');

  // Collect current metrics
  const currentMetrics = {
    ...getCIRunData(),
    tests: getTestMetrics(),
    coverage: getCoverageMetrics(),
    collected_at: new Date().toISOString()
  };

  // Load history and add current metrics
  const history = loadHistory();
  history.push(currentMetrics);

  // Limit history size
  const trimmedHistory = history.slice(-HISTORY_LIMIT);

  // Calculate trends
  const trends = calculateTrends(trimmedHistory);

  // Save metrics
  writeFileSync(METRICS_FILE, JSON.stringify(trimmedHistory, null, 2));

  // Report
  console.log('\nCI Metrics Summary:');
  console.log(`  Workflow: ${currentMetrics.workflow}`);
  console.log(`  Branch: ${currentMetrics.branch}`);
  console.log(`  Tests: ${currentMetrics.tests.passed}/${currentMetrics.tests.total_tests} passed`);
  console.log(`  Coverage: ${currentMetrics.coverage.lines}%`);

  if (trends.coverage) {
    console.log('\nTrends:');
    console.log(`  Coverage: ${trends.coverage.trend} (${trends.coverage.current}% vs ${trends.coverage.previous}%)`);
    console.log(`  Duration: ${trends.duration.trend} (${trends.duration.current}ms vs ${trends.duration.previous}ms)`);
  }

  console.log(`\n✅ Metrics collected (${trimmedHistory.length} runs tracked)`);
}

main();
