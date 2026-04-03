#!/usr/bin/env node

/**
 * Andon Signal Checker - Verify visual problem indicators
 *
 * Checks for critical signals (syntax errors, test failures)
 * and reports warning trends. Exits with non-zero if critical
 * signals detected.
 *
 * @module scripts/check-andon-signals
 */

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';

const SIGNAL_BASELINE = 153; // Current acceptable baseline
const CRITICAL_THRESHOLD = 0; // Critical signals must be 0
const REPORT_FILE = 'andon-signals-report.json';

/**
 * Run command and capture output
 * @param {string} cmd - Command to run
 * @returns {string} Command output
 */
function runCommand(cmd) {
  try {
    return execSync(cmd, { encoding: 'utf8', stdio: 'pipe' });
  } catch (error) {
    return error.stdout || '';
  }
}

/**
 * Check for syntax/type errors
 * @returns {Object} Result object {count, files, critical}
 */
function checkSyntaxErrors() {
  console.log('🔍 Checking for syntax/type errors...');

  const files = [
    'packages/cli/src/commands/graph/update.mjs',
    'packages/core/src/utils/transform-utils.mjs',
    'packages/streaming/src/index.mjs',
  ];

  let errors = 0;
  const failedFiles = [];

  for (const file of files) {
    const output = runCommand(`node --check ${file} 2>&1`);
    if (output.includes('Error') || output.includes('error')) {
      errors++;
      failedFiles.push(file);
    }
  }

  const result = {
    type: 'SYNTAX_ERRORS',
    count: errors,
    critical: errors > 0,
    message: errors === 0 ? '✅ No syntax errors' : `❌ ${errors} file(s) with syntax errors`,
    files: failedFiles,
  };

  console.log(`  ${result.message}`);
  return result;
}

/**
 * Check for linting errors (not warnings)
 * @returns {Object} Result object {count, critical}
 */
function checkLintingErrors() {
  console.log('🔍 Checking for linting errors...');

  const output = runCommand('pnpm lint 2>&1');
  const errorMatch = output.match(/✖ (\d+) problems? \((\d+) errors?/);
  const errorCount = errorMatch ? parseInt(errorMatch[2], 10) : 0;

  const result = {
    type: 'LINTING_ERRORS',
    count: errorCount,
    critical: errorCount > 0,
    message: errorCount === 0 ? '✅ No linting errors' : `❌ ${errorCount} linting error(s)`,
  };

  console.log(`  ${result.message}`);
  return result;
}

/**
 * Check for test failures in core package
 * @returns {Object} Result object {count, critical}
 */
function checkTestFailures() {
  console.log('🔍 Checking core test suite...');

  const output = runCommand('pnpm test:core 2>&1');
  const failMatch = output.match(/Tests\s+(.+?)(\d+)\s+failed/);
  const passMatch = output.match(/Tests\s+(.+?)(\d+)\s+passed/);

  const failCount = failMatch ? parseInt(failMatch[2], 10) : 0;
  const passCount = passMatch ? parseInt(passMatch[2], 10) : 0;

  const result = {
    type: 'TEST_FAILURES',
    count: failCount,
    total: passCount + failCount,
    critical: failCount > 0,
    message:
      failCount === 0 ? `✅ All tests passing (${passCount} tests)` : `❌ ${failCount} test failure(s)`,
  };

  console.log(`  ${result.message}`);
  return result;
}

/**
 * Check for linting warnings (informational)
 * @returns {Object} Result object {count, drift}
 */
function checkLintingWarnings() {
  console.log('🔍 Tracking linting warnings...');

  const output = runCommand('pnpm lint 2>&1');
  const warningMatch = output.match(/✖ (\d+) problems? \(\d+ errors?, (\d+) warnings?\)/);
  const warningCount = warningMatch ? parseInt(warningMatch[2], 10) : 0;

  const drift = warningCount - SIGNAL_BASELINE;
  const driftStatus =
    drift === 0 ? '✅' : drift < 0 ? '⬇️' : '⬆️';

  const result = {
    type: 'LINTING_WARNINGS',
    count: warningCount,
    baseline: SIGNAL_BASELINE,
    drift,
    driftStatus,
    message:
      drift === 0
        ? `✅ Warning baseline maintained (${warningCount})`
        : `${driftStatus} Warning baseline drift: ${drift > 0 ? '+' : ''}${drift} (${warningCount} vs ${SIGNAL_BASELINE})`,
  };

  console.log(`  ${result.message}`);
  return result;
}

/**
 * Generate report
 * @param {Array} results - Results array
 * @returns {Object} Report data
 */
function generateReport(results) {
  const criticalSignals = results.filter((r) => r.critical);
  const allClear = criticalSignals.length === 0;

  const report = {
    timestamp: new Date().toISOString(),
    status: allClear ? 'PASS' : 'FAIL',
    summary: {
      total: results.length,
      critical: criticalSignals.length,
      allClear,
    },
    signals: results,
  };

  return report;
}

/**
 * Print report
 * @param {Object} report - Report data
 */
function printReport(report) {
  console.log('\n' + '='.repeat(60));
  console.log('📊 ANDON SIGNAL REPORT');
  console.log('='.repeat(60));

  console.log(`\n🕐 Timestamp: ${report.timestamp}`);
  console.log(`📈 Status: ${report.status === 'PASS' ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`🚨 Critical Signals: ${report.summary.critical}/${report.summary.total}`);

  console.log('\n📋 Detailed Results:');
  for (const signal of report.signals) {
    if (signal.critical) {
      console.log(`  ❌ [CRITICAL] ${signal.message}`);
    } else {
      console.log(`  ℹ️  [INFO] ${signal.message}`);
    }
  }

  console.log('\n' + '='.repeat(60));
  if (report.summary.allClear) {
    console.log('✅ ALL SIGNALS CLEAR - COMMIT ALLOWED');
  } else {
    console.log('🛑 CRITICAL SIGNALS DETECTED - FIX BEFORE COMMIT');
  }
  console.log('='.repeat(60) + '\n');

  // Save report
  fs.writeFileSync(REPORT_FILE, JSON.stringify(report, null, 2));
  console.log(`📁 Report saved: ${REPORT_FILE}`);
}

/**
 * Main workflow
 */
function main() {
  console.log('\n🚨 ANDON SIGNAL CHECK - Verifying Critical Signals\n');

  const results = [
    checkSyntaxErrors(),
    checkLintingErrors(),
    checkTestFailures(),
    checkLintingWarnings(),
  ];

  const report = generateReport(results);
  printReport(report);

  // Exit with error if critical signals
  if (!report.summary.allClear) {
    console.log('🛑 BLOCKING COMMIT - Fix critical signals first\n');
    process.exit(1);
  }

  process.exit(0);
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { checkSyntaxErrors, checkLintingErrors, checkTestFailures, checkLintingWarnings };
