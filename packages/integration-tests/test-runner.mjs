#!/usr/bin/env node
/**
 * @file Enhanced Integration Test Runner
 * @description
 * Runs integration tests with better error messages, setup/teardown helpers,
 * and progress reporting. Focuses on DX improvements.
 */

import { spawn } from 'child_process';
import { readdir } from 'fs/promises';
import { join } from 'path';

/**
 * ANSI color codes for terminal output
 */
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Test suite categories
 */
const TEST_SUITES = [
  { name: 'workflows', path: './workflows', timeout: 30000 },
  { name: 'federation', path: './federation', timeout: 30000 },
  { name: 'streaming', path: './streaming', timeout: 30000 },
  { name: 'error-recovery', path: './error-recovery', timeout: 30000 },
  { name: 'performance', path: './performance', timeout: 60000 },
];

/**
 * Run a test suite
 * @param {Object} suite - Test suite configuration
 * @returns {Promise<Object>} Test results
 */
async function runTestSuite(suite) {
  console.log(
    `\n${colors.blue}▶${colors.reset} Running ${colors.bright}${suite.name}${colors.reset} tests...`
  );

  return new Promise((resolve) => {
    const startTime = Date.now();
    const timeout = suite.timeout;

    const vitestProcess = spawn(
      'timeout',
      [
        `${Math.floor(timeout / 1000)}s`,
        'npx',
        'vitest',
        'run',
        suite.path,
        '--reporter=verbose',
        '--no-coverage',
      ],
      {
        cwd: process.cwd(),
        stdio: 'pipe',
        env: { ...process.env, FORCE_COLOR: '1' },
      }
    );

    let stdout = '';
    let stderr = '';

    vitestProcess.stdout.on('data', (data) => {
      const output = data.toString();
      stdout += output;
      process.stdout.write(output);
    });

    vitestProcess.stderr.on('data', (data) => {
      const output = data.toString();
      stderr += output;
      process.stderr.write(output);
    });

    vitestProcess.on('close', (code) => {
      const duration = Date.now() - startTime;
      const success = code === 0;

      if (success) {
        console.log(
          `${colors.green}✓${colors.reset} ${suite.name} tests passed in ${duration}ms`
        );
      } else {
        console.error(
          `${colors.red}✗${colors.reset} ${suite.name} tests failed (exit code: ${code})`
        );
      }

      resolve({
        suite: suite.name,
        success,
        duration,
        exitCode: code,
        stdout,
        stderr,
      });
    });

    vitestProcess.on('error', (error) => {
      console.error(
        `${colors.red}✗${colors.reset} Failed to run ${suite.name}: ${error.message}`
      );
      resolve({
        suite: suite.name,
        success: false,
        duration: Date.now() - startTime,
        error: error.message,
      });
    });
  });
}

/**
 * Generate summary report
 * @param {Array} results - Test results
 */
function generateSummary(results) {
  console.log(`\n${colors.bright}═══ Test Summary ═══${colors.reset}\n`);

  const totalDuration = results.reduce((sum, r) => sum + r.duration, 0);
  const passed = results.filter((r) => r.success).length;
  const failed = results.filter((r) => !r.success).length;

  results.forEach((result) => {
    const icon = result.success
      ? `${colors.green}✓${colors.reset}`
      : `${colors.red}✗${colors.reset}`;
    const duration = `${(result.duration / 1000).toFixed(2)}s`;
    console.log(`${icon} ${result.suite.padEnd(20)} ${duration}`);
  });

  console.log(`\n${colors.bright}Results:${colors.reset}`);
  console.log(`  Passed: ${colors.green}${passed}${colors.reset}`);
  console.log(`  Failed: ${colors.red}${failed}${colors.reset}`);
  console.log(
    `  Total:  ${(totalDuration / 1000).toFixed(2)}s\n`
  );

  // Performance warnings
  const slowTests = results.filter((r) => r.duration > 10000);
  if (slowTests.length > 0) {
    console.log(`${colors.yellow}⚠ Slow tests detected:${colors.reset}`);
    slowTests.forEach((test) => {
      console.log(
        `  ${test.suite}: ${(test.duration / 1000).toFixed(2)}s`
      );
    });
    console.log();
  }

  return failed === 0;
}

/**
 * Main test runner
 */
async function main() {
  const args = process.argv.slice(2);
  const suiteName = args[0];

  console.log(
    `${colors.cyan}${colors.bright}UNRDF Integration Test Runner${colors.reset}\n`
  );

  let suitesToRun = TEST_SUITES;

  // If suite name provided, run only that suite
  if (suiteName) {
    const suite = TEST_SUITES.find((s) => s.name === suiteName);
    if (!suite) {
      console.error(
        `${colors.red}Error:${colors.reset} Unknown test suite: ${suiteName}`
      );
      console.log('\nAvailable suites:');
      TEST_SUITES.forEach((s) => console.log(`  - ${s.name}`));
      process.exit(1);
    }
    suitesToRun = [suite];
  }

  const startTime = Date.now();
  const results = [];

  // Run test suites
  for (const suite of suitesToRun) {
    const result = await runTestSuite(suite);
    results.push(result);
  }

  const totalDuration = Date.now() - startTime;

  // Generate summary
  const allPassed = generateSummary(results);

  // Final message
  if (allPassed) {
    console.log(
      `${colors.green}${colors.bright}All tests passed!${colors.reset} (${(totalDuration / 1000).toFixed(2)}s)\n`
    );
    process.exit(0);
  } else {
    console.log(
      `${colors.red}${colors.bright}Some tests failed.${colors.reset}\n`
    );
    process.exit(1);
  }
}

main().catch((error) => {
  console.error(`${colors.red}Fatal error:${colors.reset}`, error);
  process.exit(1);
});
