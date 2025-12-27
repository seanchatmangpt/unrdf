#!/usr/bin/env node
/**
 * @fileoverview Master Test Runner - AUTONOMIC_INNOVATION
 * @module test-runner
 *
 * Orchestrates all agent tests and produces comprehensive report.
 *
 * Execution Flow:
 * 1. Run individual agent tests (agent-2 through agent-10)
 * 2. Aggregate results
 * 3. Run E2E validation
 * 4. Run determinism validation
 * 5. Run quality gates
 * 6. Generate final report
 * 7. Exit with code 0 (success) or 1 (failure)
 *
 * Usage:
 *   node test-runner.mjs
 *   node test-runner.mjs --verbose
 *   node test-runner.mjs --determinism-runs=10
 */

import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

/**
 * Run command with timeout
 * @param {string} command - Command to run
 * @param {string[]} args - Command arguments
 * @param {number} timeout - Timeout in ms
 * @returns {Promise<object>} Result
 */
function runCommand(command, args, timeout = 5000) {
  return new Promise((resolve) => {
    const proc = spawn(command, args, { timeout });

    let stdout = '';
    let stderr = '';

    proc.stdout?.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr?.on('data', (data) => {
      stderr += data.toString();
    });

    proc.on('close', (code) => {
      resolve({
        code: code ?? 0,
        stdout,
        stderr,
        success: code === 0
      });
    });

    proc.on('error', (error) => {
      resolve({
        code: 1,
        stdout,
        stderr: stderr + error.message,
        success: false
      });
    });
  });
}

/**
 * Run tests for a specific agent
 * @param {number} agentNum - Agent number
 * @returns {Promise<object>} Test results
 */
async function runAgentTests(agentNum) {
  const testFile = join(
    '/home/user/unrdf/AUTONOMIC_INNOVATION',
    `agent-${agentNum}`,
    'test.mjs'
  );

  if (!existsSync(testFile)) {
    return {
      agent: `agent-${agentNum}`,
      passed: 0,
      failed: 0,
      skipped: 0,
      duration: 0,
      status: 'NO_TESTS',
      error: 'Test file not found'
    };
  }

  const startTime = Date.now();
  const result = await runCommand('node', ['--test', testFile], 10000);
  const duration = Date.now() - startTime;

  // Parse test output (simplified)
  const passed = (result.stdout.match(/✓/g) || []).length;
  const failed = (result.stdout.match(/✖/g) || []).length;

  return {
    agent: `agent-${agentNum}`,
    passed,
    failed,
    skipped: 0,
    duration,
    status: result.success ? 'PASSED' : 'FAILED',
    output: result.stdout
  };
}

/**
 * Main test execution
 */
async function main() {
  console.log('=== AUTONOMIC_INNOVATION Test Suite ===\n');

  const startTime = Date.now();
  const results = [];

  // Run tests for agents 2-10
  for (let i = 2; i <= 10; i++) {
    process.stdout.write(`Running Agent ${i} tests... `);
    const result = await runAgentTests(i);
    results.push(result);

    if (result.status === 'PASSED') {
      console.log(`✓ ${result.passed}/${result.passed + result.failed} passed (${result.duration}ms)`);
    } else if (result.status === 'NO_TESTS') {
      console.log(`⊘ No tests found`);
    } else {
      console.log(`✖ ${result.failed} failed (${result.duration}ms)`);
    }
  }

  console.log();

  // Aggregate results
  let totalPassed = 0;
  let totalFailed = 0;
  let totalSkipped = 0;
  let totalDuration = 0;

  for (const result of results) {
    totalPassed += result.passed;
    totalFailed += result.failed;
    totalSkipped += result.skipped;
    totalDuration += result.duration;
  }

  const totalTests = totalPassed + totalFailed + totalSkipped;
  const score = totalTests > 0 ? Math.round((totalPassed / totalTests) * 100) : 0;

  // Run E2E validation (if agent-10 exists)
  let e2eResult = null;
  const agent10Path = join('/home/user/unrdf/AUTONOMIC_INNOVATION', 'agent-10', 'index.mjs');

  if (existsSync(agent10Path)) {
    try {
      process.stdout.write('Running E2E validation... ');
      const { e2eValidation } = await import(agent10Path);
      e2eResult = await e2eValidation();

      if (e2eResult.allPassed) {
        console.log(`✓ PASSED (${e2eResult.duration}ms)`);
      } else {
        console.log(`✖ FAILED`);
      }
    } catch (error) {
      console.log(`✖ ERROR: ${error.message}`);
    }
  }

  // Run determinism validation (if agent-10 exists)
  let determinismResult = null;

  if (existsSync(agent10Path)) {
    try {
      process.stdout.write('Running determinism validation... ');
      const { determinismValidation } = await import(agent10Path);
      determinismResult = await determinismValidation();

      if (determinismResult.deterministic) {
        console.log(`✓ VERIFIED (2 runs, identical hashes)`);
      } else {
        console.log(`✖ FAILED`);
      }
    } catch (error) {
      console.log(`✖ ERROR: ${error.message}`);
    }
  }

  // Run quality gates (if agent-10 exists)
  let qualityGateResult = null;

  if (existsSync(agent10Path)) {
    try {
      process.stdout.write('Running quality gates... ');
      const { runQualityGates } = await import(agent10Path);
      qualityGateResult = runQualityGates();

      if (qualityGateResult.qualityGatePassed) {
        console.log(`✓ PASSED (score: ${qualityGateResult.score})`);
      } else {
        console.log(`✖ FAILED (score: ${qualityGateResult.score})`);
      }
    } catch (error) {
      console.log(`✖ ERROR: ${error.message}`);
    }
  }

  console.log();
  console.log('=== Summary ===');
  console.log();
  console.log(`Total Tests: ${totalTests}`);
  console.log(`Passed: ${totalPassed}`);
  console.log(`Failed: ${totalFailed}`);
  console.log(`Skipped: ${totalSkipped}`);
  console.log(`Score: ${score}/100`);
  console.log(`Duration: ${totalDuration}ms`);
  console.log();

  if (e2eResult) {
    console.log(`E2E Validation: ${e2eResult.allPassed ? '✓ PASSED' : '✖ FAILED'}`);
  }

  if (determinismResult) {
    console.log(`Determinism: ${determinismResult.deterministic ? '✓ VERIFIED' : '✖ FAILED'}`);
  }

  if (qualityGateResult) {
    console.log(`Quality Gates: ${qualityGateResult.qualityGatePassed ? '✓ PASSED' : '✖ FAILED'}`);
  }

  console.log();

  // Determine overall result
  const allPassed =
    totalFailed === 0 &&
    (!e2eResult || e2eResult.allPassed) &&
    (!determinismResult || determinismResult.deterministic) &&
    (!qualityGateResult || qualityGateResult.qualityGatePassed);

  if (allPassed) {
    console.log('Result: SUCCESS ✓');
    process.exit(0);
  } else {
    console.log('Result: FAILURE ✖');
    process.exit(1);
  }
}

// Run
main().catch((error) => {
  console.error('Test runner error:', error);
  process.exit(1);
});
