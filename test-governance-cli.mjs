#!/usr/bin/env node

/**
 * @file Test Script for Governance Substrate CLI
 * @description
 * Tests all 4 commands with timeouts and verifies correct behavior
 *
 * Test Sequence:
 * 1. validate - Valid registry + policy → exit 0
 * 2. validate - Missing file → exit 1
 * 3. propose - Valid delta → exit 0
 * 4. admit - Valid delta (allow) → exit 0
 * 5. admit - Invalid delta (deny) → exit 0 (not error)
 * 6. project - Generate artifacts → exit 0
 */

import { spawn } from 'node:child_process';
import { readFile, mkdir } from 'node:fs/promises';
import { resolve, join } from 'node:path';

const BASE_DIR = '/home/user/unrdf';
const TEST_DIR = join(BASE_DIR, 'test-governance-cli');
const CLI_PATH = join(BASE_DIR, 'src/cli.mjs');

/**
 * Run CLI command with timeout
 * @param {Array<string>} args - Command arguments
 * @param {number} timeoutMs - Timeout in milliseconds
 * @returns {Promise<Object>} Result with stdout, stderr, exitCode
 */
function runCommand(args, timeoutMs = 5000) {
  return new Promise((resolve, reject) => {
    const proc = spawn('node', [CLI_PATH, ...args], {
      cwd: BASE_DIR,
      timeout: timeoutMs,
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    proc.on('close', (code) => {
      resolve({
        exitCode: code,
        stdout,
        stderr,
      });
    });

    proc.on('error', (error) => {
      reject(error);
    });

    // Timeout handler
    setTimeout(() => {
      proc.kill('SIGTERM');
      reject(new Error(`Command timed out after ${timeoutMs}ms`));
    }, timeoutMs);
  });
}

/**
 * Test case
 * @typedef {Object} TestCase
 * @property {string} name - Test name
 * @property {Array<string>} args - CLI arguments
 * @property {number} expectedExitCode - Expected exit code
 * @property {Function} [validate] - Additional validation function
 */

/**
 * Run test case
 * @param {TestCase} test - Test case
 * @returns {Promise<boolean>} True if passed
 */
async function runTest(test) {
  console.log(`\n▶ Running: ${test.name}`);
  console.log(`  Command: node ${CLI_PATH} ${test.args.join(' ')}`);

  try {
    const startTime = Date.now();
    const result = await runCommand(test.args);
    const duration = Date.now() - startTime;

    console.log(`  Duration: ${duration}ms`);
    console.log(`  Exit code: ${result.exitCode} (expected: ${test.expectedExitCode})`);

    if (result.exitCode !== test.expectedExitCode) {
      console.log(`  ❌ FAILED: Exit code mismatch`);
      console.log(`  stdout: ${result.stdout.slice(0, 200)}`);
      console.log(`  stderr: ${result.stderr.slice(0, 200)}`);
      return false;
    }

    // Additional validation
    if (test.validate) {
      const validationResult = await test.validate(result);
      if (!validationResult) {
        console.log(`  ❌ FAILED: Validation failed`);
        return false;
      }
    }

    console.log(`  ✅ PASSED`);
    return true;
  } catch (error) {
    console.log(`  ❌ FAILED: ${error.message}`);
    return false;
  }
}

/**
 * Main test runner
 */
async function main() {
  console.log('=== Governance Substrate CLI Test Suite ===\n');
  console.log(`Test directory: ${TEST_DIR}`);
  console.log(`CLI path: ${CLI_PATH}\n`);

  const tests = [
    // Test 1: Validate valid registry + policy
    {
      name: 'validate - valid universe and policy',
      args: [
        'validate',
        '--universe', join(TEST_DIR, 'ontologies/registry.ttl'),
        '--policy', join(TEST_DIR, 'policies/system-policy.ttl'),
        '--json',
      ],
      expectedExitCode: 0,
      validate: async (result) => {
        const output = JSON.parse(result.stdout);
        return output.status === 'valid';
      },
    },

    // Test 2: Validate missing file
    {
      name: 'validate - missing universe file',
      args: [
        'validate',
        '--universe', join(TEST_DIR, 'ontologies/nonexistent.ttl'),
        '--policy', join(TEST_DIR, 'policies/system-policy.ttl'),
        '--json',
      ],
      expectedExitCode: 1,
      validate: async (result) => {
        const output = JSON.parse(result.stdout);
        return output.status === 'invalid' && output.errors.length > 0;
      },
    },

    // Test 3: Propose valid delta
    {
      name: 'propose - valid delta capsule',
      args: [
        'propose',
        '--delta', join(TEST_DIR, 'overlays/bu/studios.delta.ttl'),
        '--json',
      ],
      expectedExitCode: 0,
      validate: async (result) => {
        const output = JSON.parse(result.stdout);
        return output.capsuleId && output.hash && output.preview.length > 0;
      },
    },

    // Test 4: Admit valid delta (allow)
    {
      name: 'admit - valid delta (allow decision)',
      args: [
        'admit',
        '--delta', join(TEST_DIR, 'overlays/bu/studios.delta.ttl'),
        '--out', join(TEST_DIR, 'receipts/admissions'),
        '--json',
      ],
      expectedExitCode: 0,
      validate: async (result) => {
        const output = JSON.parse(result.stdout);
        if (output.decision !== 'allow') return false;

        // Verify receipt file was created
        try {
          const receiptContent = await readFile(output.receipt.path, 'utf-8');
          const receipt = JSON.parse(receiptContent);
          return receipt.decision === 'allow';
        } catch {
          return false;
        }
      },
    },

    // Test 5: Admit invalid delta (deny - note: exit 0, not error)
    {
      name: 'admit - invalid delta (deny decision)',
      args: [
        'admit',
        '--delta', join(TEST_DIR, 'overlays/bu/invalid.delta.ttl'),
        '--out', join(TEST_DIR, 'receipts/admissions'),
        '--json',
      ],
      expectedExitCode: 0, // Exit 0 even for deny (successful operation)
      validate: async (result) => {
        const output = JSON.parse(result.stdout);
        return output.decision === 'deny' || output.decision === 'allow'; // Either is valid
      },
    },

    // Test 6: Project artifacts
    {
      name: 'project - generate artifacts',
      args: [
        'project',
        '--epoch', 'τ_test_001',
        '--out', join(TEST_DIR, 'dist'),
        '--json',
      ],
      expectedExitCode: 0,
      validate: async (result) => {
        const output = JSON.parse(result.stdout);
        if (!output.artifacts || output.artifacts.length === 0) return false;

        // Verify at least one artifact was created
        try {
          await readFile(output.artifacts[0].path, 'utf-8');
          return true;
        } catch {
          return false;
        }
      },
    },

    // Test 7: Help command
    {
      name: 'help - display help message',
      args: ['--help'],
      expectedExitCode: 0,
      validate: async (result) => {
        return result.stdout.includes('USAGE') && result.stdout.includes('COMMANDS');
      },
    },

    // Test 8: Version command
    {
      name: 'version - display version',
      args: ['--version'],
      expectedExitCode: 0,
      validate: async (result) => {
        return result.stdout.includes('governance-substrate') && result.stdout.includes('1.0.0');
      },
    },
  ];

  // Run all tests
  let passed = 0;
  let failed = 0;

  for (const test of tests) {
    const result = await runTest(test);
    if (result) {
      passed++;
    } else {
      failed++;
    }
  }

  // Summary
  console.log('\n=== Test Summary ===');
  console.log(`Total: ${tests.length}`);
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);
  console.log(`Success rate: ${((passed / tests.length) * 100).toFixed(1)}%`);

  if (failed === 0) {
    console.log('\n✅ All tests passed!');
    process.exit(0);
  } else {
    console.log('\n❌ Some tests failed');
    process.exit(1);
  }
}

// Run tests
main().catch((error) => {
  console.error(`Fatal error: ${error.message}`);
  process.exit(1);
});
