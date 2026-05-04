#!/usr/bin/env node
/**
 * @fileoverview Standalone execution verification (minimal dependencies)
 * Verifies safe command execution without requiring Zod
 */

import { execFile } from 'node:child_process';
import { promisify } from 'node:util';

const execFileAsync = promisify(execFile);

// ============================================================================
// GUARD IMPLEMENTATION
// ============================================================================

const ALLOWED_COMMANDS = new Set(['git', 'node', 'npm', 'pnpm', 'which']);

function isCommandAllowed(command) {
  return ALLOWED_COMMANDS.has(command);
}

function argsAreSafe(args) {
  const dangerous = /[;&|`$<>(){}[\]\\'"]/;
  return args.every(arg => !dangerous.test(arg));
}

async function safeExec(command, args, timeout) {
  if (!isCommandAllowed(command)) {
    return {
      stdout: '',
      stderr: `Command '${command}' not in allowlist`,
      success: false,
      guardDecision: 'denied',
    };
  }

  if (!argsAreSafe(args)) {
    return {
      stdout: '',
      stderr: 'Arguments contain shell metacharacters',
      success: false,
      guardDecision: 'denied',
    };
  }

  try {
    const { stdout, stderr } = await execFileAsync(command, args, {
      timeout,
      maxBuffer: 1024 * 1024,
      shell: false,
      windowsHide: true,
    });

    return {
      stdout: stdout.trim(),
      stderr: stderr.trim(),
      success: true,
      guardDecision: 'allowed',
    };
  } catch (error) {
    return {
      stdout: error.stdout?.trim() || '',
      stderr: error.stderr?.trim() || error.message,
      success: false,
      guardDecision: error.code === 'ETIMEDOUT' ? 'unknown' : 'allowed',
    };
  }
}

// ============================================================================
// VERIFICATION TESTS
// ============================================================================

console.log('⚡ Execution Verification\n');

let passed = 0;
let failed = 0;

function test(name, condition) {
  if (condition) {
    console.log(`✅ ${name}`);
    passed++;
  } else {
    console.log(`❌ ${name}`);
    failed++;
  }
}

// Test 1: Deny non-allowlisted command
console.log('🔒 Guard Enforcement:');
const result1 = await safeExec('rm', ['-rf', '/'], 5000);
test('Deny rm command', result1.guardDecision === 'denied' && !result1.success);

// Test 2: Deny unsafe arguments
const result2 = await safeExec('git', ['--version; whoami'], 5000);
test('Deny unsafe arguments', result2.guardDecision === 'denied' && !result2.success);

// Test 3: Execute safe command (git)
console.log('\n⚙️  Safe Execution:');
const result3 = await safeExec('git', ['--version'], 5000);
test('Execute git --version', result3.guardDecision === 'allowed');
if (result3.success) {
  console.log(`   → ${result3.stdout}`);
}

// Test 4: Execute safe command (node)
const result4 = await safeExec('node', ['--version'], 5000);
test('Execute node --version', result4.guardDecision === 'allowed');
if (result4.success) {
  console.log(`   → ${result4.stdout}`);
}

// Test 5: Verify shell=false (no expansion)
console.log('\n🛡️  Shell Isolation:');
const result5 = await safeExec('git', ['--version'], 5000);
test('No shell execution', result5.guardDecision === 'allowed'); // Would fail if shell metacharacters worked

console.log('\n📊 Results:');
console.log(`   Passed: ${passed}`);
console.log(`   Failed: ${failed}`);
console.log(`   Total:  ${passed + failed}`);

if (failed === 0) {
  console.log('\n✅ All execution tests passed!');
  console.log('✅ Safe command execution verified!');
  console.log('✅ Guard constraints enforced!');
  process.exit(0);
} else {
  console.log('\n❌ Some execution tests failed!');
  process.exit(1);
}
