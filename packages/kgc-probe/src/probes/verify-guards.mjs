#!/usr/bin/env node
/**
 * @fileoverview Standalone guard verification (NO dependencies)
 * Verifies allowlist and argument safety without requiring Zod
 */

// ============================================================================
// GUARD IMPLEMENTATION (copied from tooling.mjs for standalone verification)
// ============================================================================

const ALLOWED_COMMANDS = new Set(['git', 'node', 'npm', 'pnpm', 'which']);

function isCommandAllowed(command) {
  return ALLOWED_COMMANDS.has(command);
}

function argsAreSafe(args) {
  const dangerous = /[;&|`$<>(){}[\]\\'"]/;
  return args.every(arg => !dangerous.test(arg));
}

// ============================================================================
// VERIFICATION TESTS
// ============================================================================

console.log('🔒 Guard Verification (Poka-Yoke)\n');

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

// Allowlist tests
console.log('📋 Command Allowlist:');
test('Allow git', isCommandAllowed('git'));
test('Allow node', isCommandAllowed('node'));
test('Allow npm', isCommandAllowed('npm'));
test('Allow pnpm', isCommandAllowed('pnpm'));
test('Allow which', isCommandAllowed('which'));
test('Deny rm', !isCommandAllowed('rm'));
test('Deny curl', !isCommandAllowed('curl'));
test('Deny bash', !isCommandAllowed('bash'));
test('Deny arbitrary', !isCommandAllowed('evil-command'));

console.log('\n🛡️  Argument Safety:');
test('Safe: --version', argsAreSafe(['--version']));
test('Safe: --help', argsAreSafe(['--help']));
test('Unsafe: semicolon', !argsAreSafe(['--foo; rm -rf /']));
test('Unsafe: pipe', !argsAreSafe(['--foo | cat']));
test('Unsafe: backtick', !argsAreSafe(['--foo `whoami`']));
test('Unsafe: dollar', !argsAreSafe(['--foo $(whoami)']));
test('Unsafe: redirect', !argsAreSafe(['--foo > /tmp/evil']));

console.log('\n📊 Results:');
console.log(`   Passed: ${passed}`);
console.log(`   Failed: ${failed}`);
console.log(`   Total:  ${passed + failed}`);

if (failed === 0) {
  console.log('\n✅ All guard constraints verified!');
  process.exit(0);
} else {
  console.log('\n❌ Some guard constraints failed!');
  process.exit(1);
}
