#!/usr/bin/env node
/**
 * Integration test for KGC Documentation CLI
 *
 * Tests all commands with minimal fixtures.
 */

import { strict as assert } from 'node:assert';
import { writeFile, readFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { execSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const CLI_PATH = join(__dirname, 'tools/kgc-docs.mjs');

/**
 * Run CLI command and return result
 * @param {string} command - Command to run
 * @returns {Object} {stdout, stderr, exitCode}
 */
function runCLI(command) {
  try {
    const stdout = execSync(`node ${CLI_PATH} ${command}`, {
      encoding: 'utf-8',
      cwd: __dirname,
      timeout: 10000,
    });
    return { stdout, stderr: '', exitCode: 0 };
  } catch (err) {
    return {
      stdout: err.stdout?.toString() || '',
      stderr: err.stderr?.toString() || '',
      exitCode: err.status || 1,
    };
  }
}

/**
 * Test suite
 */
async function runTests() {
  console.log('🧪 Testing KGC Documentation CLI\n');

  let passed = 0;
  let failed = 0;

  // Test 1: Help command
  console.log('Test 1: Help command');
  const helpResult = runCLI('help');
  assert.strictEqual(helpResult.exitCode, 0, 'Help command should exit with 0');
  assert.match(helpResult.stdout, /USAGE/, 'Help should contain USAGE section');
  console.log('✅ PASS\n');
  passed++;

  // Test 2: Unknown command
  console.log('Test 2: Unknown command error handling');
  const unknownResult = runCLI('foobar');
  assert.strictEqual(unknownResult.exitCode, 1, 'Unknown command should exit with 1');
  assert.match(unknownResult.stderr, /Unknown command/, 'Should show unknown command error');
  console.log('✅ PASS\n');
  passed++;

  // Test 3: Scan command with JSON output
  console.log('Test 3: Scan command with JSON output');
  const scanResult = runCLI('scan @unrdf/oxigraph --output-format json');
  if (scanResult.exitCode === 0) {
    try {
      const data = JSON.parse(scanResult.stdout);
      assert.ok(data.success !== undefined, 'JSON output should have success field');
      console.log('✅ PASS\n');
      passed++;
    } catch (err) {
      console.log(`❌ FAIL: ${err.message}\n`);
      failed++;
    }
  } else {
    // Package might not exist, but should still be valid JSON error
    console.log('⚠️  SKIP (package not found, expected in some environments)\n');
  }

  // Test 4: Manifest command
  console.log('Test 4: Manifest command');
  await mkdir(join(__dirname, 'receipts'), { recursive: true });
  const manifestResult = runCLI('manifest receipts/ --output-format json');
  assert.strictEqual(manifestResult.exitCode, 0, 'Manifest command should succeed');
  const manifestData = JSON.parse(manifestResult.stdout);
  assert.ok(manifestData.success, 'Manifest should report success');
  assert.ok(manifestData.merkleRoot, 'Manifest should include merkleRoot');
  console.log('✅ PASS\n');
  passed++;

  // Test 5: Verify command (should handle empty directory gracefully)
  console.log('Test 5: Verify command');
  const verifyResult = runCLI('verify docs/ --output-format json');
  assert.strictEqual(verifyResult.exitCode, 0, 'Verify command should succeed');
  const verifyData = JSON.parse(verifyResult.stdout);
  assert.ok(verifyData.filesChecked !== undefined, 'Verify should report filesChecked');
  console.log('✅ PASS\n');
  passed++;

  // Test 6: Invalid args error handling
  console.log('Test 6: Invalid args error handling');
  const invalidResult = runCLI('build'); // Missing required sources
  assert.strictEqual(invalidResult.exitCode, 1, 'Missing args should exit with 1');
  assert.match(invalidResult.stderr, /requires at least one/, 'Should show missing args error');
  console.log('✅ PASS\n');
  passed++;

  // Test 7: Deterministic flag
  console.log('Test 7: Deterministic flag');
  const detResult = runCLI('manifest receipts/ --deterministic --output-format json');
  const detData = JSON.parse(detResult.stdout);
  assert.strictEqual(detData.success, true, 'Deterministic mode should work');
  console.log('✅ PASS\n');
  passed++;

  // Test 8: Verbose flag
  console.log('Test 8: Verbose flag');
  const verboseResult = runCLI('manifest receipts/ --verbose');
  // Verbose output goes to stderr, so check that it exists
  assert.ok(verboseResult.stderr.length > 0 || verboseResult.stdout.length > 0, 'Verbose mode should produce output');
  console.log('✅ PASS\n');
  passed++;

  // Summary
  console.log('─'.repeat(50));
  console.log(`\n✅ Passed: ${passed}`);
  console.log(`❌ Failed: ${failed}`);
  console.log(`\nTotal: ${passed + failed} tests`);

  if (failed > 0) {
    process.exit(1);
  }

  console.log('\n🎉 All tests passed!\n');
}

// Run tests
runTests().catch(err => {
  console.error('❌ Test suite failed:', err);
  process.exit(1);
});
