#!/usr/bin/env node
/**
 * Walkthrough Test Runner
 * Executes each walkthrough validation test and reports results
 */

import { readdir } from 'fs/promises';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Run a single test file
 * @param {string} testFile - Path to test file
 * @returns {Promise<{success: boolean, output: string, duration: number}>}
 */
async function runTest(testFile) {
  return new Promise((resolve) => {
    const startTime = Date.now();
    const proc = spawn('node', [testFile], {
      cwd: __dirname,
      env: { ...process.env, NODE_ENV: 'test' }
    });

    let output = '';
    let errorOutput = '';

    proc.stdout.on('data', (data) => {
      output += data.toString();
    });

    proc.stderr.on('data', (data) => {
      errorOutput += data.toString();
    });

    proc.on('close', (code) => {
      const duration = Date.now() - startTime;
      resolve({
        success: code === 0,
        output: output + errorOutput,
        duration
      });
    });
  });
}

/**
 * Main test execution
 */
async function main() {
  console.log('🧪 UNRDF Walkthrough Validation');
  console.log('='.repeat(80));
  console.log('');

  const files = await readdir(__dirname);
  const testFiles = files
    .filter(f => f.startsWith('walkthrough-') && f.endsWith('.test.mjs'))
    .sort();

  if (testFiles.length === 0) {
    console.log('❌ No test files found');
    process.exit(1);
  }

  const results = [];

  for (const testFile of testFiles) {
    const testPath = join(__dirname, testFile);
    const walkthroughNum = testFile.match(/walkthrough-(\d+)/)?.[1] || '?';

    console.log(`\n📋 Testing Walkthrough ${walkthroughNum}: ${testFile}`);
    console.log('-'.repeat(80));

    const result = await runTest(testPath);
    results.push({ file: testFile, ...result });

    if (result.success) {
      console.log(`✅ PASSED (${result.duration}ms)`);
    } else {
      console.log(`❌ FAILED (${result.duration}ms)`);
      console.log('\nOutput:');
      console.log(result.output);
    }
  }

  // Summary
  console.log('\n' + '='.repeat(80));
  console.log('📊 SUMMARY');
  console.log('='.repeat(80));

  const passed = results.filter(r => r.success).length;
  const failed = results.filter(r => !r.success).length;
  const total = results.length;

  console.log(`\nTotal: ${total}`);
  console.log(`Passed: ${passed} (${((passed / total) * 100).toFixed(1)}%)`);
  console.log(`Failed: ${failed} (${((failed / total) * 100).toFixed(1)}%)`);

  if (failed > 0) {
    console.log('\n❌ Failed tests:');
    results.filter(r => !r.success).forEach(r => {
      console.log(`  - ${r.file}`);
    });
  }

  console.log('');
  process.exit(failed > 0 ? 1 : 0);
}

main().catch(console.error);
