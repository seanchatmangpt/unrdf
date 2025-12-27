#!/usr/bin/env node
/**
 * Master Permutation Test Runner
 * Executes all package permutation tests and generates summary report
 */

import { spawn } from 'child_process';
import { readdir } from 'fs/promises';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ANSI colors
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

const startTime = Date.now();

console.log(`${colors.bright}${colors.blue}
╔═══════════════════════════════════════════════════════════════╗
║         UNRDF Package Permutation Test Suite                 ║
║         Testing ALL package combinations                      ║
╚═══════════════════════════════════════════════════════════════╝
${colors.reset}\n`);

// Find all test files
const testFiles = (await readdir(__dirname))
  .filter(f => f.match(/^\d{2}-.+\.mjs$/))
  .sort();

console.log(`${colors.cyan}Found ${testFiles.length} permutation tests${colors.reset}\n`);

const results = [];

// Run each test
for (const testFile of testFiles) {
  const testPath = join(__dirname, testFile);
  const testName = testFile.replace('.mjs', '');

  const result = await new Promise((resolve) => {
    const child = spawn('node', [testPath], {
      stdio: 'inherit',
      timeout: 10000, // 10 second timeout
    });

    const startTestTime = Date.now();

    child.on('exit', (code) => {
      const elapsed = Date.now() - startTestTime;
      resolve({
        name: testName,
        file: testFile,
        passed: code === 0,
        exitCode: code,
        duration: elapsed,
      });
    });

    child.on('error', (error) => {
      const elapsed = Date.now() - startTestTime;
      resolve({
        name: testName,
        file: testFile,
        passed: false,
        exitCode: -1,
        error: error.message,
        duration: elapsed,
      });
    });
  });

  results.push(result);
}

// Generate summary report
const totalTests = results.length;
const passedTests = results.filter(r => r.passed).length;
const failedTests = totalTests - passedTests;
const passRate = ((passedTests / totalTests) * 100).toFixed(1);
const totalElapsed = Date.now() - startTime;

console.log(`\n${colors.bright}${colors.blue}
╔═══════════════════════════════════════════════════════════════╗
║                    SUMMARY REPORT                             ║
╚═══════════════════════════════════════════════════════════════╝
${colors.reset}`);

console.log(`\n${colors.bright}Test Results:${colors.reset}`);
console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

results.forEach((result) => {
  const status = result.passed
    ? `${colors.green}✅ PASS${colors.reset}`
    : `${colors.red}❌ FAIL${colors.reset}`;
  const duration = `${result.duration}ms`;

  console.log(`${status}  ${result.name.padEnd(35)} (${duration})`);
});

console.log(`\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

console.log(`${colors.bright}Statistics:${colors.reset}`);
console.log(`  Total Tests:    ${totalTests}`);
console.log(`  ${colors.green}Passed:         ${passedTests}${colors.reset}`);
console.log(`  ${colors.red}Failed:         ${failedTests}${colors.reset}`);
console.log(`  Pass Rate:      ${passRate}%`);
console.log(`  Total Time:     ${totalElapsed}ms\n`);

// Consolidation insights
console.log(`${colors.bright}${colors.cyan}Consolidation Insights:${colors.reset}`);
console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

const passed = results.filter(r => r.passed).map(r => r.name);
const failed = results.filter(r => !r.passed).map(r => r.name);

// Analyze patterns
const coreOnly = passed.includes('01-core-only');
const hooksOnly = passed.includes('02-hooks-only');
const kgc4dOnly = passed.includes('03-kgc4d-only');
const knowledgeOnly = passed.includes('04-knowledge-engine-only');

const coreHooks = passed.includes('05-core-hooks');
const coreKgc4d = passed.includes('06-core-kgc4d');
const fullStack = passed.includes('11-core-hooks-kgc4d');
const allPackages = passed.includes('15-all-packages');

console.log(`${colors.bright}Package Independence:${colors.reset}`);
console.log(`  Core standalone:            ${coreOnly ? colors.green + '✅ YES' : colors.red + '❌ NO'}${colors.reset}`);
console.log(`  Hooks standalone:           ${hooksOnly ? colors.green + '✅ YES' : colors.red + '❌ NO (needs core)'}${colors.reset}`);
console.log(`  KGC 4D standalone:          ${kgc4dOnly ? colors.green + '✅ YES' : colors.red + '❌ NO (needs core)'}${colors.reset}`);
console.log(`  Knowledge Engine standalone: ${knowledgeOnly ? colors.green + '✅ YES' : colors.red + '❌ NO (needs core)'}${colors.reset}\n`);

console.log(`${colors.bright}Integration Success:${colors.reset}`);
console.log(`  Core + Hooks:               ${coreHooks ? colors.green + '✅ WORKS' : colors.red + '❌ BROKEN'}${colors.reset}`);
console.log(`  Core + KGC 4D:              ${coreKgc4d ? colors.green + '✅ WORKS' : colors.red + '❌ BROKEN'}${colors.reset}`);
console.log(`  Core + Hooks + KGC 4D:      ${fullStack ? colors.green + '✅ WORKS' : colors.red + '❌ BROKEN'}${colors.reset}`);
console.log(`  All 4 packages:             ${allPackages ? colors.green + '✅ WORKS' : colors.red + '❌ BROKEN'}${colors.reset}\n`);

console.log(`${colors.bright}80/20 Recommendation:${colors.reset}`);

if (!coreOnly) {
  console.log(`  ${colors.red}⚠️  CRITICAL: Core package is broken - fix immediately${colors.reset}`);
} else if (!hooksOnly && !kgc4dOnly && !knowledgeOnly) {
  console.log(`  ${colors.yellow}📊 All packages depend on @unrdf/core (100% dependency)${colors.reset}`);
  console.log(`  ${colors.cyan}💡 Consider: Merge hooks → core (simplify architecture)${colors.reset}`);
  console.log(`  ${colors.cyan}💡 Keep: kgc-4d separate (temporal layer is distinct concern)${colors.reset}`);
  console.log(`  ${colors.cyan}💡 Evaluate: knowledge-engine usage (is it worth the complexity?)${colors.reset}`);
} else {
  console.log(`  ${colors.green}✅ Some packages work independently${colors.reset}`);
  console.log(`  ${colors.cyan}💡 Review: Which packages deliver 80% of value?${colors.reset}`);
}

console.log(`\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

// Failed tests details
if (failedTests > 0) {
  console.log(`${colors.bright}${colors.red}Failed Tests Details:${colors.reset}`);
  console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

  failed.forEach((testName) => {
    const result = results.find(r => r.name === testName);
    console.log(`  ${testName}:`);
    console.log(`    Exit code: ${result.exitCode}`);
    if (result.error) {
      console.log(`    Error: ${result.error}`);
    }
  });

  console.log(`\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);
}

// Exit with appropriate code
process.exit(failedTests > 0 ? 1 : 0);
