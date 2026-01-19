#!/usr/bin/env node
/**
 * @file Analyze test results from pnpm test:fast output
 * Extracts package-level test statistics
 */

import { readFileSync } from 'fs';

const logFile = '/tmp/test-fast-output.log';
const content = readFileSync(logFile, 'utf-8');
const lines = content.split('\n');

const packageResults = new Map();
let currentPackage = null;

for (const line of lines) {
  // Detect package name
  const pkgMatch = line.match(/packages\/([^\/\s]+)\s+test:fast/);
  if (pkgMatch) {
    currentPackage = pkgMatch[1];
    if (!packageResults.has(currentPackage)) {
      packageResults.set(currentPackage, {
        name: currentPackage,
        status: 'unknown',
        testFiles: { total: 0, passed: 0, failed: 0 },
        tests: { total: 0, passed: 0, failed: 0 },
        hasOutput: false
      });
    }
    packageResults.get(currentPackage).hasOutput = true;
  }

  // Detect test file summary
  const testFileMatch = line.match(/Test Files\s+(?:(\d+)\s+failed\s+\|\s+)?(\d+)\s+passed/);
  if (testFileMatch && currentPackage) {
    const failed = parseInt(testFileMatch[1] || '0', 10);
    const passed = parseInt(testFileMatch[2] || '0', 10);
    packageResults.get(currentPackage).testFiles = {
      failed,
      passed,
      total: failed + passed
    };
  }

  // Detect test summary
  const testMatch = line.match(/Tests\s+(?:(\d+)\s+failed\s+\|\s+)?(\d+)\s+passed/);
  if (testMatch && currentPackage) {
    const failed = parseInt(testMatch[1] || '0', 10);
    const passed = parseInt(testMatch[2] || '0', 10);
    packageResults.get(currentPackage).tests = {
      failed,
      passed,
      total: failed + passed
    };
  }

  // Detect completion status
  if (line.includes('Done') && currentPackage) {
    const pkg = packageResults.get(currentPackage);
    if (pkg.testFiles.failed === 0 && pkg.tests.failed === 0 && pkg.tests.passed > 0) {
      pkg.status = 'pass';
    } else if (pkg.tests.failed > 0 || pkg.testFiles.failed > 0) {
      pkg.status = 'fail';
    } else if (line.includes('No test files found')) {
      pkg.status = 'no-tests';
    }
  }

  // Detect no tests
  if (line.includes('No test files found') && currentPackage) {
    packageResults.get(currentPackage).status = 'no-tests';
  }

  // Detect type-only packages
  if (line.includes('type-only package') && currentPackage) {
    packageResults.get(currentPackage).status = 'type-only';
  }
}

// Categorize results
const operational = [];
const partialOperational = [];
const nonOperational = [];
const noTests = [];
const typeOnly = [];
const notRun = [];

for (const [name, result] of packageResults.entries()) {
  if (result.status === 'type-only') {
    typeOnly.push(result);
  } else if (result.status === 'no-tests') {
    noTests.push(result);
  } else if (result.status === 'pass') {
    operational.push(result);
  } else if (result.status === 'fail') {
    if (result.tests.passed > 0) {
      partialOperational.push(result);
    } else {
      nonOperational.push(result);
    }
  } else if (!result.hasOutput) {
    notRun.push({ name, ...result });
  }
}

// Print summary
console.log('# UNRDF Package Test Results Summary\n');
console.log(`Total Packages Analyzed: ${packageResults.size}`);
console.log(`Packages That Ran Tests: ${operational.length + partialOperational.length + nonOperational.length}\n`);

console.log(`## Operational (${operational.length} packages) - All tests passing`);
operational.forEach(pkg => {
  console.log(`  ✓ ${pkg.name}: ${pkg.tests.passed} tests passed`);
});

console.log(`\n## Partially Operational (${partialOperational.length} packages) - Some tests passing`);
partialOperational.forEach(pkg => {
  console.log(`  ⚠ ${pkg.name}: ${pkg.tests.passed} passed, ${pkg.tests.failed} failed`);
});

console.log(`\n## Non-Operational (${nonOperational.length} packages) - All tests failing`);
nonOperational.forEach(pkg => {
  console.log(`  ✗ ${pkg.name}: ${pkg.tests.failed} tests failed`);
});

console.log(`\n## No Tests (${noTests.length} packages)`);
noTests.forEach(pkg => {
  console.log(`  - ${pkg.name}`);
});

console.log(`\n## Type-Only (${typeOnly.length} packages)`);
typeOnly.forEach(pkg => {
  console.log(`  - ${pkg.name}`);
});

console.log(`\n## Not Run (${notRun.length} packages)`);
notRun.forEach(pkg => {
  console.log(`  - ${pkg.name}`);
});

// Statistics
const totalTests = [...packageResults.values()].reduce((sum, pkg) => sum + pkg.tests.total, 0);
const totalPassed = [...packageResults.values()].reduce((sum, pkg) => sum + pkg.tests.passed, 0);
const totalFailed = [...packageResults.values()].reduce((sum, pkg) => sum + pkg.tests.failed, 0);

console.log(`\n## Overall Statistics`);
console.log(`Total Tests: ${totalTests}`);
console.log(`Passed: ${totalPassed} (${(totalPassed / totalTests * 100).toFixed(1)}%)`);
console.log(`Failed: ${totalFailed} (${(totalFailed / totalTests * 100).toFixed(1)}%)`);
console.log(`Pass Rate: ${(totalPassed / totalTests * 100).toFixed(2)}%`);

// JSON output for further processing
const summary = {
  operational,
  partialOperational,
  nonOperational,
  noTests,
  typeOnly,
  notRun,
  stats: {
    total: packageResults.size,
    operational: operational.length,
    partialOperational: partialOperational.length,
    nonOperational: nonOperational.length,
    noTests: noTests.length,
    typeOnly: typeOnly.length,
    notRun: notRun.length,
    tests: {
      total: totalTests,
      passed: totalPassed,
      failed: totalFailed,
      passRate: (totalPassed / totalTests * 100).toFixed(2)
    }
  }
};

// Write JSON output
import { writeFileSync } from 'fs';
writeFileSync('/tmp/test-results-summary.json', JSON.stringify(summary, null, 2));

console.log(`\nDetailed results written to: /tmp/test-results-summary.json`);
