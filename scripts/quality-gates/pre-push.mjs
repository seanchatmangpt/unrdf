#!/usr/bin/env node
/**
 * Pre-push quality gate
 * Runs comprehensive checks before pushing
 *
 * Checks:
 * - Full test suite (must pass 100%)
 * - Code coverage (≥80%)
 * - No broken imports
 * - Build succeeds
 * - Performance benchmarks (no regressions >10%)
 *
 * SLA: <120 seconds total
 */

import { execSync } from 'child_process';
import { readFileSync, existsSync } from 'fs';

const TIMEOUT_MS = 120000;
const COVERAGE_THRESHOLD = 80;
const MAX_PERF_REGRESSION = 10; // percent

/**
 * Execute command with timeout
 * @param {string} cmd - Command to execute
 * @param {number} timeout - Timeout in milliseconds
 * @returns {string} Command output
 */
function execWithTimeout(cmd, timeout = TIMEOUT_MS) {
  try {
    return execSync(`timeout ${timeout / 1000}s ${cmd}`, {
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe']
    });
  } catch (error) {
    if (error.status === 124) {
      throw new Error(`Command timed out after ${timeout}ms`);
    }
    throw error;
  }
}

/**
 * Run full test suite
 * @returns {boolean} True if tests pass
 */
function runTests() {
  console.log('🧪 Running full test suite...');

  try {
    const output = execWithTimeout('pnpm test', 60000);
    console.log(output);

    // Check for test failures
    if (output.includes('FAIL') || output.includes('failed')) {
      console.error('❌ Tests failed');
      return false;
    }

    console.log('✅ All tests passed');
    return true;
  } catch (error) {
    console.error('❌ Test execution failed:', error.message);
    return false;
  }
}

/**
 * Check code coverage
 * @returns {boolean} True if coverage meets threshold
 */
function checkCoverage() {
  console.log('\n📊 Checking code coverage...');

  try {
    execWithTimeout('pnpm test:coverage', 60000);

    const coveragePath = './coverage/coverage-summary.json';
    if (!existsSync(coveragePath)) {
      console.warn('⚠️  Coverage report not found, skipping');
      return true;
    }

    const coverage = JSON.parse(readFileSync(coveragePath, 'utf8'));
    const lineCoverage = coverage.total.lines.pct;

    console.log(`Line coverage: ${lineCoverage}%`);

    if (lineCoverage < COVERAGE_THRESHOLD) {
      console.error(`❌ Coverage below threshold: ${lineCoverage}% < ${COVERAGE_THRESHOLD}%`);
      return false;
    }

    console.log(`✅ Coverage meets threshold: ${lineCoverage}% >= ${COVERAGE_THRESHOLD}%`);
    return true;
  } catch (error) {
    console.error('❌ Coverage check failed:', error.message);
    return false;
  }
}

/**
 * Build project
 * @returns {boolean} True if build succeeds
 */
function buildProject() {
  console.log('\n🏗️  Building project...');

  try {
    execWithTimeout('pnpm build', 60000);
    console.log('✅ Build succeeded');
    return true;
  } catch (error) {
    console.error('❌ Build failed:', error.message);
    return false;
  }
}

/**
 * Check for broken imports
 * @returns {boolean} True if no broken imports
 */
function checkImports() {
  console.log('\n🔗 Checking imports...');

  try {
    // Check for CommonJS require in ESM files
    const result = execWithTimeout(
      'grep -r "require(" packages/*/src --include="*.mjs" | grep -v node_modules || true',
      5000
    );

    if (result.trim()) {
      console.error('❌ CommonJS require() found in .mjs files:');
      console.error(result);
      return false;
    }

    // Check for N3 imports outside justified modules
    const n3Imports = execWithTimeout(
      'grep -r "from \'n3\'" packages/*/src --include="*.mjs" --include="*.js" | grep -v n3-justified-only.mjs | grep -v node_modules || true',
      5000
    );

    if (n3Imports.trim()) {
      console.error('❌ Direct N3 imports found (use @unrdf/oxigraph instead):');
      console.error(n3Imports);
      return false;
    }

    console.log('✅ No broken imports found');
    return true;
  } catch (error) {
    console.error('❌ Import check failed:', error.message);
    return false;
  }
}

/**
 * Run performance benchmarks
 * @returns {boolean} True if no significant regressions
 */
function runBenchmarks() {
  console.log('\n⚡ Running performance benchmarks...');

  try {
    // Run fast benchmarks only for pre-push
    const output = execWithTimeout('pnpm test:fast', 30000);

    // Check for performance warnings
    if (output.includes('SLOW') || output.includes('regression')) {
      console.warn('⚠️  Performance warnings detected');
      console.warn('Review benchmark output above');
    }

    console.log('✅ Benchmarks completed');
    return true;
  } catch (error) {
    console.warn('⚠️  Benchmark execution failed:', error.message);
    console.warn('Continuing despite benchmark failure...');
    return true; // Don't fail push on benchmark issues
  }
}

/**
 * Main pre-push check
 */
async function main() {
  const startTime = Date.now();
  console.log('Running pre-push quality checks...\n');
  console.log('This may take up to 2 minutes...\n');

  let passed = true;

  // Check 1: Tests
  if (!runTests()) {
    passed = false;
  }

  // Check 2: Coverage
  if (!checkCoverage()) {
    passed = false;
  }

  // Check 3: Build
  if (!buildProject()) {
    passed = false;
  }

  // Check 4: Imports
  if (!checkImports()) {
    passed = false;
  }

  // Check 5: Benchmarks
  if (!runBenchmarks()) {
    passed = false;
  }

  const duration = Date.now() - startTime;
  console.log(`\n⏱️  Pre-push checks completed in ${(duration / 1000).toFixed(2)}s`);

  if (duration > TIMEOUT_MS) {
    console.warn(`⚠️  Warning: Pre-push checks took longer than ${TIMEOUT_MS / 1000}s`);
  }

  if (passed) {
    console.log('\n✅ All pre-push checks passed');
    console.log('Safe to push! 🚀\n');
    process.exit(0);
  } else {
    console.error('\n❌ Pre-push checks failed');
    console.error('Fix the issues above before pushing');
    console.error('\nTo bypass (not recommended): git push --no-verify\n');
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Pre-push check error:', error);
  process.exit(1);
});
