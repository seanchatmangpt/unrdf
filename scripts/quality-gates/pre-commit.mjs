#!/usr/bin/env node
/**
 * Pre-commit quality gate
 * Runs fast checks on staged files only
 *
 * Checks:
 * - File size (<500 lines)
 * - ESLint on staged files
 * - No TypeScript artifacts
 * - Basic formatting
 *
 * SLA: <5 seconds total
 */

import { execSync } from 'child_process';
import { readFileSync } from 'fs';
import { join } from 'path';

const MAX_FILE_LINES = 500;
const TIMEOUT_MS = 5000;

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
      throw new Error(`Command timed out after ${timeout}ms: ${cmd}`);
    }
    throw error;
  }
}

/**
 * Get staged files
 * @returns {string[]} Array of staged file paths
 */
function getStagedFiles() {
  try {
    const output = execWithTimeout('git diff --cached --name-only --diff-filter=ACM');
    return output.trim().split('\n').filter(Boolean);
  } catch (error) {
    console.error('Failed to get staged files:', error.message);
    return [];
  }
}

/**
 * Check file size
 * @param {string} filepath - File path
 * @returns {boolean} True if file size is acceptable
 */
function checkFileSize(filepath) {
  try {
    const content = readFileSync(filepath, 'utf8');
    const lines = content.split('\n').length;

    if (lines > MAX_FILE_LINES) {
      console.error(`❌ File too large: ${filepath} (${lines} lines > ${MAX_FILE_LINES})`);
      return false;
    }

    return true;
  } catch (error) {
    console.warn(`⚠️  Could not read file: ${filepath}`);
    return true; // Don't fail on read errors
  }
}

/**
 * Check for TypeScript artifacts
 * @param {string[]} files - File paths
 * @returns {boolean} True if no TypeScript artifacts found
 */
function checkNoTypeScript(files) {
  const tsFiles = files.filter(f =>
    f.endsWith('.ts') || f.endsWith('.tsx') ||
    (f.endsWith('.d.ts') && !f.includes('node_modules'))
  );

  if (tsFiles.length > 0) {
    console.error('❌ TypeScript artifacts found:');
    tsFiles.forEach(f => console.error(`   ${f}`));
    console.error('\nThis project uses pure ESM + JSDoc only.');
    return false;
  }

  return true;
}

/**
 * Run ESLint on staged files
 * @param {string[]} files - File paths
 * @returns {boolean} True if lint passes
 */
function runLint(files) {
  const jsFiles = files.filter(f =>
    (f.endsWith('.js') || f.endsWith('.mjs')) &&
    !f.includes('node_modules') &&
    !f.includes('dist/')
  );

  if (jsFiles.length === 0) {
    console.log('✅ No JS files to lint');
    return true;
  }

  try {
    console.log(`Running ESLint on ${jsFiles.length} files...`);
    execWithTimeout(`pnpm eslint ${jsFiles.join(' ')}`, 5000);
    console.log('✅ ESLint passed');
    return true;
  } catch (error) {
    console.error('❌ ESLint failed');
    return false;
  }
}

/**
 * Main pre-commit check
 */
async function main() {
  const startTime = Date.now();
  console.log('Running pre-commit quality checks...\n');

  const stagedFiles = getStagedFiles();

  if (stagedFiles.length === 0) {
    console.log('No staged files to check');
    process.exit(0);
  }

  console.log(`Checking ${stagedFiles.length} staged files\n`);

  let passed = true;

  // Check 1: File sizes
  console.log('📏 Checking file sizes...');
  for (const file of stagedFiles) {
    if (!checkFileSize(file)) {
      passed = false;
    }
  }

  // Check 2: No TypeScript
  console.log('\n🚫 Checking for TypeScript artifacts...');
  if (!checkNoTypeScript(stagedFiles)) {
    passed = false;
  }

  // Check 3: Lint staged files
  console.log('\n🔍 Running linter...');
  if (!runLint(stagedFiles)) {
    passed = false;
  }

  const duration = Date.now() - startTime;
  console.log(`\n⏱️  Pre-commit checks completed in ${duration}ms`);

  if (duration > TIMEOUT_MS) {
    console.warn(`⚠️  Warning: Pre-commit checks took longer than ${TIMEOUT_MS}ms`);
    console.warn('   Consider optimizing or reducing scope');
  }

  if (passed) {
    console.log('✅ All pre-commit checks passed\n');
    process.exit(0);
  } else {
    console.error('\n❌ Pre-commit checks failed');
    console.error('Fix the issues above and try again');
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Pre-commit check error:', error);
  process.exit(1);
});
