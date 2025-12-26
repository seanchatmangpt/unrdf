#!/usr/bin/env node
/**
 * Contract Verification - Detects API contract drift and manages acceptance
 *
 * Compares current code against CONTRACTS.lock.json to detect breaking changes
 * and manage explicit contract acceptance workflows.
 *
 * @module agent-2/verify-contracts
 */

import { readFile, writeFile } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { scanAllPackages } from './contract-scanner.mjs';
import { generateLockfile, generateHash, generateSummaryReport } from './lockfile-generator.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const LOCKFILE_PATH = join(__dirname, '../CONTRACTS.lock.json');

/**
 * Verification result structure
 * @typedef {Object} VerificationResult
 * @property {boolean} passed - Whether verification passed
 * @property {Array} differences - List of detected differences
 * @property {Object} summary - Summary statistics
 */

/**
 * Loads the current lockfile
 * @returns {Promise<Object|null>} Lockfile contents or null if not found
 */
export async function loadLockfile() {
  try {
    const content = await readFile(LOCKFILE_PATH, 'utf-8');
    return JSON.parse(content);
  } catch (error) {
    if (error.code === 'ENOENT') {
      return null;
    }
    throw error;
  }
}

/**
 * Compares two contract objects and returns differences
 * @param {Object} expected - Expected contract (from lockfile)
 * @param {Object} actual - Actual contract (from current code)
 * @param {string} path - Path for error reporting
 * @returns {Array} List of differences
 */
function compareContracts(expected, actual, path = '') {
  const differences = [];

  // Check for removed exports
  for (const key of Object.keys(expected)) {
    if (!(key in actual)) {
      differences.push({
        type: 'REMOVED',
        path: `${path}.${key}`,
        expected: expected[key],
        actual: undefined,
      });
    }
  }

  // Check for added or modified exports
  for (const key of Object.keys(actual)) {
    const fullPath = path ? `${path}.${key}` : key;

    if (!(key in expected)) {
      differences.push({
        type: 'ADDED',
        path: fullPath,
        expected: undefined,
        actual: actual[key],
      });
      continue;
    }

    // Compare values
    const expectedValue = expected[key];
    const actualValue = actual[key];

    if (typeof expectedValue === 'object' && typeof actualValue === 'object') {
      if (Array.isArray(expectedValue) && Array.isArray(actualValue)) {
        // Compare arrays
        if (JSON.stringify(expectedValue.sort()) !== JSON.stringify(actualValue.sort())) {
          differences.push({
            type: 'MODIFIED',
            path: fullPath,
            expected: expectedValue,
            actual: actualValue,
          });
        }
      } else if (!Array.isArray(expectedValue) && !Array.isArray(actualValue)) {
        // Recursively compare objects
        differences.push(...compareContracts(expectedValue, actualValue, fullPath));
      } else {
        // Type mismatch
        differences.push({
          type: 'TYPE_CHANGED',
          path: fullPath,
          expected: expectedValue,
          actual: actualValue,
        });
      }
    } else if (expectedValue !== actualValue) {
      differences.push({
        type: 'MODIFIED',
        path: fullPath,
        expected: expectedValue,
        actual: actualValue,
      });
    }
  }

  return differences;
}

/**
 * Verifies current code against lockfile
 * @returns {Promise<VerificationResult>} Verification result
 */
export async function verifyContracts() {
  console.log('🔍 Verifying contracts against lockfile...\n');

  const lockfile = await loadLockfile();

  if (!lockfile) {
    return {
      passed: false,
      differences: [{
        type: 'MISSING_LOCKFILE',
        message: 'CONTRACTS.lock.json does not exist. Run acceptance first.',
      }],
      summary: {
        totalDifferences: 1,
        breaking: 1,
        nonBreaking: 0,
      },
    };
  }

  // Scan current code
  const currentInventory = await scanAllPackages();
  const currentLockfile = await generateLockfile(currentInventory, '/tmp/contracts-current.json');

  const differences = [];
  const summary = {
    totalDifferences: 0,
    breaking: 0,
    nonBreaking: 0,
    packagesChanged: new Set(),
  };

  // Compare each package
  for (const packageName of Object.keys(lockfile.packages)) {
    const expected = lockfile.packages[packageName];
    const actual = currentLockfile.packages[packageName];

    if (!actual) {
      differences.push({
        type: 'PACKAGE_REMOVED',
        package: packageName,
        severity: 'BREAKING',
      });
      summary.breaking++;
      summary.packagesChanged.add(packageName);
      continue;
    }

    // Compare package hashes
    if (expected.hash !== actual.hash) {
      const pkgDiffs = compareContracts(
        expected.exports,
        actual.exports,
        `${packageName}.exports`
      );

      for (const diff of pkgDiffs) {
        const isBreaking = diff.type === 'REMOVED' || diff.type === 'TYPE_CHANGED';

        differences.push({
          ...diff,
          package: packageName,
          severity: isBreaking ? 'BREAKING' : 'NON_BREAKING',
        });

        if (isBreaking) {
          summary.breaking++;
        } else {
          summary.nonBreaking++;
        }

        summary.packagesChanged.add(packageName);
      }
    }
  }

  // Check for new packages
  for (const packageName of Object.keys(currentLockfile.packages)) {
    if (!lockfile.packages[packageName]) {
      differences.push({
        type: 'PACKAGE_ADDED',
        package: packageName,
        severity: 'NON_BREAKING',
      });
      summary.nonBreaking++;
      summary.packagesChanged.add(packageName);
    }
  }

  summary.totalDifferences = differences.length;
  summary.packagesChanged = Array.from(summary.packagesChanged).sort();

  const passed = summary.breaking === 0;

  return {
    passed,
    differences,
    summary,
  };
}

/**
 * Accepts current contracts and updates lockfile
 * @param {string} [reason] - Reason for acceptance
 * @returns {Promise<Object>} Updated lockfile
 */
export async function acceptContracts(reason = 'Manual acceptance') {
  console.log('📝 Accepting current contracts...\n');

  const inventory = await scanAllPackages();
  const lockfile = await generateLockfile(inventory, LOCKFILE_PATH);

  // Add acceptance metadata
  lockfile.acceptance = {
    acceptedAt: new Date().toISOString(),
    reason,
    acceptedBy: process.env.USER || 'unknown',
  };

  // Write with metadata
  await writeFile(LOCKFILE_PATH, JSON.stringify(lockfile, null, 2), 'utf-8');

  console.log('✅ Contracts accepted and lockfile updated');
  console.log(`   Hash: ${lockfile.hash}`);
  console.log(`   Packages: ${lockfile.metadata.totalPackages}`);
  console.log(`   Reason: ${reason}\n`);

  return lockfile;
}

/**
 * Formats verification results for display
 * @param {VerificationResult} result - Verification result
 * @returns {string} Formatted output
 */
export function formatVerificationResult(result) {
  const lines = [];

  if (result.passed) {
    lines.push('✅ Contract verification PASSED');
    lines.push('   No breaking changes detected');
    return lines.join('\n');
  }

  lines.push('❌ Contract verification FAILED');
  lines.push('');
  lines.push('SUMMARY:');
  lines.push(`  Total Differences: ${result.summary.totalDifferences}`);
  lines.push(`  Breaking: ${result.summary.breaking}`);
  lines.push(`  Non-Breaking: ${result.summary.nonBreaking}`);

  if (result.summary.packagesChanged?.length > 0) {
    lines.push(`  Packages Changed: ${result.summary.packagesChanged.length}`);
  }

  lines.push('');
  lines.push('DIFFERENCES:');

  // Group by package
  const byPackage = {};
  for (const diff of result.differences) {
    const pkg = diff.package || 'system';
    if (!byPackage[pkg]) byPackage[pkg] = [];
    byPackage[pkg].push(diff);
  }

  for (const [pkg, diffs] of Object.entries(byPackage).sort()) {
    lines.push(`\n  ${pkg}:`);
    for (const diff of diffs) {
      const icon = diff.severity === 'BREAKING' ? '🔴' : '🟡';
      lines.push(`    ${icon} ${diff.type} - ${diff.path || diff.type}`);

      if (diff.expected !== undefined) {
        lines.push(`       Expected: ${JSON.stringify(diff.expected)}`);
      }
      if (diff.actual !== undefined) {
        lines.push(`       Actual: ${JSON.stringify(diff.actual)}`);
      }
    }
  }

  lines.push('');
  lines.push('To accept these changes: node agent-2/verify-contracts.mjs --accept');

  return lines.join('\n');
}

/**
 * Main execution when run as script
 */
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  const command = args[0];

  try {
    if (command === '--accept') {
      const reason = args.slice(1).join(' ') || 'Manual acceptance via CLI';
      await acceptContracts(reason);
    } else if (command === '--verify' || !command) {
      const result = await verifyContracts();
      console.log(formatVerificationResult(result));
      process.exit(result.passed ? 0 : 1);
    } else {
      console.error('Usage: node verify-contracts.mjs [--verify|--accept] [reason]');
      process.exit(1);
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}
