#!/usr/bin/env node
/**
 * @file Diátaxis coverage verification gate
 * @description Verifies that all workspace packages meet Diátaxis coverage requirements
 *
 * Requirements per package:
 * - At least 1 tutorial stub (tutorials.length >= 1)
 * - At least 2 how-to stubs (howtos.length >= 2)
 * - Reference page exists (reference.items.length > 0)
 * - Explanation page exists (explanation with content)
 */

import { readFile, readdir } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { existsSync } from 'node:fs';
import { stableStringify } from '../src/stable-json.mjs';

/**
 * Parse CLI arguments manually (no external dependencies)
 * @param {string[]} argv - Process arguments
 * @returns {Object} Parsed options
 */
function parseArgs(argv) {
  const options = {
    json: false,
    failFast: false,
    threshold: 0,
    help: false
  };

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];

    if (arg === '--json') {
      options.json = true;
    } else if (arg === '--fail-fast') {
      options.failFast = true;
    } else if (arg === '--threshold') {
      const value = argv[i + 1];
      if (value === undefined || isNaN(parseInt(value, 10))) {
        console.error('Error: --threshold requires a numeric value');
        process.exit(1);
      }
      options.threshold = parseInt(value, 10);
      i++; // Skip next arg
    } else if (arg === '--help' || arg === '-h') {
      options.help = true;
    }
  }

  return options;
}

/**
 * Display help message
 */
function showHelp() {
  console.log(`
Diátaxis Coverage Verification Gate

Usage: node bin/verify.mjs [options]

Options:
  --json              Output JSON instead of human-readable format
  --fail-fast         Exit on first failure (don't check all packages)
  --threshold <num>   Only fail if more than NUM packages fail (default: 0)
  --help, -h          Show this help message

Requirements per package:
  - At least 1 tutorial stub
  - At least 2 how-to stubs
  - Reference page exists (with items)
  - Explanation page exists (with content)

Exit codes:
  0 - All packages pass (or failures <= threshold)
  1 - One or more packages fail verification
  `.trim());
}

/**
 * Check if reference is populated
 * @param {Object} reference - Reference object
 * @returns {boolean} True if reference has content
 */
function isReferencPopulated(reference) {
  if (!reference || typeof reference !== 'object') {
    return false;
  }

  // Reference is populated if it has items
  if (Array.isArray(reference.items) && reference.items.length > 0) {
    return true;
  }

  // Or if it has a confidence score > 0
  if (reference.confidenceScore > 0) {
    return true;
  }

  return false;
}

/**
 * Check if explanation is populated
 * @param {Object} explanation - Explanation object
 * @returns {boolean} True if explanation has content
 */
function isExplanationPopulated(explanation) {
  if (!explanation || typeof explanation !== 'object') {
    return false;
  }

  // Explanation is populated if it has concepts, architecture, or tradeoffs
  if (Array.isArray(explanation.concepts) && explanation.concepts.length > 0) {
    return true;
  }

  if (explanation.architecture && explanation.architecture.trim() !== '') {
    return true;
  }

  if (Array.isArray(explanation.tradeoffs) && explanation.tradeoffs.length > 0) {
    return true;
  }

  // Or if it has a confidence score > 0
  if (explanation.confidenceScore > 0) {
    return true;
  }

  return false;
}

/**
 * Verify a single package's Diátaxis coverage
 * @param {string} packageName - Package name
 * @param {Object} entry - Diátaxis entry
 * @returns {Object} Verification result
 */
function verifyPackage(packageName, entry) {
  const failures = [];

  // Check tutorials (need at least 1)
  if (!Array.isArray(entry.tutorials) || entry.tutorials.length < 1) {
    failures.push(`Missing tutorials (${entry.tutorials?.length || 0}/1 required)`);
  }

  // Check how-tos (need at least 2)
  if (!Array.isArray(entry.howtos) || entry.howtos.length < 2) {
    failures.push(`Missing how-tos (${entry.howtos?.length || 0}/2 required)`);
  }

  // Check reference
  if (!isReferencPopulated(entry.reference)) {
    failures.push('Missing reference');
  }

  // Check explanation
  if (!isExplanationPopulated(entry.explanation)) {
    failures.push('Missing explanation');
  }

  return {
    packageName,
    passing: failures.length === 0,
    failures
  };
}

/**
 * Main verification logic
 * @param {Object} options - CLI options
 * @returns {Promise<number>} Exit code
 */
async function main(options) {
  const workspaceRoot = resolve(process.cwd());
  const artifactsDir = join(workspaceRoot, 'ARTIFACTS', 'diataxis');
  const inventoryPath = join(artifactsDir, 'inventory.json');

  // Check if ARTIFACTS/diataxis exists
  if (!existsSync(artifactsDir)) {
    if (options.json) {
      console.log(stableStringify({
        error: 'ARTIFACTS/diataxis directory not found',
        path: artifactsDir
      }));
    } else {
      console.error(`Error: ARTIFACTS/diataxis directory not found at ${artifactsDir}`);
    }
    return 1;
  }

  // Check if inventory.json exists
  if (!existsSync(inventoryPath)) {
    if (options.json) {
      console.log(stableStringify({
        error: 'inventory.json not found',
        path: inventoryPath
      }));
    } else {
      console.error(`Error: inventory.json not found at ${inventoryPath}`);
    }
    return 1;
  }

  // Load inventory
  let inventory;
  try {
    const inventoryContent = await readFile(inventoryPath, 'utf-8');
    inventory = JSON.parse(inventoryContent);
  } catch (error) {
    if (options.json) {
      console.log(stableStringify({
        error: 'Failed to parse inventory.json',
        message: error.message
      }));
    } else {
      console.error(`Error: Failed to parse inventory.json: ${error.message}`);
    }
    return 1;
  }

  // Get list of packages from inventory
  const packageNames = Array.isArray(inventory.packages)
    ? inventory.packages.map(p => p.name)
    : [];

  if (packageNames.length === 0) {
    if (options.json) {
      console.log(stableStringify({
        error: 'No packages found in inventory',
        total: 0,
        passing: 0,
        failing: 0,
        failures: []
      }));
    } else {
      console.error('Error: No packages found in inventory.json');
    }
    return 1;
  }

  // Sort package names for deterministic output
  packageNames.sort((a, b) => a.localeCompare(b));

  const results = [];

  // Verify each package
  for (const packageName of packageNames) {
    const packageDir = join(artifactsDir, packageName);
    const diataxisPath = join(packageDir, 'diataxis.json');

    let entry;
    let verificationResult;

    // Check if package diataxis.json exists
    if (!existsSync(diataxisPath)) {
      verificationResult = {
        packageName,
        passing: false,
        failures: ['diataxis.json not found']
      };
    } else {
      // Try to load and parse diataxis.json
      try {
        const content = await readFile(diataxisPath, 'utf-8');
        entry = JSON.parse(content);
        verificationResult = verifyPackage(packageName, entry);
      } catch (error) {
        verificationResult = {
          packageName,
          passing: false,
          failures: [`Failed to parse diataxis.json: ${error.message}`]
        };
      }
    }

    results.push(verificationResult);

    // Fail fast if requested and we have a failure
    if (options.failFast && !verificationResult.passing) {
      break;
    }
  }

  // Calculate summary
  const totalPackages = options.failFast ? results.length : packageNames.length;
  const passingPackages = results.filter(r => r.passing).length;
  const failingPackages = results.filter(r => !r.passing).length;
  const failures = results.filter(r => !r.passing);

  // Determine exit code based on threshold
  const shouldFail = failingPackages > options.threshold;

  // Output results
  if (options.json) {
    console.log(stableStringify({
      total: totalPackages,
      passing: passingPackages,
      failing: failingPackages,
      threshold: options.threshold,
      exitCode: shouldFail ? 1 : 0,
      failures: failures.map(f => ({
        package: f.packageName,
        failures: f.failures
      }))
    }));
  } else {
    console.log('Diátaxis Coverage Verification');
    console.log('==============================\n');
    console.log(`Total packages: ${totalPackages}`);
    console.log(`Passing: ${passingPackages}`);
    console.log(`Failing: ${failingPackages}`);

    if (failures.length > 0) {
      console.log('\nFAILURES:');
      console.log('--------');
      for (const failure of failures) {
        console.log(`❌ ${failure.packageName}`);
        for (const msg of failure.failures) {
          console.log(`  - ${msg}`);
        }
        console.log('');
      }
    }

    if (shouldFail) {
      console.log(`Exit code: 1 (${failingPackages} failures > threshold ${options.threshold})`);
    } else {
      console.log(`Exit code: 0 (${failingPackages} failures <= threshold ${options.threshold})`);
    }
  }

  return shouldFail ? 1 : 0;
}

// Run main if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  const options = parseArgs(args);

  if (options.help) {
    showHelp();
    process.exit(0);
  }

  main(options)
    .then(exitCode => process.exit(exitCode))
    .catch(error => {
      console.error('Fatal error:', error);
      process.exit(1);
    });
}

export { main, verifyPackage, isReferencPopulated, isExplanationPopulated };
