#!/usr/bin/env node
/**
 * @fileoverview Production Validation Runner
 *
 * Run production readiness validation on packages.
 *
 * Usage:
 *   node run-validation.mjs [package-path]
 *   node run-validation.mjs --all
 *
 * @module validation/run-validation
 */

import { readdir, stat } from 'node:fs/promises';
import { join, resolve, basename } from 'node:path';

// Import individual checks directly to avoid circular dependency
import { codeQualityCheck } from './checks/code-quality-check.mjs';
import { testCheck } from './checks/test-check.mjs';
import { dependencyCheck } from './checks/dependency-check.mjs';
import { securityCheck } from './checks/security-check.mjs';
import { documentationCheck } from './checks/documentation-check.mjs';
import { performanceCheck } from './checks/performance-check.mjs';
import { accessibilityCheck } from './checks/accessibility-check.mjs';
import { compatibilityCheck } from './checks/compatibility-check.mjs';

/**
 * Category weights
 */
const WEIGHTS = {
  codeQuality: 20,
  testing: 25,
  dependencies: 15,
  security: 20,
  documentation: 5,
  performance: 10,
  accessibility: 2.5,
  compatibility: 2.5
};

/**
 * Run validation on a single package
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Validation result
 */
async function validatePackage(packagePath) {
  const startTime = Date.now();
  const results = {};

  console.log(`\nValidating: ${packagePath}`);
  console.log('='.repeat(60));

  // Run all checks
  const checks = [
    { name: 'codeQuality', fn: codeQualityCheck },
    { name: 'testing', fn: testCheck },
    { name: 'dependencies', fn: dependencyCheck },
    { name: 'security', fn: securityCheck },
    { name: 'documentation', fn: documentationCheck },
    { name: 'performance', fn: performanceCheck },
    { name: 'accessibility', fn: accessibilityCheck },
    { name: 'compatibility', fn: compatibilityCheck }
  ];

  for (const { name, fn } of checks) {
    try {
      const result = await fn(packagePath, { dryRun: true });
      results[name] = result;
      console.log(`  ${name}: ${result.score}/100 (${result.status.toUpperCase()})`);

      if (result.failures.length > 0) {
        console.log(`    Failures: ${result.failures.slice(0, 2).join(', ')}`);
      }
    } catch (error) {
      results[name] = { score: 0, status: 'error', error: error.message };
      console.log(`  ${name}: ERROR - ${error.message}`);
    }
  }

  // Calculate overall score
  let weightedSum = 0;
  let totalWeight = 0;

  for (const [category, result] of Object.entries(results)) {
    const weight = WEIGHTS[category] || 0;
    const score = result.score || 0;
    weightedSum += score * weight;
    totalWeight += weight;
  }

  const overallScore = totalWeight > 0 ? Math.round(weightedSum / totalWeight) : 0;
  const productionReady = overallScore >= 95;

  console.log('-'.repeat(60));
  console.log(`Overall Score: ${overallScore}/100`);
  console.log(`Status: ${productionReady ? 'PRODUCTION READY' : 'NOT READY'}`);
  console.log(`Duration: ${Date.now() - startTime}ms`);

  return {
    packagePath,
    overallScore,
    productionReady,
    results,
    duration: Date.now() - startTime
  };
}

/**
 * Get all package paths
 *
 * @param {string} packagesDir - Packages directory
 * @returns {Promise<Array<string>>} Package paths
 */
async function getPackagePaths(packagesDir) {
  const paths = [];

  try {
    const entries = await readdir(packagesDir, { withFileTypes: true });

    for (const entry of entries) {
      if (entry.isDirectory()) {
        const fullPath = join(packagesDir, entry.name);
        // Check if it has a package.json
        try {
          await stat(join(fullPath, 'package.json'));
          paths.push(fullPath);
        } catch {
          // Not a package
        }
      }
    }
  } catch (error) {
    console.error(`Error reading packages directory: ${error.message}`);
  }

  return paths;
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Production Readiness Validator

Usage:
  node run-validation.mjs [package-path]   Validate a specific package
  node run-validation.mjs --all            Validate all packages
  node run-validation.mjs --summary        Show summary only

Options:
  --all       Validate all packages in /packages
  --summary   Show summary statistics only
  --help      Show this help message
`);
    return;
  }

  const rootDir = resolve(import.meta.dirname, '../..');
  const packagesDir = join(rootDir, 'packages');
  const summaryOnly = args.includes('--summary');

  let packages = [];

  if (args.includes('--all')) {
    packages = await getPackagePaths(packagesDir);
    console.log(`Found ${packages.length} packages to validate\n`);
  } else if (args.length > 0 && !args[0].startsWith('--')) {
    packages = [resolve(args[0])];
  } else {
    // Default: validate the src directory
    packages = [resolve(rootDir, 'src')];
  }

  const allResults = [];
  let totalScore = 0;
  let readyCount = 0;

  for (const pkg of packages) {
    try {
      const result = await validatePackage(pkg);
      allResults.push(result);
      totalScore += result.overallScore;
      if (result.productionReady) readyCount++;
    } catch (error) {
      console.error(`Failed to validate ${pkg}: ${error.message}`);
      allResults.push({
        packagePath: pkg,
        overallScore: 0,
        productionReady: false,
        error: error.message
      });
    }
  }

  // Print summary
  if (packages.length > 1 || summaryOnly) {
    console.log('\n' + '='.repeat(60));
    console.log('SUMMARY');
    console.log('='.repeat(60));
    console.log(`Packages Validated: ${allResults.length}`);
    console.log(`Production Ready: ${readyCount}/${allResults.length}`);
    console.log(`Average Score: ${(totalScore / allResults.length).toFixed(1)}/100`);

    // Top blockers
    const blockers = {};
    for (const result of allResults) {
      if (!result.productionReady && result.results) {
        for (const [category, check] of Object.entries(result.results)) {
          if (check.score < 80) {
            blockers[category] = (blockers[category] || 0) + 1;
          }
        }
      }
    }

    if (Object.keys(blockers).length > 0) {
      console.log('\nTop Blockers:');
      const sorted = Object.entries(blockers).sort((a, b) => b[1] - a[1]);
      for (const [category, count] of sorted.slice(0, 5)) {
        console.log(`  ${category}: ${count} package(s)`);
      }
    }

    // Packages needing attention
    const needsAttention = allResults
      .filter(r => !r.productionReady)
      .sort((a, b) => a.overallScore - b.overallScore);

    if (needsAttention.length > 0) {
      console.log('\nPackages Needing Attention:');
      for (const pkg of needsAttention.slice(0, 10)) {
        console.log(`  ${basename(pkg.packagePath)}: ${pkg.overallScore}/100`);
      }
    }
  }
}

main().catch(console.error);
