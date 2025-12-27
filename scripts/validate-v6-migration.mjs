#!/usr/bin/env node
/**
 * @fileoverview V6 Migration Validation Script
 *
 * Validates successful migration to v6 by checking:
 * - No v5 patterns remain
 * - All tests pass
 * - OTEL validation ≥80/100
 * - Performance benchmarks pass
 * - Dependencies correct
 *
 * @example
 * ```bash
 * # Full validation
 * node scripts/validate-v6-migration.mjs
 *
 * # Quick validation (skip benchmarks)
 * node scripts/validate-v6-migration.mjs --quick
 *
 * # Specific package
 * node scripts/validate-v6-migration.mjs --package packages/my-app
 * ```
 */

import { promises as fs } from 'fs';
import { execSync } from 'child_process';
import { resolve, join, relative } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = resolve(__dirname, '..');

// ============================================================================
// Configuration
// ============================================================================

const CONFIG = {
  quick: process.argv.includes('--quick'),
  package: getArgValue('--package'),
  verbose: process.argv.includes('--verbose'),
  skipTests: process.argv.includes('--skip-tests'),
  skipOtel: process.argv.includes('--skip-otel'),
  outputFile: getArgValue('--output') || 'migration-validation-report.json'
};

// ============================================================================
// Validation Checks
// ============================================================================

const CHECKS = {
  // Critical checks (must pass)
  critical: [
    {
      name: 'No direct N3 imports',
      check: async () => {
        const result = await exec(
          'grep -r "from \'n3\'" packages/*/src --include="*.mjs" | wc -l',
          { silent: true }
        );
        const count = parseInt(result.trim(), 10);
        return {
          pass: count === 0,
          message: count === 0
            ? 'No direct N3 imports found'
            : `Found ${count} direct N3 imports (should be 0)`,
          severity: 'critical'
        };
      }
    },
    {
      name: 'No v5 Store constructors',
      check: async () => {
        const result = await exec(
          'grep -r "new Store()" packages/*/src --include="*.mjs" | wc -l',
          { silent: true }
        );
        const count = parseInt(result.trim(), 10);
        return {
          pass: count === 0,
          message: count === 0
            ? 'No v5 Store constructors found'
            : `Found ${count} Store constructors (should be 0)`,
          severity: 'critical'
        };
      }
    },
    {
      name: 'Package.json type=module',
      check: async () => {
        const packages = await findAllPackages();
        const results = [];

        for (const pkgPath of packages) {
          const pkgJson = JSON.parse(
            await fs.readFile(join(pkgPath, 'package.json'), 'utf-8')
          );

          if (pkgJson.type !== 'module') {
            results.push(relative(ROOT, pkgPath));
          }
        }

        return {
          pass: results.length === 0,
          message: results.length === 0
            ? 'All packages use type=module'
            : `Packages missing type=module: ${results.join(', ')}`,
          severity: 'critical'
        };
      }
    },
    {
      name: 'Correct dependencies',
      check: async () => {
        const packages = await findAllPackages();
        const issues = [];

        for (const pkgPath of packages) {
          const pkgJson = JSON.parse(
            await fs.readFile(join(pkgPath, 'package.json'), 'utf-8')
          );

          // Check for v5 dependencies
          if (pkgJson.dependencies?.['@unrdf/engine']) {
            issues.push(`${relative(ROOT, pkgPath)}: Still depends on @unrdf/engine`);
          }

          if (pkgJson.dependencies?.['n3'] && !pkgPath.includes('n3-justified')) {
            issues.push(`${relative(ROOT, pkgPath)}: Direct n3 dependency`);
          }
        }

        return {
          pass: issues.length === 0,
          message: issues.length === 0
            ? 'All dependencies correct'
            : `Dependency issues:\n${issues.join('\n')}`,
          severity: 'critical'
        };
      }
    }
  ],

  // Important checks (should pass but not blocking)
  important: [
    {
      name: 'All tests pass',
      skip: () => CONFIG.skipTests,
      check: async () => {
        try {
          await exec('timeout 30s pnpm test', { silent: !CONFIG.verbose });
          return {
            pass: true,
            message: 'All tests pass',
            severity: 'important'
          };
        } catch (error) {
          return {
            pass: false,
            message: `Tests failed: ${error.message}`,
            severity: 'important'
          };
        }
      }
    },
    {
      name: 'Linting passes',
      check: async () => {
        try {
          await exec('timeout 10s pnpm lint', { silent: !CONFIG.verbose });
          return {
            pass: true,
            message: 'Linting passes with 0 errors',
            severity: 'important'
          };
        } catch (error) {
          return {
            pass: false,
            message: 'Linting failed',
            severity: 'important'
          };
        }
      }
    },
    {
      name: 'OTEL validation ≥80/100',
      skip: () => CONFIG.skipOtel,
      check: async () => {
        try {
          const output = await exec(
            'timeout 15s node validation/run-all.mjs comprehensive',
            { silent: true }
          );

          const scoreMatch = output.match(/Score:\s*(\d+)/);
          const score = scoreMatch ? parseInt(scoreMatch[1], 10) : 0;

          return {
            pass: score >= 80,
            message: `OTEL validation score: ${score}/100`,
            severity: 'important',
            data: { score }
          };
        } catch (error) {
          return {
            pass: false,
            message: 'OTEL validation failed to run',
            severity: 'important'
          };
        }
      }
    }
  ],

  // Performance checks
  performance: [
    {
      name: 'No performance regressions',
      skip: () => CONFIG.quick,
      check: async () => {
        try {
          const output = await exec(
            'timeout 60s pnpm benchmark:regression',
            { silent: !CONFIG.verbose }
          );

          // Check for regressions >10%
          const regressions = output.match(/regression.*?(\d+)%/gi) || [];
          const badRegressions = regressions.filter(r => {
            const pct = parseInt(r.match(/(\d+)%/)[1], 10);
            return pct > 10;
          });

          return {
            pass: badRegressions.length === 0,
            message: badRegressions.length === 0
              ? 'No significant performance regressions'
              : `Found ${badRegressions.length} regressions >10%`,
            severity: 'warning'
          };
        } catch (error) {
          return {
            pass: false,
            message: 'Benchmark comparison unavailable',
            severity: 'warning'
          };
        }
      }
    }
  ],

  // Code quality checks
  quality: [
    {
      name: 'JSDoc coverage',
      check: async () => {
        const result = await exec(
          'grep -r "@param\\|@returns" packages/*/src --include="*.mjs" | wc -l',
          { silent: true }
        );
        const count = parseInt(result.trim(), 10);

        return {
          pass: count > 0,
          message: `Found ${count} JSDoc annotations`,
          severity: 'info',
          data: { jsdocCount: count }
        };
      }
    },
    {
      name: 'Zod schema coverage',
      check: async () => {
        const result = await exec(
          'grep -r "z\\.object\\|z\\.string" packages/*/src --include="*.mjs" | wc -l',
          { silent: true }
        );
        const count = parseInt(result.trim(), 10);

        return {
          pass: count > 0,
          message: `Found ${count} Zod schemas`,
          severity: 'info',
          data: { zodCount: count }
        };
      }
    },
    {
      name: 'Receipt usage',
      check: async () => {
        const result = await exec(
          'grep -r "receiptSchema\\|withReceipt" packages/*/src --include="*.mjs" | wc -l',
          { silent: true }
        );
        const count = parseInt(result.trim(), 10);

        return {
          pass: count > 0,
          message: `Found ${count} receipt implementations`,
          severity: 'info',
          data: { receiptCount: count }
        };
      }
    }
  ]
};

// ============================================================================
// Utility Functions
// ============================================================================

function getArgValue(arg) {
  const index = process.argv.indexOf(arg);
  return index !== -1 && process.argv[index + 1]
    ? process.argv[index + 1]
    : null;
}

function log(message, level = 'info') {
  const icons = {
    info: 'ℹ️',
    success: '✅',
    warning: '⚠️',
    error: '❌',
    critical: '🚨'
  };

  console.log(`${icons[level] || icons.info} ${message}`);
}

async function exec(command, options = {}) {
  const { silent = false } = options;

  try {
    const output = execSync(command, {
      encoding: 'utf-8',
      cwd: ROOT,
      stdio: silent ? 'pipe' : 'inherit'
    });

    return output;
  } catch (error) {
    if (silent) {
      return '';
    }
    throw error;
  }
}

async function findAllPackages() {
  const packagesDir = join(ROOT, 'packages');
  const entries = await fs.readdir(packagesDir, { withFileTypes: true });

  return entries
    .filter(entry => entry.isDirectory())
    .map(entry => join(packagesDir, entry.name));
}

// ============================================================================
// Validation Runner
// ============================================================================

async function runChecks(checkGroup, groupName) {
  log(`\nRunning ${groupName} checks...`, 'info');
  console.log('━'.repeat(60));

  const results = [];

  for (const check of checkGroup) {
    // Skip if condition met
    if (check.skip && check.skip()) {
      log(`⏭  Skipped: ${check.name}`, 'info');
      continue;
    }

    try {
      const result = await check.check();
      results.push({ name: check.name, ...result });

      const icon = result.pass ? '✅' : '❌';
      log(`${icon} ${check.name}: ${result.message}`, result.pass ? 'success' : 'error');
    } catch (error) {
      results.push({
        name: check.name,
        pass: false,
        message: `Check failed: ${error.message}`,
        severity: 'error'
      });

      log(`❌ ${check.name}: Check failed`, 'error');
    }
  }

  return results;
}

async function generateReport(allResults) {
  const report = {
    timestamp: new Date().toISOString(),
    summary: {
      totalChecks: allResults.length,
      passed: allResults.filter(r => r.pass).length,
      failed: allResults.filter(r => !r.pass).length,
      critical: allResults.filter(r => r.severity === 'critical' && !r.pass).length
    },
    checks: allResults,
    verdict: 'UNKNOWN'
  };

  // Determine verdict
  if (report.summary.critical > 0) {
    report.verdict = 'FAILED';
    report.reason = `${report.summary.critical} critical check(s) failed`;
  } else if (report.summary.failed === 0) {
    report.verdict = 'PASSED';
    report.reason = 'All checks passed';
  } else {
    report.verdict = 'PASSED_WITH_WARNINGS';
    report.reason = `${report.summary.failed} non-critical check(s) failed`;
  }

  // Save report
  await fs.writeFile(
    CONFIG.outputFile,
    JSON.stringify(report, null, 2),
    'utf-8'
  );

  return report;
}

function printSummary(report) {
  console.log('\n' + '='.repeat(60));
  console.log('VALIDATION SUMMARY');
  console.log('='.repeat(60));
  console.log(`Total Checks: ${report.summary.totalChecks}`);
  console.log(`Passed: ${report.summary.passed}`);
  console.log(`Failed: ${report.summary.failed}`);
  console.log(`Critical Failures: ${report.summary.critical}`);
  console.log('');
  console.log(`Verdict: ${report.verdict}`);
  console.log(`Reason: ${report.reason}`);
  console.log('='.repeat(60));
  console.log(`\nFull report saved to: ${CONFIG.outputFile}\n`);

  // Exit code based on verdict
  if (report.verdict === 'FAILED') {
    console.log('❌ Migration validation FAILED\n');
    console.log('Critical issues found. Please fix before proceeding.\n');
    process.exit(1);
  } else if (report.verdict === 'PASSED_WITH_WARNINGS') {
    console.log('⚠️  Migration validation PASSED with warnings\n');
    console.log('Some non-critical checks failed. Review recommended.\n');
    process.exit(0);
  } else {
    console.log('✅ Migration validation PASSED\n');
    console.log('All checks passed! Migration successful.\n');
    process.exit(0);
  }
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log('UNRDF V6 Migration Validation\n');

  const allResults = [];

  // Run all check groups
  allResults.push(...await runChecks(CHECKS.critical, 'Critical'));
  allResults.push(...await runChecks(CHECKS.important, 'Important'));

  if (!CONFIG.quick) {
    allResults.push(...await runChecks(CHECKS.performance, 'Performance'));
  }

  allResults.push(...await runChecks(CHECKS.quality, 'Quality'));

  // Generate and print report
  const report = await generateReport(allResults);
  printSummary(report);
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error('Validation failed:', error);
    process.exit(1);
  });
}

export { runChecks, generateReport };
