#!/usr/bin/env node
/**
 * Validate rollback completed successfully
 *
 * Runs comprehensive validation checks to ensure rollback was successful
 * and system is in a stable state.
 *
 * Usage:
 *   node scripts/validate-rollback.mjs [--json]
 */

import { execSync } from 'child_process';
import { readFileSync, existsSync } from 'fs';
import { join } from 'path';

const ROOT_DIR = process.cwd();
const JSON_OUTPUT = process.argv.includes('--json');

/**
 * Execute command and return output
 * @param {string} cmd - Command to execute
 * @returns {{success: boolean, output: string, error: string|null}}
 */
function exec(cmd) {
  try {
    const output = execSync(cmd, {
      cwd: ROOT_DIR,
      encoding: 'utf-8',
      stdio: 'pipe'
    });
    return { success: true, output, error: null };
  } catch (err) {
    return {
      success: false,
      output: err.stdout || '',
      error: err.message
    };
  }
}

/**
 * Validation checks
 */
const checks = [
  {
    id: 'version',
    name: 'Version check',
    critical: true,
    run: () => {
      const pkgPath = join(ROOT_DIR, 'package.json');
      if (!existsSync(pkgPath)) {
        return { passed: false, message: 'package.json not found' };
      }

      const pkg = JSON.parse(readFileSync(pkgPath, 'utf-8'));
      const isV5 = pkg.version.startsWith('5.');

      return {
        passed: isV5,
        message: `Version: ${pkg.version} (expected: 5.x)`,
        data: { version: pkg.version, expected: '5.x' }
      };
    }
  },

  {
    id: 'dependencies',
    name: 'Dependencies check',
    critical: true,
    run: () => {
      const lockfilePath = join(ROOT_DIR, 'pnpm-lock.yaml');
      if (!existsSync(lockfilePath)) {
        return { passed: false, message: 'pnpm-lock.yaml not found' };
      }

      const result = exec('pnpm ls --depth=0 --json');
      if (!result.success) {
        return {
          passed: false,
          message: 'Failed to list dependencies',
          data: { error: result.error }
        };
      }

      // Check critical dependencies
      const output = result.output;
      const zodV3 = output.includes('zod 3.') || output.includes('zod@3.');
      const canonizeV2 = output.includes('rdf-canonize 2.') || output.includes('rdf-canonize@2.');

      const passed = zodV3 && canonizeV2;

      return {
        passed,
        message: passed ? 'Dependencies restored correctly' : 'Dependency version mismatch',
        data: {
          zod: zodV3 ? '3.x' : 'incorrect',
          'rdf-canonize': canonizeV2 ? '2.x' : 'incorrect'
        }
      };
    }
  },

  {
    id: 'git-state',
    name: 'Git state check',
    critical: false,
    run: () => {
      const result = exec('git rev-parse HEAD');
      if (!result.success) {
        return {
          passed: false,
          message: 'Not a git repository',
          data: { error: result.error }
        };
      }

      const commit = result.output.trim();
      const branchResult = exec('git branch --show-current');
      const branch = branchResult.output.trim();

      return {
        passed: true,
        message: `On commit ${commit.substring(0, 7)} (${branch})`,
        data: { commit, branch }
      };
    }
  },

  {
    id: 'tests',
    name: 'Tests passing',
    critical: true,
    run: () => {
      const result = exec('timeout 30s pnpm test:fast 2>&1');

      if (!result.success) {
        return {
          passed: false,
          message: 'Tests failed or timed out',
          data: {
            output: result.output.split('\n').slice(-10).join('\n'),
            error: result.error
          }
        };
      }

      const hasFail = result.output.includes('FAIL') || result.output.includes('failed');

      return {
        passed: !hasFail,
        message: hasFail ? 'Some tests failed' : 'All tests passed',
        data: { output: result.output.split('\n').slice(-5).join('\n') }
      };
    }
  },

  {
    id: 'lint',
    name: 'Linting clean',
    critical: false,
    run: () => {
      const result = exec('timeout 15s pnpm lint 2>&1');

      if (!result.success) {
        const hasErrors = result.output.includes('error');
        return {
          passed: !hasErrors,
          message: hasErrors ? 'Linting errors found' : 'Linting passed',
          data: {
            output: result.output.split('\n').filter(l => l.includes('error')).slice(0, 5).join('\n')
          }
        };
      }

      return {
        passed: true,
        message: 'Linting clean',
        data: { output: 'No errors' }
      };
    }
  },

  {
    id: 'build',
    name: 'Build successful',
    critical: true,
    run: () => {
      const result = exec('timeout 30s pnpm build 2>&1');

      if (!result.success) {
        return {
          passed: false,
          message: 'Build failed',
          data: {
            output: result.output.split('\n').slice(-10).join('\n'),
            error: result.error
          }
        };
      }

      const hasError = result.output.includes('Error') || result.output.includes('ERROR');

      return {
        passed: !hasError,
        message: hasError ? 'Build had errors' : 'Build successful',
        data: { output: result.output.split('\n').slice(-3).join('\n') }
      };
    }
  },

  {
    id: 'rollback-report',
    name: 'Rollback report exists',
    critical: false,
    run: () => {
      const reportPath = join(ROOT_DIR, 'rollback-report.json');
      if (!existsSync(reportPath)) {
        return {
          passed: false,
          message: 'Rollback report not found (manual rollback?)',
          data: { path: reportPath }
        };
      }

      const report = JSON.parse(readFileSync(reportPath, 'utf-8'));

      return {
        passed: report.success === true,
        message: report.success ? 'Rollback completed successfully' : 'Rollback had issues',
        data: report
      };
    }
  }
];

/**
 * Run all validation checks
 */
function runValidation() {
  const results = {
    timestamp: new Date().toISOString(),
    passed: 0,
    failed: 0,
    criticalFailed: 0,
    checks: []
  };

  for (const check of checks) {
    if (!JSON_OUTPUT) {
      process.stdout.write(`Running: ${check.name}... `);
    }

    try {
      const result = check.run();
      const checkResult = {
        id: check.id,
        name: check.name,
        critical: check.critical,
        passed: result.passed,
        message: result.message,
        data: result.data || {}
      };

      results.checks.push(checkResult);

      if (result.passed) {
        results.passed++;
        if (!JSON_OUTPUT) {
          console.log('✓');
        }
      } else {
        results.failed++;
        if (check.critical) {
          results.criticalFailed++;
        }
        if (!JSON_OUTPUT) {
          console.log(`✗ ${result.message}`);
        }
      }
    } catch (err) {
      results.failed++;
      if (check.critical) {
        results.criticalFailed++;
      }

      results.checks.push({
        id: check.id,
        name: check.name,
        critical: check.critical,
        passed: false,
        message: `Exception: ${err.message}`,
        data: { error: err.message }
      });

      if (!JSON_OUTPUT) {
        console.log(`✗ Exception: ${err.message}`);
      }
    }
  }

  return results;
}

/**
 * Main execution
 */
function main() {
  if (!JSON_OUTPUT) {
    console.log('='.repeat(60));
    console.log('Rollback Validation');
    console.log('='.repeat(60));
    console.log('');
  }

  const results = runValidation();

  if (JSON_OUTPUT) {
    console.log(JSON.stringify(results, null, 2));
  } else {
    console.log('');
    console.log('='.repeat(60));
    console.log(`Results: ${results.passed} passed, ${results.failed} failed`);

    if (results.criticalFailed > 0) {
      console.log(`⚠ WARNING: ${results.criticalFailed} critical checks failed`);
    }

    console.log('='.repeat(60));
  }

  // Exit code based on critical failures
  process.exit(results.criticalFailed === 0 ? 0 : 1);
}

main();
