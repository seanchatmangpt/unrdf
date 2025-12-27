#!/usr/bin/env node
/**
 * Test Rollback System - Validate rollback capabilities
 *
 * Comprehensive testing of snapshot and rollback functionality
 * WITHOUT making actual changes to the system.
 *
 * Usage:
 *   node scripts/test-rollback-system.mjs
 */

import { execSync } from 'child_process';
import { existsSync, readFileSync } from 'fs';
import { join } from 'path';

const ROOT_DIR = process.cwd();
const BACKUP_DIR = join(ROOT_DIR, '.rollback-snapshots');

/**
 * Execute command with timeout
 * @param {string} cmd - Command to execute
 * @param {number} timeout - Timeout in seconds
 * @returns {{success: boolean, output: string, duration: number}}
 */
function exec(cmd, timeout = 30) {
  const start = Date.now();
  try {
    const output = execSync(`timeout ${timeout}s ${cmd}`, {
      cwd: ROOT_DIR,
      encoding: 'utf-8',
      stdio: 'pipe'
    });
    return {
      success: true,
      output,
      duration: Date.now() - start
    };
  } catch (err) {
    return {
      success: false,
      output: err.stdout || err.message,
      duration: Date.now() - start
    };
  }
}

/**
 * Test cases
 */
const tests = [
  {
    id: 'snapshot-creation',
    name: 'Snapshot creation works',
    critical: true,
    run: () => {
      const snapshotName = `test-${Date.now()}`;
      const result = exec(`node scripts/v6-snapshot.mjs ${snapshotName}`, 15);

      if (!result.success) {
        return {
          passed: false,
          message: 'Snapshot creation failed',
          data: { output: result.output }
        };
      }

      // Verify snapshot exists
      const snapshotPath = join(BACKUP_DIR, snapshotName);
      if (!existsSync(snapshotPath)) {
        return {
          passed: false,
          message: 'Snapshot directory not created',
          data: { expectedPath: snapshotPath }
        };
      }

      // Verify required files
      const requiredFiles = [
        'manifest.json',
        'package.json',
        'pnpm-lock.yaml',
        'git-state.json'
      ];

      for (const file of requiredFiles) {
        if (!existsSync(join(snapshotPath, file))) {
          return {
            passed: false,
            message: `Missing required file: ${file}`,
            data: { file, snapshotPath }
          };
        }
      }

      return {
        passed: true,
        message: `Snapshot created successfully (${result.duration}ms)`,
        data: { snapshotName, duration: result.duration }
      };
    }
  },

  {
    id: 'rollback-dry-run',
    name: 'Rollback dry-run executes',
    critical: true,
    run: () => {
      const result = exec('node scripts/v6-rollback.mjs --dry-run --skip-tests', 30);

      // Dry run may "fail" due to no v5 snapshots, but should execute
      const hasOutput = result.output.length > 0;
      const hasDryRunText = result.output.includes('[DRY-RUN]');

      if (!hasOutput) {
        return {
          passed: false,
          message: 'Rollback script produced no output',
          data: { output: result.output }
        };
      }

      return {
        passed: hasDryRunText,
        message: hasDryRunText
          ? `Dry-run executed (${result.duration}ms)`
          : 'Dry-run text not found in output',
        data: { duration: result.duration, output: result.output.split('\n').slice(0, 10) }
      };
    }
  },

  {
    id: 'snapshot-manifest',
    name: 'Snapshot manifest is valid JSON',
    critical: true,
    run: () => {
      // Find latest snapshot
      if (!existsSync(BACKUP_DIR)) {
        return {
          passed: false,
          message: 'No snapshots directory exists',
          data: { backupDir: BACKUP_DIR }
        };
      }

      const result = exec('ls -t .rollback-snapshots/', 5);
      if (!result.success || !result.output.trim()) {
        return {
          passed: false,
          message: 'No snapshots found',
          data: { output: result.output }
        };
      }

      const latestSnapshot = result.output.trim().split('\n')[0];
      const manifestPath = join(BACKUP_DIR, latestSnapshot, 'manifest.json');

      if (!existsSync(manifestPath)) {
        return {
          passed: false,
          message: 'Manifest not found',
          data: { manifestPath }
        };
      }

      try {
        const manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));

        // Validate required fields
        const requiredFields = ['name', 'timestamp', 'git', 'environment', 'files'];
        for (const field of requiredFields) {
          if (!manifest[field]) {
            return {
              passed: false,
              message: `Manifest missing field: ${field}`,
              data: { manifest, field }
            };
          }
        }

        return {
          passed: true,
          message: 'Manifest is valid',
          data: { snapshot: latestSnapshot, manifest }
        };
      } catch (err) {
        return {
          passed: false,
          message: `Manifest parsing failed: ${err.message}`,
          data: { error: err.message }
        };
      }
    }
  },

  {
    id: 'rollback-report',
    name: 'Rollback generates report',
    critical: false,
    run: () => {
      const reportPath = join(ROOT_DIR, 'rollback-report.json');

      if (!existsSync(reportPath)) {
        return {
          passed: false,
          message: 'No rollback report found (run rollback first)',
          data: { reportPath }
        };
      }

      try {
        const report = JSON.parse(readFileSync(reportPath, 'utf-8'));

        const requiredFields = ['timestamp', 'success', 'duration'];
        for (const field of requiredFields) {
          if (report[field] === undefined) {
            return {
              passed: false,
              message: `Report missing field: ${field}`,
              data: { report, field }
            };
          }
        }

        return {
          passed: true,
          message: 'Rollback report is valid',
          data: { report }
        };
      } catch (err) {
        return {
          passed: false,
          message: `Report parsing failed: ${err.message}`,
          data: { error: err.message }
        };
      }
    }
  },

  {
    id: 'scripts-executable',
    name: 'Rollback scripts are executable',
    critical: true,
    run: () => {
      const scripts = [
        'scripts/v6-snapshot.mjs',
        'scripts/v6-rollback.mjs',
        'scripts/validate-rollback.mjs'
      ];

      const nonExecutable = [];

      for (const script of scripts) {
        const scriptPath = join(ROOT_DIR, script);
        if (!existsSync(scriptPath)) {
          nonExecutable.push({ script, reason: 'not found' });
          continue;
        }

        // Check if executable
        const result = exec(`test -x ${scriptPath} && echo yes || echo no`, 2);
        if (!result.output.includes('yes')) {
          nonExecutable.push({ script, reason: 'not executable' });
        }
      }

      if (nonExecutable.length > 0) {
        return {
          passed: false,
          message: 'Some scripts are not executable',
          data: { nonExecutable }
        };
      }

      return {
        passed: true,
        message: 'All scripts are executable',
        data: { scripts }
      };
    }
  },

  {
    id: 'snapshot-size',
    name: 'Snapshots are reasonable size',
    critical: false,
    run: () => {
      if (!existsSync(BACKUP_DIR)) {
        return { passed: true, message: 'No snapshots yet', data: {} };
      }

      const result = exec('du -sh .rollback-snapshots/', 5);
      if (!result.success) {
        return {
          passed: false,
          message: 'Failed to check snapshot size',
          data: { output: result.output }
        };
      }

      const sizeMatch = result.output.match(/^(\d+)([KMG])/);
      if (!sizeMatch) {
        return {
          passed: true,
          message: 'Could not parse size',
          data: { output: result.output }
        };
      }

      const size = parseInt(sizeMatch[1]);
      const unit = sizeMatch[2];

      // Warn if >100MB
      const tooBig = (unit === 'G') || (unit === 'M' && size > 100);

      return {
        passed: !tooBig,
        message: tooBig
          ? `Snapshots are large: ${sizeMatch[0]}`
          : `Snapshots size OK: ${sizeMatch[0]}`,
        data: { size: result.output.trim() }
      };
    }
  }
];

/**
 * Run all tests
 */
function runTests() {
  console.log('='.repeat(60));
  console.log('Rollback System Test Suite');
  console.log('='.repeat(60));
  console.log('');

  const results = {
    timestamp: new Date().toISOString(),
    passed: 0,
    failed: 0,
    criticalFailed: 0,
    tests: []
  };

  for (const test of tests) {
    process.stdout.write(`${test.name}... `);

    try {
      const result = test.run();

      results.tests.push({
        id: test.id,
        name: test.name,
        critical: test.critical,
        passed: result.passed,
        message: result.message,
        data: result.data
      });

      if (result.passed) {
        results.passed++;
        console.log(`✓ ${result.message}`);
      } else {
        results.failed++;
        if (test.critical) {
          results.criticalFailed++;
        }
        console.log(`✗ ${result.message}`);
      }
    } catch (err) {
      results.failed++;
      if (test.critical) {
        results.criticalFailed++;
      }

      results.tests.push({
        id: test.id,
        name: test.name,
        critical: test.critical,
        passed: false,
        message: `Exception: ${err.message}`,
        data: { error: err.message }
      });

      console.log(`✗ Exception: ${err.message}`);
    }
  }

  console.log('');
  console.log('='.repeat(60));
  console.log(`Results: ${results.passed} passed, ${results.failed} failed`);

  if (results.criticalFailed > 0) {
    console.log(`⚠ WARNING: ${results.criticalFailed} critical tests failed`);
    console.log('');
    console.log('Rollback system is NOT ready for production.');
    console.log('Fix critical failures before deploying v6.');
  } else if (results.failed > 0) {
    console.log(`⚠ ${results.failed} non-critical tests failed`);
    console.log('');
    console.log('Rollback system is functional but has issues.');
  } else {
    console.log('');
    console.log('✓ All tests passed! Rollback system is ready.');
  }

  console.log('='.repeat(60));

  return results;
}

// Execute
const results = runTests();
process.exit(results.criticalFailed === 0 ? 0 : 1);
