#!/usr/bin/env node
/**
 * V6 Rollback Script - Automated rollback to previous stable version
 *
 * CRITICAL: This script provides automated rollback capabilities for v6 deployment failures.
 *
 * Usage:
 *   node scripts/v6-rollback.mjs [options]
 *
 * Options:
 *   --dry-run              Show what would be rolled back without executing
 *   --snapshot <name>      Rollback to specific snapshot (default: latest)
 *   --skip-tests          Skip post-rollback validation tests
 *   --force               Force rollback even if validation fails
 *
 * @example
 * ```bash
 * # Dry run to see what would happen
 * node scripts/v6-rollback.mjs --dry-run
 *
 * # Full rollback with validation
 * node scripts/v6-rollback.mjs
 *
 * # Rollback to specific snapshot
 * node scripts/v6-rollback.mjs --snapshot pre-v6-deployment-2025-12-27
 * ```
 */

import { execSync } from 'child_process';
import { existsSync, readFileSync, writeFileSync, mkdirSync, copyFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = join(__dirname, '..');
const BACKUP_DIR = join(ROOT_DIR, '.rollback-snapshots');
const LOG_FILE = join(ROOT_DIR, 'rollback.log');

/**
 * Logging utility with timestamps
 * @param {string} message - Message to log
 * @param {'info'|'warn'|'error'|'success'} level - Log level
 */
function log(message, level = 'info') {
  const timestamp = new Date().toISOString();
  const prefix = {
    info: '[INFO]',
    warn: '[WARN]',
    error: '[ERROR]',
    success: '[SUCCESS]'
  }[level];

  const logMessage = `${timestamp} ${prefix} ${message}`;
  console.log(logMessage);

  // Append to log file
  try {
    writeFileSync(LOG_FILE, logMessage + '\n', { flag: 'a' });
  } catch (err) {
    console.error('Failed to write to log file:', err.message);
  }
}

/**
 * Execute command with error handling
 * @param {string} cmd - Command to execute
 * @param {boolean} dryRun - If true, only log command without executing
 * @returns {string|null} Command output or null if dry run
 */
function exec(cmd, dryRun = false) {
  if (dryRun) {
    log(`[DRY-RUN] Would execute: ${cmd}`, 'info');
    return null;
  }

  try {
    log(`Executing: ${cmd}`, 'info');
    const output = execSync(cmd, {
      cwd: ROOT_DIR,
      encoding: 'utf-8',
      stdio: ['pipe', 'pipe', 'pipe']
    });
    return output;
  } catch (err) {
    log(`Command failed: ${err.message}`, 'error');
    throw err;
  }
}

/**
 * Parse command line arguments
 * @returns {{dryRun: boolean, snapshot: string|null, skipTests: boolean, force: boolean}}
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const options = {
    dryRun: args.includes('--dry-run'),
    snapshot: null,
    skipTests: args.includes('--skip-tests'),
    force: args.includes('--force')
  };

  const snapshotIndex = args.indexOf('--snapshot');
  if (snapshotIndex !== -1 && args[snapshotIndex + 1]) {
    options.snapshot = args[snapshotIndex + 1];
  }

  return options;
}

/**
 * Create pre-rollback snapshot
 * @param {boolean} dryRun - Dry run mode
 */
function createPreRollbackSnapshot(dryRun) {
  log('Creating pre-rollback snapshot...', 'info');

  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const snapshotName = `pre-rollback-${timestamp}`;
  const snapshotPath = join(BACKUP_DIR, snapshotName);

  if (!dryRun) {
    mkdirSync(snapshotPath, { recursive: true });

    // Backup critical files
    const filesToBackup = [
      'package.json',
      'pnpm-lock.yaml',
      'packages/core/package.json',
      'packages/oxigraph/package.json'
    ];

    for (const file of filesToBackup) {
      const sourcePath = join(ROOT_DIR, file);
      if (existsSync(sourcePath)) {
        const destPath = join(snapshotPath, file);
        mkdirSync(dirname(destPath), { recursive: true });
        copyFileSync(sourcePath, destPath);
        log(`  Backed up: ${file}`, 'info');
      }
    }

    // Save git state
    const gitState = {
      branch: exec('git branch --show-current', false).trim(),
      commit: exec('git rev-parse HEAD', false).trim(),
      status: exec('git status --porcelain', false)
    };
    writeFileSync(
      join(snapshotPath, 'git-state.json'),
      JSON.stringify(gitState, null, 2)
    );
  }

  log(`Pre-rollback snapshot created: ${snapshotName}`, 'success');
  return snapshotName;
}

/**
 * Find latest snapshot or specific snapshot
 * @param {string|null} snapshotName - Specific snapshot name or null for latest
 * @returns {string} Snapshot path
 */
function findSnapshot(snapshotName) {
  if (!existsSync(BACKUP_DIR)) {
    throw new Error('No rollback snapshots found. Cannot proceed with rollback.');
  }

  if (snapshotName) {
    const snapshotPath = join(BACKUP_DIR, snapshotName);
    if (!existsSync(snapshotPath)) {
      throw new Error(`Snapshot not found: ${snapshotName}`);
    }
    return snapshotPath;
  }

  // Find latest v5 snapshot
  const snapshots = execSync('ls -t', { cwd: BACKUP_DIR, encoding: 'utf-8' })
    .trim()
    .split('\n')
    .filter(s => s.includes('v5') || s.includes('pre-v6'));

  if (snapshots.length === 0) {
    throw new Error('No v5 snapshots found');
  }

  return join(BACKUP_DIR, snapshots[0]);
}

/**
 * Rollback dependencies
 * @param {string} snapshotPath - Path to snapshot
 * @param {boolean} dryRun - Dry run mode
 */
function rollbackDependencies(snapshotPath, dryRun) {
  log('Rolling back dependencies...', 'info');

  const packageJsonPath = join(snapshotPath, 'package.json');
  const lockfilePath = join(snapshotPath, 'pnpm-lock.yaml');

  if (!existsSync(packageJsonPath)) {
    throw new Error('Snapshot does not contain package.json');
  }

  if (!dryRun) {
    // Restore package.json
    copyFileSync(packageJsonPath, join(ROOT_DIR, 'package.json'));
    log('  Restored package.json', 'info');

    // Restore lockfile if exists
    if (existsSync(lockfilePath)) {
      copyFileSync(lockfilePath, join(ROOT_DIR, 'pnpm-lock.yaml'));
      log('  Restored pnpm-lock.yaml', 'info');
    }

    // Reinstall dependencies
    log('  Reinstalling dependencies...', 'info');
    exec('pnpm install --frozen-lockfile', false);
  } else {
    log('[DRY-RUN] Would restore package.json and pnpm-lock.yaml', 'info');
    log('[DRY-RUN] Would run: pnpm install --frozen-lockfile', 'info');
  }

  log('Dependencies rolled back successfully', 'success');
}

/**
 * Rollback package versions
 * @param {string} snapshotPath - Path to snapshot
 * @param {boolean} dryRun - Dry run mode
 */
function rollbackPackageVersions(snapshotPath, dryRun) {
  log('Rolling back package versions...', 'info');

  const packages = ['core', 'oxigraph'];

  for (const pkg of packages) {
    const pkgJsonPath = join(snapshotPath, 'packages', pkg, 'package.json');
    if (existsSync(pkgJsonPath)) {
      const destPath = join(ROOT_DIR, 'packages', pkg, 'package.json');

      if (!dryRun) {
        copyFileSync(pkgJsonPath, destPath);
        log(`  Restored packages/${pkg}/package.json`, 'info');
      } else {
        log(`[DRY-RUN] Would restore packages/${pkg}/package.json`, 'info');
      }
    }
  }

  log('Package versions rolled back', 'success');
}

/**
 * Rollback git state
 * @param {string} snapshotPath - Path to snapshot
 * @param {boolean} dryRun - Dry run mode
 * @param {boolean} force - Force rollback even if there are uncommitted changes
 */
function rollbackGitState(snapshotPath, dryRun, force) {
  log('Rolling back git state...', 'info');

  const gitStatePath = join(snapshotPath, 'git-state.json');
  if (!existsSync(gitStatePath)) {
    log('No git state in snapshot, skipping git rollback', 'warn');
    return;
  }

  const gitState = JSON.parse(readFileSync(gitStatePath, 'utf-8'));

  // Check for uncommitted changes
  const currentStatus = exec('git status --porcelain', false);
  if (currentStatus && !force) {
    log('Uncommitted changes detected. Use --force to rollback anyway.', 'warn');
    throw new Error('Uncommitted changes present. Rollback aborted.');
  }

  if (!dryRun) {
    // Checkout the commit
    exec(`git checkout ${gitState.commit}`, false);
    log(`  Checked out commit: ${gitState.commit}`, 'info');

    // If on a branch, checkout the branch
    if (gitState.branch) {
      exec(`git checkout ${gitState.branch}`, false);
      log(`  Checked out branch: ${gitState.branch}`, 'info');
    }
  } else {
    log(`[DRY-RUN] Would checkout commit: ${gitState.commit}`, 'info');
    if (gitState.branch) {
      log(`[DRY-RUN] Would checkout branch: ${gitState.branch}`, 'info');
    }
  }

  log('Git state rolled back', 'success');
}

/**
 * Run post-rollback validation
 * @param {boolean} dryRun - Dry run mode
 * @returns {boolean} True if validation passed
 */
function runValidation(dryRun) {
  log('Running post-rollback validation...', 'info');

  if (dryRun) {
    log('[DRY-RUN] Would run validation tests', 'info');
    return true;
  }

  try {
    // Run quick tests
    log('  Running tests...', 'info');
    exec('timeout 30s pnpm test:fast', false);
    log('  Tests passed ✓', 'success');

    // Run linting
    log('  Running linter...', 'info');
    exec('timeout 15s pnpm lint', false);
    log('  Linting passed ✓', 'success');

    // Check build
    log('  Building packages...', 'info');
    exec('timeout 30s pnpm build', false);
    log('  Build successful ✓', 'success');

    return true;
  } catch (err) {
    log('Validation failed: ' + err.message, 'error');
    return false;
  }
}

/**
 * Generate rollback report
 * @param {string} snapshotPath - Path to snapshot used
 * @param {boolean} success - Whether rollback was successful
 * @param {number} startTime - Rollback start timestamp
 */
function generateReport(snapshotPath, success, startTime) {
  const duration = ((Date.now() - startTime) / 1000).toFixed(2);

  const report = {
    timestamp: new Date().toISOString(),
    success,
    duration: `${duration}s`,
    snapshot: snapshotPath,
    packageVersions: {},
    validation: success
  };

  // Read package versions
  try {
    const rootPkg = JSON.parse(readFileSync(join(ROOT_DIR, 'package.json'), 'utf-8'));
    report.packageVersions.root = rootPkg.version;

    const corePkg = JSON.parse(readFileSync(join(ROOT_DIR, 'packages/core/package.json'), 'utf-8'));
    report.packageVersions.core = corePkg.version;
  } catch (err) {
    log('Failed to read package versions: ' + err.message, 'warn');
  }

  const reportPath = join(ROOT_DIR, 'rollback-report.json');
  writeFileSync(reportPath, JSON.stringify(report, null, 2));

  log(`Rollback report saved to: ${reportPath}`, 'info');

  return report;
}

/**
 * Main rollback execution
 */
async function main() {
  const startTime = Date.now();
  const options = parseArgs();

  log('='.repeat(60), 'info');
  log('V6 Rollback Script', 'info');
  log('='.repeat(60), 'info');
  log(`Dry Run: ${options.dryRun}`, 'info');
  log(`Skip Tests: ${options.skipTests}`, 'info');
  log(`Force: ${options.force}`, 'info');
  log('='.repeat(60), 'info');

  try {
    // Step 1: Create pre-rollback snapshot
    const preRollbackSnapshot = createPreRollbackSnapshot(options.dryRun);

    // Step 2: Find snapshot to restore
    const snapshotPath = findSnapshot(options.snapshot);
    log(`Using snapshot: ${snapshotPath}`, 'info');

    // Step 3: Rollback dependencies
    rollbackDependencies(snapshotPath, options.dryRun);

    // Step 4: Rollback package versions
    rollbackPackageVersions(snapshotPath, options.dryRun);

    // Step 5: Rollback git state
    rollbackGitState(snapshotPath, options.dryRun, options.force);

    // Step 6: Run validation
    let validationPassed = true;
    if (!options.skipTests && !options.dryRun) {
      validationPassed = runValidation(options.dryRun);
    }

    if (!validationPassed) {
      throw new Error('Post-rollback validation failed');
    }

    // Step 7: Generate report
    const report = generateReport(snapshotPath, true, startTime);

    log('='.repeat(60), 'info');
    log('✓ Rollback completed successfully', 'success');
    log(`Duration: ${report.duration}`, 'info');
    log('='.repeat(60), 'info');

    if (options.dryRun) {
      log('', 'info');
      log('This was a DRY RUN. No changes were made.', 'warn');
      log('Remove --dry-run to execute the rollback.', 'warn');
    }

    process.exit(0);

  } catch (err) {
    log('='.repeat(60), 'error');
    log('✗ Rollback failed', 'error');
    log(`Error: ${err.message}`, 'error');
    log('='.repeat(60), 'error');

    generateReport(options.snapshot || 'unknown', false, startTime);

    process.exit(1);
  }
}

// Execute
main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
