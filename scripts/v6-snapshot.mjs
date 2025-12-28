#!/usr/bin/env node
/**
 * V6 Snapshot Script - Create deployment snapshots for rollback
 *
 * Creates comprehensive snapshots of the system state for rollback purposes.
 *
 * Usage:
 *   node scripts/v6-snapshot.mjs [snapshot-name]
 *
 * @example
 * ```bash
 * # Create snapshot with auto-generated name
 * node scripts/v6-snapshot.mjs
 *
 * # Create snapshot with custom name
 * node scripts/v6-snapshot.mjs pre-v6-production-deploy
 * ```
 */

import { execSync } from 'child_process';
import { existsSync, readFileSync, writeFileSync, mkdirSync, copyFileSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = join(__dirname, '..');
const BACKUP_DIR = join(ROOT_DIR, '.rollback-snapshots');

/**
 * Execute command synchronously
 * @param {string} cmd - Command to execute
 * @returns {string} Command output
 */
function exec(cmd) {
  return execSync(cmd, { cwd: ROOT_DIR, encoding: 'utf-8', stdio: 'pipe' });
}

/**
 * Copy directory recursively
 * @param {string} src - Source directory
 * @param {string} dest - Destination directory
 */
function copyDirRecursive(src, dest) {
  if (!existsSync(src)) return;

  mkdirSync(dest, { recursive: true });

  const entries = readdirSync(src, { withFileTypes: true });
  for (const entry of entries) {
    const srcPath = join(src, entry.name);
    const destPath = join(dest, entry.name);

    if (entry.isDirectory()) {
      copyDirRecursive(srcPath, destPath);
    } else {
      copyFileSync(srcPath, destPath);
    }
  }
}

/**
 * Create snapshot
 * @param {string} snapshotName - Name of snapshot
 */
function createSnapshot(snapshotName) {
  console.log('Creating deployment snapshot...');
  console.log(`Snapshot name: ${snapshotName}`);

  const snapshotPath = join(BACKUP_DIR, snapshotName);

  if (existsSync(snapshotPath)) {
    throw new Error(`Snapshot already exists: ${snapshotName}`);
  }

  mkdirSync(snapshotPath, { recursive: true });

  // 1. Backup package.json files
  console.log('  Backing up package.json files...');
  const packageJsonFiles = [
    'package.json',
    'packages/core/package.json',
    'packages/oxigraph/package.json',
    'packages/hooks/package.json',
    'packages/federation/package.json'
  ];

  for (const file of packageJsonFiles) {
    const sourcePath = join(ROOT_DIR, file);
    if (existsSync(sourcePath)) {
      const destPath = join(snapshotPath, file);
      mkdirSync(dirname(destPath), { recursive: true });
      copyFileSync(sourcePath, destPath);
      console.log(`    ✓ ${file}`);
    }
  }

  // 2. Backup lockfile
  console.log('  Backing up pnpm-lock.yaml...');
  const lockfilePath = join(ROOT_DIR, 'pnpm-lock.yaml');
  if (existsSync(lockfilePath)) {
    copyFileSync(lockfilePath, join(snapshotPath, 'pnpm-lock.yaml'));
    console.log('    ✓ pnpm-lock.yaml');
  }

  // 3. Save git state
  console.log('  Saving git state...');
  const gitState = {
    branch: exec('git branch --show-current').trim(),
    commit: exec('git rev-parse HEAD').trim(),
    shortCommit: exec('git rev-parse --short HEAD').trim(),
    status: exec('git status --porcelain'),
    tags: exec('git tag --points-at HEAD').trim().split('\n').filter(Boolean),
    remotes: exec('git remote -v').trim()
  };
  writeFileSync(
    join(snapshotPath, 'git-state.json'),
    JSON.stringify(gitState, null, 2)
  );
  console.log('    ✓ git-state.json');

  // 4. Save dependency tree
  console.log('  Saving dependency tree...');
  try {
    const depTree = exec('pnpm ls --depth=1 --json');
    writeFileSync(join(snapshotPath, 'dependency-tree.json'), depTree);
    console.log('    ✓ dependency-tree.json');
  } catch (err) {
    console.warn('    ⚠ Failed to save dependency tree:', err.message);
  }

  // 5. Save environment info
  console.log('  Saving environment info...');
  const envInfo = {
    nodeVersion: process.version,
    platform: process.platform,
    arch: process.arch,
    timestamp: new Date().toISOString(),
    snapshotName
  };
  writeFileSync(
    join(snapshotPath, 'environment.json'),
    JSON.stringify(envInfo, null, 2)
  );
  console.log('    ✓ environment.json');

  // 6. Create snapshot manifest
  console.log('  Creating snapshot manifest...');
  const manifest = {
    name: snapshotName,
    timestamp: new Date().toISOString(),
    git: gitState,
    environment: envInfo,
    files: packageJsonFiles.filter(f => existsSync(join(ROOT_DIR, f)))
  };
  writeFileSync(
    join(snapshotPath, 'manifest.json'),
    JSON.stringify(manifest, null, 2)
  );
  console.log('    ✓ manifest.json');

  // 7. Create README for snapshot
  const readme = `# Snapshot: ${snapshotName}

Created: ${new Date().toISOString()}
Commit: ${gitState.commit}
Branch: ${gitState.branch}

## Contents

- package.json files (root + packages)
- pnpm-lock.yaml
- Git state snapshot
- Dependency tree
- Environment metadata

## Restore

To restore this snapshot:

\`\`\`bash
node scripts/v6-rollback.mjs --snapshot ${snapshotName}
\`\`\`

## Manifest

See \`manifest.json\` for complete snapshot details.
`;

  writeFileSync(join(snapshotPath, 'README.md'), readme);
  console.log('    ✓ README.md');

  console.log('');
  console.log('✓ Snapshot created successfully');
  console.log(`  Location: ${snapshotPath}`);
  console.log(`  Commit: ${gitState.shortCommit} (${gitState.branch})`);
  console.log('');
  console.log('To restore this snapshot:');
  console.log(`  node scripts/v6-rollback.mjs --snapshot ${snapshotName}`);

  return snapshotPath;
}

/**
 * Main execution
 */
function main() {
  const args = process.argv.slice(2);

  let snapshotName = args[0];
  if (!snapshotName) {
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-').split('T')[0];
    const branch = exec('git branch --show-current').trim().replace(/[^a-zA-Z0-9-]/g, '-');
    snapshotName = `v5-${branch}-${timestamp}`;
  }

  try {
    createSnapshot(snapshotName);
    process.exit(0);
  } catch (err) {
    console.error('✗ Snapshot creation failed');
    console.error(`  Error: ${err.message}`);
    process.exit(1);
  }
}

main();
