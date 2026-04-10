/**
 * @file Auto-Fix Logic
 * @module cli/commands/doctor/fixes
 *
 * @description
 * Applies automatic fixes for detected issues.
 * Only applies safe, reversible operations.
 */

import { execSync } from 'node:child_process';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Map of fixable checks to their implementations
 */
const FIX_IMPLEMENTATIONS = {
  'node_modules consistency': fixNodeModules,
  'Workspace dependencies': fixWorkspaceDeps,
  'Build artifacts': fixBuildArtifacts,
  'ESLint status': fixLint,
  // Manual fixes that cannot be automated
  'Node.js version': null,
  'pnpm version': null,
  'Environment variables': null,
  'Required tools': null,
  'Daemon status': null,
  'MCP server status': null,
  'RDF store': null,
  'Port availability': null,
  'Disk space': null,
  'Test coverage': null,
  'File size violations': null,
  'TypeScript contamination': null,
  'N3 import violations': null,
  'Skipped tests': null,
  'Federation peers': null,
  'OTEL exporter': null,
  'Test containers (Docker)': null,
  'Optional services': null,
};

/**
 * Fix node_modules consistency
 */
async function fixNodeModules() {
  console.log('   Running: pnpm install --frozen-lockfile');
  execSync('pnpm install --frozen-lockfile', {
    cwd: projectRoot,
    stdio: 'inherit',
  });
  console.log('   ✅ node_modules fixed');
}

/**
 * Fix workspace dependencies
 */
async function fixWorkspaceDeps() {
  console.log('   Running: pnpm install');
  execSync('pnpm install', {
    cwd: projectRoot,
    stdio: 'inherit',
  });
  console.log('   ✅ Workspace dependencies fixed');
}

/**
 * Fix build artifacts
 */
async function fixBuildArtifacts() {
  console.log('   Running: pnpm build');
  execSync('pnpm build', {
    cwd: projectRoot,
    stdio: 'inherit',
  });
  console.log('   ✅ Build artifacts created');
}

/**
 * Fix ESLint issues
 */
async function fixLint() {
  console.log('   Running: pnpm lint:fix');
  execSync('pnpm lint:fix', {
    cwd: projectRoot,
    stdio: 'inherit',
  });
  console.log('   ✅ ESLint issues fixed');
}

/**
 * Apply auto-fix for a specific check
 */
export async function applyAutoFix(check) {
  const fixFn = FIX_IMPLEMENTATIONS[check.name];

  if (!fixFn) {
    throw new Error(`No auto-fix available for: ${check.name}`);
  }

  await fixFn();
}

/**
 * Get list of fixable checks
 */
export function getFixableChecks() {
  return Object.keys(FIX_IMPLEMENTATIONS).filter(key => FIX_IMPLEMENTATIONS[key] !== null);
}
