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
import { copyFileSync, existsSync, readFileSync, writeFileSync } from 'node:fs';
import { glob } from 'glob';

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
  'Environment variables': fixEnvVars,
  'N3 import violations': fixN3Imports,
  'Skipped tests': fixSkippedTests,
  // Manual fixes that cannot be automated
  'Node.js version': null,
  'pnpm version': null,
  'Required tools': null,
  'Daemon status': null,
  'MCP server status': null,
  'RDF store': null,
  'Port availability': null,
  'Disk space': null,
  'Test coverage': null,
  'File size violations': null,
  'TypeScript contamination': null,
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
 * Fix missing environment variables
 */
async function fixEnvVars() {
  const examplePath = join(projectRoot, '.env.example');
  const localPath = join(projectRoot, '.env.local');

  if (existsSync(examplePath) && !existsSync(localPath)) {
    console.log('   Copying .env.example to .env.local');
    copyFileSync(examplePath, localPath);
    console.log('   ✅ .env.local created (please configure secrets manually)');
  } else {
    console.log('   ⚠️  Manual configuration of .env.local required');
  }
}

/**
 * Fix N3 import violations
 */
async function fixN3Imports() {
  console.log('   Searching for N3 import violations...');
  const files = glob.sync('packages/**/*.mjs', {
    cwd: projectRoot,
    ignore: ['**/node_modules/**', '**/n3-justified-only.mjs'],
  });

  let fixedCount = 0;
  for (const file of files) {
    const filePath = join(projectRoot, file);
    const content = readFileSync(filePath, 'utf-8');

    const hasN3Import = /^import\s+.*from\s+['"]n3['"]/m.test(content) || /^const\s+.*=\s+require\(['"]n3['"]\)/m.test(content);

    if (hasN3Import) {
      const updated = content
        .replace(/import\s+\{\s*([^}]+)\s*\}\s+from\s+['"]n3['"]/g, "import { $1 } from '@unrdf/core/rdf/n3-justified-only.mjs'")
        .replace(/import\s+N3\s+from\s+['"]n3['"]/g, "import * as N3 from '@unrdf/core/rdf/n3-justified-only.mjs'");

      if (updated !== content) {
        writeFileSync(filePath, updated, 'utf-8');
        fixedCount++;
      }
    }
  }

  console.log(`   ✅ Fixed N3 imports in ${fixedCount} file(s)`);
}

/**
 * Fix skipped tests
 */
async function fixSkippedTests() {
  console.log('   Unskipping tests...');
  const testFiles = glob.sync('**/*.{test.mjs,test.ts,test.js}', {
    cwd: projectRoot,
    ignore: ['**/node_modules/**', '**/vendors/**'],
  });

  let fixedCount = 0;
  for (const file of testFiles) {
    const filePath = join(projectRoot, file);
    const content = readFileSync(filePath, 'utf-8');

    const updated = content
      .replace(/\bdescribe\.skip\(/g, 'describe(')
      .replace(/\bit\.skip\(/g, 'it(')
      .replace(/\btest\.skip\(/g, 'test(')
      .replace(/\bxit\(/g, 'it(');

    if (updated !== content) {
      writeFileSync(filePath, updated, 'utf-8');
      fixedCount++;
    }
  }

  console.log(`   ✅ Unskipped tests in ${fixedCount} file(s)`);
}

/**
 * Apply auto-fix for a specific check
 * @param {Object} check - Check result object
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
 * @returns {string[]} List of check names
 */
export function getFixableChecks() {
  return Object.keys(FIX_IMPLEMENTATIONS).filter(key => FIX_IMPLEMENTATIONS[key] !== null);
}
