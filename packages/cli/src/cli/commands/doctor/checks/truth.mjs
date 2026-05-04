/**
 * @file Epistemic Truth Checks
 * @module cli/commands/doctor/checks/truth
 *
 * @description
 * Checks for "lies" in the system state:
 * - Version consistency (all packages match root version)
 * - Build drift (src files newer than dist artifacts)
 * - Configuration alignment
 */

import { readFileSync, statSync, existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { glob } from 'glob';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Get latest modification time for a set of files
 */
function getLatestMtime(files) {
  let latest = 0;
  for (const file of files) {
    try {
      const stats = statSync(join(projectRoot, file));
      if (stats.mtimeMs > latest) {
        latest = stats.mtimeMs;
      }
    } catch {
      // Ignore missing files
    }
  }
  return latest;
}

/**
 * Check if workspace versions are truthful (consistent)
 */
function checkVersionTruth() {
  try {
    const rootPkgPath = join(projectRoot, 'package.json');
    if (!existsSync(rootPkgPath)) {
      return {
        status: 'warn',
        actual: 'Root package.json not found',
        expected: 'Root package.json exists',
      };
    }

    const rootPkg = JSON.parse(readFileSync(rootPkgPath, 'utf-8'));
    const rootVersion = rootPkg.version;

    const pkgFiles = glob.sync('packages/*/package.json', { cwd: projectRoot });
    const violations = [];

    for (const file of pkgFiles) {
      const pkg = JSON.parse(readFileSync(join(projectRoot, file), 'utf-8'));
      if (pkg.version && pkg.version !== rootVersion) {
        violations.push(`${pkg.name}: ${pkg.version} (expected ${rootVersion})`);
      }
    }

    if (violations.length === 0) {
      return {
        status: 'pass',
        actual: `All packages match root version (${rootVersion})`,
        expected: 'Consistent versions across workspace',
      };
    }

    return {
      status: 'fail',
      actual: `${violations.length} packages have mismatched versions`,
      expected: `All packages match root version (${rootVersion})`,
      violations: violations.slice(0, 10),
      fix: 'Run the version upgrade script to synchronize versions',
    };
  } catch (error) {
    return {
      status: 'error',
      actual: `Version check failed: ${error.message}`,
      expected: 'Consistent versions',
    };
  }
}

/**
 * Check if build artifacts are truthful (not stale)
 */
function checkBuildTruth() {
  try {
    const packages = glob.sync('packages/*', { cwd: projectRoot });
    const violations = [];

    for (const pkg of packages) {
      const srcDir = join(projectRoot, pkg, 'src');
      const distDir = join(projectRoot, pkg, 'dist');

      if (existsSync(srcDir) && existsSync(distDir)) {
        const srcFiles = glob.sync(`${pkg}/src/**/*`, { cwd: projectRoot, nodir: true });
        const distFiles = glob.sync(`${pkg}/dist/**/*`, { cwd: projectRoot, nodir: true });

        if (srcFiles.length > 0 && distFiles.length > 0) {
          const latestSrc = getLatestMtime(srcFiles);
          const latestDist = getLatestMtime(distFiles);

          // If src is newer than dist, the build is stale
          if (latestSrc > latestDist) {
            violations.push(`${pkg} (src modified after dist)`);
          }
        }
      }
    }

    if (violations.length === 0) {
      return {
        status: 'pass',
        actual: 'No stale build artifacts detected',
        expected: 'Build artifacts up to date with source',
      };
    }

    return {
      status: 'fail',
      actual: `${violations.length} packages have stale builds`,
      expected: 'Build artifacts up to date with source',
      violations: violations.slice(0, 10),
      fix: 'Run: pnpm build',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Build truth check failed: ${error.message}`,
      expected: 'Build artifacts up to date with source',
    };
  }
}

/**
 * Export all truth/epistemic checks
 */
export async function checkTruth() {
  return {
    category: 'Epistemic Truth',
    checks: [
      {
        name: 'Version Consistency',
        ...checkVersionTruth(),
      },
      {
        name: 'Build Artifact Truth',
        ...checkBuildTruth(),
      },
    ],
  };
}
