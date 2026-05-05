/**
 * @file Publish Doctor Check
 * @module cli/commands/doctor/checks/publish
 */

import fs from 'fs';
import path from 'path';

/**
 * Validates package metadata and readiness for publication
 * @param {string} pkgPath - Path to package directory
 * @returns {Promise<Object>} Health check result
 */
export async function checkPublishReadiness(pkgPath) {
  const pkgJsonPath = path.join(pkgPath, 'package.json');
  if (!fs.existsSync(pkgJsonPath)) {
    return { success: false, message: 'package.json missing' };
  }

  const pkg = JSON.parse(fs.readFileSync(pkgJsonPath, 'utf8'));
  const issues = [];

  // Check versioning
  if (pkg.version === '0.0.0-agnostic') {
    issues.push('Package version is agnostic; must be set to a valid semver for publication.');
  }

  // Check workspace protocols
  const sections = ['dependencies', 'devDependencies', 'peerDependencies'];
  for (const section of sections) {
    if (pkg[section]) {
      for (const [dep, version] of Object.entries(pkg[section])) {
        if (version === 'workspace:*') {
          issues.push(`Internal dependency ${dep} still using 'workspace:*' protocol.`);
        }
      }
    }
  }

  // Check main entry point
  if (pkg.main && !fs.existsSync(path.join(pkgPath, pkg.main))) {
      issues.push(`Main entry point '${pkg.main}' does not exist.`);
  }

  return {
    success: issues.length === 0,
    issues,
    package: pkg.name
  };
}
