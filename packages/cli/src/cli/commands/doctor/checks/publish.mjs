/**
 * @file Publish Doctor Check
 * @module cli/commands/doctor/checks/publish
 */

import fs from 'node:fs';
import path from 'node:path';

function checkPackageJson(pkgPath) {
  const pkgJsonPath = path.join(pkgPath, 'package.json');
  if (!fs.existsSync(pkgJsonPath)) {
    return {
      status: 'fail',
      actual: 'Missing',
      expected: 'Exists',
      critical: true,
      fix: 'Create a package.json file.'
    };
  }
  return { status: 'pass', actual: 'Exists', expected: 'Exists' };
}

function checkSemver(pkg) {
  if (pkg.version === '0.0.0-agnostic' || pkg.version === '[VERSION]' || !pkg.version) {
    return {
      status: 'fail',
      actual: pkg.version || 'Missing',
      expected: 'Valid semver (e.g. 1.0.0)',
      critical: true,
      fix: 'Update version in package.json to a publishable semver.'
    };
  }
  return { status: 'pass', actual: pkg.version, expected: 'Valid semver' };
}

function checkWorkspaceProtocols(pkg) {
  const issues = [];
  const sections = ['dependencies', 'devDependencies', 'peerDependencies'];
  for (const section of sections) {
    if (pkg[section]) {
      for (const [dep, version] of Object.entries(pkg[section])) {
        if (version === 'workspace:*') {
          issues.push(dep);
        }
      }
    }
  }
  
  if (issues.length > 0) {
    return {
      status: 'fail',
      actual: `${issues.length} workspace protocols found`,
      expected: '0 workspace protocols',
      critical: true,
      fix: `Resolve 'workspace:*' dependencies before publishing: ${issues.join(', ')}`
    };
  }
  return { status: 'pass', actual: '0 workspace protocols', expected: '0 workspace protocols' };
}

function checkMetadata(pkg) {
  const missing = [];
  if (!pkg.license) missing.push('license');
  if (!pkg.repository) missing.push('repository');
  if (!pkg.description) missing.push('description');
  
  if (missing.length > 0) {
    return {
      status: 'warn',
      actual: `Missing: ${missing.join(', ')}`,
      expected: 'Complete metadata',
      fix: `Add ${missing.join(', ')} to package.json`
    };
  }
  return { status: 'pass', actual: 'Complete', expected: 'Complete metadata' };
}

function checkMainEntry(pkgPath, pkg) {
  if (!pkg.main) return { status: 'pass', actual: 'No main defined', expected: 'Optional' };
  
  const mainPath = path.join(pkgPath, pkg.main);
  if (!fs.existsSync(mainPath)) {
    return {
      status: 'fail',
      actual: 'Missing',
      expected: `File exists at ${pkg.main}`,
      critical: true,
      fix: `Ensure build script runs and generates ${pkg.main}`
    };
  }
  return { status: 'pass', actual: 'Exists', expected: 'File exists' };
}

/**
 * Validates package metadata and readiness for publication
 * @param {string} pkgPath - Path to package directory
 * @returns {Promise<Object>} Health check result category
 */
export async function checkPublishReadiness(pkgPath = process.cwd()) {
  const checks = [
    { name: 'Package Manifest', ...checkPackageJson(pkgPath) }
  ];

  const pkgJsonPath = path.join(pkgPath, 'package.json');
  if (fs.existsSync(pkgJsonPath)) {
    try {
      const pkg = JSON.parse(fs.readFileSync(pkgJsonPath, 'utf8'));
      checks.push({ name: 'Version Strategy', ...checkSemver(pkg) });
      checks.push({ name: 'Dependency Resolution', ...checkWorkspaceProtocols(pkg) });
      checks.push({ name: 'Package Metadata', ...checkMetadata(pkg) });
      checks.push({ name: 'Artifact Readiness', ...checkMainEntry(pkgPath, pkg) });
    } catch (e) {
      checks.push({
        name: 'Manifest Parsing',
        status: 'fail',
        actual: 'Parse Error',
        expected: 'Valid JSON',
        critical: true,
        fix: `Fix JSON syntax in package.json: ${e.message}`
      });
    }
  }

  return {
    category: 'Publication Readiness',
    checks
  };
}
