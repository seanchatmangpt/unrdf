/**
 * @file Publish Doctor Check
 * @module cli/commands/doctor/checks/publish
 */

import fs from 'node:fs';
import path from 'node:path';
import { execSync } from 'node:child_process';

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

function checkExports(pkgPath, pkg) {
  if (!pkg.exports) return { status: 'pass', actual: 'No exports defined', expected: 'Optional' };
  
  const missing = [];
  const traverseExports = (obj) => {
    if (typeof obj === 'string') {
      if (!fs.existsSync(path.join(pkgPath, obj))) {
        missing.push(obj);
      }
    } else if (typeof obj === 'object' && obj !== null) {
      for (const val of Object.values(obj)) {
        traverseExports(val);
      }
    }
  };

  traverseExports(pkg.exports);

  if (missing.length > 0) {
    return {
      status: 'fail',
      actual: `${missing.length} missing export files`,
      expected: 'All export paths exist',
      critical: true,
      fix: `Ensure build artifacts exist for: ${missing.join(', ')}`
    };
  }
  return { status: 'pass', actual: 'All valid', expected: 'All export paths exist' };
}

function checkPublishConfig(pkg) {
  if (!pkg.publishConfig || pkg.publishConfig.access !== 'public') {
    return {
      status: 'warn',
      actual: 'Missing or private',
      expected: 'access: "public"',
      fix: 'Add `"publishConfig": { "access": "public" }` to prevent NPM 402 Payment Required errors for scoped packages.'
    };
  }
  return { status: 'pass', actual: 'Public access configured', expected: 'access: "public"' };
}

function checkFilesArray(pkg) {
  if (!pkg.files || !Array.isArray(pkg.files) || pkg.files.length === 0) {
    return {
      status: 'warn',
      actual: 'Missing or empty',
      expected: 'Explicit files array',
      fix: 'Define a strict `"files"` array to prevent publishing tests, benchmarks, or Rust source directories.'
    };
  }
  return { status: 'pass', actual: 'Defined', expected: 'Explicit files array' };
}

function checkTypes(pkgPath, pkg) {
  if (!pkg.types) return { status: 'pass', actual: 'No types defined', expected: 'Optional' };
  
  if (!fs.existsSync(path.join(pkgPath, pkg.types))) {
    return {
      status: 'fail',
      actual: 'Missing',
      expected: `Exists at ${pkg.types}`,
      critical: true,
      fix: `Run tsc --emitDeclarationOnly to generate ${pkg.types}`
    };
  }
  return { status: 'pass', actual: 'Exists', expected: 'Types file exists' };
}

function checkEngines(pkg) {
  const expectedNode = ">=18.0.0";
  if (!pkg.engines || pkg.engines.node !== expectedNode) {
    return {
      status: 'warn',
      actual: pkg.engines?.node || 'Missing',
      expected: expectedNode,
      fix: `Set "engines": { "node": "${expectedNode}" } for ecosystem consistency.`
    };
  }
  return { status: 'pass', actual: pkg.engines.node, expected: expectedNode };
}

function checkGitDirty(pkgPath) {
  try {
    const status = execSync('git status --porcelain', { cwd: pkgPath, encoding: 'utf8' }).trim();
    if (status.length > 0) {
      return {
        status: 'fail',
        actual: 'Dirty working tree',
        expected: 'Clean working tree',
        critical: true,
        fix: 'Commit or stash changes before publishing to ensure repeatable builds.'
      };
    }
  } catch (e) {
    return { status: 'warn', actual: 'Git execution failed', expected: 'Clean working tree', fix: 'Ensure git is installed and repository is accessible.' };
  }
  return { status: 'pass', actual: 'Clean', expected: 'Clean working tree' };
}

function checkNPMTokenPermissions(pkg) {
  try {
    // Check if we are authenticated
    const whoami = execSync('npm whoami', { stdio: 'pipe', encoding: 'utf8' }).trim();
    
    // For scoped packages, ensure the token isn't blindly failing 404s due to Granular Access Token bugs
    if (pkg.name && pkg.name.startsWith('@')) {
      const scope = pkg.name.split('/')[0];
      try {
        // Ping the registry for the specific package or scope to see if we get a 401/403/404 that indicates a permission issue
        // This is a heuristic: if we are authenticated but the registry rejects an info request for our own scope
        execSync(`npm access ls-packages ${scope} --json`, { stdio: 'pipe', encoding: 'utf8' });
      } catch (e) {
        // If npm access fails, it might be a Granular Token bug or missing org
        return {
          status: 'warn',
          actual: `Authenticated as ${whoami}, but scope access check failed`,
          expected: `Write access to ${scope}`,
          fix: `If you are using a Granular Access Token and receive 404s on publish, recreate it as a Classic Automation Token, or ensure the scope '${scope}' is explicitly granted.`
        };
      }
    }

    return { status: 'pass', actual: `Authenticated (${whoami})`, expected: 'Authenticated' };
  } catch (e) {
    return {
      status: 'warn',
      actual: 'Not authenticated or invalid token',
      expected: 'Valid NPM_TOKEN or npm login',
      fix: 'Run `npm login` or set a valid NPM_TOKEN environment variable.'
    };
  }
}

/**
 * Validates package metadata and readiness for publication
 * @param {string} pkgPath - Path to package directory
 * @returns {Promise<Object>} Health check result category
 */
export async function checkPublishReadiness(pkgPath = process.cwd()) {
  const checks = [
    { name: 'Package Manifest', ...checkPackageJson(pkgPath) },
    { name: 'Git Working Tree', ...checkGitDirty(pkgPath) }
  ];

  const pkgJsonPath = path.join(pkgPath, 'package.json');
  if (fs.existsSync(pkgJsonPath)) {
    try {
      const pkg = JSON.parse(fs.readFileSync(pkgJsonPath, 'utf8'));
      checks.push({ name: 'Version Strategy', ...checkSemver(pkg) });
      checks.push({ name: 'Dependency Resolution', ...checkWorkspaceProtocols(pkg) });
      checks.push({ name: 'Export Integrity', ...checkExports(pkgPath, pkg) });
      checks.push({ name: 'Artifact Readiness', ...checkMainEntry(pkgPath, pkg) });
      checks.push({ name: 'TypeScript Definitions', ...checkTypes(pkgPath, pkg) });
      checks.push({ name: 'Leakage Prevention (Files Array)', ...checkFilesArray(pkg) });
      checks.push({ name: 'Scoped Package Access', ...checkPublishConfig(pkg) });
      checks.push({ name: 'Engine Consistency', ...checkEngines(pkg) });
      checks.push({ name: 'Package Metadata', ...checkMetadata(pkg) });
      checks.push({ name: 'NPM Registry Authentication', ...checkNPMTokenPermissions(pkg) });
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
