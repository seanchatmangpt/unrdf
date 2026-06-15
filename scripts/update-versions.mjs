#!/usr/bin/env node
import { readFileSync, writeFileSync } from 'fs';
import { execSync } from 'child_process';
import path from 'path';

// Get the version from the command-line arguments
const newVersion = process.argv[2];
if (!newVersion) {
  console.error('Usage: node scripts/update-versions.mjs <new-version>');
  process.exit(1);
}

// Validate version format
if (!/^[0-9]+\.[0-9]+\.[0-9]+(-(alpha|beta|rc)\.[0-9]+)?$/.test(newVersion)) {
  console.error(`Error: Invalid version format "${newVersion}". Expected X.Y.Z or X.Y.Z-{alpha|beta|rc}.N`);
  process.exit(1);
}

// 1. Update root package.json
try {
  const rootPkgPath = path.resolve('package.json');
  const rootPkg = JSON.parse(readFileSync(rootPkgPath, 'utf8'));
  const oldRootVersion = rootPkg.version;
  rootPkg.version = newVersion;
  writeFileSync(rootPkgPath, JSON.stringify(rootPkg, null, 2) + '\n', 'utf8');
  console.log(`✓ root package.json: ${oldRootVersion} → ${newVersion}`);
} catch (err) {
  console.error('Failed to update root package.json:', err.message);
}

// 2. Get all workspace packages using pnpm
let packages = [];
try {
  const output = execSync('pnpm list -r --depth -1 --json', { encoding: 'utf8' });
  packages = JSON.parse(output);
} catch (err) {
  console.error('Failed to retrieve workspace packages via pnpm:', err.message);
  process.exit(1);
}

const workspacePackageNames = new Set(packages.map(p => p.name));
console.log(`Found ${packages.length} workspace packages to update\n`);

const rootPath = path.resolve('.');

for (const pkgInfo of packages) {
  const resolvedPkgPath = path.resolve(pkgInfo.path);
  // Skip the root workspace package itself
  if (resolvedPkgPath === rootPath) {
    continue;
  }

  const pkgJsonPath = path.join(resolvedPkgPath, 'package.json');
  try {
    const content = readFileSync(pkgJsonPath, 'utf8');
    const pkg = JSON.parse(content);

    const oldVersion = pkg.version || 'none';
    pkg.version = newVersion;

    const isPrivate = pkg.private === true;

    // Ensure publishConfig is set or removed correctly
    if (isPrivate) {
      delete pkg.publishConfig;
    } else {
      if (!pkg.publishConfig || pkg.publishConfig.access !== 'public') {
        pkg.publishConfig = { ...pkg.publishConfig, access: 'public' };
      }
    }

    // Update hardcoded cross-dependencies to workspace packages
    for (const key of ['dependencies', 'devDependencies', 'peerDependencies']) {
      if (pkg[key]) {
        for (const depName of Object.keys(pkg[key])) {
          if (workspacePackageNames.has(depName)) {
            const depVal = pkg[key][depName];
            // If it's a specific version (not workspace:* or similar), bump it
            if (/^\d/.test(depVal)) {
              pkg[key][depName] = newVersion;
            }
          }
        }
      }
    }

    // Write back with pretty formatting
    writeFileSync(pkgJsonPath, JSON.stringify(pkg, null, 2) + '\n', 'utf8');

    const status = isPrivate ? 'PRIVATE' : 'PUBLIC';
    console.log(`✓ ${pkg.name}: ${oldVersion} → ${newVersion} [${status}]`);
  } catch (error) {
    console.error(`✗ Failed to update ${pkgJsonPath}:`, error.message);
  }
}

console.log(`\n✓ All packages updated to v${newVersion}`);
