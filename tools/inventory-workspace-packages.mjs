#!/usr/bin/env node
/**
 * Workspace Package Inventory Scanner
 * Enumerates all workspace packages from pnpm-workspace.yaml and generates INVENTORY.json
 *
 * @file tools/inventory-workspace-packages.mjs
 */

import { readdirSync, statSync, readFileSync, writeFileSync } from 'fs';
import { join, resolve } from 'path';
import { fileURLToPath } from 'url';
import { createHash } from 'crypto';

const __dirname = fileURLToPath(import.meta.url).replace(/\/[^/]+$/, '');
const repoRoot = resolve(__dirname, '..');
const packagesDir = join(repoRoot, 'packages');

/**
 * Compute deterministic checksum of a file
 * @param {string} filePath - Path to file
 * @returns {string} Hex-encoded SHA256 checksum
 */
function computeChecksum(filePath) {
  try {
    const content = readFileSync(filePath, 'utf8');
    return createHash('sha256').update(content).digest('hex').slice(0, 16);
  } catch (err) {
    return 'UNKNOWN';
  }
}

/**
 * Scan a single package directory
 * @param {string} pkgDir - Path to package directory
 * @returns {Object | null} Package metadata or null if invalid
 */
function scanPackage(pkgDir) {
  try {
    const pkgJsonPath = join(pkgDir, 'package.json');
    const content = readFileSync(pkgJsonPath, 'utf8');
    const pkg = JSON.parse(content);

    const name = pkg.name || `@workspace/${pkgDir.split('/').pop()}`;
    const mainEntryPoint = pkg.main || pkg.exports?.['./']?.import || pkg.exports?.['./']?.require || 'index.js';
    const srcDir = join(pkgDir, 'src');
    let srcExists = false;
    try {
      srcExists = statSync(srcDir).isDirectory();
    } catch (e) {
      srcExists = false;
    }

    return {
      name,
      dir: pkgDir.replace(repoRoot + '/', ''),
      version: pkg.version || 'unknown',
      type: pkg.type || 'commonjs',
      entryPoint: mainEntryPoint,
      hasTests: readdirSync(pkgDir).includes('test') || readdirSync(pkgDir).includes('tests'),
      hasSrc: srcExists,
      description: pkg.description || '',
      keywords: pkg.keywords || [],
      checksum: computeChecksum(pkgJsonPath)
    };
  } catch (err) {
    return null;
  }
}

/**
 * Main inventory function
 * @returns {Object[]} Array of package metadata
 */
function inventoryWorkspace() {
  const packages = [];

  // Scan packages/ directory
  try {
    const dirs = readdirSync(packagesDir);
    for (const dir of dirs) {
      const pkgDir = join(packagesDir, dir);
      const stat = statSync(pkgDir);
      if (stat.isDirectory()) {
        const pkg = scanPackage(pkgDir);
        if (pkg) {
          packages.push(pkg);
        }
      }
    }
  } catch (err) {
    console.error(`Error scanning ${packagesDir}:`, err.message);
  }

  // Sort by name for determinism
  packages.sort((a, b) => a.name.localeCompare(b.name));

  return packages;
}

/**
 * Write inventory to INVENTORY.json
 * @param {Object[]} packages - Package metadata
 */
function writeInventory(packages) {
  const inventoryPath = join(repoRoot, 'AUTONOMIC_ALLPACKAGES', 'INVENTORY.json');
  const inventory = {
    timestamp: new Date().toISOString(),
    count: packages.length,
    packages
  };

  writeFileSync(inventoryPath, JSON.stringify(inventory, null, 2));
  console.log(`✅ Inventory written to ${inventoryPath}`);
  console.log(`   Total packages: ${packages.length}`);
}

// Execute
const packages = inventoryWorkspace();
writeInventory(packages);

// Export for use in other scripts
export { inventoryWorkspace, scanPackage, computeChecksum };
