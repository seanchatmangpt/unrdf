#!/usr/bin/env node
/**
 * All-Packages Hard Gate Validator
 * Verifies that EVERY workspace package has been imported and exercised
 *
 * @file tools/verify-all-packages-used.mjs
 */

import { readFileSync, writeFileSync } from 'fs';
import { join, resolve } from 'path';
import { fileURLToPath } from 'url';
import { createHash } from 'crypto';

const __dirname = fileURLToPath(import.meta.url).replace(/\/[^/]+$/, '');
const repoRoot = resolve(__dirname, '..');

/**
 * Load inventory from INVENTORY.json
 * @returns {Object[]} Array of package metadata
 */
function loadInventory() {
  const inventoryPath = join(repoRoot, 'AUTONOMIC_ALLPACKAGES', 'INVENTORY.json');
  try {
    const content = readFileSync(inventoryPath, 'utf8');
    const data = JSON.parse(content);
    return data.packages || [];
  } catch (err) {
    console.error(`❌ Failed to load inventory: ${err.message}`);
    process.exit(1);
  }
}

/**
 * Load registry snapshot from AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json
 * @returns {Object} Registry snapshot or null
 */
function loadRegistrySnapshot() {
  const snapshotPath = join(repoRoot, 'AUTONOMIC_ALLPACKAGES', 'REGISTRY_SNAPSHOT.json');
  try {
    const content = readFileSync(snapshotPath, 'utf8');
    return JSON.parse(content);
  } catch (err) {
    console.log(`⚠️  Registry snapshot not found (expected after demo run)`);
    return null;
  }
}

/**
 * Verify all packages are registered
 * @param {Object[]} inventory - Inventory packages
 * @param {Object} snapshot - Registry snapshot
 * @returns {Object} Validation result
 */
function verifyAllPackagesUsed(inventory, snapshot) {
  if (!snapshot) {
    return {
      valid: false,
      error: 'Registry snapshot not found',
      message: '❌ Run demo first: node AUTONOMIC_ALLPACKAGES/demo.mjs'
    };
  }

  const registeredNames = new Set(snapshot.usages.map(u => u.packageName));
  const inventoryNames = inventory.map(p => p.name);
  const missing = inventoryNames.filter(name => !registeredNames.has(name));
  const extra = Array.from(registeredNames).filter(name => !inventoryNames.includes(name));

  const valid = missing.length === 0 && extra.length === 0;

  return {
    valid,
    inventory: inventoryNames.length,
    registered: registeredNames.size,
    missing,
    extra,
    matchRate: ((registeredNames.size / inventoryNames.length) * 100).toFixed(1),
    message: valid
      ? `✅ All ${inventoryNames.length} packages successfully registered`
      : `❌ Package mismatch: ${missing.length} missing, ${extra.length} extra`
  };
}

/**
 * Generate verification report
 * @param {Object[]} inventory - Inventory packages
 * @param {Object} snapshot - Registry snapshot
 * @returns {Object} Report
 */
function generateReport(inventory, snapshot) {
  const result = verifyAllPackagesUsed(inventory, snapshot);

  const report = {
    timestamp: new Date().toISOString(),
    status: result.valid ? 'PASS' : 'FAIL',
    summary: {
      inventory: result.inventory,
      registered: result.registered,
      missing: result.missing.length,
      extra: result.extra.length,
      matchRate: result.matchRate + '%'
    },
    details: {
      message: result.message,
      missingPackages: result.missing,
      extraPackages: result.extra
    }
  };

  // Add snapshot info if available
  if (snapshot) {
    report.snapshotHash = snapshot.snapshotHash;
    report.usages = snapshot.usages.map(u => ({
      package: u.packageName,
      feature: u.feature,
      operation: u.operation || '(no description)',
      proofHash: u.proofHash
    }));
  }

  return report;
}

/**
 * Main verification function
 */
function main() {
  console.log('\n🔐 All-Packages Hard Gate Validator\n');

  const inventory = loadInventory();
  const snapshot = loadRegistrySnapshot();

  const report = generateReport(inventory, snapshot);
  const valid = report.status === 'PASS';

  // Print report
  console.log(`Inventory: ${report.summary.inventory} packages`);
  console.log(`Registered: ${report.summary.registered} packages`);
  console.log(`Match rate: ${report.summary.matchRate}\n`);

  if (report.details.missingPackages.length > 0) {
    console.log(`❌ Missing (${report.details.missingPackages.length}):`);
    report.details.missingPackages.forEach(pkg => console.log(`   - ${pkg}`));
  }

  if (report.details.extraPackages && report.details.extraPackages.length > 0) {
    console.log(`⚠️  Extra (${report.details.extraPackages.length}):`);
    report.details.extraPackages.forEach(pkg => console.log(`   - ${pkg}`));
  }

  console.log(`\n${report.details.message}\n`);

  // Write report
  const reportPath = join(repoRoot, 'AUTONOMIC_ALLPACKAGES', 'VERIFICATION_REPORT.json');
  writeFileSync(reportPath, JSON.stringify(report, null, 2));
  console.log(`📊 Report written to AUTONOMIC_ALLPACKAGES/VERIFICATION_REPORT.json\n`);

  // Exit code
  if (!valid) {
    console.log('🛑 HARD GATE FAILED: Not all packages are used\n');
    process.exit(1);
  }

  console.log('✅ HARD GATE PASSED: All packages successfully registered\n');
  process.exit(0);
}

// Execute
main();
