#!/usr/bin/env node
/**
 * Lockfile Generator - Creates CONTRACTS.lock.json from contract inventory
 *
 * Generates a deterministic, versioned lockfile that captures API contracts
 * for enterprise governance and change detection.
 *
 * @module agent-2/lockfile-generator
 */

import { createHash } from 'node:crypto';
import { writeFile } from 'node:fs/promises';

/**
 * Generates a stable hash for a contract section
 * @param {any} data - Data to hash
 * @returns {string} SHA-256 hash
 */
export function generateHash(data) {
  const normalized = JSON.stringify(data, Object.keys(data).sort());
  return createHash('sha256').update(normalized).digest('hex').slice(0, 16);
}

/**
 * Sorts object keys recursively for deterministic output
 * @param {any} obj - Object to sort
 * @returns {any} Sorted object
 */
export function sortObjectKeys(obj) {
  if (obj === null || typeof obj !== 'object') {
    return obj;
  }

  if (Array.isArray(obj)) {
    return obj.map(sortObjectKeys);
  }

  const sorted = {};
  const keys = Object.keys(obj).sort();

  for (const key of keys) {
    sorted[key] = sortObjectKeys(obj[key]);
  }

  return sorted;
}

/**
 * Transforms contract inventory into lockfile format
 * @param {Object} inventory - Raw contract inventory from scanner
 * @returns {Object} Lockfile-formatted contracts
 */
export function transformToLockfileFormat(inventory) {
  const lockfile = {
    version: '1.0.0',
    generatedAt: inventory.scannedAt,
    generator: 'agent-2/contract-scanner.mjs',
    packages: {},
    metadata: {
      totalPackages: inventory.summary.totalPackages,
      totalExports: inventory.summary.totalExports,
      totalFunctions: inventory.summary.totalFunctions,
    },
  };

  // Process each package
  for (const [packageName, contract] of Object.entries(inventory.packages)) {
    const packageContract = {
      version: contract.version,
      description: contract.description,
      exports: {},
      errors: {
        codes: contract.errors.codes || [],
        patterns: contract.errors.patterns?.slice(0, 10) || [], // Limit patterns
      },
      logging: {
        fields: contract.logging.fields || [],
        levels: contract.logging.levels || [],
      },
      idRules: contract.idRules || {},
    };

    // Process exports
    for (const [exportName, exportData] of Object.entries(contract.exports)) {
      if (exportData.status === 'unreadable') {
        packageContract.exports[exportName] = {
          path: exportData.path,
          status: 'unreadable',
        };
        continue;
      }

      packageContract.exports[exportName] = {
        path: exportData.path,
        exports: sortObjectKeys(exportData.exports || []),
        functions: sortObjectKeys(exportData.functions || {}),
        reExports: sortObjectKeys(exportData.reExports || []),
      };

      // Generate hash for this export
      packageContract.exports[exportName].hash = generateHash(
        packageContract.exports[exportName]
      );
    }

    // Generate hash for entire package contract
    packageContract.hash = generateHash(packageContract);

    lockfile.packages[packageName] = packageContract;
  }

  // Sort packages alphabetically
  lockfile.packages = sortObjectKeys(lockfile.packages);

  // Generate overall lockfile hash
  lockfile.hash = generateHash(lockfile.packages);

  return lockfile;
}

/**
 * Generates the CONTRACTS.lock.json file
 * @param {Object} inventory - Contract inventory from scanner
 * @param {string} outputPath - Path to write lockfile
 * @returns {Promise<Object>} Generated lockfile
 */
export async function generateLockfile(inventory, outputPath) {
  const lockfile = transformToLockfileFormat(inventory);

  // Pretty-print with 2-space indentation for readability
  const content = JSON.stringify(lockfile, null, 2);

  await writeFile(outputPath, content, 'utf-8');

  return lockfile;
}

/**
 * Extracts DTOs (Data Transfer Objects) from package contracts
 * @param {Object} packageContract - Single package contract
 * @returns {Object} DTO schemas
 */
export function extractDTOs(packageContract) {
  const dtos = {
    request: {},
    response: {},
    schemas: [],
  };

  // Look for Zod schemas or type definitions
  for (const [exportName, exportData] of Object.entries(packageContract.exports)) {
    const schemaExports = (exportData.exports || []).filter(name =>
      name.endsWith('Schema') || name.endsWith('DTO')
    );

    dtos.schemas.push(...schemaExports);
  }

  return dtos;
}

/**
 * Generates a summary report of contracts
 * @param {Object} lockfile - Generated lockfile
 * @returns {string} Human-readable summary
 */
export function generateSummaryReport(lockfile) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('CONTRACT LOCKFILE SUMMARY');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Generated: ${lockfile.generatedAt}`);
  lines.push(`Version: ${lockfile.version}`);
  lines.push(`Hash: ${lockfile.hash}`);
  lines.push('');
  lines.push('TOTALS:');
  lines.push(`  Packages: ${lockfile.metadata.totalPackages}`);
  lines.push(`  Exports: ${lockfile.metadata.totalExports}`);
  lines.push(`  Functions: ${lockfile.metadata.totalFunctions}`);
  lines.push('');
  lines.push('PACKAGES:');

  const sortedPackages = Object.entries(lockfile.packages)
    .sort(([a], [b]) => a.localeCompare(b));

  for (const [name, pkg] of sortedPackages) {
    const exportCount = Object.keys(pkg.exports).length;
    const errorCount = pkg.errors.codes.length;
    lines.push(`  ${name}@${pkg.version}`);
    lines.push(`    Exports: ${exportCount}, Errors: ${errorCount}, Hash: ${pkg.hash}`);
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Main execution when run as script
 */
if (import.meta.url === `file://${process.argv[1]}`) {
  console.error('⚠️  This module should be used via contract-scanner.mjs');
  console.error('Usage: node agent-2/contract-scanner.mjs');
  process.exit(1);
}
