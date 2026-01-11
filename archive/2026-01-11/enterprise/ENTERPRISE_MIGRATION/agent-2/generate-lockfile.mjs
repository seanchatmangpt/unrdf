#!/usr/bin/env node
/**
 * Generate CONTRACTS.lock.json - Main entry point
 *
 * Scans all packages and generates the contract lockfile
 */

import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { scanAllPackages } from './contract-scanner.mjs';
import { generateLockfile, generateSummaryReport } from './lockfile-generator.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const LOCKFILE_PATH = join(__dirname, '../CONTRACTS.lock.json');

async function main() {
  console.log('🔍 Scanning UNRDF packages...\n');

  // Scan all packages
  const inventory = await scanAllPackages();

  console.log(`\n📦 Found ${inventory.summary.totalPackages} packages\n`);

  // Generate lockfile
  console.log('🔒 Generating CONTRACTS.lock.json...\n');
  const lockfile = await generateLockfile(inventory, LOCKFILE_PATH);

  // Generate and display summary
  const summary = generateSummaryReport(lockfile);
  console.log(summary);

  console.log(`\n✅ Lockfile written to: ${LOCKFILE_PATH}`);
  console.log(`   Overall Hash: ${lockfile.hash}`);
}

main().catch(error => {
  console.error('❌ Error:', error);
  process.exit(1);
});
