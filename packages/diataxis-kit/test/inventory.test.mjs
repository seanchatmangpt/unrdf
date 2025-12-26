/**
 * @file Inventory discovery tests
 * @description Validates package discovery and validation functions
 */

import assert from 'node:assert/strict';
import { resolve } from 'node:path';
import { discoverPackages, validatePackageEntry } from '../src/inventory.mjs';
import { existsSync } from 'node:fs';

let passCount = 0;
let failCount = 0;

/**
 * Test runner
 * @param {string} name - Test name
 * @param {Function} fn - Async test function
 */
async function test(name, fn) {
  try {
    await fn();
    console.log(`✅ ${name}`);
    passCount++;
  } catch (err) {
    console.log(`❌ ${name}: ${err.message}`);
    if (err.stack && process.env.VERBOSE) {
      console.log(err.stack);
    }
    failCount++;
  }
}

/**
 * Test: discoverPackages finds at least 42 packages
 */
async function testDiscoverPackagesCount() {
  const workspaceRoot = resolve(process.cwd(), '../..');
  const packages = await discoverPackages(workspaceRoot);

  if (packages.length < 42) {
    throw new Error(`Expected >= 42 packages, found ${packages.length}`);
  }

  console.log(`  Found ${packages.length} packages`);
}

/**
 * Test: All packages have required fields
 */
async function testRequiredFields() {
  const workspaceRoot = resolve(process.cwd(), '../..');
  const packages = await discoverPackages(workspaceRoot);

  const requiredFields = ['name', 'dir', 'version', 'description'];
  const missingFields = [];

  for (const pkg of packages) {
    for (const field of requiredFields) {
      if (typeof pkg[field] !== 'string') {
        missingFields.push(`${pkg.name || 'unknown'}: missing or invalid field '${field}'`);
      }
    }
  }

  if (missingFields.length > 0) {
    throw new Error(`Missing fields:\n${missingFields.join('\n')}`);
  }

  console.log(`  Verified required fields for ${packages.length} packages`);
}

/**
 * Test: Package names are unique
 */
async function testUniqueNames() {
  const workspaceRoot = resolve(process.cwd(), '../..');
  const packages = await discoverPackages(workspaceRoot);

  const nameSet = new Set();
  const duplicates = [];

  for (const pkg of packages) {
    if (nameSet.has(pkg.name)) {
      duplicates.push(pkg.name);
    }
    nameSet.add(pkg.name);
  }

  if (duplicates.length > 0) {
    throw new Error(`Duplicate package names: ${duplicates.join(', ')}`);
  }

  console.log(`  Verified ${packages.length} unique package names`);
}

/**
 * Test: All package directories exist and are readable
 */
async function testDirectoriesExist() {
  const workspaceRoot = resolve(process.cwd(), '../..');
  const packages = await discoverPackages(workspaceRoot);

  const missingDirs = [];

  for (const pkg of packages) {
    if (!existsSync(pkg.dir)) {
      missingDirs.push(`${pkg.name}: directory not found at ${pkg.dir}`);
    }
  }

  if (missingDirs.length > 0) {
    throw new Error(`Missing directories:\n${missingDirs.join('\n')}`);
  }

  console.log(`  Verified all ${packages.length} directories exist`);
}

/**
 * Test: validatePackageEntry works correctly
 */
async function testValidatePackageEntry() {
  const workspaceRoot = resolve(process.cwd(), '../..');
  const packages = await discoverPackages(workspaceRoot);

  const validationErrors = [];

  for (const pkg of packages) {
    const result = validatePackageEntry(pkg);
    if (!result.valid) {
      validationErrors.push(`${pkg.name}: ${result.errors.join(', ')}`);
    }
  }

  if (validationErrors.length > 0) {
    throw new Error(`Validation errors:\n${validationErrors.join('\n')}`);
  }

  console.log(`  Validated ${packages.length} package entries`);
}

/**
 * Test: Invalid entries are rejected
 */
async function testValidateRejectsInvalid() {
  const invalidEntries = [
    null,
    undefined,
    {},
    { name: 'test' }, // Missing required fields
    { name: 'test', dir: '/path', version: 123 }, // Wrong type
    { name: 'test', dir: '/path', version: '1.0.0', description: 'test', exports: null }, // Invalid exports
  ];

  for (const entry of invalidEntries) {
    const result = validatePackageEntry(entry);
    if (result.valid) {
      throw new Error(`Expected validation to fail for: ${JSON.stringify(entry)}`);
    }
  }

  console.log(`  Verified validation rejects ${invalidEntries.length} invalid entries`);
}

/**
 * Test: Packages are sorted by name
 */
async function testPackagesSorted() {
  const workspaceRoot = resolve(process.cwd(), '../..');
  const packages = await discoverPackages(workspaceRoot);

  const names = packages.map(p => p.name);
  const sortedNames = [...names].sort((a, b) => a.localeCompare(b));

  for (let i = 0; i < names.length; i++) {
    if (names[i] !== sortedNames[i]) {
      throw new Error(`Packages not sorted: ${names[i]} at index ${i}, expected ${sortedNames[i]}`);
    }
  }

  console.log(`  Verified ${packages.length} packages sorted alphabetically`);
}

// Run all tests
async function runTests() {
  console.log('Running inventory tests...\n');

  await test('inventory: discovers >= 42 packages', testDiscoverPackagesCount);
  await test('inventory: all packages have required fields', testRequiredFields);
  await test('inventory: package names are unique', testUniqueNames);
  await test('inventory: all directories exist', testDirectoriesExist);
  await test('inventory: validatePackageEntry accepts valid entries', testValidatePackageEntry);
  await test('inventory: validatePackageEntry rejects invalid entries', testValidateRejectsInvalid);
  await test('inventory: packages sorted by name', testPackagesSorted);

  console.log(`\n${passCount}/${passCount + failCount} tests passed`);
  process.exit(failCount > 0 ? 1 : 0);
}

runTests();
