/**
 * Autonomic All-Packages Usage Registry
 * Runtime registration of package exercises with deterministic proof strings
 *
 * @file AUTONOMIC_ALLPACKAGES/usage-registry.mjs
 */

import { createHash } from 'crypto';

/**
 * Global usage registry
 * @type {Map<string, Object>}
 */
const usageRegistry = new Map();

/**
 * Register a package as "used" with proof of exercise
 *
 * @param {string} packageName - Full package name (e.g., "@unrdf/core")
 * @param {Object} proof - Proof object
 * @param {string} proof.feature - Feature name (e.g., "createStore", "executeQuery")
 * @param {string} proof.file - Source file where exercise occurs
 * @param {string | number | Object} proof.result - Result of exercise (serializable)
 * @param {string} [proof.operation] - Human-readable operation description
 * @returns {string} Deterministic proof hash
 *
 * @example
 * registerUsage("@unrdf/core", {
 *   feature: "executeQuery",
 *   file: "agents/agent-4.mjs",
 *   result: { bindingCount: 5 },
 *   operation: "SPARQL SELECT query execution"
 * })
 */
export function registerUsage(packageName, proof) {
  // Validate input
  if (typeof packageName !== 'string' || !packageName.length) {
    throw new Error(`Invalid package name: ${packageName}`);
  }
  if (!proof || typeof proof !== 'object') {
    throw new Error(`Invalid proof object for ${packageName}`);
  }
  if (!proof.feature || !proof.file || proof.result === undefined) {
    throw new Error(`Proof missing required fields (feature, file, result) for ${packageName}`);
  }

  // Compute deterministic proof hash
  const proofString = JSON.stringify({
    feature: proof.feature,
    file: proof.file,
    operation: proof.operation || '',
    result: proof.result
  }, Object.keys(proof).sort()); // Sorted keys for determinism

  const proofHash = createHash('sha256').update(proofString).digest('hex').slice(0, 16);

  // Record usage
  const entry = {
    packageName,
    ...proof,
    proofHash,
    timestamp: process.env.DETERMINISTIC ? 0 : Date.now(),
    registeredAt: process.env.DETERMINISTIC ? '2025-12-26T00:00:00Z' : new Date().toISOString()
  };

  usageRegistry.set(packageName, entry);

  return proofHash;
}

/**
 * Get all registered usage entries
 * @returns {Object[]} Array of registered usages
 */
export function getAllUsages() {
  return Array.from(usageRegistry.values());
}

/**
 * Get usage entry for a specific package
 * @param {string} packageName - Package name
 * @returns {Object | undefined} Usage entry or undefined
 */
export function getUsage(packageName) {
  return usageRegistry.get(packageName);
}

/**
 * Check if a package has been used
 * @param {string} packageName - Package name
 * @returns {boolean} True if registered
 */
export function isUsed(packageName) {
  return usageRegistry.has(packageName);
}

/**
 * Get count of registered packages
 * @returns {number} Number of registered packages
 */
export function getUsageCount() {
  return usageRegistry.size;
}

/**
 * Export registry as JSON snapshot with deterministic ordering
 * @returns {Object} Registry snapshot
 */
export function getRegistrySnapshot() {
  const usages = getAllUsages()
    .sort((a, b) => a.packageName.localeCompare(b.packageName));

  return {
    timestamp: process.env.DETERMINISTIC ? '2025-12-26T00:00:00Z' : new Date().toISOString(),
    count: usages.length,
    usages,
    snapshotHash: computeRegistryHash(usages)
  };
}

/**
 * Compute deterministic hash of registry snapshot
 * @param {Object[]} usages - Usage array
 * @returns {string} SHA256 hex hash
 */
export function computeRegistryHash(usages) {
  const canonical = JSON.stringify(usages, Object.keys(usages[0] || {}).sort());
  return createHash('sha256').update(canonical).digest('hex');
}

/**
 * Clear all registrations (for testing)
 */
export function clearRegistry() {
  usageRegistry.clear();
}

/**
 * Verify that all expected packages are registered
 * @param {string[]} expectedPackages - List of expected package names
 * @returns {Object} { valid: boolean, missing: string[], message: string }
 */
export function verifyAllUsed(expectedPackages) {
  const missing = expectedPackages.filter(name => !isUsed(name));
  const valid = missing.length === 0;

  return {
    valid,
    missing,
    registered: getUsageCount(),
    expected: expectedPackages.length,
    message: valid
      ? `✅ All ${getUsageCount()} packages registered`
      : `❌ ${missing.length} packages missing: ${missing.join(', ')}`
  };
}

export { usageRegistry };
