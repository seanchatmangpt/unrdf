/**
 * @fileoverview Lens registration and lookup registry
 * @module lens-registry
 */

import { validateString } from './lens-utils.mjs';

/**
 * @typedef {Object} LensDefinition
 * @property {string} name - Unique lens name
 * @property {string} version - Semantic version
 * @property {Function} toSubstrate - Transform legacy request to substrate operations
 * @property {Function} fromSubstrate - Transform substrate data to legacy response
 * @property {string} [description] - Optional description
 * @property {Object} [metadata] - Optional metadata
 */

/**
 * In-memory lens registry
 * @type {Map<string, LensDefinition>}
 */
const lensRegistry = new Map();

/**
 * Defines and registers a new lens
 *
 * @param {LensDefinition} config - Lens configuration
 * @returns {LensDefinition} The registered lens
 * @throws {TypeError} If config is invalid
 * @throws {Error} If lens with same name already exists
 *
 * @example
 * defineLens({
 *   name: 'user-profile',
 *   version: '1.0.0',
 *   toSubstrate: (req) => ({ type: 'UPSERT_USER', payload: req.body }),
 *   fromSubstrate: (data) => ({ id: data.id, name: data.name }),
 * });
 */
export function defineLens(config) {
  // Validate required fields
  if (!config || typeof config !== 'object') {
    throw new TypeError('Lens config must be an object');
  }

  validateString(config.name, 'Lens name');
  validateString(config.version, 'Lens version');

  if (typeof config.toSubstrate !== 'function') {
    throw new TypeError('toSubstrate must be a function');
  }

  if (typeof config.fromSubstrate !== 'function') {
    throw new TypeError('fromSubstrate must be a function');
  }

  // Check for duplicates
  if (lensRegistry.has(config.name)) {
    throw new Error(`Lens '${config.name}' is already registered`);
  }

  // Validate version format (basic semver check)
  const versionRegex = /^\d+\.\d+\.\d+$/;
  if (!versionRegex.test(config.version)) {
    throw new Error(`Invalid version format: ${config.version}. Expected semver (e.g., 1.0.0)`);
  }

  // Create lens definition
  const lens = {
    name: config.name,
    version: config.version,
    toSubstrate: config.toSubstrate,
    fromSubstrate: config.fromSubstrate,
    description: config.description || '',
    metadata: config.metadata || {},
    registeredAt: new Date().toISOString(),
  };

  // Register lens
  lensRegistry.set(config.name, lens);

  return lens;
}

/**
 * Retrieves a lens by name
 *
 * @param {string} name - Lens name
 * @returns {LensDefinition} The lens definition
 * @throws {Error} If lens not found
 *
 * @example
 * const lens = getLens('user-profile');
 */
export function getLens(name) {
  validateString(name, 'Lens name');

  const lens = lensRegistry.get(name);

  if (!lens) {
    throw new Error(`Lens '${name}' not found. Available: ${[...lensRegistry.keys()].join(', ')}`);
  }

  return lens;
}

/**
 * Lists all registered lenses
 *
 * @returns {Array<LensDefinition>} Array of all lens definitions
 *
 * @example
 * const allLenses = listLenses();
 * console.log(allLenses.map(l => l.name));
 */
export function listLenses() {
  return Array.from(lensRegistry.values());
}

/**
 * Checks if a lens exists
 *
 * @param {string} name - Lens name
 * @returns {boolean} True if lens exists
 *
 * @example
 * if (hasLens('user-profile')) { ... }
 */
export function hasLens(name) {
  return lensRegistry.has(name);
}

/**
 * Unregisters a lens (useful for testing)
 *
 * @param {string} name - Lens name
 * @returns {boolean} True if lens was removed
 *
 * @example
 * unregisterLens('user-profile');
 */
export function unregisterLens(name) {
  return lensRegistry.delete(name);
}

/**
 * Clears all registered lenses (useful for testing)
 *
 * @returns {void}
 *
 * @example
 * clearRegistry();
 */
export function clearRegistry() {
  lensRegistry.clear();
}

/**
 * Gets registry statistics
 *
 * @returns {Object} Registry stats
 *
 * @example
 * const stats = getRegistryStats();
 * console.log(`${stats.count} lenses registered`);
 */
export function getRegistryStats() {
  return {
    count: lensRegistry.size,
    lenses: Array.from(lensRegistry.keys()),
  };
}
