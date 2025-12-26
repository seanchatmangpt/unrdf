/**
 * Routing Modes - Mode definitions and management for safe migration
 * @module agent-7/routing-modes
 */

/**
 * Available routing modes for shadow execution
 * @readonly
 * @enum {string}
 */
export const ROUTING_MODES = {
  /** Only legacy system handles requests */
  LEGACY_ONLY: 'LEGACY_ONLY',
  /** Legacy writes, substrate receives shadow copy */
  SHADOW_WRITE: 'SHADOW_WRITE',
  /** Read from both systems, compare results */
  SHADOW_READ: 'SHADOW_READ',
  /** Route subset of traffic to substrate */
  PARTIAL_SERVE: 'PARTIAL_SERVE',
  /** Full cutover to substrate */
  SUBSTRATE_ONLY: 'SUBSTRATE_ONLY',
};

/**
 * In-memory mode registry
 * @type {Map<string, string>}
 */
const modeRegistry = new Map();

/**
 * Default mode for new operations
 * @type {string}
 */
const DEFAULT_MODE = ROUTING_MODES.LEGACY_ONLY;

/**
 * Validate routing mode
 * @param {string} mode - Mode to validate
 * @returns {boolean} True if valid mode
 */
function isValidMode(mode) {
  return Object.values(ROUTING_MODES).includes(mode);
}

/**
 * Set routing mode for an operation
 * @param {string} operation - Operation identifier (e.g., 'GET_USER', 'CREATE_ORDER')
 * @param {string} mode - Routing mode from ROUTING_MODES enum
 * @returns {Object} Result object
 * @throws {Error} If mode is invalid
 */
export function setMode(operation, mode) {
  if (!operation || typeof operation !== 'string') {
    throw new Error('Operation must be a non-empty string');
  }

  if (!isValidMode(mode)) {
    throw new Error(
      `Invalid mode: ${mode}. Must be one of: ${Object.values(ROUTING_MODES).join(', ')}`
    );
  }

  const previousMode = modeRegistry.get(operation);
  modeRegistry.set(operation, mode);

  return {
    success: true,
    operation,
    mode,
    previousMode: previousMode || null,
    timestamp: Date.now(),
  };
}

/**
 * Get current routing mode for an operation
 * @param {string} operation - Operation identifier
 * @returns {string} Current routing mode (defaults to LEGACY_ONLY)
 */
export function getMode(operation) {
  if (!operation || typeof operation !== 'string') {
    throw new Error('Operation must be a non-empty string');
  }

  return modeRegistry.get(operation) || DEFAULT_MODE;
}

/**
 * List all operation modes
 * @returns {Array<Object>} Array of {operation, mode} objects
 */
export function listModes() {
  const modes = [];

  for (const [operation, mode] of modeRegistry.entries()) {
    modes.push({
      operation,
      mode,
      isDefault: mode === DEFAULT_MODE,
    });
  }

  // Sort by operation name for deterministic output
  modes.sort((a, b) => a.operation.localeCompare(b.operation));

  return modes;
}

/**
 * Reset all modes (for testing)
 * @returns {Object} Result object
 */
export function resetModes() {
  const count = modeRegistry.size;
  modeRegistry.clear();

  return {
    success: true,
    clearedCount: count,
    timestamp: Date.now(),
  };
}

/**
 * Get mode statistics
 * @returns {Object} Statistics about mode usage
 */
export function getModeStats() {
  const stats = {
    total: modeRegistry.size,
    byMode: {},
  };

  // Initialize counts
  for (const mode of Object.values(ROUTING_MODES)) {
    stats.byMode[mode] = 0;
  }

  // Count operations per mode
  for (const mode of modeRegistry.values()) {
    stats.byMode[mode] = (stats.byMode[mode] || 0) + 1;
  }

  return stats;
}

/**
 * Export mode configuration as JSON
 * @returns {Object} Serializable mode configuration
 */
export function exportConfig() {
  const config = {
    version: '1.0.0',
    defaultMode: DEFAULT_MODE,
    timestamp: Date.now(),
    modes: listModes(),
  };

  return config;
}

/**
 * Import mode configuration from JSON
 * @param {Object} config - Configuration object
 * @returns {Object} Result object
 */
export function importConfig(config) {
  if (!config || typeof config !== 'object') {
    throw new Error('Config must be an object');
  }

  if (!Array.isArray(config.modes)) {
    throw new Error('Config must contain modes array');
  }

  let imported = 0;
  const errors = [];

  for (const entry of config.modes) {
    try {
      setMode(entry.operation, entry.mode);
      imported++;
    } catch (error) {
      errors.push({
        operation: entry.operation,
        error: error.message,
      });
    }
  }

  return {
    success: errors.length === 0,
    imported,
    errors,
    timestamp: Date.now(),
  };
}
