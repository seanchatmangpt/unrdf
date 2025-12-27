/**
 * UNRDF v6 Compatibility Layer
 *
 * Provides migration bridge from v5 to v6 with:
 * - API adapters for breaking changes
 * - Deprecation warnings with migration hints
 * - Receipt generation wrappers
 * - Schema generators
 *
 * @module @unrdf/v6-compat
 */

export * from './adapters.mjs';
export * from './lint-rules.mjs';
export * from './schema-generator.mjs';

/**
 * Version information
 */
export const VERSION = '6.0.0-alpha.1';
export const COMPAT_VERSION = '5.0.x';

/**
 * Check if running in compatibility mode
 */
export const isCompatMode = () => {
  return process.env.UNRDF_COMPAT_MODE === 'true';
};

/**
 * Enable compatibility mode
 */
export const enableCompatMode = () => {
  process.env.UNRDF_COMPAT_MODE = 'true';
};

/**
 * Disable compatibility mode
 */
export const disableCompatMode = () => {
  process.env.UNRDF_COMPAT_MODE = 'false';
};
