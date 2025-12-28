/**
 * @unrdf/v6-core - UNRDF v6 Alpha Entry Point
 *
 * Receipt-driven RDF with Merkle proofs, delta proposals, and versioned grammar.
 *
 * @module @unrdf/v6-core
 */

// Version and Feature Flags

/**
 * UNRDF v6 version identifier
 * @constant {string}
 */
export const V6_VERSION = '6.0.0-alpha.1';

/**
 * UNRDF v6 feature flags
 * Indicates which v6 features are enabled in this build
 * @constant {Object}
 * @property {boolean} receipts - Receipt-driven operations with Merkle proofs
 * @property {boolean} delta - Delta proposal system
 * @property {boolean} cli - CLI spine integration
 * @property {boolean} grammar - Versioned grammar definitions
 * @property {boolean} docs - Documentation capsule
 * @property {boolean} otel - OTEL validation (not in alpha)
 * @property {boolean} performance - Performance benchmarks (not in alpha)
 */
export const V6_FEATURES = {
  receipts: true,           // Receipt-driven operations with Merkle proofs
  delta: true,              // Delta proposal system
  cli: true,                // CLI spine integration
  grammar: true,            // Versioned grammar definitions
  docs: true,               // Documentation capsule
  otel: false,              // OTEL validation (not in alpha)
  performance: false,       // Performance benchmarks (not in alpha)
};

// Core Capsule Exports
export * from './receipts/index.mjs';
export * from './delta/index.mjs';
export * from './cli/index.mjs';
export * from './grammar/index.mjs';
export * from './docs/index.mjs';

/**
 * Get v6 alpha status
 * @returns {{ version: string, features: Object, status: string }}
 */
export function getV6Status() {
  return {
    version: V6_VERSION,
    features: V6_FEATURES,
    status: 'alpha',
    timestamp: new Date().toISOString(),
  };
}

/**
 * Check if a feature is enabled
 * @param {string} feature - Feature name
 * @returns {boolean}
 */
export function isFeatureEnabled(feature) {
  return V6_FEATURES[feature] === true;
}
