/**
 * @file Project Engine Capabilities Manifest
 * @module project-engine/capabilities-manifest
 * @description Defines all available capabilities with feature flags for the project initialization pipeline
 */

/**
 * @typedef {Object} CapabilityFeatureFlag
 * @property {string} name - Feature flag name (e.g., 'code_complexity_js')
 * @property {string} status - Feature status: 'stable', 'beta', 'experimental', 'deprecated'
 * @property {string} [description] - Human-readable description
 * @property {string} [since] - Version when introduced
 * @property {boolean} [enabled] - Whether feature is enabled by default
 */

/**
 * @typedef {Object} CapabilityMetadata
 * @property {string} id - Unique capability ID
 * @property {string} name - Human-readable name
 * @property {string} description - Detailed description
 * @property {string} phase - Pipeline phase name (e.g., 'Phase 6.5')
 * @property {string} module - Module path relative to project-engine
 * @property {string} exportName - Function export name
 * @property {string} version - Capability version
 * @property {string[]} tags - Searchable tags
 * @property {CapabilityFeatureFlag} featureFlag - Associated feature flag
 * @property {Object} [config] - Default configuration
 */

/**
 * JavaScript Code Complexity Analysis Capability
 * @type {CapabilityMetadata}
 */
export const CODE_COMPLEXITY_JS = {
  id: 'code_complexity_js',
  name: 'JavaScript Code Complexity Analysis',
  description:
    'Analyzes JavaScript/TypeScript source code for complexity metrics including cyclomatic complexity, Halstead volume, and maintainability index',
  phase: 'Phase 6.5',
  module: './code-complexity-js.mjs',
  exportName: 'analyzeJsComplexity',
  version: '1.0.0',
  tags: ['analysis', 'metrics', 'complexity', 'javascript', 'typescript', 'rdf'],
  featureFlag: {
    name: 'code_complexity_js',
    status: 'stable',
    description: 'Enable JavaScript/TypeScript code complexity analysis in project initialization',
    since: '4.0.0',
    enabled: true,
  },
  config: {
    mode: 'observe', // 'off', 'observe', 'enforce'
    excludePatterns: [
      '**/node_modules/**',
      '**/dist/**',
      '**/build/**',
      '**/coverage/**',
      '**/.next/**',
      '**/test/**',
      '**/__tests__/**',
      '**/spec/**',
      '**/*.test.mjs',
      '**/*.test.js',
      '**/*.spec.mjs',
      '**/*.spec.js',
    ],
  },
};

/**
 * All available capabilities in the project engine
 * @type {CapabilityMetadata[]}
 */
export const CAPABILITIES = [CODE_COMPLEXITY_JS];

/**
 * Feature flag registry - controls which capabilities are enabled
 * @type {Map<string, boolean>}
 */
export const FEATURE_FLAGS = new Map([
  // Phase 6.5: Code Complexity Analysis
  [CODE_COMPLEXITY_JS.featureFlag.name, CODE_COMPLEXITY_JS.featureFlag.enabled],
]);

/**
 * Check if a capability is enabled via feature flag
 *
 * @param {string} capabilityId - Capability ID (e.g., 'code_complexity_js')
 * @param {Object} [overrides] - Override feature flags for this check
 * @returns {boolean} - True if capability is enabled
 */
export function isCapabilityEnabled(capabilityId, overrides = {}) {
  const flags = new Map([...FEATURE_FLAGS, ...Object.entries(overrides || {})]);
  return flags.get(capabilityId) ?? false;
}

/**
 * Get capability metadata by ID
 *
 * @param {string} capabilityId - Capability ID
 * @returns {CapabilityMetadata|null} - Capability metadata or null if not found
 */
export function getCapabilityMetadata(capabilityId) {
  return CAPABILITIES.find(cap => cap.id === capabilityId) || null;
}

/**
 * Get all enabled capabilities
 *
 * @param {Object} [overrides] - Override feature flags
 * @returns {CapabilityMetadata[]} - Array of enabled capabilities
 */
export function getEnabledCapabilities(overrides = {}) {
  return CAPABILITIES.filter(cap => isCapabilityEnabled(cap.id, overrides));
}

/**
 * Enable/disable capability at runtime
 *
 * @param {string} capabilityId - Capability ID
 * @param {boolean} enabled - Enable or disable
 */
export function setCapabilityEnabled(capabilityId, enabled) {
  FEATURE_FLAGS.set(capabilityId, enabled);
}
