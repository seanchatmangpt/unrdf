/**
 * @file Observability Manager Stub - Full implementation in @unrdf/observability
 * @module streaming/observability
 *
 * @description
 * Stub file for observability. Import from @unrdf/observability for full functionality.
 */

/**
 * Create an observability manager
 * @param {Object} [config] - Configuration options
 * @returns {Object} Observability manager
 */
export function createObservabilityManager(config = {}) {
  return {
    async initialize() {
      // Stub - no-op
    },
    async shutdown() {
      // Stub - no-op
    },
    recordError(error, context) {
      // Stub - console log only
      console.error('[observability]', error, context);
    },
  };
}
