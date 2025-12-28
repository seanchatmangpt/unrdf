/**
 * @unrdf/v6-core - Documentation Module
 *
 * Documentation pipeline for Diataxis-based thesis generation.
 *
 * @module @unrdf/v6-core/docs
 */

export * from './pipeline.mjs';
export * from './latex-generator.mjs';
export * from './thesis-builder.mjs';

// Export schemas
export * from './latex-generator.schema.mjs';
export * from './thesis-builder.schema.mjs';

// =============================================================================
// Legacy API Compatibility Layer (v6-smoke tests)
// =============================================================================

/**
 * Legacy V6 documentation registry for backward compatibility
 * @deprecated Use documentation pipeline directly
 * @constant {Object}
 * @property {string} version - Documentation version
 * @property {string[]} topics - Available documentation topics
 * @property {string} format - Documentation format (Diataxis)
 */
export const V6_DOCS = {
  version: '6.0.0-alpha.1',
  topics: ['architecture', 'api', 'tutorials', 'reference'],
  format: 'diataxis',
};

/**
 * Get documentation for a specific topic
 * @deprecated Legacy compatibility function
 * @param {string} topic - Documentation topic to retrieve
 * @returns {Object} Documentation object with topic, content, and timestamp
 * @example
 * const docs = getDocumentation('architecture');
 * // { topic: 'architecture', content: 'Documentation for architecture', lastUpdated: '...' }
 */
export function getDocumentation(topic) {
  return {
    topic,
    content: `Documentation for ${topic}`,
    lastUpdated: new Date().toISOString(),
  };
}

/**
 * List all available documentation topics
 * @deprecated Legacy compatibility function
 * @returns {string[]} Array of available documentation topics
 * @example
 * const topics = listTopics();
 * // ['architecture', 'api', 'tutorials', 'reference']
 */
export function listTopics() {
  return V6_DOCS.topics;
}

