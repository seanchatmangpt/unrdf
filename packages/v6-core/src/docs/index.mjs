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
 * Legacy: V6_DOCS object
 */
export const V6_DOCS = {
  version: '6.0.0-alpha.1',
  topics: ['architecture', 'api', 'tutorials', 'reference'],
  format: 'diataxis',
};

/**
 * Legacy: getDocumentation()
 */
export function getDocumentation(topic) {
  return {
    topic,
    content: `Documentation for ${topic}`,
    lastUpdated: new Date().toISOString(),
  };
}

/**
 * Legacy: listTopics()
 */
export function listTopics() {
  return V6_DOCS.topics;
}

