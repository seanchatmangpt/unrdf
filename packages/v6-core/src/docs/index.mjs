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
 * Get documentation for a topic
 * @param {string} topic - Topic name
 * @returns {{title: string, content: string, examples?: string[]}}
 */
export function getDocumentation(topic) {
  const docs = {
    overview: {
      title: 'UNRDF v6 Overview',
      content: 'UNRDF v6 provides receipt-driven RDF operations with deterministic execution.',
      examples: ['createReceipt()', 'verifyReceipt()']
    },
    receipts: {
      title: 'Receipt System',
      content: 'Receipts provide cryptographic proof of operations.',
      examples: ['ExecutionReceipt', 'AllocationReceipt']
    },
    delta: {
      title: 'Delta System',
      content: 'Deltas represent proposed state changes.',
      examples: ['createDelta()', 'applyDelta()']
    }
  };
  return docs[topic] || { title: 'Unknown Topic', content: 'Topic not found' };
}

/**
 * List all available documentation topics
 * @deprecated Legacy compatibility function
 * @returns {string[]} Array of available documentation topics
 * @example
 * const topics = listTopics();
 * // ['overview', 'receipts', 'delta', 'grammar', 'cli', 'adapters', 'migration', 'examples']
 */
export function listTopics() {
  return ['overview', 'receipts', 'delta', 'grammar', 'cli', 'adapters', 'migration', 'examples'];
}

/**
 * Legacy V6 documentation registry for backward compatibility
 * @deprecated Use documentation pipeline directly
 * @constant {Object}
 * @property {string} version - Documentation version
 * @property {string[]} topics - Available documentation topics
 * @property {Function} getDocumentation - Get documentation for a topic
 * @property {Function} listTopics - List all available topics
 */
export const V6_DOCS = {
  version: '6.0.0',
  topics: listTopics(),
  getDocumentation,
  listTopics,
};

