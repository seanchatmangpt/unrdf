/**
 * @fileoverview Shared utility functions for grammar parsing
 * @module @unrdf/v6-core/grammar/parser/utils
 * @version 6.0.0-alpha.1
 */

/**
 * Count AST nodes (recursive)
 * @param {*} obj - AST object
 * @returns {number} Node count
 */
export function countASTNodes(obj) {
  if (!obj || typeof obj !== 'object') return 1;

  let count = 1;
  for (const value of Object.values(obj)) {
    if (Array.isArray(value)) {
      count += value.reduce((sum, item) => sum + countASTNodes(item), 0);
    } else if (typeof value === 'object') {
      count += countASTNodes(value);
    }
  }

  return count;
}

/**
 * Create empty complexity bounds
 * @returns {Object} Empty complexity
 */
export function createEmptyComplexity() {
  return {
    estimatedTimeMs: 0,
    astNodeCount: 0,
    maxDepth: 0,
  };
}

/**
 * Create parse receipt metadata
 * @param {string} grammarType - Grammar type
 * @returns {Object} Parse receipt
 */
export function createParseReceipt(grammarType) {
  return {
    timestamp: new Date().toISOString(),
    grammarVersion: '1.1', // Default to latest versions
    parser: `v6-${grammarType}-parser`,
  };
}
