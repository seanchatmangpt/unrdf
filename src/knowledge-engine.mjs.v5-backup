/**
 * @file Main entry point for the Knowledge Engine.
 * @module knowledge-engine
 *
 * A comprehensive RDF processing engine that provides:
 * - Parsing and serialization (Turtle, N-Quads, JSON-LD)
 * - SPARQL querying with Comunica
 * - SHACL validation
 * - N3 reasoning with eyereasoner
 * - Canonicalization and isomorphism checks
 * - Transaction management with hooks and receipts
 *
 * @version 1.0.0
 * @license MIT
 */

// Core parsing and serialization
export {
  parseTurtle,
  toTurtle,
  toNQuads,
  parseJsonLd,
  toJsonLd,
} from './knowledge-engine/parse.mjs';

// SPARQL querying
export {
  query,
  select,
  ask,
  construct,
  describe,
  update,
  getQueryStats,
} from './knowledge-engine/query.mjs';

// SHACL validation
export {
  validateShacl,
  validateShaclMultiple,
  formatValidationReport,
  hasValidationErrors,
  getValidationErrors,
  getValidationWarnings,
} from './knowledge-engine/validate.mjs';

// N3 reasoning
export {
  reason,
  reasonMultiple,
  extractInferred,
  getReasoningStats,
  validateRules,
  createReasoningSession,
} from './knowledge-engine/reason.mjs';

// Canonicalization and isomorphism
export {
  canonicalize,
  isIsomorphic,
  getCanonicalHash,
  groupByIsomorphism,
  findDuplicates,
  getCanonicalizationStats,
  createCanonicalizationSession,
} from './knowledge-engine/canonicalize.mjs';

// Transaction management
export { TransactionManager, printReceipt } from './knowledge-engine/transaction.mjs';

/**
 * Create a complete knowledge engine instance with all capabilities.
 * @param {Object} [options] - Engine options
 * @param {string} [options.baseIRI] - Base IRI for parsing
 * @param {boolean} [options.strictMode] - Enable strict validation mode
 * @param {number} [options.maxHooks] - Maximum number of transaction hooks
 * @returns {Object} Knowledge engine instance
 *
 * @example
 * const engine = createKnowledgeEngine({
 *   baseIRI: 'http://example.org/',
 *   strictMode: true
 * });
 *
 * // Parse data
 * const store = await engine.parseTurtle(ttl);
 *
 * // Query data
 * const results = await engine.query(store, 'SELECT ?s ?o WHERE { ?s ?p ?o }');
 *
 * // Validate data
 * const report = engine.validateShacl(store, shapes);
 *
 * // Reason over data
 * const reasoned = await engine.reason(store, rules);
 *
 * // Canonicalize data
 * const canonical = await engine.canonicalize(store);
 *
 * // Manage transactions
 * const tx = new engine.TransactionManager();
 * const result = await tx.apply(store, delta);
 */
export function createKnowledgeEngine(options = {}) {
  const { baseIRI = 'http://example.org/', strictMode = false, maxHooks = 100 } = options;

  return {
    // Core options
    baseIRI,
    strictMode,
    maxHooks,

    // Parsing and serialization
    async parseTurtle(ttl) {
      const { parseTurtle } = await import('./knowledge-engine/parse.mjs');
      return parseTurtle(ttl, baseIRI);
    },

    async toTurtle(store, options = {}) {
      const { toTurtle } = await import('./knowledge-engine/parse.mjs');
      return toTurtle(store, options);
    },

    async toNQuads(store, options = {}) {
      const { toNQuads } = await import('./knowledge-engine/parse.mjs');
      return toNQuads(store, options);
    },

    async parseJsonLd(jsonld, options = {}) {
      const { parseJsonLd } = await import('./knowledge-engine/parse.mjs');
      return parseJsonLd(jsonld, { baseIRI, ...options });
    },

    async toJsonLd(store, options = {}) {
      const { toJsonLd } = await import('./knowledge-engine/parse.mjs');
      return toJsonLd(store, options);
    },

    // SPARQL querying
    async query(store, sparql, options = {}) {
      const { query } = await import('./knowledge-engine/query.mjs');
      return query(store, sparql, options);
    },

    async select(store, sparql, options = {}) {
      const { select } = await import('./knowledge-engine/query.mjs');
      return select(store, sparql, options);
    },

    async ask(store, sparql, options = {}) {
      const { ask } = await import('./knowledge-engine/query.mjs');
      return ask(store, sparql, options);
    },

    async construct(store, sparql, options = {}) {
      const { construct } = await import('./knowledge-engine/query.mjs');
      return construct(store, sparql, options);
    },

    async describe(store, sparql, options = {}) {
      const { describe } = await import('./knowledge-engine/query.mjs');
      return describe(store, sparql, options);
    },

    async update(store, sparql, options = {}) {
      const { update } = await import('./knowledge-engine/query.mjs');
      return update(store, sparql, options);
    },

    async getQueryStats(store, sparql, options = {}) {
      const { getQueryStats } = await import('./knowledge-engine/query.mjs');
      return getQueryStats(store, sparql, options);
    },

    // SHACL validation
    async validateShacl(store, shapes, options = {}) {
      const { validateShacl } = await import('./knowledge-engine/validate.mjs');
      return validateShacl(store, shapes, options);
    },

    async validateShaclMultiple(store, shapesList, options = {}) {
      const { validateShaclMultiple } = await import('./knowledge-engine/validate.mjs');
      return validateShaclMultiple(store, shapesList, options);
    },

    async formatValidationReport(validationResult, options = {}) {
      const { formatValidationReport } = await import('./knowledge-engine/validate.mjs');
      return formatValidationReport(validationResult, options);
    },

    async hasValidationErrors(validationResult) {
      const { hasValidationErrors } = await import('./knowledge-engine/validate.mjs');
      return hasValidationErrors(validationResult);
    },

    async getValidationErrors(validationResult) {
      const { getValidationErrors } = await import('./knowledge-engine/validate.mjs');
      return getValidationErrors(validationResult);
    },

    async getValidationWarnings(validationResult) {
      const { getValidationWarnings } = await import('./knowledge-engine/validate.mjs');
      return getValidationWarnings(validationResult);
    },

    // N3 reasoning
    async reason(store, rules, options = {}) {
      const { reason } = await import('./knowledge-engine/reason.mjs');
      return reason(store, rules, options);
    },

    async reasonMultiple(store, rulesList, options = {}) {
      const { reasonMultiple } = await import('./knowledge-engine/reason.mjs');
      return reasonMultiple(store, rulesList, options);
    },

    async extractInferred(originalStore, reasonedStore) {
      const { extractInferred } = await import('./knowledge-engine/reason.mjs');
      return extractInferred(originalStore, reasonedStore);
    },

    async getReasoningStats(originalStore, reasonedStore) {
      const { getReasoningStats } = await import('./knowledge-engine/reason.mjs');
      return getReasoningStats(originalStore, reasonedStore);
    },

    async validateRules(rules) {
      const { validateRules } = await import('./knowledge-engine/reason.mjs');
      return validateRules(rules);
    },

    async createReasoningSession(initialStore, rules, options = {}) {
      const { createReasoningSession } = await import('./knowledge-engine/reason.mjs');
      return createReasoningSession(initialStore, rules, options);
    },

    // Canonicalization and isomorphism
    async canonicalize(store, options = {}) {
      const { canonicalize } = await import('./knowledge-engine/canonicalize.mjs');
      return canonicalize(store, options);
    },

    async isIsomorphic(storeA, storeB, options = {}) {
      const { isIsomorphic } = await import('./knowledge-engine/canonicalize.mjs');
      return isIsomorphic(storeA, storeB, options);
    },

    async getCanonicalHash(store, options = {}) {
      const { getCanonicalHash } = await import('./knowledge-engine/canonicalize.mjs');
      return getCanonicalHash(store, options);
    },

    async groupByIsomorphism(stores, options = {}) {
      const { groupByIsomorphism } = await import('./knowledge-engine/canonicalize.mjs');
      return groupByIsomorphism(stores, options);
    },

    async findDuplicates(stores, options = {}) {
      const { findDuplicates } = await import('./knowledge-engine/canonicalize.mjs');
      return findDuplicates(stores, options);
    },

    async getCanonicalizationStats(store, options = {}) {
      const { getCanonicalizationStats } = await import('./knowledge-engine/canonicalize.mjs');
      return getCanonicalizationStats(store, options);
    },

    async createCanonicalizationSession(options = {}) {
      const { createCanonicalizationSession } = await import('./knowledge-engine/canonicalize.mjs');
      return createCanonicalizationSession(options);
    },

    // Transaction management
    async getTransactionManagerClass() {
      const { TransactionManager } = await import('./knowledge-engine/transaction.mjs');
      return TransactionManager;
    },

    async createTransactionManager(options = {}) {
      const { TransactionManager } = await import('./knowledge-engine/transaction.mjs');
      return new TransactionManager({
        strictMode,
        maxHooks,
        ...options,
      });
    },
  };
}

/**
 * Utility function to create a simple knowledge engine with default settings.
 * @param {string} [baseIRI] - Base IRI for parsing
 * @returns {Object} Simple knowledge engine instance
 *
 * @example
 * const engine = createSimpleEngine('http://example.org/');
 * const store = await engine.parseTurtle(ttl);
 * const results = await engine.query(store, 'SELECT * WHERE { ?s ?p ?o }');
 */
export function createSimpleEngine(baseIRI = 'http://example.org/') {
  return createKnowledgeEngine({ baseIRI });
}

/**
 * Utility function to create a strict knowledge engine with validation enabled.
 * @param {string} [baseIRI] - Base IRI for parsing
 * @returns {Object} Strict knowledge engine instance
 *
 * @example
 * const engine = createStrictEngine('http://example.org/');
 * // All operations will use strict validation
 */
export function createStrictEngine(baseIRI = 'http://example.org/') {
  return createKnowledgeEngine({
    baseIRI,
    strictMode: true,
    maxHooks: 50,
  });
}
