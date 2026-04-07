/**
 * @file @unrdf/test-utils — Shared test fixtures and factories
 *
 * Single import eliminates 8+ lines of repeated setup in every test file.
 *
 * @example
 * import {
 *   // Data factory
 *   df, namedNode, literal, quad,
 *   // Quads
 *   testQuad, createTestQuad, generateQuads,
 *   // Stores
 *   createTestStore, createStoreWith,
 *   // Vocabulary
 *   VOCAB, terms, entities, socialGraphQuads,
 *   // SPARQL
 *   SPARQL, getBindingValues,
 *   // Mocks (require vitest context)
 *   createMockSpan, createMockTracer, createMockFetch, createTestCleanup,
 *   // Timing
 *   benchmarkSync, assertPerf,
 *   // Assertions
 *   assertThrowsCode, assertStoreSize, assertStoreContains, assertRollback,
 * } from '@unrdf/test-utils';
 *
 * @module @unrdf/test-utils
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';

// ============================================================================
// Data factory (re-export for convenience)
// ============================================================================

/** Oxigraph data factory — namedNode, literal, quad, blankNode, defaultGraph */
export const df = dataFactory;
export const { namedNode, literal, quad, blankNode, defaultGraph } = dataFactory;

// ============================================================================
// Standard test IRIs
// ============================================================================

export const TEST_BASE = 'http://example.org/';
export const TEST_SUBJECT = `${TEST_BASE}subject`;
export const TEST_PREDICATE = `${TEST_BASE}predicate`;
export const TEST_OBJECT_VALUE = 'test value';
export const TEST_GRAPH = `${TEST_BASE}graph`;

// ============================================================================
// Quad factories
// ============================================================================

/**
 * Create a single test quad with default or custom values.
 *
 * @param {object} [overrides] - Optional field overrides
 * @param {*} [overrides.subject]   - Subject term (default: namedNode(TEST_SUBJECT))
 * @param {*} [overrides.predicate] - Predicate term (default: namedNode(TEST_PREDICATE))
 * @param {*} [overrides.object]    - Object term (default: literal(TEST_OBJECT_VALUE))
 * @param {*} [overrides.graph]     - Graph term (default: defaultGraph())
 * @returns {object} RDF quad
 *
 * @example
 * const q = createTestQuad();
 * const q2 = createTestQuad({ object: literal('custom') });
 */
export function createTestQuad(overrides = {}) {
  return quad(
    overrides.subject ?? namedNode(TEST_SUBJECT),
    overrides.predicate ?? namedNode(TEST_PREDICATE),
    overrides.object ?? literal(TEST_OBJECT_VALUE),
    overrides.graph ?? defaultGraph()
  );
}

/**
 * Pre-built default test quad (subject/predicate/object/defaultGraph).
 * Suitable for read-only use in tests that don't need custom values.
 */
export const testQuad = createTestQuad();

/**
 * Generate an array of N test quads with predictable, indexed values.
 *
 * @param {number} count - Number of quads to generate
 * @param {object} [options]
 * @param {string} [options.base]        - Base IRI prefix (default: TEST_BASE)
 * @param {number} [options.predicates]  - Number of distinct predicates to cycle through (default: 10)
 * @returns {object[]} Array of RDF quads
 *
 * @example
 * const quads = generateQuads(1000);
 * // quads[0]: s0 / p0 / "value0"
 * // quads[1]: s1 / p1 / "value1"
 */
export function generateQuads(count, { base = TEST_BASE, predicates = 10 } = {}) {
  const result = [];
  for (let i = 0; i < count; i++) {
    result.push(
      quad(
        namedNode(`${base}subject${i}`),
        namedNode(`${base}predicate${i % predicates}`),
        literal(`value${i}`)
      )
    );
  }
  return result;
}

// ============================================================================
// Store factories
// ============================================================================

/**
 * Create a fresh empty Oxigraph-backed RDF store.
 *
 * @returns {import('@unrdf/oxigraph').OxigraphStore}
 *
 * @example
 * const store = createTestStore();
 */
export function createTestStore() {
  return createStore();
}

/**
 * Create an Oxigraph store pre-loaded with the given quads.
 *
 * @param {object[]} quads - Quads to add on creation
 * @returns {import('@unrdf/oxigraph').OxigraphStore}
 *
 * @example
 * const store = createStoreWith(generateQuads(100));
 */
export function createStoreWith(quads) {
  const store = createStore();
  for (const q of quads) store.add(q);
  return store;
}

/**
 * Create a store with a single default test quad already loaded.
 *
 * @returns {import('@unrdf/oxigraph').OxigraphStore}
 */
export function createPopulatedTestStore() {
  return createStoreWith([testQuad]);
}

// ============================================================================
// Re-exports from sub-modules
// ============================================================================

export {
  // Vocabulary: VOCAB, terms, entities, socialGraphQuads, RDF_NS, RDFS_NS, ...
  VOCAB, terms, entities, socialGraphQuads,
  RDF_NS, RDFS_NS, OWL_NS, XSD_NS, FOAF_NS, SCHEMA_NS, DCTERMS_NS,
} from './vocab.mjs';

export {
  // SPARQL fixtures and result helpers
  SPARQL, getBindingValues, countResults,
} from './sparql.mjs';

export {
  // Mock factories (require vitest context)
  createMockSpan, createMockTracer, createMockMeter,
  createMockFetch, createTestCleanup, createMockEmitter,
} from './mocks.mjs';

export {
  // Performance timing and assertions
  timeSync, benchmarkSync, measureThroughput, assertPerf,
} from './timing.mjs';

export {
  // Custom RDF/store assertion helpers
  assertThrowsCode, assertRejects,
  assertStoreSize, assertStoreContains, assertRollback, assertBindingContains,
} from './assertions.mjs';
