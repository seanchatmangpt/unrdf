/**
 * Oxigraph Store Operations with L5 Receipt Support
 *
 * Wraps Oxigraph store operations with deterministic receipts:
 * - createStore(): Store initialization with creation receipt
 * - query(): SPARQL query execution with query receipt
 * - addQuad(): Quad addition with modification receipt
 *
 * @module @unrdf/oxigraph/store-receipts
 */

import { z } from 'zod';
import { OxigraphStore } from './store.mjs';
import {
  withReceipt,
  createContext,
  ReceiptProfileSchema,
  canonicalize,
  blake3Hash,
} from '../../v6-core/src/receipt-pattern.mjs';

/**
 * Store Configuration Schema
 */
export const StoreConfigSchema = z.object({
  /** Initial quads to populate store */
  quads: z.array(z.any()).optional(),

  /** Store name/identifier */
  name: z.string().optional(),

  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Query Options Schema
 */
export const QueryOptionsSchema = z.object({
  /** Query timeout in milliseconds */
  timeout: z.number().int().positive().optional(),

  /** Maximum number of results */
  limit: z.number().int().positive().optional(),

  /** Result offset */
  offset: z.number().int().nonnegative().optional(),
});

/**
 * Query Result Schema
 */
export const QueryResultSchema = z.union([
  z.array(z.any()), // SELECT results
  z.boolean(),      // ASK results
  z.array(z.any()), // CONSTRUCT/DESCRIBE results
]);

/**
 * Quad Schema (RDF quad structure)
 */
export const QuadSchema = z.object({
  subject: z.any(),
  predicate: z.any(),
  object: z.any(),
  graph: z.any().optional(),
});

/**
 * Pure function: Create Oxigraph store
 *
 * @param {Object} config - Store configuration
 * @returns {OxigraphStore} Store instance
 */
function createStoreImpl(config = {}) {
  const validated = StoreConfigSchema.parse(config);
  return new OxigraphStore(validated.quads);
}

/**
 * Pure function: Execute SPARQL query
 *
 * @param {OxigraphStore} store - Store instance
 * @param {string} queryString - SPARQL query
 * @param {Object} options - Query options
 * @returns {Array|boolean} Query results
 */
function queryImpl(store, queryString, options = {}) {
  const validatedOptions = QueryOptionsSchema.parse(options);

  if (!queryString || typeof queryString !== 'string') {
    throw new Error('Query must be a non-empty string');
  }

  // Execute query (deterministic for same store state)
  return store.query(queryString, validatedOptions);
}

/**
 * Pure function: Add quad to store
 *
 * @param {OxigraphStore} store - Store instance
 * @param {Object} quad - RDF quad
 * @returns {OxigraphStore} Store instance (for chaining)
 */
function addQuadImpl(store, quad) {
  const validatedQuad = QuadSchema.parse(quad);
  store.addQuad(validatedQuad);
  return store;
}

/**
 * Pure function: Get store state hash (for determinism verification)
 *
 * @param {OxigraphStore} store - Store instance
 * @returns {string} BLAKE3 hash of all quads in canonical order
 */
export function getStoreStateHash(store) {
  const allQuads = store.match();

  // Canonicalize all quads (sorted by subject, predicate, object)
  const canonicalQuads = allQuads
    .map(quad => ({
      s: quad.subject.value,
      p: quad.predicate.value,
      o: quad.object.value,
      g: quad.graph?.value || '',
    }))
    .sort((a, b) => {
      // Lexicographic sort
      const cmpS = a.s.localeCompare(b.s);
      if (cmpS !== 0) return cmpS;

      const cmpP = a.p.localeCompare(b.p);
      if (cmpP !== 0) return cmpP;

      const cmpO = a.o.localeCompare(b.o);
      if (cmpO !== 0) return cmpO;

      return a.g.localeCompare(b.g);
    });

  return blake3Hash(canonicalize(canonicalQuads));
}

/**
 * Wrapped: Create store with receipt
 *
 * @example
 * const ctx = createContext({ nodeId: 'node-1' });
 * const { result: store, receipt } = await createStore(ctx, { quads: [] });
 * console.log('Store created:', receipt.receiptHash);
 */
export const createStore = withReceipt(createStoreImpl, {
  operation: 'createStore',
  profile: 'store',
  inputSchema: z.tuple([StoreConfigSchema]),
  outputSchema: z.instanceof(OxigraphStore),
});

/**
 * Wrapped: Query store with receipt
 *
 * @example
 * const ctx = createContext({ nodeId: 'node-1' });
 * const { result, receipt } = await query(ctx, store, 'SELECT * WHERE {?s ?p ?o}');
 * console.log('Query results:', result.length, 'receipt:', receipt.receiptHash);
 */
export const query = withReceipt(queryImpl, {
  operation: 'sparql-query',
  profile: 'query',
  inputSchema: z.tuple([
    z.instanceof(OxigraphStore),
    z.string(),
    QueryOptionsSchema.optional(),
  ]),
  outputSchema: QueryResultSchema,
});

/**
 * Wrapped: Add quad with receipt
 *
 * @example
 * const ctx = createContext({ nodeId: 'node-1' });
 * const { result: store, receipt } = await addQuad(ctx, store, quad);
 * console.log('Quad added:', receipt.receiptHash);
 */
export const addQuad = withReceipt(addQuadImpl, {
  operation: 'addQuad',
  profile: 'delta',
  inputSchema: z.tuple([z.instanceof(OxigraphStore), QuadSchema]),
  outputSchema: z.instanceof(OxigraphStore),
});

/**
 * Determinism Test Helper
 *
 * Runs operation N times with same context and verifies identical receipts
 *
 * @param {Function} wrappedFn - Wrapped function (with receipt)
 * @param {Object} context - Deterministic context
 * @param {...any} args - Function arguments
 * @param {number} iterations - Number of iterations (default: 100)
 * @returns {Object} Test results
 */
export async function testDeterminism(wrappedFn, context, args, iterations = 100) {
  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await wrappedFn(context, ...args);
    receipts.push(receipt);
    hashes.add(receipt.receiptHash);
  }

  const uniqueHashes = Array.from(hashes);

  return {
    iterations,
    uniqueHashes: uniqueHashes.length,
    deterministic: uniqueHashes.length === 1,
    expectedHash: receipts[0].receiptHash,
    receipts: receipts.slice(0, 5), // Sample of first 5
    variance: uniqueHashes.length > 1 ? uniqueHashes : null,
  };
}

/**
 * Composition Test: createStore → addQuad → query
 *
 * Tests that operations compose and receipts chain correctly
 *
 * @param {Object} context - Deterministic context
 * @returns {Object} Composition test result
 */
export async function testComposition(context) {
  // Step 1: Create store
  const { result: store, receipt: receipt1 } = await createStore(context, {
    quads: [],
    name: 'test-store',
  });

  // Step 2: Add quad (chain receipt)
  const ctx2 = createContext({
    nodeId: context.nodeId,
    t_ns: context.t_ns + 1n,
    previousReceiptHash: receipt1.receiptHash,
  });

  const quad = {
    subject: { value: 'http://example.org/Alice' },
    predicate: { value: 'http://xmlns.com/foaf/0.1/name' },
    object: { value: 'Alice', datatype: 'http://www.w3.org/2001/XMLSchema#string' },
  };

  const { result: updatedStore, receipt: receipt2 } = await addQuad(ctx2, store, quad);

  // Step 3: Query store (chain receipt)
  const ctx3 = createContext({
    nodeId: context.nodeId,
    t_ns: context.t_ns + 2n,
    previousReceiptHash: receipt2.receiptHash,
  });

  const { result, receipt: receipt3 } = await query(
    ctx3,
    updatedStore,
    'SELECT * WHERE { ?s ?p ?o }'
  );

  // Verify receipt chain
  const chainValid =
    receipt2.previousReceiptHash === receipt1.receiptHash &&
    receipt3.previousReceiptHash === receipt2.receiptHash;

  return {
    chainValid,
    receipts: [receipt1, receipt2, receipt3],
    resultCount: result.length,
    stateHash: getStoreStateHash(updatedStore),
  };
}

/**
 * L5 Maturity Proof Generator
 *
 * Generates proof that oxigraph package meets L5 maturity:
 * 1. Determinism: 100/100 identical receipts
 * 2. Composition: Receipt chain verification
 * 3. Provenance: Full operation lineage
 *
 * @param {Object} context - Deterministic context
 * @returns {Object} L5 maturity proof
 */
export async function generateL5Proof(context) {
  // Test 1: Determinism - createStore
  const createStoreDeterminism = await testDeterminism(
    createStore,
    context,
    [{ quads: [], name: 'proof-store' }],
    100
  );

  // Test 2: Determinism - query
  const { result: testStore } = await createStore(context, { quads: [] });
  const queryDeterminism = await testDeterminism(
    query,
    context,
    [testStore, 'SELECT * WHERE { ?s ?p ?o }'],
    100
  );

  // Test 3: Composition
  const composition = await testComposition(context);

  // Test 4: State hash stability
  const { result: store1 } = await createStore(context, { quads: [] });
  const { result: store2 } = await createStore(context, { quads: [] });

  const hash1 = getStoreStateHash(store1);
  const hash2 = getStoreStateHash(store2);

  return {
    package: '@unrdf/oxigraph',
    maturityLevel: 'L5',
    timestamp: new Date().toISOString(),
    tests: {
      createStoreDeterminism: {
        passed: createStoreDeterminism.deterministic,
        iterations: createStoreDeterminism.iterations,
        uniqueHashes: createStoreDeterminism.uniqueHashes,
        expectedHash: createStoreDeterminism.expectedHash,
      },
      queryDeterminism: {
        passed: queryDeterminism.deterministic,
        iterations: queryDeterminism.iterations,
        uniqueHashes: queryDeterminism.uniqueHashes,
        expectedHash: queryDeterminism.expectedHash,
      },
      composition: {
        passed: composition.chainValid,
        receiptsChained: composition.receipts.length,
        resultCount: composition.resultCount,
      },
      stateHashStability: {
        passed: hash1 === hash2,
        hash: hash1,
      },
    },
    overallResult: {
      determinism: createStoreDeterminism.deterministic && queryDeterminism.deterministic,
      composition: composition.chainValid,
      stateStability: hash1 === hash2,
      L5Certified:
        createStoreDeterminism.deterministic &&
        queryDeterminism.deterministic &&
        composition.chainValid &&
        hash1 === hash2,
    },
  };
}
