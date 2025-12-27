/**
 * Federation Operations with L5 Receipt Support
 *
 * Wraps multi-store operations with receipt aggregation
 *
 * @module @unrdf/federation/federation-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  createContext,
  canonicalize,
  blake3Hash,
} from '../../v6-core/src/receipt-pattern.mjs';

/**
 * Federation Query Schema
 */
export const FederationQuerySchema = z.object({
  stores: z.array(z.any()).min(1),
  query: z.string().min(1),
  strategy: z.enum(['sequential', 'parallel']).default('parallel'),
});

/**
 * Federation Result Schema
 */
export const FederationResultSchema = z.object({
  results: z.array(z.any()),
  storeCount: z.number().int().positive(),
  mergedCount: z.number().int().nonnegative(),
});

/**
 * Pure function: Execute federated query
 *
 * @param {Object} config - Federation config
 * @returns {Object} Merged results
 */
async function executeFederatedQueryImpl(config) {
  const validated = FederationQuerySchema.parse(config);

  const results = [];

  if (validated.strategy === 'parallel') {
    const promises = validated.stores.map(store =>
      store.query ? store.query(validated.query) : []
    );
    const storeResults = await Promise.all(promises);
    results.push(...storeResults.flat());
  } else {
    for (const store of validated.stores) {
      const storeResults = store.query ? await store.query(validated.query) : [];
      results.push(...storeResults);
    }
  }

  return {
    results,
    storeCount: validated.stores.length,
    mergedCount: results.length,
  };
}

/**
 * Wrapped: Execute federated query with receipt
 */
export const executeFederatedQuery = withReceipt(executeFederatedQueryImpl, {
  operation: 'federatedQuery',
  profile: 'query',
  inputSchema: z.tuple([FederationQuerySchema]),
  outputSchema: FederationResultSchema,
});

/**
 * Aggregate receipts from multiple stores
 *
 * @param {Array<Object>} receipts - Array of store receipts
 * @returns {Object} Aggregated receipt proof
 */
export function aggregateStoreReceipts(receipts) {
  const hashes = receipts.map(r => r.receiptHash);
  const combinedHash = blake3Hash(canonicalize(hashes.sort()));

  return {
    aggregateHash: combinedHash,
    storeCount: receipts.length,
    storeHashes: hashes,
  };
}

/**
 * L5 Determinism Test
 */
export async function testFederationDeterminism(context, iterations = 100) {
  const config = {
    stores: [{ query: () => [] }, { query: () => [] }],
    query: 'SELECT * WHERE { ?s ?p ?o }',
    strategy: 'parallel',
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await executeFederatedQuery(context, config);
    receipts.push(receipt);
    hashes.add(receipt.receiptHash);
  }

  return {
    iterations,
    uniqueHashes: hashes.size,
    deterministic: hashes.size === 1,
    expectedHash: receipts[0].receiptHash,
  };
}
