/**
 * @file use-distributed-query.mjs
 * @description React hook for executing distributed SPARQL queries across federated stores
 * @since 3.2.0
 */

import { useState, useCallback, useEffect } from 'react';
import { z } from 'zod';
import { useFederatedSystem } from './use-federated-system.mjs';

/**
 * Maximum SPARQL query size (100KB) to prevent DoS attacks
 */
const MAX_SPARQL_SIZE = 100 * 1024;

/**
 * Zod schema for SPARQL query validation
 * Validates query string size and format to prevent injection
 */
const SparqlQuerySchema = z
  .string()
  .max(MAX_SPARQL_SIZE, `SPARQL query exceeds maximum size of ${MAX_SPARQL_SIZE} bytes`)
  .nullable()
  .optional();

/**
 * Zod schema for store ID validation
 * Store IDs must be alphanumeric with limited special chars to prevent injection
 */
const StoreIdSchema = z
  .string()
  .min(1)
  .max(256)
  .regex(/^[a-zA-Z0-9_\-:.]+$/, 'Store ID contains invalid characters');

/**
 * Zod schema for distributed query options validation
 * Prevents XSS/injection by validating all inputs
 */
const DistributedQueryOptionsSchema = z
  .object({
    /** Specific stores to query - validated store IDs */
    stores: z.array(StoreIdSchema).optional(),
    /** Query strategy must be one of allowed values */
    strategy: z.enum(['fastest', 'quorum', 'all', 'leader']).optional().default('fastest'),
    /** Result aggregation strategy */
    aggregation: z.enum(['union', 'intersection']).optional().default('union'),
    /** Timeout must be positive number, max 5 minutes */
    timeout: z.number().positive().max(300000).optional().default(30000),
    /** Enable result caching */
    cache: z.boolean().optional().default(true),
    /** Federation options (passed through) */
    federation: z.object({}).passthrough().optional(),
  })
  .strict();

/**
 * Hook for distributed SPARQL query execution with intelligent routing
 *
 * @since 3.2.0
 * @param {string|null} sparql - SPARQL query string (null for manual execution)
 * @param {Object} options - Query options
 * @param {string[]} [options.stores] - Specific stores to query
 * @param {string} [options.strategy='fastest'] - Query strategy: 'fastest', 'quorum', 'all', 'leader'
 * @param {string} [options.aggregation='union'] - Result aggregation: 'union', 'intersection'
 * @param {number} [options.timeout=30000] - Query timeout
 * @param {boolean} [options.cache=true] - Enable result caching
 * @returns {Object} Query state and execution function
 * @throws {Error} When federation system not initialized
 * @throws {Error} When query times out across all stores
 * @throws {Error} When refetch is called without initial sparql query
 * @performance 'fastest' strategy returns first response - lowest latency but potential inconsistency.
 *   'quorum' waits for majority - better consistency but higher latency. Cache reduces network calls.
 *
 * @example
 * // Auto-execute on mount
 * const { data, loading, error } = useDistributedQuery(`
 *   SELECT ?s ?p ?o WHERE {
 *     ?s ?p ?o
 *   } LIMIT 100
 * `, { strategy: 'quorum', aggregation: 'union' });
 *
 * @example
 * // Manual execution with stats
 * const { execute, data, loading, executionStats } = useDistributedQuery(null);
 * const result = await execute('SELECT * WHERE { ?s ?p ?o }');
 * console.log('Queried stores:', executionStats.storesQueried);
 */
export function useDistributedQuery(sparql = null, options = {}) {
  // Validate SPARQL query with Zod schema to prevent injection
  const validatedSparql = SparqlQuerySchema.parse(sparql);

  // Validate options with Zod schema to prevent XSS/injection
  const validatedOptions = DistributedQueryOptionsSchema.parse(options);

  const { query: federatedQuery, system } = useFederatedSystem(validatedOptions.federation || {});
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(!!sparql);
  const [error, setError] = useState(null);
  const [executionStats, setExecutionStats] = useState(null);

  // Execute query on mount if sparql provided
  useEffect(() => {
    if (!validatedSparql || !system) return;

    let mounted = true;

    async function executeQuery() {
      try {
        setLoading(true);
        setError(null);

        const startTime = performance.now();

        const result = await federatedQuery(validatedSparql, {
          stores: validatedOptions.stores,
          strategy: validatedOptions.strategy,
          aggregation: validatedOptions.aggregation,
          timeout: validatedOptions.timeout,
          cache: validatedOptions.cache,
        });

        const duration = performance.now() - startTime;

        if (!mounted) return;

        setData(result.bindings || result);
        setExecutionStats({
          duration,
          storesQueried: result.storesQueried || [],
          strategy: result.strategy,
          cacheHit: result.cacheHit || false,
        });
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    executeQuery();

    return () => {
      mounted = false;
    };
  }, [validatedSparql, system, JSON.stringify(validatedOptions)]);

  // Manual execute function
  const execute = useCallback(
    async (queryString, executeOptions = {}) => {
      if (!system) {
        throw new Error('Federation system not initialized');
      }

      // Validate manual query string
      const validatedQueryString = SparqlQuerySchema.parse(queryString);
      if (!validatedQueryString) {
        throw new Error('Query string is required for manual execution');
      }

      // Validate execute options
      const validatedExecuteOptions = DistributedQueryOptionsSchema.partial().parse(executeOptions);

      try {
        setLoading(true);
        setError(null);

        const startTime = performance.now();

        const result = await federatedQuery(validatedQueryString, {
          ...validatedOptions,
          ...validatedExecuteOptions,
        });

        const duration = performance.now() - startTime;

        setData(result.bindings || result);
        setExecutionStats({
          duration,
          storesQueried: result.storesQueried || [],
          strategy: result.strategy,
          cacheHit: result.cacheHit || false,
        });
        setLoading(false);

        return result;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [federatedQuery, system, validatedOptions]
  );

  // Refetch current query
  const refetch = useCallback(() => {
    if (!validatedSparql) {
      throw new Error('No query to refetch. Use execute() for manual queries.');
    }
    return execute(validatedSparql);
  }, [validatedSparql, execute]);

  return {
    data,
    loading,
    error,
    executionStats,
    execute,
    refetch,
  };
}
