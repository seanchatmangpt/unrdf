/**
 * Advanced SPARQL Federation using Comunica
 * Real-time distributed SPARQL with streaming results
 *
 * @module @unrdf/federation/advanced-sparql-federation
 * @description
 * Integrates Comunica's federated query engine with UNRDF's federation
 * coordinator for advanced distributed SPARQL execution with streaming
 * observability and real-time results.
 */

import { z } from 'zod';
import { QueryEngine } from '@comunica/query-sparql';
import { createCoordinator } from './federation/coordinator.mjs';
import { executeFederatedQuery } from './federation/distributed-query.mjs';
import { recordQuery, recordError } from './federation/metrics.mjs';

// =============================================================================
// Configuration Schemas
// =============================================================================

/**
 * Federated source configuration schema
 */
const FederatedSourceSchema = z.object({
  /** Source URL or SPARQL endpoint */
  url: z.string().url(),
  /** Source type: sparql, file, or other */
  type: z.enum(['sparql', 'file', 'other']).default('sparql'),
  /** Optional authentication */
  auth: z.object({
    username: z.string(),
    password: z.string(),
  }).optional(),
  /** Optional headers for HTTP requests */
  headers: z.record(z.string(), z.string()).optional(),
});

/**
 * Advanced federation configuration schema
 */
export const AdvancedFederationConfigSchema = z.object({
  /** List of federated SPARQL sources */
  sources: z.array(FederatedSourceSchema).min(1),
  /** Enable streaming results (default: true) */
  streaming: z.boolean().default(true),
  /** Query timeout in milliseconds (default: 30000) */
  timeout: z.number().int().positive().default(30000),
  /** Enable caching (default: true) */
  cache: z.boolean().default(true),
  /** Optimization level: none, basic, aggressive */
  optimization: z.enum(['none', 'basic', 'aggressive']).default('basic'),
  /** Enable observability tracing */
  trace: z.boolean().default(true),
});

/**
 * Query result binding schema
 */
const BindingSchema = z.record(z.string(), z.object({
  value: z.string(),
  type: z.enum(['uri', 'literal', 'bnode']),
  datatype: z.string().optional(),
  'xml:lang': z.string().optional(),
}));

/**
 * Query execution result schema
 */
export const QueryExecutionResultSchema = z.object({
  /** Query results as array of bindings */
  bindings: z.array(BindingSchema),
  /** Execution metadata */
  metadata: z.object({
    /** Query execution time in milliseconds */
    executionTime: z.number(),
    /** Number of sources queried */
    sourcesQueried: z.number(),
    /** Number of results returned */
    resultCount: z.number(),
    /** Whether results were cached */
    cached: z.boolean(),
  }),
});

// =============================================================================
// Advanced SPARQL Federation Engine
// =============================================================================

/**
 * Creates an advanced SPARQL federation engine
 *
 * @param {Object} config - Federation configuration
 * @returns {Promise<Object>} Federation engine instance
 *
 * @example
 * const engine = await createAdvancedFederationEngine({
 *   sources: [
 *     { url: 'https://dbpedia.org/sparql', type: 'sparql' },
 *     { url: 'https://query.wikidata.org/sparql', type: 'sparql' }
 *   ],
 *   streaming: true,
 *   optimization: 'aggressive'
 * });
 *
 * const results = await engine.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
 */
export async function createAdvancedFederationEngine(config) {
  const validated = AdvancedFederationConfigSchema.parse(config);

  // Initialize Comunica query engine
  const comunica = new QueryEngine();

  // Initialize UNRDF federation coordinator
  const coordinator = await createCoordinator({
    nodeId: `federation-${Date.now()}`,
    peers: validated.sources.map((source) => ({
      id: `peer-${source.url}`,
      url: source.url,
      type: source.type,
    })),
  });

  /**
   * Execute a federated SPARQL query
   *
   * @param {string} sparqlQuery - SPARQL query string
   * @param {Object} options - Query execution options
   * @returns {Promise<Object>} Query results with metadata
   */
  async function query(sparqlQuery, options = {}) {
    const startTime = Date.now();

    try {
      // Record query metrics
      recordQuery({
        query: sparqlQuery,
        sources: validated.sources.length,
        timestamp: startTime,
      });

      // Prepare sources for Comunica
      const comunicaSources = validated.sources.map((source) => ({
        type: source.type === 'sparql' ? 'sparql' : 'file',
        value: source.url,
        ...(source.headers && { headers: source.headers }),
      }));

      // Execute query with Comunica
      const bindingsStream = await comunica.queryBindings(sparqlQuery, {
        sources: comunicaSources,
        lenient: validated.optimization === 'aggressive',
      });

      // Collect bindings
      const bindings = [];
      for await (const binding of bindingsStream) {
        const bindingObj = {};
        for (const [key, value] of binding.entries()) {
          bindingObj[key] = {
            value: value.value,
            type: value.termType === 'NamedNode' ? 'uri' : 'literal',
            ...(value.datatype && { datatype: value.datatype.value }),
            ...(value.language && { 'xml:lang': value.language }),
          };
        }
        bindings.push(bindingObj);

        // Yield results in streaming mode
        if (validated.streaming && options.onBinding) {
          options.onBinding(bindingObj);
        }
      }

      const executionTime = Date.now() - startTime;

      // Return validated results
      return QueryExecutionResultSchema.parse({
        bindings,
        metadata: {
          executionTime,
          sourcesQueried: validated.sources.length,
          resultCount: bindings.length,
          cached: false,
        },
      });
    } catch (error) {
      recordError({
        error: error.message,
        query: sparqlQuery,
        timestamp: Date.now(),
      });
      throw new Error(`Federation query failed: ${error.message}`);
    }
  }

  /**
   * Execute a federated query using UNRDF's distributed query mechanism
   *
   * @param {string} sparqlQuery - SPARQL query string
   * @returns {Promise<Object>} Aggregated query results
   */
  async function distributedQuery(sparqlQuery) {
    const startTime = Date.now();

    try {
      // Use UNRDF's federated query execution
      const results = await executeFederatedQuery({
        query: sparqlQuery,
        sources: validated.sources.map((s) => s.url),
        timeout: validated.timeout,
      });

      const executionTime = Date.now() - startTime;

      return {
        bindings: results.results || [],
        metadata: {
          executionTime,
          sourcesQueried: validated.sources.length,
          resultCount: (results.results || []).length,
          cached: false,
        },
      };
    } catch (error) {
      recordError({
        error: error.message,
        query: sparqlQuery,
        timestamp: Date.now(),
      });
      throw new Error(`Distributed query failed: ${error.message}`);
    }
  }

  /**
   * Query metadata from all federated sources
   *
   * @returns {Promise<Array>} Source metadata
   */
  async function getSourcesMetadata() {
    const metadata = [];

    for (const source of validated.sources) {
      try {
        const metadataQuery = `
          SELECT (COUNT(*) AS ?count)
          WHERE { ?s ?p ?o }
        `;

        const result = await query(metadataQuery);

        metadata.push({
          url: source.url,
          type: source.type,
          tripleCount: result.bindings[0]?.count?.value || 0,
          status: 'online',
        });
      } catch (error) {
        metadata.push({
          url: source.url,
          type: source.type,
          status: 'offline',
          error: error.message,
        });
      }
    }

    return metadata;
  }

  /**
   * Close the federation engine and cleanup resources
   *
   * @returns {Promise<void>}
   */
  async function close() {
    // Cleanup coordinator resources if needed
    await coordinator?.cleanup?.();
  }

  return {
    query,
    distributedQuery,
    getSourcesMetadata,
    close,
    // Expose underlying engines
    comunica,
    coordinator,
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Create a simple federated query over multiple endpoints
 *
 * @param {Array<string>} endpoints - Array of SPARQL endpoint URLs
 * @param {string} query - SPARQL query string
 * @returns {Promise<Object>} Query results
 *
 * @example
 * const results = await federatedQuery(
 *   ['https://dbpedia.org/sparql', 'https://query.wikidata.org/sparql'],
 *   'SELECT * WHERE { ?s a dbo:Person } LIMIT 10'
 * );
 */
export async function federatedQuery(endpoints, query) {
  const engine = await createAdvancedFederationEngine({
    sources: endpoints.map((url) => ({ url, type: 'sparql' })),
    streaming: false,
  });

  try {
    const results = await engine.query(query);
    return results;
  } finally {
    await engine.close();
  }
}

/**
 * Stream results from a federated query
 *
 * @param {Array<string>} endpoints - Array of SPARQL endpoint URLs
 * @param {string} query - SPARQL query string
 * @param {Function} onBinding - Callback for each result binding
 * @returns {Promise<void>}
 *
 * @example
 * await streamFederatedQuery(
 *   ['https://dbpedia.org/sparql'],
 *   'SELECT * WHERE { ?s ?p ?o } LIMIT 100',
 *   (binding) => console.log('Result:', binding)
 * );
 */
export async function streamFederatedQuery(endpoints, query, onBinding) {
  const engine = await createAdvancedFederationEngine({
    sources: endpoints.map((url) => ({ url, type: 'sparql' })),
    streaming: true,
  });

  try {
    await engine.query(query, { onBinding });
  } finally {
    await engine.close();
  }
}
