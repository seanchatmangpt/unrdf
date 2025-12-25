/**
 * @file Distributed Query Engine - Execute SPARQL queries across federated peers
 * @module federation/distributed-query
 */

import { z } from 'zod';

/**
 * @typedef {Object} QueryResult
 * @property {boolean} success - Whether the query succeeded
 * @property {any} data - Query result data
 * @property {string} [error] - Error message if query failed
 * @property {number} duration - Query execution time in milliseconds
 * @property {string} peerId - Peer that executed the query
 */

/**
 * @typedef {Object} AggregatedResult
 * @property {boolean} success - Whether aggregation succeeded
 * @property {any[]} results - Combined results from all peers
 * @property {QueryResult[]} peerResults - Individual peer results
 * @property {number} totalDuration - Total execution time
 * @property {number} successCount - Number of successful queries
 * @property {number} failureCount - Number of failed queries
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const QueryConfigSchema = z.object({
  sparql: z.string().min(1, 'SPARQL query must not be empty'),
  timeout: z.number().positive().optional().default(30000),
  format: z.enum(['json', 'xml', 'turtle', 'ntriples']).optional().default('json'),
});

export const QueryResultSchema = z.object({
  success: z.boolean(),
  data: z.any(),
  error: z.string().optional(),
  duration: z.number(),
  peerId: z.string(),
});

/* ========================================================================= */
/* Query Execution                                                          */
/* ========================================================================= */

/**
 * Execute a SPARQL query against a single peer.
 *
 * @param {string} peerId - Peer identifier
 * @param {string} endpoint - Peer endpoint URL
 * @param {string} sparqlQuery - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {number} [options.timeout=30000] - Request timeout in milliseconds
 * @param {string} [options.format='json'] - Response format
 * @returns {Promise<QueryResult>} Query result
 */
export async function executeFederatedQuery(peerId, endpoint, sparqlQuery, options = {}) {
  const config = QueryConfigSchema.parse({
    sparql: sparqlQuery,
    timeout: options.timeout,
    format: options.format,
  });

  const startTime = Date.now();

  try {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), config.timeout);

    const response = await fetch(endpoint, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/sparql-query',
        Accept: getAcceptHeader(config.format),
      },
      body: config.sparql,
      signal: controller.signal,
    });

    clearTimeout(timeoutId);

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }

    const data = await parseResponse(response, config.format);
    const duration = Date.now() - startTime;

    return {
      success: true,
      data,
      duration,
      peerId,
    };
  } catch (error) {
    const duration = Date.now() - startTime;
    return {
      success: false,
      data: null,
      error: error.message,
      duration,
      peerId,
    };
  }
}

/**
 * Execute a SPARQL query across multiple peers.
 *
 * @param {Array<{id: string, endpoint: string}>} peers - Array of peers to query
 * @param {string} sparqlQuery - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {number} [options.timeout=30000] - Request timeout per peer
 * @param {string} [options.format='json'] - Response format
 * @param {string} [options.strategy='parallel'] - Execution strategy (parallel or sequential)
 * @returns {Promise<AggregatedResult>} Aggregated query results
 */
export async function executeDistributedQuery(peers, sparqlQuery, options = {}) {
  const strategy = options.strategy || 'parallel';
  const startTime = Date.now();

  let peerResults;

  if (strategy === 'parallel') {
    // Execute all queries in parallel
    peerResults = await Promise.all(
      peers.map(peer => executeFederatedQuery(peer.id, peer.endpoint, sparqlQuery, options))
    );
  } else {
    // Execute queries sequentially
    peerResults = [];
    for (const peer of peers) {
      const result = await executeFederatedQuery(peer.id, peer.endpoint, sparqlQuery, options);
      peerResults.push(result);
    }
  }

  const totalDuration = Date.now() - startTime;
  const successCount = peerResults.filter(r => r.success).length;
  const failureCount = peerResults.length - successCount;

  return {
    success: successCount > 0,
    results: aggregateResults(peerResults),
    peerResults,
    totalDuration,
    successCount,
    failureCount,
  };
}

/**
 * Aggregate results from multiple peers.
 *
 * @param {QueryResult[]} results - Array of query results from peers
 * @returns {any[]} Combined and deduplicated results
 */
export function aggregateResults(results) {
  const successfulResults = results.filter(r => r.success);

  if (successfulResults.length === 0) {
    return [];
  }

  // Combine all results
  const combined = [];
  const seen = new Set();

  for (const result of successfulResults) {
    if (!result.data) continue;

    // Handle SPARQL JSON results format
    if (result.data.results && Array.isArray(result.data.results.bindings)) {
      for (const binding of result.data.results.bindings) {
        const key = JSON.stringify(binding);
        if (!seen.has(key)) {
          seen.add(key);
          combined.push(binding);
        }
      }
    } else if (Array.isArray(result.data)) {
      // Handle array results
      for (const item of result.data) {
        const key = JSON.stringify(item);
        if (!seen.has(key)) {
          seen.add(key);
          combined.push(item);
        }
      }
    } else {
      // Handle single object results
      const key = JSON.stringify(result.data);
      if (!seen.has(key)) {
        seen.add(key);
        combined.push(result.data);
      }
    }
  }

  return combined;
}

/**
 * Route a query to specific peers based on strategy.
 *
 * @param {Array<{id: string, endpoint: string, metadata?: Object}>} allPeers - All available peers
 * @param {string} sparqlQuery - SPARQL query to route
 * @param {string} [strategy='broadcast'] - Routing strategy
 * @returns {Array<{id: string, endpoint: string}>} Selected peers for query
 */
export function routeQuery(allPeers, sparqlQuery, strategy = 'broadcast') {
  if (strategy === 'broadcast') {
    // Query all peers
    return allPeers;
  }

  if (strategy === 'selective') {
    // Simple selective routing: only query peers with relevant metadata
    // This can be enhanced based on query analysis
    return allPeers.filter(peer => {
      if (!peer.metadata) return true;
      // Example: only query peers that handle specific datasets
      return true;
    });
  }

  if (strategy === 'first-available') {
    // Query only the first peer
    return allPeers.slice(0, 1);
  }

  // Default to broadcast
  return allPeers;
}

/* ========================================================================= */
/* Helpers                                                                  */
/* ========================================================================= */

/**
 * Get HTTP Accept header for response format.
 *
 * @param {string} format - Response format
 * @returns {string} Accept header value
 */
function getAcceptHeader(format) {
  const headers = {
    json: 'application/sparql-results+json',
    xml: 'application/sparql-results+xml',
    turtle: 'text/turtle',
    ntriples: 'application/n-triples',
  };
  return headers[format] || headers.json;
}

/**
 * Parse HTTP response based on format.
 *
 * @param {Response} response - Fetch response
 * @param {string} format - Expected format
 * @returns {Promise<any>} Parsed response data
 */
async function parseResponse(response, format) {
  if (format === 'json') {
    return response.json();
  }
  return response.text();
}
