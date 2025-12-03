/**
 * @file Performance Metrics - Query execution tracking
 * @module @unrdf/dark-matter/performance-metrics
 */

import { z } from 'zod';

/**
 * Query record schema
 */
const QueryRecordSchema = z.object({
  query: z.string(),
  executionTime: z.number(),
  resultCount: z.number(),
  timestamp: z.number(),
});

/**
 * Create metrics collector
 * @returns {Object} Metrics collector instance
 *
 * @example
 * const metrics = createMetricsCollector();
 * metrics.recordQuery(query, 150, 42);
 * const stats = metrics.analyzePerformance();
 */
export function createMetricsCollector() {
  const queries = [];
  const queryFrequency = new Map();

  return {
    /**
     * Record a query execution
     * @param {string} query - SPARQL query
     * @param {number} executionTime - Time in milliseconds
     * @param {number} resultCount - Number of results
     */
    recordQuery(query, executionTime, resultCount) {
      if (typeof query !== 'string') {
        throw new TypeError('recordQuery: query must be a string');
      }

      if (typeof executionTime !== 'number' || executionTime < 0) {
        throw new TypeError('recordQuery: executionTime must be a non-negative number');
      }

      if (typeof resultCount !== 'number' || resultCount < 0) {
        throw new TypeError('recordQuery: resultCount must be a non-negative number');
      }

      const record = {
        query,
        executionTime,
        resultCount,
        timestamp: Date.now(),
      };

      QueryRecordSchema.parse(record);
      queries.push(record);

      // Track frequency
      const normalized = normalizeQuery(query);
      queryFrequency.set(normalized, (queryFrequency.get(normalized) || 0) + 1);
    },

    /**
     * Analyze performance statistics
     * @param {Store} [store] - Optional store reference
     * @returns {Object} Performance statistics
     */
    analyzePerformance(_store = null) {
      if (queries.length === 0) {
        return {
          totalQueries: 0,
          averageExecutionTime: 0,
          slowestQueries: [],
          mostExecutedQueries: [],
        };
      }

      // Calculate average execution time
      const totalTime = queries.reduce((sum, q) => sum + q.executionTime, 0);
      const averageExecutionTime = totalTime / queries.length;

      // Find slowest queries
      const sorted = [...queries].sort((a, b) => b.executionTime - a.executionTime);
      const slowestQueries = sorted.slice(0, 5).map(q => ({
        query: q.query.substring(0, 100),
        executionTime: q.executionTime,
        resultCount: q.resultCount,
      }));

      // Most executed queries
      const frequencyEntries = Array.from(queryFrequency.entries())
        .sort((a, b) => b[1] - a[1])
        .slice(0, 5);

      const mostExecutedQueries = frequencyEntries.map(([query, count]) => ({
        query: query.substring(0, 100),
        executionCount: count,
      }));

      return {
        totalQueries: queries.length,
        averageExecutionTime: Math.round(averageExecutionTime * 100) / 100,
        slowestQueries,
        mostExecutedQueries,
      };
    },

    /**
     * Get all collected metrics
     * @returns {Array<Object>} All query records
     */
    getMetrics() {
      return [...queries];
    },

    /**
     * Clear all metrics
     */
    clearMetrics() {
      queries.length = 0;
      queryFrequency.clear();
    },
  };
}

/**
 * Normalize query for frequency tracking
 * @param {string} query - SPARQL query
 * @returns {string} Normalized query
 */
function normalizeQuery(query) {
  // Remove whitespace variations
  return query.trim().replace(/\s+/g, ' ').toUpperCase();
}

/**
 * Record query execution with timing
 * @param {string} query - SPARQL query
 * @param {number} executionTime - Execution time in ms
 * @param {number} resultCount - Number of results
 * @returns {Object} Query record
 *
 * @throws {TypeError} If parameters are invalid
 *
 * @example
 * const record = recordQuery(query, 150, 42);
 * console.log('Recorded:', record.timestamp);
 */
export function recordQuery(query, executionTime, resultCount) {
  if (typeof query !== 'string') {
    throw new TypeError('recordQuery: query must be a string');
  }

  if (typeof executionTime !== 'number' || executionTime < 0) {
    throw new TypeError('recordQuery: executionTime must be a non-negative number');
  }

  if (typeof resultCount !== 'number' || resultCount < 0) {
    throw new TypeError('recordQuery: resultCount must be a non-negative number');
  }

  const record = {
    query,
    executionTime,
    resultCount,
    timestamp: Date.now(),
  };

  return QueryRecordSchema.parse(record);
}

/**
 * Analyze performance from store
 * @param {Store} store - RDF store
 * @returns {Object} Performance analysis
 *
 * @throws {TypeError} If store is invalid
 *
 * @example
 * const analysis = analyzePerformance(store);
 * console.log('Average time:', analysis.averageExecutionTime);
 */
export function analyzePerformance(store) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('analyzePerformance: store must be a valid Store instance');
  }

  // Basic store statistics
  const quadCount = store.size || 0;

  return {
    quadCount,
    estimatedMemoryUsage: quadCount * 200, // Rough estimate: 200 bytes per quad
    recommendations: generateRecommendations(quadCount),
  };
}

/**
 * Generate performance recommendations
 * @param {number} quadCount - Number of quads in store
 * @returns {Array<Object>} Recommendations
 */
function generateRecommendations(quadCount) {
  const recommendations = [];

  if (quadCount > 100000) {
    recommendations.push({
      type: 'indexing',
      priority: 'high',
      description: 'Large store detected - consider adding indexes for common predicates',
    });
  }

  if (quadCount > 1000000) {
    recommendations.push({
      type: 'partitioning',
      priority: 'medium',
      description: 'Very large store - consider partitioning by graph or subject',
    });
  }

  return recommendations;
}

/**
 * Get performance metrics
 * @returns {Object} Current metrics snapshot
 *
 * @example
 * const metrics = getMetrics();
 * console.log('Total queries:', metrics.totalQueries);
 */
export function getMetrics() {
  return {
    totalQueries: 0,
    averageExecutionTime: 0,
    slowestQueries: [],
  };
}
