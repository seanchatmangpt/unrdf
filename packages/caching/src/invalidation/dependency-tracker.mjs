/**
 * @file Dependency Tracker - Smart cache invalidation with dependency tracking
 * @module @unrdf/caching/invalidation
 *
 * Tracks dependencies between cached queries and automatically invalidates
 * dependent caches when data changes.
 *
 * Features:
 * - Subject-based dependency tracking
 * - Cascade invalidation
 * - Graph-aware invalidation
 * - Query pattern analysis
 *
 * @example
 * import { DependencyTracker } from '@unrdf/caching/invalidation';
 *
 * const tracker = new DependencyTracker(cache);
 *
 * // Track dependencies
 * tracker.trackQuery('query1', ['subject1', 'subject2']);
 *
 * // Invalidate all queries depending on subject1
 * await tracker.invalidateSubject('subject1');
 */

import { z } from 'zod';

// =============================================================================
// SCHEMAS & TYPES
// =============================================================================

/**
 * Dependency tracker configuration
 */
const TrackerConfigSchema = z.object({
  cache: z.any(), // MultiLayerCache instance
  enableGraphTracking: z.boolean().default(true),
  maxDependencies: z.number().int().positive().default(10000),
});

/**
 * @typedef {z.infer<typeof TrackerConfigSchema>} TrackerConfig
 */

/**
 * Dependency statistics
 * @typedef {Object} DependencyStats
 * @property {number} totalQueries - Total tracked queries
 * @property {number} totalSubjects - Total tracked subjects
 * @property {number} invalidations - Total invalidations performed
 * @property {number} cascadeInvalidations - Cascade invalidations
 */

// =============================================================================
// DEPENDENCY TRACKER
// =============================================================================

/**
 * Dependency tracker for intelligent cache invalidation
 */
export class DependencyTracker {
  /**
   * Create a new dependency tracker
   * @param {any} cache - MultiLayerCache instance
   * @param {Partial<TrackerConfig>} config - Tracker configuration
   */
  constructor(cache, config = {}) {
    const validated = TrackerConfigSchema.parse({ ...config, cache });

    this.cache = validated.cache;
    this.config = validated;

    // Query → Subjects mapping
    // Map<queryKey, Set<subjectUri>>
    this.queryDependencies = new Map();

    // Subject → Queries mapping (reverse index)
    // Map<subjectUri, Set<queryKey>>
    this.subjectQueries = new Map();

    // Graph → Subjects mapping
    // Map<graphUri, Set<subjectUri>>
    this.graphSubjects = new Map();

    // Statistics
    this.stats = {
      totalQueries: 0,
      totalSubjects: 0,
      invalidations: 0,
      cascadeInvalidations: 0,
    };
  }

  /**
   * Track query dependencies
   * @param {string} queryKey - Cache key for query
   * @param {Array<string>} subjects - Subject URIs the query depends on
   * @param {string} [graph] - Named graph URI
   */
  trackQuery(queryKey, subjects, graph = null) {
    // Track query → subjects
    if (!this.queryDependencies.has(queryKey)) {
      this.queryDependencies.set(queryKey, new Set());
      this.stats.totalQueries++;
    }

    const querySubjects = this.queryDependencies.get(queryKey);

    for (const subject of subjects) {
      // Add to query → subjects
      querySubjects.add(subject);

      // Add to subject → queries (reverse index)
      if (!this.subjectQueries.has(subject)) {
        this.subjectQueries.set(subject, new Set());
        this.stats.totalSubjects++;
      }
      this.subjectQueries.get(subject).add(queryKey);

      // Track graph → subjects if enabled
      if (this.config.enableGraphTracking && graph) {
        if (!this.graphSubjects.has(graph)) {
          this.graphSubjects.set(graph, new Set());
        }
        this.graphSubjects.get(graph).add(subject);
      }
    }

    // Enforce max dependencies limit
    this._enforceLimit();
  }

  /**
   * Invalidate all queries depending on a subject
   * @param {string} subject - Subject URI
   * @returns {Promise<number>} Number of queries invalidated
   */
  async invalidateSubject(subject) {
    const queryKeys = this.subjectQueries.get(subject);

    if (!queryKeys || queryKeys.size === 0) {
      return 0;
    }

    let invalidated = 0;

    for (const queryKey of queryKeys) {
      await this.cache.delete(queryKey);
      this._removeQuery(queryKey);
      invalidated++;
    }

    this.stats.invalidations += invalidated;
    return invalidated;
  }

  /**
   * Invalidate all queries in a graph
   * @param {string} graph - Named graph URI
   * @returns {Promise<number>} Number of queries invalidated
   */
  async invalidateGraph(graph) {
    if (!this.config.enableGraphTracking) {
      throw new Error('Graph tracking is disabled');
    }

    const subjects = this.graphSubjects.get(graph);

    if (!subjects || subjects.size === 0) {
      return 0;
    }

    let invalidated = 0;

    for (const subject of subjects) {
      invalidated += await this.invalidateSubject(subject);
    }

    // Clear graph mapping
    this.graphSubjects.delete(graph);

    this.stats.cascadeInvalidations += invalidated;
    return invalidated;
  }

  /**
   * Invalidate queries matching a subject pattern
   * @param {string} pattern - Subject URI pattern (glob-style with *)
   * @returns {Promise<number>} Number of queries invalidated
   */
  async invalidatePattern(pattern) {
    const regex = new RegExp(
      '^' + pattern.replace(/\*/g, '.*') + '$'
    );

    let invalidated = 0;

    for (const subject of this.subjectQueries.keys()) {
      if (regex.test(subject)) {
        invalidated += await this.invalidateSubject(subject);
      }
    }

    this.stats.cascadeInvalidations += invalidated;
    return invalidated;
  }

  /**
   * Invalidate multiple subjects at once
   * @param {Array<string>} subjects - Subject URIs
   * @returns {Promise<number>} Total queries invalidated
   */
  async invalidateSubjects(subjects) {
    let total = 0;

    for (const subject of subjects) {
      total += await this.invalidateSubject(subject);
    }

    return total;
  }

  /**
   * Remove query from tracking
   * @private
   * @param {string} queryKey - Query cache key
   */
  _removeQuery(queryKey) {
    const subjects = this.queryDependencies.get(queryKey);

    if (subjects) {
      // Remove from reverse index
      for (const subject of subjects) {
        const queries = this.subjectQueries.get(subject);
        if (queries) {
          queries.delete(queryKey);

          // Clean up empty sets
          if (queries.size === 0) {
            this.subjectQueries.delete(subject);
            this.stats.totalSubjects--;
          }
        }
      }

      // Remove query
      this.queryDependencies.delete(queryKey);
      this.stats.totalQueries--;
    }
  }

  /**
   * Enforce maximum dependencies limit
   * @private
   */
  _enforceLimit() {
    if (this.queryDependencies.size > this.config.maxDependencies) {
      // Remove oldest 10% of queries (simple FIFO)
      const toRemove = Math.floor(this.config.maxDependencies * 0.1);
      let removed = 0;

      for (const [queryKey] of this.queryDependencies) {
        if (removed >= toRemove) break;

        this._removeQuery(queryKey);
        removed++;
      }
    }
  }

  /**
   * Get dependencies for a query
   * @param {string} queryKey - Query cache key
   * @returns {Array<string>} Subject URIs
   */
  getQueryDependencies(queryKey) {
    const subjects = this.queryDependencies.get(queryKey);
    return subjects ? Array.from(subjects) : [];
  }

  /**
   * Get queries depending on a subject
   * @param {string} subject - Subject URI
   * @returns {Array<string>} Query cache keys
   */
  getSubjectQueries(subject) {
    const queries = this.subjectQueries.get(subject);
    return queries ? Array.from(queries) : [];
  }

  /**
   * Get all tracked subjects in a graph
   * @param {string} graph - Named graph URI
   * @returns {Array<string>} Subject URIs
   */
  getGraphSubjects(graph) {
    const subjects = this.graphSubjects.get(graph);
    return subjects ? Array.from(subjects) : [];
  }

  /**
   * Get tracker statistics
   * @returns {DependencyStats} Current statistics
   */
  getStats() {
    return { ...this.stats };
  }

  /**
   * Clear all tracked dependencies
   */
  clear() {
    this.queryDependencies.clear();
    this.subjectQueries.clear();
    this.graphSubjects.clear();

    this.stats = {
      totalQueries: 0,
      totalSubjects: 0,
      invalidations: 0,
      cascadeInvalidations: 0,
    };
  }
}

/**
 * Extract subjects from SPARQL query
 * @param {string} query - SPARQL query string
 * @returns {Array<string>} Extracted subject URIs
 */
export function extractQuerySubjects(query) {
  const subjects = new Set();

  // Extract URIs from angle brackets
  const uriMatches = query.matchAll(/<([^>]+)>/g);
  for (const match of uriMatches) {
    subjects.add(match[1]);
  }

  // Extract prefixed names (simplified)
  const prefixedMatches = query.matchAll(/\b([a-z]+):([a-zA-Z0-9_-]+)\b/g);
  for (const match of prefixedMatches) {
    // Would need prefix resolution in real implementation
    subjects.add(`${match[1]}:${match[2]}`);
  }

  return Array.from(subjects);
}

/**
 * Create a dependency tracker instance
 * @param {any} cache - MultiLayerCache instance
 * @param {Partial<TrackerConfig>} config - Tracker configuration
 * @returns {DependencyTracker} Tracker instance
 */
export function createDependencyTracker(cache, config = {}) {
  return new DependencyTracker(cache, config);
}

export default {
  DependencyTracker,
  createDependencyTracker,
  extractQuerySubjects,
};
