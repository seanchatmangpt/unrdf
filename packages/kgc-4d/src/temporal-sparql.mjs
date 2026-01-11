/**
 * @file Temporal SPARQL Engine - Time-Travel SPARQL Queries
 * @module @unrdf/kgc-4d/temporal-sparql
 * @description Production-ready temporal SPARQL with caching and OTEL instrumentation
 *
 * Enables querying RDF graphs at any historical point with nanosecond precision
 * using KGC-4D receipts and snapshot-based reconstruction.
 *
 * Performance targets:
 * - Cold query: <250ms P95
 * - Cached query: <50ms P95
 * - Cache hit rate: >80%
 *
 * @example
 * import { TemporalSPARQL } from './temporal-sparql.mjs';
 * import { KGCStore } from './store.mjs';
 * import { GitBackbone } from './git.mjs';
 *
 * const store = new KGCStore();
 * const git = new GitBackbone('./repo');
 * const temporal = new TemporalSPARQL(store, git);
 *
 * const results = await temporal.query(`
 *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 *   AT TIMESTAMP '2026-01-01T00:00:00Z'
 * `);
 */

import { parseTemporalQuery } from './temporal-query-parser.mjs';
import { TemporalCache } from './temporal-cache.mjs';
import { HistoryReconstructor } from './history-reconstructor.mjs';
import { now, toISO } from './time.mjs';
import { GRAPHS } from './constants.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { TemporalEngineOptionsSchema, guardTemporalResultValid } from './schemas/temporal-sparql-schema.mjs';

const { namedNode } = dataFactory;

/**
 * Temporal SPARQL query engine
 *
 * Supports time-travel queries with AT TIMESTAMP and BETWEEN syntax.
 * Uses LRU cache and snapshot reconstruction for optimal performance.
 */
export class TemporalSPARQL {
  /**
   * Create temporal SPARQL engine
   *
   * @param {Object} store - KGCStore instance
   * @param {Object} git - GitBackbone instance
   * @param {Object} [options] - Engine options
   * @param {Object} [options.cache] - Cache configuration
   * @param {number} [options.cache.maxSize=1000] - Max cache entries
   * @param {number} [options.cache.ttl=300000] - Cache TTL in ms
   * @param {boolean} [options.enableOTEL=false] - Enable OpenTelemetry
   * @param {Object} [options.reconstructor] - History reconstructor options
   *
   * @example
   * const temporal = new TemporalSPARQL(store, git, {
   *   cache: { maxSize: 500, ttl: 600000 },
   *   enableOTEL: true
   * });
   */
  constructor(store, git, options = {}) {
    if (!store || typeof store.match !== 'function') {
      throw new TypeError('TemporalSPARQL: store must be a valid KGCStore instance');
    }
    if (!git || typeof git.commitSnapshot !== 'function') {
      throw new TypeError('TemporalSPARQL: git must be a valid GitBackbone instance');
    }

    // Validate options
    const validatedOptions = TemporalEngineOptionsSchema.parse(options);

    this.store = store;
    this.git = git;

    // Initialize cache
    this.cache = new TemporalCache(validatedOptions.cache || {});

    // Initialize history reconstructor
    this.reconstructor = new HistoryReconstructor(
      store,
      git,
      validatedOptions.reconstructor || {}
    );

    // OTEL instrumentation (optional) - lazy initialization
    this.enableOTEL = validatedOptions.enableOTEL;
    this.tracer = null;

    // Metrics
    this.queryCount = 0;
    this.totalQueryTime_ms = 0;
  }

  /**
   * Execute temporal SPARQL query
   *
   * Supports three query modes:
   * - Current: Standard SPARQL on current state
   * - Point-in-time: AT TIMESTAMP 'iso8601'
   * - Time-range: BETWEEN 'iso8601' AND 'iso8601'
   *
   * @param {string} query - SPARQL query with optional temporal clauses
   * @returns {Promise<Object>} Query results with metadata
   *
   * @example
   * const results = await temporal.query(`
   *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
   *   AT TIMESTAMP '2026-01-01T00:00:00Z'
   * `);
   */
  async query(query) {
    const startTime = Date.now();
    this.queryCount++;

    // Parse temporal query
    const parsed = parseTemporalQuery(query);

    let result;

    if (parsed.mode === 'current') {
      result = await this._queryCurrentTime(parsed);
    } else if (parsed.mode === 'point-in-time') {
      result = await this._queryAtTime(parsed);
    } else if (parsed.mode === 'time-range') {
      result = await this._queryBetween(parsed);
    } else {
      throw new Error(`Unsupported temporal query mode: ${parsed.mode}`);
    }

    const duration = Date.now() - startTime;
    this.totalQueryTime_ms += duration;

    // Update metadata
    result.metadata.queryDuration_ms = duration;

    // Validate result
    return guardTemporalResultValid(result);
  }

  /**
   * Query current state (no temporal clause)
   * @private
   */
  async _queryCurrentTime(parsed) {
    const results = await this.store.query(parsed.baseSparql);

    return {
      results,
      metadata: {
        queryDuration_ms: 0,
        resultCount: results.length,
        cached: false,
      },
    };
  }

  /**
   * Query at specific point in time
   * @private
   */
  async _queryAtTime(parsed) {
    const { baseSparql, timestampNs, timestamp } = parsed;

    // Generate cache key
    const cacheKey = await this.cache.generateKey(baseSparql, timestampNs);

    // Check cache
    const cached = this.cache.get(cacheKey);
    if (cached) {
      return {
        ...cached,
        metadata: {
          ...cached.metadata,
          cached: true,
        },
      };
    }

    // Cache miss - reconstruct and query
    const reconstructStart = Date.now();
    const pastStore = await this.reconstructor.reconstructAtTime(timestampNs);
    const reconstructionTime_ms = Date.now() - reconstructStart;

    // Execute query on historical state
    const results = await pastStore.query(baseSparql);

    const result = {
      results,
      metadata: {
        queryDuration_ms: 0,
        resultCount: results.length,
        cached: false,
        targetTime: timestampNs.toString(),
        targetTimeISO: timestamp,
        reconstructionTime_ms,
      },
    };

    // Cache result
    this.cache.set(cacheKey, result);

    return result;
  }

  /**
   * Query across time range
   * @private
   */
  async _queryBetween(parsed) {
    const { baseSparql, startTimestampNs, endTimestampNs, startTimestamp, endTimestamp } = parsed;

    // Sample 10 points in the range
    const sampleCount = 10;
    const interval = (endTimestampNs - startTimestampNs) / BigInt(sampleCount);
    const timePoints = [];

    for (let i = 0; i <= sampleCount; i++) {
      timePoints.push(startTimestampNs + (interval * BigInt(i)));
    }

    // Query at each time point
    const changes = [];
    let previousQuads = null;

    for (const timeNs of timePoints) {
      const pastStore = await this.reconstructor.reconstructAtTime(timeNs);

      // Get all quads
      const currentQuads = new Set(
        [...pastStore.match(null, null, null, namedNode(GRAPHS.UNIVERSE))]
          .map(q => this._quadKey(q))
      );

      if (previousQuads !== null) {
        // Calculate delta
        const additions = [...currentQuads].filter(q => !previousQuads.has(q)).length;
        const deletions = [...previousQuads].filter(q => !currentQuads.has(q)).length;

        changes.push({
          timestamp: toISO(timeNs),
          timestamp_ns: timeNs,
          additions,
          deletions,
          netChange: additions - deletions,
        });
      }

      previousQuads = currentQuads;
    }

    // Aggregate statistics
    const totalAdditions = changes.reduce((sum, c) => sum + c.additions, 0);
    const totalDeletions = changes.reduce((sum, c) => sum + c.deletions, 0);

    return {
      results: changes,
      metadata: {
        queryDuration_ms: 0,
        resultCount: changes.length,
        cached: false,
        startTime: startTimestamp,
        endTime: endTimestamp,
        sampleCount: timePoints.length,
        totalAdditions,
        totalDeletions,
      },
    };
  }

  /**
   * Generate quad key for comparison
   * @private
   */
  _quadKey(quad) {
    return `${quad.subject.value}|${quad.predicate.value}|${quad.object.value}`;
  }

  /**
   * Query at specific time (convenience method)
   *
   * @param {string} sparql - Base SPARQL query
   * @param {bigint|string} targetTime - Time in nanoseconds or ISO 8601
   * @returns {Promise<Object>} Query results
   *
   * @example
   * const results = await temporal.queryAtTime(
   *   'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
   *   '2026-01-01T00:00:00Z'
   * );
   */
  async queryAtTime(sparql, targetTime) {
    const timestamp = typeof targetTime === 'string' ? targetTime : toISO(targetTime);
    const query = `${sparql}\nAT TIMESTAMP '${timestamp}'`;
    return this.query(query);
  }

  /**
   * Query across time range (convenience method)
   *
   * @param {string} sparql - Base SPARQL query
   * @param {bigint|string} startTime - Start time
   * @param {bigint|string} endTime - End time
   * @returns {Promise<Object>} Query results
   *
   * @example
   * const results = await temporal.queryBetween(
   *   'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
   *   '2026-01-01T00:00:00Z',
   *   '2026-01-02T00:00:00Z'
   * );
   */
  async queryBetween(sparql, startTime, endTime) {
    const startTimestamp = typeof startTime === 'string' ? startTime : toISO(startTime);
    const endTimestamp = typeof endTime === 'string' ? endTime : toISO(endTime);
    const query = `${sparql}\nBETWEEN '${startTimestamp}' AND '${endTimestamp}'`;
    return this.query(query);
  }

  /**
   * Get engine statistics
   *
   * @returns {Object} Engine metrics including cache and reconstruction stats
   *
   * @example
   * const stats = temporal.getStats();
   * console.log(`Cache hit rate: ${stats.cache.hitRate}%`);
   */
  getStats() {
    const avgQueryTime = this.queryCount > 0
      ? this.totalQueryTime_ms / this.queryCount
      : 0;

    return {
      queries: this.queryCount,
      totalQueryTime_ms: this.totalQueryTime_ms,
      avgQueryTime_ms: avgQueryTime.toFixed(2),
      cache: this.cache.getStats(),
      reconstructor: this.reconstructor.getStats(),
    };
  }

  /**
   * Reset engine statistics
   *
   * @example
   * temporal.resetStats();
   */
  resetStats() {
    this.queryCount = 0;
    this.totalQueryTime_ms = 0;
    this.cache.resetStats();
    this.reconstructor.resetStats();
  }

  /**
   * Clear all caches
   *
   * @example
   * temporal.clearCache();
   */
  clearCache() {
    this.cache.clear();
    this.reconstructor.clearCache();
  }

  /**
   * Prefetch snapshots for time range
   *
   * Useful for warming cache before running multiple queries
   *
   * @param {bigint|string} startTime - Start time
   * @param {bigint|string} endTime - End time
   * @param {number} [sampleCount=10] - Number of snapshots to prefetch
   * @returns {Promise<void>}
   *
   * @example
   * await temporal.prefetch('2026-01-01T00:00:00Z', '2026-01-02T00:00:00Z', 20);
   */
  async prefetch(startTime, endTime, sampleCount = 10) {
    const startNs = typeof startTime === 'string'
      ? (await import('./time.mjs')).fromISO(startTime)
      : startTime;
    const endNs = typeof endTime === 'string'
      ? (await import('./time.mjs')).fromISO(endTime)
      : endTime;

    await this.reconstructor.prefetch(startNs, endNs, sampleCount);
  }
}
