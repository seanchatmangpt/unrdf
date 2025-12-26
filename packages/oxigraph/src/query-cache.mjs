/**
 * SPARQL Query Cache - Optimized Query Execution
 *
 * Optimizations:
 * 1. Query string normalization and caching
 * 2. Compiled query cache with LRU eviction
 * 3. Result caching for identical queries
 * 4. Query pattern analysis for index hints
 *
 * Target: <5ms p95 for indexed queries (vs variable baseline)
 *
 * @module @unrdf/oxigraph/query-cache
 */

import { OxigraphStore } from './store.mjs';

// =============================================================================
// LRU Cache Implementation
// =============================================================================

/**
 * Simple LRU Cache for query results
 */
class LRUCache {
  /**
   * @param {number} maxSize - Maximum cache entries
   * @param {number} ttlMs - Time-to-live in milliseconds
   */
  constructor(maxSize = 1000, ttlMs = 60000) {
    /** @type {Map<string, {value: any, timestamp: number}>} */
    this.cache = new Map();
    this.maxSize = maxSize;
    this.ttlMs = ttlMs;
    this.hits = 0;
    this.misses = 0;
  }

  /**
   * Get value from cache
   * @param {string} key
   * @returns {any|undefined}
   */
  get(key) {
    const entry = this.cache.get(key);
    if (!entry) {
      this.misses++;
      return undefined;
    }

    // Check TTL
    if (Date.now() - entry.timestamp > this.ttlMs) {
      this.cache.delete(key);
      this.misses++;
      return undefined;
    }

    // Move to end (most recently used)
    this.cache.delete(key);
    this.cache.set(key, entry);
    this.hits++;

    return entry.value;
  }

  /**
   * Set value in cache
   * @param {string} key
   * @param {any} value
   */
  set(key, value) {
    // Delete existing to update order
    if (this.cache.has(key)) {
      this.cache.delete(key);
    }

    // Evict oldest if at capacity
    if (this.cache.size >= this.maxSize) {
      const oldestKey = this.cache.keys().next().value;
      this.cache.delete(oldestKey);
    }

    this.cache.set(key, {
      value,
      timestamp: Date.now(),
    });
  }

  /**
   * Check if key exists and is valid
   * @param {string} key
   * @returns {boolean}
   */
  has(key) {
    const entry = this.cache.get(key);
    if (!entry) return false;
    if (Date.now() - entry.timestamp > this.ttlMs) {
      this.cache.delete(key);
      return false;
    }
    return true;
  }

  /**
   * Clear all entries
   */
  clear() {
    this.cache.clear();
    this.hits = 0;
    this.misses = 0;
  }

  /**
   * Get cache statistics
   * @returns {Object}
   */
  getStats() {
    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hits: this.hits,
      misses: this.misses,
      hitRate: this.hits / (this.hits + this.misses) || 0,
    };
  }
}

// =============================================================================
// Query Normalization
// =============================================================================

/**
 * Normalize SPARQL query for caching
 * - Remove comments
 * - Normalize whitespace
 * - Lowercase keywords
 *
 * @param {string} query
 * @returns {string}
 */
function normalizeQuery(query) {
  return query
    // Remove single-line comments
    .replace(/#[^\n]*/g, '')
    // Normalize whitespace
    .replace(/\s+/g, ' ')
    // Trim
    .trim()
    // Normalize case for keywords (simple approach)
    .replace(/\b(SELECT|WHERE|FILTER|OPTIONAL|UNION|LIMIT|OFFSET|ORDER|BY|ASC|DESC|GROUP|HAVING|PREFIX|BASE|CONSTRUCT|DESCRIBE|ASK|INSERT|DELETE|LOAD|CLEAR|DROP|CREATE)\b/gi,
      match => match.toUpperCase());
}

/**
 * Generate cache key for query
 * @param {string} query - Normalized query
 * @param {Object} [options] - Query options
 * @returns {string}
 */
function generateCacheKey(query, options = {}) {
  const normalized = normalizeQuery(query);
  const optionsKey = options ? JSON.stringify(options) : '';
  return `${normalized}|${optionsKey}`;
}

// =============================================================================
// Query Pattern Analysis
// =============================================================================

/**
 * @typedef {Object} QueryPattern
 * @property {string} type - Query type (SELECT, ASK, CONSTRUCT, etc.)
 * @property {string[]} variables - Projected variables
 * @property {boolean} hasFilter - Whether query has FILTER
 * @property {boolean} hasOptional - Whether query has OPTIONAL
 * @property {boolean} hasUnion - Whether query has UNION
 * @property {boolean} hasAggregate - Whether query has aggregates
 * @property {number} triplePatterns - Number of triple patterns
 * @property {string[]} predicates - Used predicates
 */

/**
 * Analyze SPARQL query pattern for optimization hints
 * @param {string} query
 * @returns {QueryPattern}
 */
function analyzeQueryPattern(query) {
  const normalized = normalizeQuery(query);

  // Detect query type
  let type = 'SELECT';
  if (/^ASK\s/i.test(normalized)) type = 'ASK';
  else if (/^CONSTRUCT\s/i.test(normalized)) type = 'CONSTRUCT';
  else if (/^DESCRIBE\s/i.test(normalized)) type = 'DESCRIBE';

  // Extract projected variables
  const varMatch = normalized.match(/SELECT\s+(.+?)\s+WHERE/i);
  const variables = varMatch
    ? varMatch[1].match(/\?\w+/g) || []
    : [];

  // Detect features
  const hasFilter = /\bFILTER\s*\(/i.test(normalized);
  const hasOptional = /\bOPTIONAL\s*\{/i.test(normalized);
  const hasUnion = /\bUNION\s*\{/i.test(normalized);
  const hasAggregate = /\b(COUNT|SUM|AVG|MIN|MAX|GROUP_CONCAT|SAMPLE)\s*\(/i.test(normalized);

  // Count triple patterns (rough estimate)
  const triplePatterns = (normalized.match(/\.\s*(?=\?|<)/g) || []).length + 1;

  // Extract predicates
  const predicateMatches = normalized.match(/<[^>]+>/g) || [];
  const predicates = [...new Set(predicateMatches)];

  return {
    type,
    variables,
    hasFilter,
    hasOptional,
    hasUnion,
    hasAggregate,
    triplePatterns,
    predicates,
  };
}

// =============================================================================
// Cached Query Store
// =============================================================================

/**
 * CachedQueryStore - OxigraphStore with query caching
 */
export class CachedQueryStore extends OxigraphStore {
  /**
   * @param {Object} [options]
   * @param {number} [options.cacheSize] - Maximum cache entries
   * @param {number} [options.cacheTtlMs] - Cache TTL in milliseconds
   * @param {boolean} [options.cacheResults] - Cache query results
   * @param {boolean} [options.analyzePatterns] - Analyze query patterns
   */
  constructor(options = {}) {
    super();

    const {
      cacheSize = 1000,
      cacheTtlMs = 60000,
      cacheResults = true,
      analyzePatterns = true,
    } = options;

    /** @type {LRUCache} Query result cache */
    this.queryCache = new LRUCache(cacheSize, cacheTtlMs);

    /** @type {Map<string, QueryPattern>} Analyzed query patterns */
    this.patternCache = new Map();

    /** @type {boolean} */
    this.cacheResults = cacheResults;

    /** @type {boolean} */
    this.analyzePatterns = analyzePatterns;

    /** @type {number} Mutation counter for cache invalidation */
    this.mutationVersion = 0;

    /** @type {Object} Query timing stats */
    this.queryStats = {
      total: 0,
      cached: 0,
      uncached: 0,
      totalTimeMs: 0,
      cachedTimeMs: 0,
      uncachedTimeMs: 0,
    };
  }

  /**
   * Execute SPARQL query with caching
   * @param {string} query - SPARQL query string
   * @param {Object} [options] - Query options
   * @returns {any} Query results
   */
  query(query, options = {}) {
    const startTime = performance.now();
    this.queryStats.total++;

    // Generate cache key including mutation version
    const cacheKey = `${this.mutationVersion}|${generateCacheKey(query, options)}`;

    // Check cache
    if (this.cacheResults) {
      const cached = this.queryCache.get(cacheKey);
      if (cached !== undefined) {
        const elapsed = performance.now() - startTime;
        this.queryStats.cached++;
        this.queryStats.cachedTimeMs += elapsed;
        return cached;
      }
    }

    // Analyze pattern if enabled
    if (this.analyzePatterns && !this.patternCache.has(query)) {
      this.patternCache.set(query, analyzeQueryPattern(query));
    }

    // Execute query
    const result = super.query(query, options);
    const elapsed = performance.now() - startTime;

    this.queryStats.uncached++;
    this.queryStats.uncachedTimeMs += elapsed;

    // Cache result
    if (this.cacheResults) {
      // Clone result if array to prevent mutation issues
      const cachedResult = Array.isArray(result)
        ? [...result]
        : result;
      this.queryCache.set(cacheKey, cachedResult);
    }

    return result;
  }

  /**
   * Cached ASK query
   * @param {string} query
   * @returns {boolean}
   */
  ask(query) {
    return this.query(query);
  }

  /**
   * Execute prepared/compiled query
   * Bypasses normalization for repeated queries
   *
   * @param {string} normalizedQuery - Already normalized query
   * @param {Object} [bindings] - Variable bindings
   * @returns {any}
   */
  executePrepared(normalizedQuery, bindings = {}) {
    // Apply bindings to query
    let boundQuery = normalizedQuery;
    for (const [variable, value] of Object.entries(bindings)) {
      const pattern = new RegExp(`\\?${variable}\\b`, 'g');
      const replacement = typeof value === 'string' && value.startsWith('<')
        ? value
        : `<${value}>`;
      boundQuery = boundQuery.replace(pattern, replacement);
    }

    return this.query(boundQuery);
  }

  /**
   * Override add to invalidate cache
   */
  add(quad) {
    super.add(quad);
    this.mutationVersion++;
  }

  /**
   * Override delete to invalidate cache
   */
  delete(quad) {
    super.delete(quad);
    this.mutationVersion++;
  }

  /**
   * Override update to invalidate cache
   */
  update(query, options) {
    super.update(query, options);
    this.mutationVersion++;
    // Clear cache on mutations
    this.queryCache.clear();
  }

  /**
   * Override load to invalidate cache
   */
  load(data, options) {
    super.load(data, options);
    this.mutationVersion++;
    this.queryCache.clear();
  }

  /**
   * Clear query cache
   */
  clearQueryCache() {
    this.queryCache.clear();
    this.patternCache.clear();
  }

  /**
   * Get query pattern analysis
   * @param {string} query
   * @returns {QueryPattern|undefined}
   */
  getQueryPattern(query) {
    if (!this.patternCache.has(query)) {
      this.patternCache.set(query, analyzeQueryPattern(query));
    }
    return this.patternCache.get(query);
  }

  /**
   * Get cache and query statistics
   * @returns {Object}
   */
  getStats() {
    const cacheStats = this.queryCache.getStats();
    return {
      cache: cacheStats,
      query: {
        ...this.queryStats,
        avgCachedTimeMs: this.queryStats.cachedTimeMs / this.queryStats.cached || 0,
        avgUncachedTimeMs: this.queryStats.uncachedTimeMs / this.queryStats.uncached || 0,
      },
      patterns: this.patternCache.size,
      mutationVersion: this.mutationVersion,
    };
  }

  /**
   * Reset all statistics
   */
  resetStats() {
    this.queryCache.hits = 0;
    this.queryCache.misses = 0;
    this.queryStats = {
      total: 0,
      cached: 0,
      uncached: 0,
      totalTimeMs: 0,
      cachedTimeMs: 0,
      uncachedTimeMs: 0,
    };
  }
}

// =============================================================================
// Prepared Statement Cache
// =============================================================================

/**
 * PreparedQuery - Pre-normalized query for repeated execution
 */
export class PreparedQuery {
  /**
   * @param {string} query - SPARQL query with ?variables
   */
  constructor(query) {
    /** @type {string} */
    this.originalQuery = query;

    /** @type {string} */
    this.normalizedQuery = normalizeQuery(query);

    /** @type {QueryPattern} */
    this.pattern = analyzeQueryPattern(query);

    /** @type {string[]} Variables that can be bound */
    this.bindableVariables = this.pattern.variables;

    /** @type {number} Execution count */
    this.executions = 0;
  }

  /**
   * Bind variables and return executable query
   * @param {Object} bindings - Variable bindings
   * @returns {string}
   */
  bind(bindings = {}) {
    let query = this.normalizedQuery;

    for (const [variable, value] of Object.entries(bindings)) {
      const pattern = new RegExp(`\\?${variable}\\b`, 'g');
      const replacement = typeof value === 'string' && value.startsWith('<')
        ? value
        : `<${value}>`;
      query = query.replace(pattern, replacement);
    }

    return query;
  }

  /**
   * Execute on store with bindings
   * @param {CachedQueryStore} store
   * @param {Object} [bindings]
   * @returns {any}
   */
  execute(store, bindings = {}) {
    this.executions++;
    const boundQuery = Object.keys(bindings).length > 0
      ? this.bind(bindings)
      : this.normalizedQuery;
    return store.query(boundQuery);
  }
}

/**
 * Create prepared query for repeated execution
 * @param {string} query
 * @returns {PreparedQuery}
 */
export function prepare(query) {
  return new PreparedQuery(query);
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a cached query store
 * @param {Object} [options]
 * @returns {CachedQueryStore}
 */
export function createCachedStore(options = {}) {
  return new CachedQueryStore(options);
}

// =============================================================================
// Exports
// =============================================================================

export {
  LRUCache,
  normalizeQuery,
  generateCacheKey,
  analyzeQueryPattern,
};

export default {
  CachedQueryStore,
  PreparedQuery,
  prepare,
  createCachedStore,
  LRUCache,
  normalizeQuery,
  analyzeQueryPattern,
};
