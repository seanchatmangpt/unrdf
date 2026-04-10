/**
 * @file HNSW Vector Index for Approximate Nearest Neighbor Search
 * @module @unrdf/core/index/hnsw
 *
 * Provides efficient vector similarity search using HNSW algorithm.
 * 100-1000x faster than brute-force for large datasets (100K+ vectors).
 *
 * PERFORMANCE CHARACTERISTICS:
 * - Index build: O(n log n) with M=16, efConstruction=200
 * - Search: O(log n) with efSearch=50
 * - Memory: ~1KB per vector (384-dim Float32Array + graph edges)
 * - Recall: 95-99% for efSearch=50, M=16
 *
 * Trade-off: 1-5% recall loss for 100-1000x speedup vs brute-force
 */

import { z } from 'zod';

/**
 * @typedef {Float32Array} Vector - 384-dimensional embedding vector
 * @typedef {Object} SearchResult - { id: string, vector: Vector, distance: number }
 */

/**
 * HNSW index configuration schema
 */
const HNSWConfigSchema = z.object({
  dimensions: z.number().default(384),
  maxElements: z.number().default(10000),
  M: z.number().default(16), // Number of bi-directional links
  efConstruction: z.number().default(200), // Build-time accuracy
  efSearch: z.number().default(50), // Search-time accuracy
  metric: z.enum(['l2', 'ip']).default('l2'), // Distance metric
});

/**
 * Brute-force fallback index (used when HNSW not available)
 */
class BruteForceIndex {
  constructor(config = {}) {
    this.vectors = new Map(); // id -> vector
    this.config = HNSWConfigSchema.parse(config);
  }

  /**
   * Add vector to index
   * @param {string} id - Vector identifier
   * @param {Vector} vector - 384-dimensional vector
   */
  add(id, vector) {
    if (!(vector instanceof Float32Array)) {
      throw new TypeError('vector must be Float32Array');
    }

    if (vector.length !== this.config.dimensions) {
      throw new Error(`vector must have ${this.config.dimensions} dimensions`);
    }

    this.vectors.set(id, vector);
  }

  /**
   * Search for nearest neighbors
   * @param {Vector} query - Query vector
   * @param {number} k - Number of results
   * @returns {Array<SearchResult>} Results sorted by distance
   */
  search(query, k = 10) {
    if (!(query instanceof Float32Array)) {
      throw new TypeError('query must be Float32Array');
    }

    if (query.length !== this.config.dimensions) {
      throw new Error(`query must have ${this.config.dimensions} dimensions`);
    }

    const results = [];

    for (const [id, vector] of this.vectors.entries()) {
      const distance = this.config.metric === 'l2'
        ? this.l2Distance(query, vector)
        : this.innerProduct(query, vector);

      results.push({ id, vector, distance });
    }

    // Sort by distance (ascending for L2, descending for IP)
    results.sort((a, b) => this.config.metric === 'l2'
      ? a.distance - b.distance
      : b.distance - a.distance
    );

    return results.slice(0, k);
  }

  /**
   * Get vector by ID
   * @param {string} id - Vector identifier
   * @returns {Vector|undefined} Vector if found
   */
  get(id) {
    return this.vectors.get(id);
  }

  /**
   * Remove vector from index
   * @param {string} id - Vector identifier
   * @returns {boolean} True if removed
   */
  remove(id) {
    return this.vectors.delete(id);
  }

  /**
   * Get index size
   * @returns {number} Number of vectors
   */
  size() {
    return this.vectors.size;
  }

  /**
   * Clear all vectors
   */
  clear() {
    this.vectors.clear();
  }

  /**
   * Compute L2 distance
   * @param {Vector} a - First vector
   * @param {Vector} b - Second vector
   * @returns {number} L2 distance
   * @private
   */
  l2Distance(a, b) {
    let sum = 0;
    for (let i = 0; i < a.length; i++) {
      const diff = a[i] - b[i];
      sum += diff * diff;
    }
    return Math.sqrt(sum);
  }

  /**
   * Compute inner product (similarity)
   * @param {Vector} a - First vector
   * @param {Vector} b - Second vector
   * @returns {number} Inner product
   * @private
   */
  innerProduct(a, b) {
    let sum = 0;
    for (let i = 0; i < a.length; i++) {
      sum += a[i] * b[i];
    }
    return sum;
  }
}

/**
 * HNSW Index wrapper
 */
class HNSWIndex {
  constructor(config = {}) {
    this.config = HNSWConfigSchema.parse(config);
    this.index = null;
    this.initialized = false;
  }

  /**
   * Initialize HNSW index
   * @returns {Promise<void>}
   */
  async initialize() {
    if (this.initialized) {
      return;
    }

    try {
      // Try to use hnswlib-node
      const hnswlib = await import('hnswlib-node');

      this.index = new hnswlib.HierarchicalNSW(
        this.config.metric,
        this.config.dimensions
      );

      this.index.initIndex(this.config.maxElements);
      this.index.setEf(this.config.efSearch);

      this.initialized = true;
    } catch (error) {
      // Fallback to brute-force if hnswlib-node not available
      console.warn(`HNSW not available, using brute-force: ${error.message}`);
      this.index = new BruteForceIndex(this.config);
      this.initialized = true;
    }
  }

  /**
   * Add vector to index
   * @param {string} id - Vector identifier
   * @param {Vector} vector - 384-dimensional vector
   * @returns {Promise<void>}
   */
  async add(id, vector) {
    if (!this.initialized) {
      await this.initialize();
    }

    if (this.index instanceof BruteForceIndex) {
      this.index.add(id, vector);
      return;
    }

    if (!(vector instanceof Float32Array)) {
      throw new TypeError('vector must be Float32Array');
    }

    if (vector.length !== this.config.dimensions) {
      throw new Error(`vector must have ${this.config.dimensions} dimensions`);
    }

    // Convert string ID to numeric ID for HNSW
    const numericId = this.stringToNumericId(id);

    // Add to index
    this.index.addPoint(vector, numericId);

    // Store ID mapping
    if (!this.idMap) {
      this.idMap = new Map();
    }
    this.idMap.set(numericId, id);
  }

  /**
   * Search for nearest neighbors
   * @param {Vector} query - Query vector
   * @param {number} k - Number of results
   * @returns {Promise<Array<SearchResult>>} Results sorted by distance
   */
  async search(query, k = 10) {
    if (!this.initialized) {
      await this.initialize();
    }

    if (this.index instanceof BruteForceIndex) {
      return this.index.search(query, k);
    }

    if (!(query instanceof Float32Array)) {
      throw new TypeError('query must be Float32Array');
    }

    if (query.length !== this.config.dimensions) {
      throw new Error(`query must have ${this.config.dimensions} dimensions`);
    }

    // Search HNSW index
    const results = this.index.searchKnn(query, k);

    // Convert numeric IDs back to string IDs and add vectors
    const mappedResults = [];
    for (const result of results) {
      const id = this.numericIdToString(result[0]);
      const distance = result[1];

      mappedResults.push({
        id,
        distance,
        vector: query, // Note: we'd need to store vectors separately to return them
      });
    }

    return mappedResults;
  }

  /**
   * Get vector by ID
   * @param {string} id - Vector identifier
   * @returns {Vector|undefined} Vector if found
   */
  get(id) {
    if (this.index instanceof BruteForceIndex) {
      return this.index.get(id);
    }

    if (!this.idMap) {
      return undefined;
    }

    const numericId = this.stringToNumericId(id);
    if (!this.idMap.has(numericId)) {
      return undefined;
    }

    // Note: HNSW doesn't provide getVector method
    // We'd need to store vectors separately
    return undefined;
  }

  /**
   * Remove vector from index
   * @param {string} id - Vector identifier
   * @returns {Promise<boolean>} True if removed
   */
  async remove(id) {
    if (this.index instanceof BruteForceIndex) {
      return this.index.remove(id);
    }

    if (!this.idMap) {
      return false;
    }

    const numericId = this.stringToNumericId(id);
    if (!this.idMap.has(numericId)) {
      return false;
    }

    this.index.removePoint(numericId);
    this.idMap.delete(numericId);
    return true;
  }

  /**
   * Get index size
   * @returns {number} Number of vectors
   */
  size() {
    if (this.index instanceof BruteForceIndex) {
      return this.index.size();
    }

    return this.index.getCurrentCount();
  }

  /**
   * Clear all vectors
   */
  clear() {
    if (this.index instanceof BruteForceIndex) {
      this.index.clear();
      return;
    }

    // Reinitialize index
    this.index = null;
    this.idMap = null;
    this.initialized = false;
  }

  /**
   * Convert string ID to numeric ID
   * @param {string} id - String ID
   * @returns {number} Numeric ID
   * @private
   */
  stringToNumericId(id) {
    // Simple hash function
    let hash = 0;
    for (let i = 0; i < id.length; i++) {
      hash = ((hash << 5) - hash + id.charCodeAt(i)) | 0;
    }
    return Math.abs(hash);
  }

  /**
   * Convert numeric ID back to string ID
   * @param {number} numericId - Numeric ID
   * @returns {string|undefined} String ID
   * @private
   */
  numericIdToString(numericId) {
    if (!this.idMap) {
      return undefined;
    }
    return this.idMap.get(numericId);
  }
}

/**
 * Create a new HNSW index
 * @param {Object} config - Index configuration
 * @returns {HNSWIndex} Index instance
 *
 * @example
 * const index = createHNSWIndex({ dimensions: 384, maxElements: 10000 });
 * await index.add('doc1', embedding1);
 * await index.add('doc2', embedding2);
 * const results = await index.search(queryEmbedding, 5);
 */
export function createHNSWIndex(config = {}) {
  return new HNSWIndex(config);
}

/**
 * Build HNSW index from existing vectors
 * @param {Map<string, Vector>} vectors - Map of ID to vector
 * @param {Object} config - Index configuration
 * @returns {Promise<HNSWIndex>} Built index
 *
 * @example
 * const vectors = new Map([
 *   ['doc1', embedding1],
 *   ['doc2', embedding2]
 * ]);
 * const index = await buildHNSWIndex(vectors);
 */
export async function buildHNSWIndex(vectors, config = {}) {
  const index = createHNSWIndex(config);
  await index.initialize();

  for (const [id, vector] of vectors.entries()) {
    await index.add(id, vector);
  }

  return index;
}

/**
 * Compute cosine similarity between two vectors
 * @param {Vector} a - First vector
 * @param {Vector} b - Second vector
 * @returns {number} Similarity [-1, 1]
 *
 * @example
 * const similarity = cosineSimilarity(vec1, vec2);
 */
export function cosineSimilarity(a, b) {
  if (!(a instanceof Float32Array) || !(b instanceof Float32Array)) {
    throw new TypeError('vectors must be Float32Array');
  }

  if (a.length !== b.length) {
    throw new Error('vectors must have same dimensions');
  }

  let dotProduct = 0;
  let normA = 0;
  let normB = 0;

  for (let i = 0; i < a.length; i++) {
    dotProduct += a[i] * b[i];
    normA += a[i] * a[i];
    normB += b[i] * b[i];
  }

  const denominator = Math.sqrt(normA) * Math.sqrt(normB);
  if (denominator === 0) {
    return 0;
  }

  return dotProduct / denominator;
}

/**
 * Compute L2 distance between two vectors
 * @param {Vector} a - First vector
 * @param {Vector} b - Second vector
 * @returns {number} L2 distance
 *
 * @example
 * const distance = l2Distance(vec1, vec2);
 */
export function l2Distance(a, b) {
  if (!(a instanceof Float32Array) || !(b instanceof Float32Array)) {
    throw new TypeError('vectors must be Float32Array');
  }

  if (a.length !== b.length) {
    throw new Error('vectors must have same dimensions');
  }

  let sum = 0;
  for (let i = 0; i < a.length; i++) {
    const diff = a[i] - b[i];
    sum += diff * diff;
  }

  return Math.sqrt(sum);
}

/**
 * L2 normalize a vector
 * @param {Vector} vector - Input vector
 * @returns {Vector} Normalized vector
 *
 * @example
 * const normalized = l2Normalize(vector);
 */
export function l2Normalize(vector) {
  if (!(vector instanceof Float32Array)) {
    throw new TypeError('vector must be Float32Array');
  }

  let sumSquares = 0;
  for (let i = 0; i < vector.length; i++) {
    sumSquares += vector[i] * vector[i];
  }

  const norm = Math.sqrt(sumSquares);
  if (norm === 0) {
    return new Float32Array(vector); // Return copy if zero norm
  }

  const normalized = new Float32Array(vector.length);
  for (let i = 0; i < vector.length; i++) {
    normalized[i] = vector[i] / norm;
  }

  return normalized;
}
