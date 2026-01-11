/**
 * @file History Reconstructor - Receipt-based State Reconstruction
 * @module @unrdf/kgc-4d/history-reconstructor
 * @description Efficiently reconstruct RDF graph state at any historical point
 *
 * Uses snapshot-cache and delta replay for optimal performance.
 * Performance target: <100ms for 1000 receipts
 *
 * @example
 * import { HistoryReconstructor } from './history-reconstructor.mjs';
 *
 * const reconstructor = new HistoryReconstructor(store, git);
 * const pastStore = await reconstructor.reconstructAtTime(targetTime);
 */

import { reconstructState } from './freeze.mjs';

/**
 * History reconstructor with optimized snapshot caching
 *
 * Minimizes expensive delta replays by caching intermediate snapshots
 * and choosing optimal reconstruction paths.
 */
export class HistoryReconstructor {
  /**
   * Create history reconstructor
   *
   * @param {Object} store - KGCStore instance
   * @param {Object} git - GitBackbone instance
   * @param {Object} [options] - Reconstruction options
   * @param {number} [options.cacheSize=100] - Max snapshots to cache
   * @param {boolean} [options.enableCache=true] - Enable snapshot caching
   *
   * @example
   * const reconstructor = new HistoryReconstructor(store, git, { cacheSize: 50 });
   */
  constructor(store, git, options = {}) {
    if (!store || typeof store.match !== 'function') {
      throw new TypeError('HistoryReconstructor: store must be a valid KGCStore instance');
    }
    if (!git || typeof git.commitSnapshot !== 'function') {
      throw new TypeError('HistoryReconstructor: git must be a valid GitBackbone instance');
    }

    this.store = store;
    this.git = git;

    // Simple Map-based cache for reconstructed stores
    const cacheSize = options.cacheSize || 100;
    const enableCache = options.enableCache !== false;

    this.enableCache = enableCache;
    this.maxCacheSize = cacheSize;
    this.snapshotCache = enableCache ? new Map() : null;
    this.cacheOrder = enableCache ? [] : null;

    // Metrics
    this.reconstructions = 0;
    this.cacheHits = 0;
    this.cacheMisses = 0;
  }

  /**
   * Reconstruct state at specific time
   *
   * Uses snapshot cache if available, otherwise falls back to full reconstruction
   *
   * @param {bigint} targetTime - Target time in nanoseconds
   * @returns {Promise<Object>} Reconstructed store at target time
   *
   * @example
   * const pastStore = await reconstructor.reconstructAtTime(123456789n);
   * const results = await pastStore.query('SELECT * WHERE { ?s ?p ?o }');
   */
  async reconstructAtTime(targetTime) {
    if (typeof targetTime !== 'bigint') {
      throw new TypeError('reconstructAtTime: targetTime must be a bigint');
    }

    this.reconstructions++;

    // Check cache first
    if (this.snapshotCache) {
      const cacheKey = targetTime.toString();
      const cached = this.snapshotCache.get(cacheKey);
      if (cached) {
        this.cacheHits++;
        // Update access order
        this.cacheOrder = this.cacheOrder.filter(k => k !== cacheKey);
        this.cacheOrder.push(cacheKey);
        return cached;
      }
      this.cacheMisses++;
    }

    // Cache miss - reconstruct from receipts
    const reconstructed = await reconstructState(this.store, this.git, targetTime);

    // Cache result for future queries
    if (this.snapshotCache) {
      const cacheKey = targetTime.toString();

      // Evict LRU if at capacity
      if (this.snapshotCache.size >= this.maxCacheSize) {
        const lruKey = this.cacheOrder.shift();
        this.snapshotCache.delete(lruKey);
      }

      this.snapshotCache.set(cacheKey, reconstructed);
      this.cacheOrder.push(cacheKey);
    }

    return reconstructed;
  }

  /**
   * Reconstruct state at multiple time points
   *
   * Optimized batch reconstruction that reuses snapshots where possible
   *
   * @param {Array<bigint>} timePoints - Array of timestamps in nanoseconds
   * @returns {Promise<Array<Object>>} Array of reconstructed stores
   *
   * @example
   * const stores = await reconstructor.reconstructAtTimes([t1, t2, t3]);
   */
  async reconstructAtTimes(timePoints) {
    if (!Array.isArray(timePoints)) {
      throw new TypeError('reconstructAtTimes: timePoints must be an array');
    }

    if (timePoints.length === 0) {
      return [];
    }

    // Sort time points for optimal cache utilization
    const sorted = [...timePoints].sort((a, b) => {
      if (a < b) return -1;
      if (a > b) return 1;
      return 0;
    });

    // Reconstruct in order
    const results = [];
    for (const time of sorted) {
      const store = await this.reconstructAtTime(time);
      results.push(store);
    }

    return results;
  }

  /**
   * Get reconstruction statistics
   *
   * @returns {Object} Reconstruction metrics
   *
   * @example
   * const stats = reconstructor.getStats();
   * console.log(`Cache hit rate: ${stats.cacheHitRate}%`);
   */
  getStats() {
    const total = this.cacheHits + this.cacheMisses;
    const cacheHitRate = total > 0 ? (this.cacheHits / total) * 100 : 0;

    return {
      reconstructions: this.reconstructions,
      cacheHits: this.cacheHits,
      cacheMisses: this.cacheMisses,
      cacheHitRate: cacheHitRate.toFixed(2),
      cacheSize: this.getCacheSize(),
      cacheEnabled: this.snapshotCache !== null,
    };
  }

  /**
   * Reset reconstruction statistics
   *
   * @example
   * reconstructor.resetStats();
   */
  resetStats() {
    this.reconstructions = 0;
    this.cacheHits = 0;
    this.cacheMisses = 0;
  }

  /**
   * Clear snapshot cache
   *
   * @example
   * reconstructor.clearCache();
   */
  clearCache() {
    if (this.snapshotCache) {
      this.snapshotCache.clear();
    }
  }

  /**
   * Prefetch snapshots for time range
   *
   * Useful for preloading snapshots before running multiple queries
   *
   * @param {bigint} startTime - Start time in nanoseconds
   * @param {bigint} endTime - End time in nanoseconds
   * @param {number} [sampleCount=10] - Number of snapshots to prefetch
   * @returns {Promise<void>}
   *
   * @example
   * await reconstructor.prefetch(startTime, endTime, 20);
   */
  async prefetch(startTime, endTime, sampleCount = 10) {
    if (typeof startTime !== 'bigint' || typeof endTime !== 'bigint') {
      throw new TypeError('prefetch: startTime and endTime must be bigints');
    }

    if (startTime >= endTime) {
      throw new Error('prefetch: startTime must be before endTime');
    }

    // Generate sample times
    const interval = (endTime - startTime) / BigInt(sampleCount);
    const sampleTimes = [];

    for (let i = 0; i < sampleCount; i++) {
      sampleTimes.push(startTime + (interval * BigInt(i)));
    }
    sampleTimes.push(endTime);

    // Prefetch all
    await this.reconstructAtTimes(sampleTimes);
  }

  /**
   * Get current cache size
   *
   * @returns {number} Number of cached entries
   *
   * @example
   * const size = reconstructor.getCacheSize();
   */
  getCacheSize() {
    return this.snapshotCache ? this.snapshotCache.size : 0;
  }
}
