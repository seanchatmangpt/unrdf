/** Size-bounded TTL/LRU cache with injectable clock. */
export class BoundedCache {
  #entries = new Map();

  /**
   * Create a cache.
   * @param {Object} [options] - Cache options.
   * @param {number} [options.maxSize] - Maximum entries before the least recently used is evicted (positive integer).
   * @param {number} [options.ttlMs] - Default time-to-live in milliseconds; 0 means entries never expire.
   * @param {Function} [options.now] - Clock returning the current time in milliseconds.
   * @throws {TypeError} If `maxSize` or `ttlMs` is invalid.
   */
  constructor({ maxSize = 1000, ttlMs = 60000, now = () => Date.now() } = {}) {
    if (!Number.isInteger(maxSize) || maxSize <= 0)
      throw new TypeError('maxSize must be a positive integer');
    if (!Number.isFinite(ttlMs) || ttlMs < 0) throw new TypeError('ttlMs must be non-negative');
    this.maxSize = maxSize;
    this.ttlMs = ttlMs;
    this.now = now;
    this.stats = { hits: 0, misses: 0, evictions: 0, expirations: 0 };
  }

  /**
   * Store a value, marking it most recently used and evicting the oldest entries beyond `maxSize`.
   * @param {*} key - Cache key.
   * @param {*} value - Value to store.
   * @param {number} [ttlMs] - TTL for this entry in milliseconds; 0 means no expiry. Defaults to the cache TTL.
   * @returns {BoundedCache} This cache, for chaining.
   */
  set(key, value, ttlMs = this.ttlMs) {
    if (this.#entries.has(key)) this.#entries.delete(key);
    this.#entries.set(key, { value, expiresAt: ttlMs === 0 ? Infinity : this.now() + ttlMs });
    while (this.#entries.size > this.maxSize) {
      const oldest = this.#entries.keys().next().value;
      this.#entries.delete(oldest);
      this.stats.evictions++;
    }
    return this;
  }

  /**
   * Look up a live value, refreshing its recency and updating hit/miss/expiration stats. Expired entries are removed.
   * @param {*} key - Cache key.
   * @returns {*} The stored value, or undefined on a miss or expiry.
   */
  get(key) {
    const entry = this.#entries.get(key);
    if (!entry) {
      this.stats.misses++;
      return undefined;
    }
    if (entry.expiresAt <= this.now()) {
      this.#entries.delete(key);
      this.stats.expirations++;
      this.stats.misses++;
      return undefined;
    }
    this.#entries.delete(key);
    this.#entries.set(key, entry);
    this.stats.hits++;
    return entry.value;
  }

  /**
   * Check whether a live value exists for the key (counts as a get for stats and recency).
   * @param {*} key - Cache key.
   * @returns {boolean} True if a defined value is cached.
   */
  has(key) {
    return this.get(key) !== undefined;
  }
  /**
   * Remove an entry without touching stats.
   * @param {*} key - Cache key.
   * @returns {boolean} True if an entry was removed.
   */
  delete(key) {
    return this.#entries.delete(key);
  }
  /**
   * Remove all entries (stats are kept).
   */
  clear() {
    this.#entries.clear();
  }
  /**
   * Number of stored entries, including any expired ones not yet touched.
   * @returns {number} Entry count.
   */
  get size() {
    return this.#entries.size;
  }
  /**
   * Capture cache configuration, a copy of the stats, and keys in LRU-to-MRU order.
   * @returns {{size: number, maxSize: number, ttlMs: number, stats: Object, keys: Array}} Snapshot object.
   */
  snapshot() {
    return {
      size: this.size,
      maxSize: this.maxSize,
      ttlMs: this.ttlMs,
      stats: { ...this.stats },
      keys: [...this.#entries.keys()],
    };
  }
}

/**
 * Create a BoundedCache.
 * @param {Object} [options] - Options forwarded to the BoundedCache constructor.
 * @returns {BoundedCache} A new cache.
 */
export function createBoundedCache(options) {
  return new BoundedCache(options);
}
