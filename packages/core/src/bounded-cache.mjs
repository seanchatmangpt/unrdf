/** Size-bounded TTL/LRU cache with injectable clock. */
export class BoundedCache {
  #entries = new Map();

  constructor({ maxSize = 1000, ttlMs = 60000, now = () => Date.now() } = {}) {
    if (!Number.isInteger(maxSize) || maxSize <= 0) throw new TypeError('maxSize must be a positive integer');
    if (!Number.isFinite(ttlMs) || ttlMs < 0) throw new TypeError('ttlMs must be non-negative');
    this.maxSize = maxSize;
    this.ttlMs = ttlMs;
    this.now = now;
    this.stats = { hits: 0, misses: 0, evictions: 0, expirations: 0 };
  }

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

  get(key) {
    const entry = this.#entries.get(key);
    if (!entry) { this.stats.misses++; return undefined; }
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

  has(key) { return this.get(key) !== undefined; }
  delete(key) { return this.#entries.delete(key); }
  clear() { this.#entries.clear(); }
  get size() { return this.#entries.size; }
  snapshot() { return { size: this.size, maxSize: this.maxSize, ttlMs: this.ttlMs, stats: { ...this.stats }, keys: [...this.#entries.keys()] }; }
}

export function createBoundedCache(options) { return new BoundedCache(options); }
