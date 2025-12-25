/**
 * Snapshot Cache - In-Memory LRU Cache for Time-Travel
 *
 * Optimizations:
 * 1. LRU cache for recent snapshots
 * 2. Lazy loading with prefetch
 * 3. Memory-efficient serialization
 * 4. Background cache warming
 *
 * Target: <10ms p95 for cached time-travel (vs 100ms+ git checkout)
 *
 * @module @unrdf/kgc-4d/snapshot-cache
 */

import { blake3 } from 'hash-wasm';

// =============================================================================
// LRU Cache Implementation
// =============================================================================

/**
 * LRU Cache for snapshot data
 * Uses Map for O(1) access with LRU eviction
 */
export class SnapshotLRUCache {
  /**
   * @param {Object} options
   * @param {number} [options.maxSize] - Maximum entries
   * @param {number} [options.maxMemoryMB] - Maximum memory in MB
   * @param {number} [options.ttlMs] - Entry TTL in milliseconds
   */
  constructor(options = {}) {
    const {
      maxSize = 100,
      maxMemoryMB = 256,
      ttlMs = 3600000, // 1 hour default
    } = options;

    /** @type {Map<string, CacheEntry>} */
    this.cache = new Map();

    /** @type {number} */
    this.maxSize = maxSize;

    /** @type {number} */
    this.maxMemoryBytes = maxMemoryMB * 1024 * 1024;

    /** @type {number} */
    this.ttlMs = ttlMs;

    /** @type {number} */
    this.currentMemoryBytes = 0;

    /** @type {Object} Statistics */
    this.stats = {
      hits: 0,
      misses: 0,
      evictions: 0,
      loads: 0,
    };
  }

  /**
   * Get entry from cache
   * @param {string} key - Timestamp or git ref
   * @returns {Object|undefined} Cached store or undefined
   */
  get(key) {
    const entry = this.cache.get(key);

    if (!entry) {
      this.stats.misses++;
      return undefined;
    }

    // Check TTL
    if (Date.now() - entry.timestamp > this.ttlMs) {
      this._evict(key, entry);
      this.stats.misses++;
      return undefined;
    }

    // Move to end (most recently used)
    this.cache.delete(key);
    entry.accessCount++;
    entry.lastAccess = Date.now();
    this.cache.set(key, entry);

    this.stats.hits++;
    return entry.data;
  }

  /**
   * Set entry in cache
   * @param {string} key - Timestamp or git ref
   * @param {Object} data - Store state to cache
   * @param {Object} [metadata] - Additional metadata
   */
  set(key, data, metadata = {}) {
    // Calculate entry size
    const serialized = this._serialize(data);
    const sizeBytes = serialized.length;

    // Delete existing entry if present
    if (this.cache.has(key)) {
      const existing = this.cache.get(key);
      this.currentMemoryBytes -= existing.sizeBytes;
      this.cache.delete(key);
    }

    // Evict entries if needed
    this._evictIfNeeded(sizeBytes);

    // Create cache entry
    const entry = {
      data,
      serialized,
      sizeBytes,
      timestamp: Date.now(),
      lastAccess: Date.now(),
      accessCount: 1,
      metadata,
    };

    this.cache.set(key, entry);
    this.currentMemoryBytes += sizeBytes;
    this.stats.loads++;
  }

  /**
   * Check if key exists
   * @param {string} key
   * @returns {boolean}
   */
  has(key) {
    const entry = this.cache.get(key);
    if (!entry) return false;

    // Check TTL
    if (Date.now() - entry.timestamp > this.ttlMs) {
      this._evict(key, entry);
      return false;
    }

    return true;
  }

  /**
   * Delete entry
   * @param {string} key
   */
  delete(key) {
    const entry = this.cache.get(key);
    if (entry) {
      this._evict(key, entry);
    }
  }

  /**
   * Clear all entries
   */
  clear() {
    this.cache.clear();
    this.currentMemoryBytes = 0;
    this.stats.evictions = 0;
  }

  /**
   * Get cache statistics
   * @returns {Object}
   */
  getStats() {
    return {
      ...this.stats,
      size: this.cache.size,
      maxSize: this.maxSize,
      memoryUsedMB: this.currentMemoryBytes / (1024 * 1024),
      maxMemoryMB: this.maxMemoryBytes / (1024 * 1024),
      hitRate: this.stats.hits / (this.stats.hits + this.stats.misses) || 0,
    };
  }

  /**
   * Get all cached keys
   * @returns {string[]}
   */
  keys() {
    return [...this.cache.keys()];
  }

  /**
   * Serialize store for caching
   * @param {Object} data
   * @returns {string}
   */
  _serialize(data) {
    // If data has a dump method, use it
    if (typeof data.dump === 'function') {
      try {
        return data.dump({ format: 'application/n-quads' });
      } catch {
        // Fall back to JSON
      }
    }

    // If data has quads, serialize them
    if (typeof data.match === 'function') {
      try {
        const quads = [...data.match(null, null, null, null)];
        return JSON.stringify(quads.map(q => ({
          s: q.subject?.value,
          p: q.predicate?.value,
          o: q.object?.value,
          g: q.graph?.value,
        })));
      } catch {
        // Fall back to JSON
      }
    }

    // Default: JSON stringify
    return JSON.stringify(data);
  }

  /**
   * Evict entries until space is available
   * @param {number} neededBytes
   */
  _evictIfNeeded(neededBytes) {
    // Evict by count
    while (this.cache.size >= this.maxSize) {
      const oldestKey = this.cache.keys().next().value;
      const oldestEntry = this.cache.get(oldestKey);
      this._evict(oldestKey, oldestEntry);
    }

    // Evict by memory
    while (this.currentMemoryBytes + neededBytes > this.maxMemoryBytes && this.cache.size > 0) {
      const oldestKey = this.cache.keys().next().value;
      const oldestEntry = this.cache.get(oldestKey);
      this._evict(oldestKey, oldestEntry);
    }
  }

  /**
   * Evict a single entry
   * @param {string} key
   * @param {Object} entry
   */
  _evict(key, entry) {
    this.cache.delete(key);
    this.currentMemoryBytes -= entry.sizeBytes;
    this.stats.evictions++;
  }
}

// =============================================================================
// Cached Snapshot Manager
// =============================================================================

/**
 * CachedSnapshotManager - Manages snapshot caching for time-travel
 */
export class CachedSnapshotManager {
  /**
   * @param {Object} options
   * @param {Object} options.gitBackbone - Git backbone for loading snapshots
   * @param {Object} [options.cacheOptions] - LRU cache options
   * @param {boolean} [options.enablePrefetch] - Enable prefetch of adjacent snapshots
   */
  constructor(options = {}) {
    const {
      gitBackbone,
      cacheOptions = {},
      enablePrefetch = true,
    } = options;

    if (!gitBackbone) {
      throw new Error('gitBackbone is required');
    }

    /** @type {Object} */
    this.gitBackbone = gitBackbone;

    /** @type {SnapshotLRUCache} */
    this.cache = new SnapshotLRUCache(cacheOptions);

    /** @type {boolean} */
    this.enablePrefetch = enablePrefetch;

    /** @type {Map<string, Promise>} Pending loads to prevent duplicates */
    this.pendingLoads = new Map();

    /** @type {string[]} Snapshot refs in chronological order */
    this.snapshotIndex = [];
  }

  /**
   * Get snapshot from cache or load from git
   *
   * @param {string} gitRef - Git reference
   * @returns {Promise<string>} N-Quads content
   */
  async getSnapshot(gitRef) {
    // Check cache
    const cached = this.cache.get(gitRef);
    if (cached !== undefined) {
      // Prefetch adjacent snapshots in background
      if (this.enablePrefetch) {
        this._prefetchAdjacent(gitRef);
      }
      return cached;
    }

    // Check if load is already in progress
    if (this.pendingLoads.has(gitRef)) {
      return this.pendingLoads.get(gitRef);
    }

    // Load from git
    const loadPromise = this._loadFromGit(gitRef);
    this.pendingLoads.set(gitRef, loadPromise);

    try {
      const data = await loadPromise;
      this.cache.set(gitRef, data, { loadedAt: Date.now() });

      // Prefetch adjacent
      if (this.enablePrefetch) {
        this._prefetchAdjacent(gitRef);
      }

      return data;
    } finally {
      this.pendingLoads.delete(gitRef);
    }
  }

  /**
   * Load snapshot from git
   * @param {string} gitRef
   * @returns {Promise<string>}
   */
  async _loadFromGit(gitRef) {
    return this.gitBackbone.readSnapshot(gitRef);
  }

  /**
   * Prefetch adjacent snapshots in background
   * @param {string} currentRef
   */
  _prefetchAdjacent(currentRef) {
    const index = this.snapshotIndex.indexOf(currentRef);
    if (index === -1) return;

    // Prefetch previous and next
    const prefetchRefs = [];
    if (index > 0) prefetchRefs.push(this.snapshotIndex[index - 1]);
    if (index < this.snapshotIndex.length - 1) prefetchRefs.push(this.snapshotIndex[index + 1]);

    for (const ref of prefetchRefs) {
      if (!this.cache.has(ref) && !this.pendingLoads.has(ref)) {
        // Fire and forget prefetch
        this.getSnapshot(ref).catch(() => {});
      }
    }
  }

  /**
   * Update snapshot index for prefetch optimization
   * @param {string[]} refs - Snapshot refs in chronological order
   */
  updateSnapshotIndex(refs) {
    this.snapshotIndex = refs;
  }

  /**
   * Preload multiple snapshots
   *
   * @param {string[]} gitRefs - Git references to preload
   * @returns {Promise<{loaded: number, failed: number}>}
   */
  async preloadSnapshots(gitRefs) {
    const results = await Promise.allSettled(
      gitRefs.map(ref => this.getSnapshot(ref))
    );

    const loaded = results.filter(r => r.status === 'fulfilled').length;
    const failed = results.filter(r => r.status === 'rejected').length;

    return { loaded, failed };
  }

  /**
   * Invalidate cached snapshot
   * @param {string} gitRef
   */
  invalidate(gitRef) {
    this.cache.delete(gitRef);
  }

  /**
   * Clear all cached snapshots
   */
  clearCache() {
    this.cache.clear();
    this.pendingLoads.clear();
  }

  /**
   * Get cache statistics
   * @returns {Object}
   */
  getStats() {
    return {
      cache: this.cache.getStats(),
      pendingLoads: this.pendingLoads.size,
      indexedSnapshots: this.snapshotIndex.length,
    };
  }
}

// =============================================================================
// Optimized reconstructState with Caching
// =============================================================================

/**
 * Reconstruct state at timestamp with snapshot caching
 *
 * @param {Object} store - KGC Store
 * @param {CachedSnapshotManager} snapshotManager - Cached snapshot manager
 * @param {bigint} targetTime - Target nanosecond timestamp
 * @param {Object} dataFactory - RDF data factory
 * @returns {Promise<Object>} Reconstructed store
 */
export async function reconstructStateWithCache(store, snapshotManager, targetTime, dataFactory) {
  const startTime = performance.now();

  // Find best snapshot before target time (using store's event log)
  const bestSnapshot = await findBestSnapshot(store, targetTime, dataFactory);

  if (!bestSnapshot) {
    throw new Error(`No snapshot found before time ${targetTime}`);
  }

  // Get snapshot from cache (or load from git)
  const snapshotNQuads = await snapshotManager.getSnapshot(bestSnapshot.git_ref);

  // Create temporary store from snapshot
  const TempStore = store.constructor;
  const tempStore = new TempStore();

  await tempStore.load(snapshotNQuads, {
    format: 'application/n-quads',
    graph: 'http://kgc.io/Universe',
  });

  // Replay events between snapshot and target
  await replayEvents(store, tempStore, bestSnapshot.t_ns, targetTime, dataFactory);

  const elapsed = performance.now() - startTime;

  // Add timing metadata
  tempStore._reconstructionTime = elapsed;
  tempStore._sourceSnapshot = bestSnapshot.git_ref;

  return tempStore;
}

/**
 * Find best snapshot before target time
 * @param {Object} store
 * @param {bigint} targetTime
 * @param {Object} dataFactory
 * @returns {Promise<{git_ref: string, t_ns: bigint}|null>}
 */
async function findBestSnapshot(store, targetTime, dataFactory) {
  const eventLogGraph = dataFactory.namedNode('http://kgc.io/EventLog');
  const typePredi = dataFactory.namedNode('http://kgc.io/type');
  const tNsPredi = dataFactory.namedNode('http://kgc.io/t_ns');
  const gitRefPredi = dataFactory.namedNode('http://kgc.io/git_ref');

  // Find SNAPSHOT events
  const snapshotQuads = [...store.match(
    null,
    typePredi,
    dataFactory.literal('SNAPSHOT'),
    eventLogGraph
  )];

  if (snapshotQuads.length === 0) {
    return null;
  }

  let bestSnapshot = null;
  let bestTime = 0n;

  for (const quad of snapshotQuads) {
    const subject = quad.subject;
    const timeQuads = [...store.match(subject, tNsPredi, null, eventLogGraph)];
    const gitRefQuads = [...store.match(subject, gitRefPredi, null, eventLogGraph)];

    if (timeQuads.length > 0 && gitRefQuads.length > 0) {
      const time = BigInt(timeQuads[0].object.value);
      if (time <= targetTime && time > bestTime) {
        bestTime = time;
        bestSnapshot = {
          t_ns: time,
          git_ref: gitRefQuads[0].object.value,
        };
      }
    }
  }

  return bestSnapshot;
}

/**
 * Replay events between two timestamps
 * @param {Object} sourceStore - Store with event log
 * @param {Object} targetStore - Store to apply events to
 * @param {bigint} fromTime - Start time (exclusive)
 * @param {bigint} toTime - End time (inclusive)
 * @param {Object} dataFactory
 */
async function replayEvents(sourceStore, targetStore, fromTime, toTime, dataFactory) {
  const eventLogGraph = dataFactory.namedNode('http://kgc.io/EventLog');
  const tNsPredi = dataFactory.namedNode('http://kgc.io/t_ns');
  const payloadPredi = dataFactory.namedNode('http://kgc.io/payload');
  const universeGraph = 'http://kgc.io/Universe';

  // Get all event times
  const allTimeQuads = [...sourceStore.match(null, tNsPredi, null, eventLogGraph)];

  // Filter events in range
  const eventsToReplay = [];
  for (const timeQuad of allTimeQuads) {
    const eventTime = BigInt(timeQuad.object.value);
    if (eventTime > fromTime && eventTime <= toTime) {
      eventsToReplay.push({
        subject: timeQuad.subject,
        t_ns: eventTime,
      });
    }
  }

  // Sort by time
  eventsToReplay.sort((a, b) => {
    if (a.t_ns < b.t_ns) return -1;
    if (a.t_ns > b.t_ns) return 1;
    return 0;
  });

  // Apply each event's deltas
  for (const event of eventsToReplay) {
    const payloadQuads = [...sourceStore.match(event.subject, payloadPredi, null, eventLogGraph)];

    if (payloadQuads.length > 0) {
      try {
        const payload = JSON.parse(payloadQuads[0].object.value);
        const deltas = payload.deltas || [];

        for (const delta of deltas) {
          const quad = deltaToQuad(delta, universeGraph, dataFactory);

          if (delta.type === 'add') {
            targetStore.add(quad);
          } else if (delta.type === 'delete') {
            targetStore.delete(quad);
          }
        }
      } catch {
        // Skip malformed payloads
        continue;
      }
    }
  }
}

/**
 * Convert delta to quad
 * @param {Object} delta
 * @param {string} graphUri
 * @param {Object} dataFactory
 * @returns {Object}
 */
function deltaToQuad(delta, graphUri, dataFactory) {
  const subject = delta.subjectType === 'BlankNode'
    ? dataFactory.blankNode(delta.subject)
    : dataFactory.namedNode(delta.subject);

  const predicate = dataFactory.namedNode(delta.predicate);

  let object;
  if (delta.object.type === 'Literal') {
    if (delta.object.language) {
      object = dataFactory.literal(delta.object.value, delta.object.language);
    } else if (delta.object.datatype) {
      object = dataFactory.literal(delta.object.value, dataFactory.namedNode(delta.object.datatype));
    } else {
      object = dataFactory.literal(delta.object.value);
    }
  } else if (delta.object.type === 'BlankNode') {
    object = dataFactory.blankNode(delta.object.value);
  } else {
    object = dataFactory.namedNode(delta.object.value);
  }

  return dataFactory.quad(subject, predicate, object, dataFactory.namedNode(graphUri));
}

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * Create a snapshot cache with default options
 * @param {Object} options
 * @returns {SnapshotLRUCache}
 */
export function createSnapshotCache(options = {}) {
  return new SnapshotLRUCache(options);
}

/**
 * Create a cached snapshot manager
 * @param {Object} options
 * @returns {CachedSnapshotManager}
 */
export function createCachedSnapshotManager(options) {
  return new CachedSnapshotManager(options);
}

// =============================================================================
// Exports
// =============================================================================

export default {
  SnapshotLRUCache,
  CachedSnapshotManager,
  reconstructStateWithCache,
  createSnapshotCache,
  createCachedSnapshotManager,
};
