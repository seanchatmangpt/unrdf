/**
 * @file Comprehensive tests for @unrdf/caching package
 * @module @unrdf/caching/test
 *
 * Tests MultiLayerCache, DependencyTracker, SparqlCache, and integration.
 * All external dependencies (Redis, Oxigraph store) are mocked.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';

// Mock ioredis before imports
vi.mock('ioredis', () => {
  const RedisMock = vi.fn().mockImplementation(() => ({
    connect: vi.fn().mockResolvedValue(undefined),
    getBuffer: vi.fn().mockResolvedValue(null),
    setex: vi.fn().mockResolvedValue('OK'),
    del: vi.fn().mockResolvedValue(1),
    scan: vi.fn().mockResolvedValue(['0', []]),
    quit: vi.fn().mockResolvedValue('OK'),
  }));
  return { default: RedisMock };
});

// Mock msgpackr
vi.mock('msgpackr', () => ({
  pack: vi.fn((v) => Buffer.from(JSON.stringify(v))),
  unpack: vi.fn((v) => JSON.parse(v.toString())),
}));

import {
  MultiLayerCache,
  createMultiLayerCache,
  DependencyTracker,
  createDependencyTracker,
  extractQuerySubjects,
  SparqlCache,
  createSparqlCache,
  createCachingSystem,
} from '../src/index.mjs';

// =============================================================================
// MultiLayerCache Tests
// =============================================================================

describe('MultiLayerCache', () => {
  /** @type {MultiLayerCache} */
  let cache;

  beforeEach(() => {
    cache = new MultiLayerCache({ enableL2: false });
  });

  afterEach(async () => {
    await cache.close();
  });

  it('should create cache with default options', () => {
    // Arrange & Act
    const defaultCache = new MultiLayerCache({ enableL2: false });

    // Assert
    expect(defaultCache).toBeInstanceOf(MultiLayerCache);
    expect(defaultCache.config.l1MaxSize).toBe(1000);
    expect(defaultCache.config.l1TtlMs).toBe(60000);
    expect(defaultCache.config.l2TtlSeconds).toBe(300);
    expect(defaultCache.config.keyPrefix).toBe('unrdf:cache:');
  });

  it('should set and get a value (L1 hit)', async () => {
    // Arrange
    const key = 'test-key';
    const value = { data: 'hello world' };

    // Act
    await cache.set(key, value);
    const result = await cache.get(key);

    // Assert
    expect(result).toEqual(value);
    expect(cache.stats.l1Hits).toBe(1);
    expect(cache.stats.sets).toBe(1);
  });

  it('should return null on cache miss', async () => {
    // Arrange & Act
    const result = await cache.get('nonexistent-key');

    // Assert
    expect(result).toBeNull();
    expect(cache.stats.l1Misses).toBe(1);
  });

  it('should use fetcher on cache miss and populate cache', async () => {
    // Arrange
    const fetchedValue = { fetched: true };
    const fetcher = vi.fn().mockResolvedValue(fetchedValue);

    // Act
    const result = await cache.get('fetcher-key', fetcher);

    // Assert
    expect(result).toEqual(fetchedValue);
    expect(fetcher).toHaveBeenCalledOnce();
    expect(cache.stats.l3Hits).toBe(1);

    // Value should now be in L1
    const cached = await cache.get('fetcher-key');
    expect(cached).toEqual(fetchedValue);
    expect(cache.stats.l1Hits).toBe(1);
  });

  it('should track L1 size correctly after set and delete', async () => {
    // Arrange & Act
    await cache.set('key1', 'val1');
    await cache.set('key2', 'val2');
    expect(cache.l1.size).toBe(2);

    await cache.delete('key1');

    // Assert
    expect(cache.l1.size).toBe(1);
    expect(cache.stats.deletes).toBe(1);
    const result = await cache.get('key1');
    expect(result).toBeNull();
  });

  it('should clear all layers', async () => {
    // Arrange
    await cache.set('a', 1);
    await cache.set('b', 2);
    await cache.set('c', 3);
    expect(cache.l1.size).toBe(3);

    // Act
    await cache.clear();

    // Assert
    expect(cache.l1.size).toBe(0);
    expect(cache.stats.l1Size).toBe(0);
  });

  it('should delete keys matching a pattern', async () => {
    // Arrange
    await cache.set('sparql:abc', 'result1');
    await cache.set('sparql:def', 'result2');
    await cache.set('other:key', 'result3');

    // Act
    const deleted = await cache.deletePattern('sparql:*');

    // Assert
    expect(deleted).toBe(2);
    const sparqlResult = await cache.get('sparql:abc');
    expect(sparqlResult).toBeNull();
    const otherResult = await cache.get('other:key');
    expect(otherResult).toBe('result3');
  });

  it('should report statistics with hit rates', async () => {
    // Arrange
    await cache.set('key', 'value');
    await cache.get('key'); // L1 hit
    await cache.get('miss'); // L1 miss

    // Act
    const stats = cache.getStats();

    // Assert
    expect(stats.l1Hits).toBe(1);
    expect(stats.l1Misses).toBe(1);
    expect(stats.l1HitRate).toBe(0.5);
    expect(stats.sets).toBe(1);
  });

  it('should reset statistics', async () => {
    // Arrange
    await cache.set('key', 'value');
    await cache.get('key');
    expect(cache.stats.l1Hits).toBe(1);

    // Act
    cache.resetStats();

    // Assert
    expect(cache.stats.l1Hits).toBe(0);
    expect(cache.stats.l1Misses).toBe(0);
    expect(cache.stats.sets).toBe(0);
  });
});

describe('createMultiLayerCache', () => {
  it('should create a MultiLayerCache instance via factory', () => {
    // Act
    const cache = createMultiLayerCache({ enableL2: false });

    // Assert
    expect(cache).toBeInstanceOf(MultiLayerCache);
  });
});

// =============================================================================
// DependencyTracker Tests
// =============================================================================

describe('DependencyTracker', () => {
  /** @type {DependencyTracker} */
  let tracker;
  let mockCache;

  beforeEach(() => {
    mockCache = {
      delete: vi.fn().mockResolvedValue(undefined),
      deletePattern: vi.fn().mockResolvedValue(0),
    };
    tracker = new DependencyTracker(mockCache);
  });

  it('should create tracker with default config', () => {
    // Assert
    expect(tracker).toBeInstanceOf(DependencyTracker);
    expect(tracker.config.enableGraphTracking).toBe(true);
    expect(tracker.config.maxDependencies).toBe(10000);
  });

  it('should track dependency between query and subjects', () => {
    // Arrange & Act
    tracker.trackQuery('query1', ['http://ex.org/s1', 'http://ex.org/s2']);

    // Assert
    const deps = tracker.getQueryDependencies('query1');
    expect(deps).toEqual(['http://ex.org/s1', 'http://ex.org/s2']);
    expect(tracker.getStats().totalQueries).toBe(1);
    expect(tracker.getStats().totalSubjects).toBe(2);
  });

  it('should return affected queries when invalidating by subject', async () => {
    // Arrange
    tracker.trackQuery('query1', ['http://ex.org/s1']);
    tracker.trackQuery('query2', ['http://ex.org/s1', 'http://ex.org/s2']);
    tracker.trackQuery('query3', ['http://ex.org/s2']);

    // Act
    const invalidated = await tracker.invalidateSubject('http://ex.org/s1');

    // Assert
    expect(invalidated).toBe(2);
    expect(mockCache.delete).toHaveBeenCalledTimes(2);
    expect(tracker.getStats().invalidations).toBe(2);
  });

  it('should handle multiple dependencies for the same query', () => {
    // Arrange & Act
    tracker.trackQuery('query1', ['http://ex.org/s1']);
    tracker.trackQuery('query1', ['http://ex.org/s2', 'http://ex.org/s3']);

    // Assert
    const deps = tracker.getQueryDependencies('query1');
    expect(deps).toContain('http://ex.org/s1');
    expect(deps).toContain('http://ex.org/s2');
    expect(deps).toContain('http://ex.org/s3');
    expect(deps).toHaveLength(3);
  });

  it('should return 0 when invalidating a subject with no dependents', async () => {
    // Act
    const invalidated = await tracker.invalidateSubject('http://ex.org/unknown');

    // Assert
    expect(invalidated).toBe(0);
    expect(mockCache.delete).not.toHaveBeenCalled();
  });

  it('should track graph-level dependencies', () => {
    // Arrange & Act
    tracker.trackQuery('query1', ['http://ex.org/s1'], 'http://ex.org/graph1');

    // Assert
    const graphSubjects = tracker.getGraphSubjects('http://ex.org/graph1');
    expect(graphSubjects).toContain('http://ex.org/s1');
  });

  it('should invalidate by graph', async () => {
    // Arrange
    tracker.trackQuery('query1', ['http://ex.org/s1'], 'http://ex.org/graph1');
    tracker.trackQuery('query2', ['http://ex.org/s1'], 'http://ex.org/graph1');

    // Act
    const invalidated = await tracker.invalidateGraph('http://ex.org/graph1');

    // Assert
    expect(invalidated).toBe(2);
  });

  it('should clear all dependencies', () => {
    // Arrange
    tracker.trackQuery('q1', ['http://ex.org/s1']);
    tracker.trackQuery('q2', ['http://ex.org/s2']);

    // Act
    tracker.clear();

    // Assert
    expect(tracker.getStats().totalQueries).toBe(0);
    expect(tracker.getStats().totalSubjects).toBe(0);
    expect(tracker.getQueryDependencies('q1')).toEqual([]);
  });
});

describe('extractQuerySubjects', () => {
  it('should extract URIs from angle brackets in SPARQL', () => {
    // Arrange
    const query = 'SELECT * WHERE { <http://ex.org/s1> <http://ex.org/p1> ?o }';

    // Act
    const subjects = extractQuerySubjects(query);

    // Assert
    expect(subjects).toContain('http://ex.org/s1');
    expect(subjects).toContain('http://ex.org/p1');
  });

  it('should extract prefixed names from SPARQL', () => {
    // Arrange
    const query = 'SELECT * WHERE { ex:alice foaf:knows ?person }';

    // Act
    const subjects = extractQuerySubjects(query);

    // Assert
    expect(subjects).toContain('ex:alice');
    expect(subjects).toContain('foaf:knows');
  });

  it('should return empty array for query with only variables', () => {
    // Arrange
    const query = 'SELECT * WHERE { ?s ?p ?o }';

    // Act
    const subjects = extractQuerySubjects(query);

    // Assert
    expect(subjects).toHaveLength(0);
  });
});

describe('createDependencyTracker', () => {
  it('should create a DependencyTracker via factory', () => {
    // Act
    const tracker = createDependencyTracker({ delete: vi.fn() });

    // Assert
    expect(tracker).toBeInstanceOf(DependencyTracker);
  });
});

// =============================================================================
// SparqlCache Tests
// =============================================================================

describe('SparqlCache', () => {
  /** @type {SparqlCache} */
  let sparqlCache;
  let mockStore;
  let mockCache;
  let mockTracker;

  beforeEach(() => {
    mockStore = {
      query: vi.fn().mockReturnValue([
        { s: 'http://ex.org/s1', p: 'http://ex.org/p1', o: 'value1' },
      ]),
    };
    mockCache = {
      get: vi.fn().mockResolvedValue(null),
      set: vi.fn().mockResolvedValue(undefined),
      delete: vi.fn().mockResolvedValue(undefined),
      deletePattern: vi.fn().mockResolvedValue(0),
    };
    mockTracker = {
      trackQuery: vi.fn(),
    };
    sparqlCache = new SparqlCache({
      store: mockStore,
      cache: mockCache,
      tracker: mockTracker,
    });
  });

  it('should create SparqlCache with config', () => {
    // Assert
    expect(sparqlCache).toBeInstanceOf(SparqlCache);
    expect(sparqlCache.config.enableNormalization).toBe(true);
    expect(sparqlCache.config.defaultTtl).toBe(300);
  });

  it('should cache query results on first execution', async () => {
    // Arrange
    const query = 'SELECT * WHERE { ?s ?p ?o }';

    // Act
    const results = await sparqlCache.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(mockStore.query).toHaveBeenCalledWith(query);
    expect(mockCache.set).toHaveBeenCalled();
    expect(sparqlCache.stats.misses).toBe(1);
    expect(sparqlCache.stats.queries).toBe(1);
  });

  it('should return cached results on second query', async () => {
    // Arrange
    const query = 'SELECT * WHERE { ?s ?p ?o }';
    const cachedResult = [{ s: 'cached' }];
    mockCache.get.mockResolvedValueOnce(null).mockResolvedValueOnce(cachedResult);

    // Act
    await sparqlCache.query(query); // First call -> miss
    const results = await sparqlCache.query(query); // Second call -> hit

    // Assert
    expect(results).toEqual(cachedResult);
    expect(sparqlCache.stats.hits).toBe(1);
    expect(sparqlCache.stats.misses).toBe(1);
  });

  it('should generate different cache keys for different queries', async () => {
    // Arrange
    const query1 = 'SELECT * WHERE { ?s ?p ?o }';
    const query2 = 'SELECT ?s WHERE { ?s <http://ex.org/p> ?o }';

    // Act
    await sparqlCache.query(query1);
    await sparqlCache.query(query2);

    // Assert
    expect(mockCache.set).toHaveBeenCalledTimes(2);
    const firstKey = mockCache.set.mock.calls[0][0];
    const secondKey = mockCache.set.mock.calls[1][0];
    expect(firstKey).not.toBe(secondKey);
    expect(firstKey).toMatch(/^sparql:/);
    expect(secondKey).toMatch(/^sparql:/);
  });

  it('should normalize queries for consistent caching', async () => {
    // Arrange - same query with different whitespace
    const query1 = 'SELECT * WHERE { ?s ?p ?o }';
    const query2 = 'SELECT  *  WHERE  {  ?s  ?p  ?o  }';

    // Act
    await sparqlCache.query(query1);
    await sparqlCache.query(query2);

    // Assert - both should generate same cache key
    const firstKey = mockCache.set.mock.calls[0][0];
    const secondKey = mockCache.set.mock.calls[1][0];
    expect(firstKey).toBe(secondKey);
  });

  it('should skip cache when useCache is false', async () => {
    // Arrange
    const query = 'SELECT * WHERE { ?s ?p ?o }';

    // Act
    await sparqlCache.query(query, { useCache: false });

    // Assert
    expect(mockCache.get).not.toHaveBeenCalled();
    expect(mockCache.set).not.toHaveBeenCalled();
    expect(mockStore.query).toHaveBeenCalledWith(query);
  });

  it('should track dependencies when enabled', async () => {
    // Arrange
    const query = 'SELECT * WHERE { <http://ex.org/s1> ?p ?o }';

    // Act
    await sparqlCache.query(query);

    // Assert
    expect(mockTracker.trackQuery).toHaveBeenCalled();
    const [cacheKey, subjects] = mockTracker.trackQuery.mock.calls[0];
    expect(cacheKey).toMatch(/^sparql:/);
    expect(subjects).toContain('http://ex.org/s1');
  });

  it('should invalidate a specific cached query', async () => {
    // Arrange
    const query = 'SELECT * WHERE { ?s ?p ?o }';

    // Act
    await sparqlCache.invalidate(query);

    // Assert
    expect(mockCache.delete).toHaveBeenCalledWith(expect.stringMatching(/^sparql:/));
  });

  it('should report statistics with hit rate', async () => {
    // Arrange
    mockCache.get.mockResolvedValueOnce(null).mockResolvedValueOnce([{ cached: true }]);
    await sparqlCache.query('SELECT * WHERE { ?s ?p ?o }');
    await sparqlCache.query('SELECT * WHERE { ?s ?p ?o }');

    // Act
    const stats = sparqlCache.getStats();

    // Assert
    expect(stats.queries).toBe(2);
    expect(stats.hits).toBe(1);
    expect(stats.misses).toBe(1);
    expect(stats.hitRate).toBe(0.5);
  });

  it('should clear all cached queries', async () => {
    // Act
    await sparqlCache.clear();

    // Assert
    expect(mockCache.deletePattern).toHaveBeenCalledWith('sparql:*');
    expect(sparqlCache.stats.cachedQueries).toBe(0);
  });
});

describe('createSparqlCache', () => {
  it('should create a SparqlCache via factory', () => {
    // Act
    const sc = createSparqlCache({
      store: {},
      cache: { get: vi.fn(), set: vi.fn() },
    });

    // Assert
    expect(sc).toBeInstanceOf(SparqlCache);
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('Caching System Integration', () => {
  it('should create a complete caching system via createCachingSystem', async () => {
    // Arrange
    const mockStore = {};

    // Act
    const system = await createCachingSystem({
      store: mockStore,
      enableL2: false,
    });

    // Assert
    expect(system).toHaveProperty('cache');
    expect(system).toHaveProperty('tracker');
    expect(system).toHaveProperty('sparqlCache');
    expect(system).toHaveProperty('getStats');
    expect(system).toHaveProperty('clear');
    expect(system).toHaveProperty('close');
  });

  it('should return combined stats from all components', async () => {
    // Arrange
    const system = await createCachingSystem({
      store: {},
      enableL2: false,
    });

    // Act
    const stats = system.getStats();

    // Assert
    expect(stats).toHaveProperty('cache');
    expect(stats).toHaveProperty('tracker');
    expect(stats).toHaveProperty('sparql');
    expect(stats.cache).toHaveProperty('l1Hits');
    expect(stats.tracker).toHaveProperty('totalQueries');
    expect(stats.sparql).toHaveProperty('queries');
  });

  it('should clear all components when system clear is called', async () => {
    // Arrange
    const mockStore = {
      query: vi.fn().mockReturnValue([{ s: 'test' }]),
    };
    const system = await createCachingSystem({
      store: mockStore,
      enableL2: false,
    });

    // Populate caches
    await system.cache.set('test-key', 'test-value');
    system.tracker.trackQuery('q1', ['http://ex.org/s1']);

    // Act
    await system.clear();

    // Assert
    const result = await system.cache.get('test-key');
    expect(result).toBeNull();
    expect(system.tracker.getStats().totalQueries).toBe(0);
  });
});
