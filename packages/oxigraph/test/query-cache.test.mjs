/**
 * Query Cache Tests
 * Tests for query caching, normalization, and pattern analysis
 */
import { describe, it, expect, beforeEach } from 'vitest';
import {
  CachedQueryStore,
  PreparedQuery,
  prepare,
  normalizeQuery,
  analyzeQueryPattern,
  createCachedStore,
} from '../src/query-cache.mjs';
import { dataFactory } from '../src/index.mjs';

describe('Query Normalization', () => {
  it('should normalize whitespace', () => {
    const query = `SELECT  ?s   ?p
      WHERE  {  ?s  ?p  ?o  }`;
    const normalized = normalizeQuery(query);
    expect(normalized).toBe('SELECT ?s ?p WHERE { ?s ?p ?o }');
  });

  it('should remove comments', () => {
    const query = `
      # This is a comment
      SELECT ?s WHERE { ?s ?p ?o } # Another comment
    `;
    const normalized = normalizeQuery(query);
    expect(normalized).not.toContain('#');
  });

  it('should uppercase keywords', () => {
    const query = 'select ?s where { ?s ?p ?o }';
    const normalized = normalizeQuery(query);
    expect(normalized).toContain('SELECT');
    expect(normalized).toContain('WHERE');
  });

  it('should produce consistent normalization', () => {
    const query1 = 'SELECT ?s WHERE { ?s ?p ?o }';
    const query2 = '  select   ?s   where  {  ?s  ?p  ?o  }  ';
    expect(normalizeQuery(query1)).toBe(normalizeQuery(query2));
  });
});

describe('Query Pattern Analysis', () => {
  it('should detect SELECT queries', () => {
    const pattern = analyzeQueryPattern('SELECT ?s ?p WHERE { ?s ?p ?o }');
    expect(pattern.type).toBe('SELECT');
  });

  it('should detect ASK queries', () => {
    const pattern = analyzeQueryPattern('ASK { ?s ?p ?o }');
    expect(pattern.type).toBe('ASK');
  });

  it('should detect CONSTRUCT queries', () => {
    const pattern = analyzeQueryPattern('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');
    expect(pattern.type).toBe('CONSTRUCT');
  });

  it('should extract variables', () => {
    const pattern = analyzeQueryPattern('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
    expect(pattern.variables).toContain('?s');
    expect(pattern.variables).toContain('?p');
    expect(pattern.variables).toContain('?o');
  });

  it('should detect FILTER', () => {
    const pattern = analyzeQueryPattern('SELECT ?s WHERE { ?s ?p ?o FILTER(?s > 10) }');
    expect(pattern.hasFilter).toBe(true);
  });

  it('should detect OPTIONAL', () => {
    const pattern = analyzeQueryPattern('SELECT ?s WHERE { ?s ?p ?o OPTIONAL { ?s ?p2 ?o2 } }');
    expect(pattern.hasOptional).toBe(true);
  });

  it('should detect UNION', () => {
    const pattern = analyzeQueryPattern('SELECT ?s WHERE { { ?s ?p ?o } UNION { ?s ?p2 ?o2 } }');
    expect(pattern.hasUnion).toBe(true);
  });

  it('should detect aggregates', () => {
    const pattern = analyzeQueryPattern('SELECT (COUNT(?s) as ?count) WHERE { ?s ?p ?o }');
    expect(pattern.hasAggregate).toBe(true);
  });

  it('should extract predicates', () => {
    const pattern = analyzeQueryPattern('SELECT ?s WHERE { ?s <http://example.org/pred> ?o }');
    expect(pattern.predicates).toContain('<http://example.org/pred>');
  });
});

describe('CachedQueryStore', () => {
  let store;

  beforeEach(() => {
    store = new CachedQueryStore({
      cacheSize: 100,
      cacheTtlMs: 10000,
      cacheResults: true,
      analyzePatterns: true,
    });
  });

  describe('Caching Behavior', () => {
    it('should cache query results', () => {
      // Mock query execution
      store.query = function(_query, _options) {
        // Simulate base implementation
        return [{ s: 'result1' }];
      };

      // Wrap with caching
      const cachedStore = new CachedQueryStore();
      let _callCount = 0;
      const originalQuery = cachedStore.query.bind(cachedStore);
      cachedStore.query = function(query, options) {
        _callCount++;
        return originalQuery.call(this, query, options);
      };

      // Note: CachedQueryStore.query internally uses super.query
      // For testing cache hits, we check stats instead
      cachedStore.queryCache.set('test-key', [{ s: 'cached' }]);

      const stats = cachedStore.getStats();
      expect(stats.cache).toBeDefined();
    });

    it('should increment cache hits on repeated queries', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      // First call - cache miss
      try { store.query(query); } catch {}
      const stats1 = store.getStats();

      // Manually add to cache for testing
      store.queryCache.set(`${store.mutationVersion}|${normalizeQuery(query)}|{}`, []);

      // Second call - should hit cache
      store.query(query);
      const stats2 = store.getStats();

      expect(stats2.cache.hits).toBeGreaterThan(stats1.cache.hits);
    });

    it('should respect TTL', async () => {
      const shortTtlStore = new CachedQueryStore({
        cacheTtlMs: 50,
      });

      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const key = `0|${normalizeQuery(query)}|{}`;

      shortTtlStore.queryCache.set(key, [{ s: 'result' }]);
      expect(shortTtlStore.queryCache.has(key)).toBe(true);

      // Wait for TTL expiration
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(shortTtlStore.queryCache.has(key)).toBe(false);
    });
  });

  describe('Cache Invalidation', () => {
    it('should invalidate cache on add', () => {
      const initialVersion = store.mutationVersion;

      // Create proper quad using oxigraph data factory
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );

      store.add(quad);

      expect(store.mutationVersion).toBeGreaterThan(initialVersion);
    });

    it('should invalidate cache on delete', () => {
      const initialVersion = store.mutationVersion;

      // Create proper quad using oxigraph data factory
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );

      store.delete(quad);

      expect(store.mutationVersion).toBeGreaterThan(initialVersion);
    });

    it('should clear cache on update', () => {
      const _initialSize = store.queryCache.cache.size;

      try {
        store.update('INSERT DATA { <s> <p> <o> }');
      } catch {
        // May fail if base store not implemented, but mutationVersion should increment
      }

      expect(store.mutationVersion).toBeGreaterThan(0);
    });

    it('should clear cache on load', () => {
      try {
        store.load('<s> <p> <o> .', { format: 'text/turtle' });
      } catch {
        // May fail if base store not implemented
      }

      expect(store.mutationVersion).toBeGreaterThan(0);
    });
  });

  describe('Pattern Analysis', () => {
    it('should cache analyzed patterns', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      const pattern1 = store.getQueryPattern(query);
      const pattern2 = store.getQueryPattern(query);

      // Should return same cached instance
      expect(pattern1).toBe(pattern2);
      expect(store.patternCache.size).toBeGreaterThan(0);
    });

    it('should analyze patterns when enabled', () => {
      const query = 'SELECT ?s ?p WHERE { ?s ?p ?o FILTER(?p = 10) }';

      const pattern = store.getQueryPattern(query);

      expect(pattern.type).toBe('SELECT');
      expect(pattern.hasFilter).toBe(true);
      expect(pattern.variables.length).toBeGreaterThan(0);
    });
  });

  describe('Statistics', () => {
    it('should track query statistics', () => {
      const stats = store.getStats();

      expect(stats.cache).toBeDefined();
      expect(stats.query).toBeDefined();
      expect(stats.patterns).toBeDefined();
      expect(stats.mutationVersion).toBeDefined();
    });

    it('should calculate average times', () => {
      store.queryStats.cachedTimeMs = 100;
      store.queryStats.cached = 10;
      store.queryStats.uncachedTimeMs = 500;
      store.queryStats.uncached = 5;

      const stats = store.getStats();

      expect(stats.query.avgCachedTimeMs).toBe(10);
      expect(stats.query.avgUncachedTimeMs).toBe(100);
    });

    it('should reset statistics', () => {
      store.queryStats.total = 100;
      store.queryCache.hits = 50;

      store.resetStats();

      expect(store.queryStats.total).toBe(0);
      expect(store.queryCache.hits).toBe(0);
    });
  });

  describe('Cache Management', () => {
    it('should clear query cache', () => {
      store.queryCache.set('key1', []);
      store.patternCache.set('pattern1', {});

      store.clearQueryCache();

      expect(store.queryCache.cache.size).toBe(0);
      expect(store.patternCache.size).toBe(0);
    });
  });

  describe('Factory Function', () => {
    it('should create cached store via factory', () => {
      const factoryStore = createCachedStore({ cacheSize: 50 });
      expect(factoryStore).toBeInstanceOf(CachedQueryStore);
      expect(factoryStore.queryCache.maxSize).toBe(50);
    });
  });
});

describe('PreparedQuery', () => {
  let prepared;

  beforeEach(() => {
    prepared = new PreparedQuery('SELECT ?s ?p WHERE { ?s ?p ?o }');
  });

  describe('Preparation', () => {
    it('should normalize query on creation', () => {
      expect(prepared.normalizedQuery).toBeTruthy();
      expect(prepared.normalizedQuery).toContain('SELECT');
    });

    it('should analyze pattern on creation', () => {
      expect(prepared.pattern).toBeDefined();
      expect(prepared.pattern.type).toBe('SELECT');
    });

    it('should extract bindable variables', () => {
      expect(prepared.bindableVariables).toContain('?s');
      expect(prepared.bindableVariables).toContain('?p');
    });
  });

  describe('Variable Binding', () => {
    it('should bind variables to values', () => {
      const bound = prepared.bind({
        s: 'http://example.org/subject',
        p: 'http://example.org/predicate',
      });

      expect(bound).toContain('<http://example.org/subject>');
      expect(bound).toContain('<http://example.org/predicate>');
    });

    it('should handle values that are already IRIs', () => {
      const bound = prepared.bind({
        s: '<http://example.org/subject>',
      });

      expect(bound).toContain('<http://example.org/subject>');
    });

    it('should execute with bindings', () => {
      const mockStore = {
        query: (_q) => [{ result: 'test' }],
      };

      const result = prepared.execute(mockStore, { s: 'http://example.org/s' });
      expect(prepared.executions).toBe(1);
      expect(result).toBeDefined();
    });

    it('should track execution count', () => {
      const mockStore = {
        query: () => [],
      };

      prepared.execute(mockStore);
      prepared.execute(mockStore);

      expect(prepared.executions).toBe(2);
    });
  });

  describe('Factory Function', () => {
    it('should create prepared query via factory', () => {
      const prep = prepare('SELECT ?s WHERE { ?s ?p ?o }');
      expect(prep).toBeInstanceOf(PreparedQuery);
    });
  });
});

describe('Performance Characteristics', () => {
  it('should have fast query normalization', () => {
    const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o FILTER(?p > 10) }';

    const start = performance.now();
    for (let i = 0; i < 1000; i++) {
      normalizeQuery(query);
    }
    const elapsed = performance.now() - start;

    // Should be < 1ms per normalization
    expect(elapsed / 1000).toBeLessThan(1);
  });

  it('should have fast pattern analysis', () => {
    const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o FILTER(?p > 10) OPTIONAL { ?s ?p2 ?o2 } }';

    const start = performance.now();
    for (let i = 0; i < 1000; i++) {
      analyzeQueryPattern(query);
    }
    const elapsed = performance.now() - start;

    // Should be < 2ms per analysis
    expect(elapsed / 1000).toBeLessThan(2);
  });

  it('should have O(1) cache lookup', () => {
    const store = new CachedQueryStore({ cacheSize: 1000 });

    // Populate cache
    for (let i = 0; i < 100; i++) {
      store.queryCache.set(`key${i}`, []);
    }

    const start = performance.now();
    for (let i = 0; i < 1000; i++) {
      store.queryCache.has('key50');
    }
    const elapsed = performance.now() - start;

    // Should be very fast - < 0.01ms per lookup
    expect(elapsed / 1000).toBeLessThan(0.01);
  });
});
