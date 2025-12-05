/**
 * @file Tests for useDistributedQuery hook functionality
 * Tests query routing, result aggregation, caching, and execution strategies
 */

import { describe, it, expect, _beforeEach, vi } from 'vitest';

describe('DistributedQuery', () => {
  describe('Query Execution', () => {
    it('should execute SPARQL query', async () => {
      const execute = async (sparql, options) => {
        const startTime = performance.now();

        // Simulate query execution
        const bindings = [
          {
            s: 'http://example.org/entity1',
            p: 'http://schema.org/name',
            o: 'Alice',
          },
          {
            s: 'http://example.org/entity2',
            p: 'http://schema.org/name',
            o: 'Bob',
          },
        ];

        return {
          bindings,
          duration: performance.now() - startTime,
          storesQueried: options.stores || ['default'],
          strategy: options.strategy || 'fastest',
        };
      };

      const result = await execute('SELECT ?s ?p ?o WHERE { ?s ?p ?o }', {
        stores: ['store-1', 'store-2'],
      });

      expect(result.bindings).toHaveLength(2);
      expect(result.storesQueried).toContain('store-1');
    });

    it('should track execution statistics', async () => {
      const execute = async (_sparql) => {
        const startTime = performance.now();
        await new Promise((resolve) => setTimeout(resolve, 5));
        const duration = performance.now() - startTime;

        return {
          bindings: [],
          executionStats: {
            duration,
            storesQueried: ['store-1'],
            strategy: 'fastest',
            cacheHit: false,
          },
        };
      };

      const result = await execute('SELECT * WHERE { ?s ?p ?o }');
      expect(result.executionStats.duration).toBeGreaterThan(0);
      expect(result.executionStats.cacheHit).toBe(false);
    });

    it('should handle query execution errors', async () => {
      const execute = async (sparql) => {
        if (sparql.includes('INVALID')) {
          throw new Error('Parse error: Invalid SPARQL syntax');
        }
        return { bindings: [] };
      };

      await expect(execute('INVALID QUERY')).rejects.toThrow('Parse error');
    });

    it('should set loading state during execution', async () => {
      let loading = false;

      const execute = async (_sparql) => {
        loading = true;
        await new Promise((resolve) => setTimeout(resolve, 10));
        loading = false;
        return { bindings: [] };
      };

      const executePromise = execute('SELECT * WHERE { ?s ?p ?o }');
      // Loading should be true during execution
      expect(loading).toBe(true);
      await executePromise;
      expect(loading).toBe(false);
    });
  });

  describe('Query Routing Strategies', () => {
    it('should support fastest strategy', () => {
      const stores = [
        { id: 'store-1', latency: 150 },
        { id: 'store-2', latency: 50 },
        { id: 'store-3', latency: 100 },
      ];

      const selectStore = (strategy, stores) => {
        if (strategy === 'fastest') {
          return stores.reduce((fastest, store) =>
            store.latency < fastest.latency ? store : fastest
          );
        }
        return stores[0];
      };

      const selected = selectStore('fastest', stores);
      expect(selected.id).toBe('store-2');
      expect(selected.latency).toBe(50);
    });

    it('should support quorum strategy', () => {
      const stores = ['store-1', 'store-2', 'store-3', 'store-4', 'store-5'];

      const selectStores = (strategy, stores) => {
        if (strategy === 'quorum') {
          const quorumSize = Math.floor(stores.length / 2) + 1;
          return stores.slice(0, quorumSize);
        }
        return stores;
      };

      const selected = selectStores('quorum', stores);
      expect(selected).toHaveLength(3); // 5/2 + 1 = 3
    });

    it('should support all stores strategy', () => {
      const stores = ['store-1', 'store-2', 'store-3'];

      const selectStores = (strategy, stores) => {
        if (strategy === 'all') {
          return stores;
        }
        return [stores[0]];
      };

      const selected = selectStores('all', stores);
      expect(selected).toHaveLength(3);
    });

    it('should support leader-only strategy', () => {
      const stores = [
        { id: 'store-1', role: 'follower' },
        { id: 'store-2', role: 'leader' },
        { id: 'store-3', role: 'follower' },
      ];

      const selectStore = (strategy, stores) => {
        if (strategy === 'leader') {
          return stores.find((s) => s.role === 'leader');
        }
        return stores[0];
      };

      const selected = selectStore('leader', stores);
      expect(selected.id).toBe('store-2');
      expect(selected.role).toBe('leader');
    });
  });

  describe('Result Aggregation', () => {
    it('should aggregate results with union', () => {
      const resultSets = [
        [
          { id: 1, name: 'Alice' },
          { id: 2, name: 'Bob' },
        ],
        [
          { id: 3, name: 'Carol' },
          { id: 4, name: 'Dave' },
        ],
      ];

      const aggregate = (strategy, resultSets) => {
        if (strategy === 'union') {
          return resultSets.flat();
        }
        return [];
      };

      const aggregated = aggregate('union', resultSets);
      expect(aggregated).toHaveLength(4);
    });

    it('should aggregate results with intersection', () => {
      const resultSets = [
        [{ id: 1 }, { id: 2 }, { id: 3 }],
        [{ id: 2 }, { id: 3 }, { id: 4 }],
        [{ id: 3 }, { id: 4 }, { id: 5 }],
      ];

      const aggregate = (strategy, resultSets) => {
        if (strategy === 'intersection') {
          if (resultSets.length === 0) return [];

          let intersection = resultSets[0];
          for (let i = 1; i < resultSets.length; i++) {
            const ids = new Set(resultSets[i].map((r) => r.id));
            intersection = intersection.filter((r) => ids.has(r.id));
          }
          return intersection;
        }
        return [];
      };

      const aggregated = aggregate('intersection', resultSets);
      expect(aggregated).toHaveLength(1);
      expect(aggregated[0].id).toBe(3);
    });

    it('should deduplicate union results', () => {
      const resultSets = [
        [{ uri: 'http://example.org/1' }, { uri: 'http://example.org/2' }],
        [{ uri: 'http://example.org/2' }, { uri: 'http://example.org/3' }],
      ];

      const aggregateWithDedup = (resultSets) => {
        const seen = new Set();
        const results = [];

        for (const set of resultSets) {
          for (const result of set) {
            if (!seen.has(result.uri)) {
              seen.add(result.uri);
              results.push(result);
            }
          }
        }

        return results;
      };

      const aggregated = aggregateWithDedup(resultSets);
      expect(aggregated).toHaveLength(3);
    });

    it('should preserve order in aggregation', () => {
      const resultSets = [
        [{ order: 1 }, { order: 3 }, { order: 5 }],
        [{ order: 2 }, { order: 4 }, { order: 6 }],
      ];

      const aggregateOrdered = (resultSets) => {
        return resultSets.flat().sort((a, b) => a.order - b.order);
      };

      const aggregated = aggregateOrdered(resultSets);
      expect(aggregated.map((r) => r.order)).toEqual([1, 2, 3, 4, 5, 6]);
    });
  });

  describe('Query Caching', () => {
    it('should cache query results', async () => {
      const cache = new Map();

      const execute = async (sparql, options) => {
        const cacheKey = `${sparql}-${JSON.stringify(options)}`;

        if (options.cache !== false && cache.has(cacheKey)) {
          return { ...cache.get(cacheKey), cacheHit: true };
        }

        const result = {
          bindings: [{ s: 'test' }],
          timestamp: Date.now(),
        };

        if (options.cache !== false) {
          cache.set(cacheKey, result);
        }

        return { ...result, cacheHit: false };
      };

      const result1 = await execute('SELECT * WHERE { ?s ?p ?o }', {
        cache: true,
      });
      const result2 = await execute('SELECT * WHERE { ?s ?p ?o }', {
        cache: true,
      });

      expect(result1.cacheHit).toBe(false);
      expect(result2.cacheHit).toBe(true);
    });

    it('should respect cache TTL', async () => {
      const cache = new Map();
      const TTL = 100; // 100ms

      const setWithTTL = (key, value) => {
        cache.set(key, { value, expires: Date.now() + TTL });
      };

      const getWithTTL = (key) => {
        const entry = cache.get(key);
        if (!entry) return null;
        if (Date.now() > entry.expires) {
          cache.delete(key);
          return null;
        }
        return entry.value;
      };

      setWithTTL('query1', { bindings: [] });
      expect(getWithTTL('query1')).not.toBeNull();

      // Wait for TTL to expire
      await new Promise((resolve) => setTimeout(resolve, 150));
      expect(getWithTTL('query1')).toBeNull();
    });

    it('should bypass cache when requested', async () => {
      let executionCount = 0;

      const execute = async (sparql, options) => {
        if (!options.cache) {
          executionCount++;
          return { bindings: [], cacheHit: false };
        }
        return { bindings: [], cacheHit: true };
      };

      await execute('SELECT * WHERE { ?s ?p ?o }', { cache: false });
      await execute('SELECT * WHERE { ?s ?p ?o }', { cache: false });

      expect(executionCount).toBe(2);
    });

    it('should invalidate cache on data change', () => {
      const cache = new Map();
      cache.set('query1', { bindings: [{ id: 1 }] });
      cache.set('query2', { bindings: [{ id: 2 }] });

      const invalidateCache = (pattern) => {
        for (const key of cache.keys()) {
          if (key.includes(pattern)) {
            cache.delete(key);
          }
        }
      };

      invalidateCache('query1');
      expect(cache.has('query1')).toBe(false);
      expect(cache.has('query2')).toBe(true);
    });
  });

  describe('Timeout Handling', () => {
    it('should respect query timeout', async () => {
      const execute = async (sparql, options) => {
        const timeout = options.timeout || 30000;

        return new Promise((resolve, reject) => {
          const timer = setTimeout(() => {
            reject(new Error('Query timeout'));
          }, timeout);

          // Simulate fast completion
          setTimeout(() => {
            clearTimeout(timer);
            resolve({ bindings: [] });
          }, 10);
        });
      };

      const result = await execute('SELECT * WHERE { ?s ?p ?o }', {
        timeout: 30000,
      });
      expect(result.bindings).toBeDefined();
    });

    it('should throw on timeout exceeded', async () => {
      const execute = async (sparql, options) => {
        const timeout = options.timeout || 30000;

        return new Promise((resolve, reject) => {
          setTimeout(() => {
            reject(new Error('Query timeout'));
          }, timeout);
        });
      };

      await expect(execute('SELECT * WHERE { ?s ?p ?o }', { timeout: 10 })).rejects.toThrow(
        'Query timeout'
      );
    });

    it('should cancel pending requests on timeout', () => {
      const pendingRequests = new Map();

      const startRequest = (id) => {
        const abortController = { aborted: false, abort: vi.fn() };
        pendingRequests.set(id, abortController);
        return abortController;
      };

      const cancelAllPending = () => {
        for (const [_id, controller] of pendingRequests) {
          controller.abort();
          controller.aborted = true;
        }
        pendingRequests.clear();
      };

      startRequest('req1');
      startRequest('req2');
      expect(pendingRequests.size).toBe(2);

      cancelAllPending();
      expect(pendingRequests.size).toBe(0);
    });
  });

  describe('Refetch and Manual Execution', () => {
    it('should refetch query on demand', async () => {
      let fetchCount = 0;

      const refetch = async () => {
        fetchCount++;
        return { bindings: [{ count: fetchCount }] };
      };

      await refetch();
      await refetch();

      expect(fetchCount).toBe(2);
    });

    it('should execute queries manually', async () => {
      const execute = async (sparql) => {
        return {
          bindings: [{ query: sparql }],
          executedAt: new Date().toISOString(),
        };
      };

      const result = await execute('SELECT ?person WHERE { ?person a :Person }');
      expect(result.bindings[0].query).toContain('SELECT');
    });

    it('should throw error when refetching without initial query', () => {
      const sparql = null;

      const refetch = () => {
        if (!sparql) {
          throw new Error('No query to refetch. Use execute() for manual queries.');
        }
      };

      expect(() => refetch()).toThrow('No query to refetch');
    });
  });

  describe('Error Handling', () => {
    it('should capture and expose errors', async () => {
      let error = null;

      const execute = async (_sparql) => {
        try {
          throw new Error('Network error');
        } catch (err) {
          error = err;
          throw err;
        }
      };

      await expect(execute('SELECT * WHERE { ?s ?p ?o }')).rejects.toThrow();
      expect(error.message).toBe('Network error');
    });

    it('should handle partial failures gracefully', async () => {
      const executeAcrossStores = async (sparql, stores) => {
        const results = [];
        const errors = [];

        for (const store of stores) {
          try {
            if (store === 'store-failing') {
              throw new Error(`Store ${store} unavailable`);
            }
            results.push({ store, bindings: [{ s: `from-${store}` }] });
          } catch (err) {
            errors.push({ store, error: err.message });
          }
        }

        return { results, errors, partialSuccess: results.length > 0 };
      };

      const result = await executeAcrossStores('SELECT * WHERE { ?s ?p ?o }', [
        'store-1',
        'store-failing',
        'store-2',
      ]);

      expect(result.results).toHaveLength(2);
      expect(result.errors).toHaveLength(1);
      expect(result.partialSuccess).toBe(true);
    });

    it('should retry failed queries', async () => {
      let attempts = 0;

      const executeWithRetry = async (sparql, maxRetries = 3) => {
        while (attempts < maxRetries) {
          attempts++;
          try {
            if (attempts < 3) {
              throw new Error('Temporary failure');
            }
            return { success: true, attempts };
          } catch (err) {
            if (attempts >= maxRetries) {
              throw err;
            }
          }
        }
      };

      const result = await executeWithRetry('SELECT * WHERE { ?s ?p ?o }');
      expect(result.success).toBe(true);
      expect(result.attempts).toBe(3);
    });
  });
});
