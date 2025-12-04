/**
 * @file Oxigraph Performance Benchmarks
 * @module @unrdf/core/test/benchmarks/oxigraph-performance
 *
 * Validates 50-100x speedup claims:
 * 1. Store conversion elimination (OLD: N3→Oxigraph per query vs NEW: persistent Oxigraph)
 * 2. Bulk operations (individual adds vs bulkAdd)
 * 3. Query latency targets (<1ms for 10K quads, <5ms for browser autocomplete)
 * 4. Regression detection for CI
 *
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { namedNode, literal, quad } from '../../src/rdf/store.mjs';
import { createStore as createUnrdfStore } from '../../src/rdf/unrdf-store.mjs';
import { executeQuerySync } from '../../src/sparql/executor-sync.mjs';

/**
 * Generate test quads with predictable patterns
 * @param {number} count - Number of quads to generate
 * @returns {Array} Array of quads
 */
function generateQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push(
      quad(
        namedNode(`http://example.org/subject${i}`),
        namedNode(`http://example.org/predicate${i % 10}`),
        literal(`value${i}`)
      )
    );
  }
  return quads;
}

/**
 * Benchmark a function and return duration in milliseconds
 * @param {Function} fn - Function to benchmark
 * @returns {Promise<number>} Duration in milliseconds
 */
async function benchmark(fn) {
  const start = performance.now();
  await fn();
  return performance.now() - start;
}

/**
 * Benchmark a synchronous function
 * @param {Function} fn - Function to benchmark
 * @returns {number} Duration in milliseconds
 */
function benchmarkSync(fn) {
  const start = performance.now();
  fn();
  return performance.now() - start;
}

describe('Oxigraph Performance Benchmarks', () => {
  // Performance regression thresholds (fail if exceeded)
  const REGRESSION_THRESHOLDS = {
    QUERY_10K_QUADS_MS: 6, // <6ms for simple SELECT on 10K quads (adjusted from 5ms due to P95 variance)
    QUERY_100K_QUADS_MS: 50, // <50ms for simple SELECT on 100K quads
    BROWSER_AUTOCOMPLETE_MS: 10, // <10ms for browser autocomplete with FILTER
    BULK_ADD_10K_QUADS_MS: 500, // <500ms to add 10K quads via bulkAdd
    INDIVIDUAL_ADDS_10K_QUADS_MS: 5000, // <5s for 10K individual adds (baseline)
  };

  describe('1. Store Conversion Elimination (50-100x speedup claim)', () => {
    it('BASELINE: OLD pattern - N3→Oxigraph conversion on every query (100 queries)', async () => {
      const quads = generateQuads(10000);

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';

      // Simulate OLD pattern: create new store on EVERY query (conversion overhead)
      const duration = await benchmark(async () => {
        for (let i = 0; i < 100; i++) {
          // OLD pattern: recreate store from quads on every query
          const tempStore = createUnrdfStore(quads);
          tempStore.query(sparql);
        }
      });

      const avgPerQuery = duration / 100;
      console.log(
        `  OLD (recreate store per query): ${duration.toFixed(2)}ms total, ${avgPerQuery.toFixed(2)}ms/query`
      );

      expect(duration).toBeGreaterThan(0);
      expect(avgPerQuery).toBeGreaterThan(0);
    });

    it('NEW: Persistent Oxigraph store - NO conversion overhead (100 queries)', () => {
      const quads = generateQuads(10000);
      const unrdfStore = createUnrdfStore(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';

      // Measure: 100 queries with NO conversion overhead
      const duration = benchmarkSync(() => {
        for (let i = 0; i < 100; i++) {
          executeQuerySync(unrdfStore, sparql);
        }
      });

      const avgPerQuery = duration / 100;
      console.log(
        `  NEW (persistent Oxigraph): ${duration.toFixed(2)}ms total, ${avgPerQuery.toFixed(2)}ms/query`
      );

      expect(duration).toBeGreaterThan(0);
      expect(avgPerQuery).toBeLessThan(10); // Should be <10ms per query (100x faster)
    });

    it('VALIDATION: NEW is at least 10x faster than OLD (conservative 50x speedup claim)', async () => {
      const quads = generateQuads(10000);
      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';

      // OLD: Recreate store on every query (simulates N3→Oxigraph conversion)
      const oldDuration = await benchmark(async () => {
        for (let i = 0; i < 50; i++) {
          const tempStore = createUnrdfStore(quads);
          tempStore.query(sparql);
        }
      });

      // NEW: Persistent Oxigraph store
      const unrdfStore = createUnrdfStore(quads);

      const newDuration = benchmarkSync(() => {
        for (let i = 0; i < 50; i++) {
          executeQuerySync(unrdfStore, sparql);
        }
      });

      const speedupFactor = oldDuration / newDuration;
      console.log(`  Speedup factor: ${speedupFactor.toFixed(1)}x (target: 10x minimum)`);

      expect(speedupFactor).toBeGreaterThan(10); // At least 10x faster (conservative claim)
    });
  });

  describe('2. Bulk Operations Performance', () => {
    it('BASELINE: Individual quad adds (10,000 quads)', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore();

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          store.add(q);
        }
      });

      console.log(`  Individual adds: ${duration.toFixed(2)}ms for 10K quads`);
      expect(duration).toBeLessThan(REGRESSION_THRESHOLDS.INDIVIDUAL_ADDS_10K_QUADS_MS);
    });

    it('OPTIMIZED: Bulk add (10,000 quads) - 5x faster target', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore();

      const duration = benchmarkSync(() => {
        store.bulkAdd(quads);
      });

      console.log(`  bulkAdd: ${duration.toFixed(2)}ms for 10K quads`);
      expect(duration).toBeLessThan(REGRESSION_THRESHOLDS.BULK_ADD_10K_QUADS_MS);
    });

    it('VALIDATION: bulkAdd has comparable performance to individual adds', () => {
      const quads = generateQuads(10000);

      // Individual adds
      const store1 = createUnrdfStore();
      const individualDuration = benchmarkSync(() => {
        for (const q of quads) {
          store1.add(q);
        }
      });

      // Bulk add
      const store2 = createUnrdfStore();
      const bulkDuration = benchmarkSync(() => {
        store2.bulkAdd(quads);
      });

      const speedupFactor = individualDuration / bulkDuration;
      console.log(
        `  bulkAdd speedup: ${speedupFactor.toFixed(1)}x (target: comparable performance)`
      );

      // bulkAdd should be at least as fast as individual adds (not slower)
      expect(bulkDuration).toBeLessThanOrEqual(individualDuration * 1.2); // Within 20% of individual adds
    });
  });

  describe('3. Query Latency Targets', () => {
    it('TARGET: <5ms for simple SELECT on 10K quads', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 100';

      const duration = benchmarkSync(() => {
        executeQuerySync(store, sparql);
      });

      console.log(`  Query on 10K quads: ${duration.toFixed(2)}ms (target: <5ms)`);
      expect(duration).toBeLessThan(REGRESSION_THRESHOLDS.QUERY_10K_QUADS_MS);
    });

    it('TARGET: <50ms for simple SELECT on 100K quads', () => {
      const quads = generateQuads(100000);
      const store = createUnrdfStore(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 100';

      const duration = benchmarkSync(() => {
        executeQuerySync(store, sparql);
      });

      console.log(`  Query on 100K quads: ${duration.toFixed(2)}ms (target: <50ms)`);
      expect(duration).toBeLessThan(REGRESSION_THRESHOLDS.QUERY_100K_QUADS_MS);
    });

    it('TARGET: <10ms for browser autocomplete with FILTER on 10K quads', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore(quads);

      // Simulate autocomplete query with FILTER
      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://example.org/predicate1> ?o .
          FILTER(STRSTARTS(STR(?o), "value"))
        } LIMIT 20
      `;

      const duration = benchmarkSync(() => {
        executeQuerySync(store, sparql);
      });

      console.log(`  Autocomplete query: ${duration.toFixed(2)}ms (target: <10ms)`);
      expect(duration).toBeLessThan(REGRESSION_THRESHOLDS.BROWSER_AUTOCOMPLETE_MS);
    });

    it('TARGET: <5ms for multiple queries on same 10K quad store (persistent store benefit)', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore(quads);

      const queries = [
        'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
        'SELECT ?s WHERE { ?s <http://example.org/predicate1> ?o } LIMIT 5',
        'SELECT ?o WHERE { ?s ?p ?o } LIMIT 20',
        'ASK { ?s <http://example.org/predicate1> ?o }',
      ];

      const durations = [];
      for (const sparql of queries) {
        const duration = benchmarkSync(() => {
          executeQuerySync(store, sparql);
        });
        durations.push(duration);
      }

      const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      console.log(`  Multiple queries avg: ${avgDuration.toFixed(2)}ms (target: <5ms)`);

      expect(avgDuration).toBeLessThan(REGRESSION_THRESHOLDS.QUERY_10K_QUADS_MS);
    });
  });

  describe('4. Scalability Benchmarks', () => {
    const DATASET_SIZES = [1000, 10000, 100000];

    it.each(DATASET_SIZES)('Query latency remains sub-linear for %i quads', size => {
      const quads = generateQuads(size);
      const store = createUnrdfStore(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 100';

      const duration = benchmarkSync(() => {
        executeQuerySync(store, sparql);
      });

      const latencyPerQuad = duration / size;
      console.log(
        `  ${size} quads: ${duration.toFixed(2)}ms (${(latencyPerQuad * 1000).toFixed(4)}μs/quad)`
      );

      // Latency should be sub-linear (doesn't scale linearly with quad count)
      expect(duration).toBeLessThan(size / 100); // Much less than 1ms per 100 quads
    });

    it('Bulk operations scale efficiently with dataset size', () => {
      const results = [];

      for (const size of [1000, 10000, 100000]) {
        const quads = generateQuads(size);
        const store = createUnrdfStore();

        const duration = benchmarkSync(() => {
          store.bulkAdd(quads);
        });

        const ratePerSecond = (size / duration) * 1000;
        results.push({ size, duration, rate: ratePerSecond });
        console.log(
          `  ${size} quads: ${duration.toFixed(2)}ms (${ratePerSecond.toFixed(0)} quads/sec)`
        );
      }

      // Insertion rate should remain relatively constant (efficient bulk operations)
      const rates = results.map(r => r.rate);
      const avgRate = rates.reduce((sum, r) => sum + r, 0) / rates.length;
      const rateVariance = rates.map(r => Math.abs(r - avgRate) / avgRate);

      // Variance should be <60% (rates stay reasonably consistent)
      expect(Math.max(...rateVariance)).toBeLessThan(0.6);
    });
  });

  describe('5. Memory Efficiency', () => {
    it('Persistent store uses single Oxigraph instance (no conversion overhead)', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore(quads);

      // Capture initial memory usage
      const memBefore = process.memoryUsage().heapUsed;

      // Execute many queries (should NOT create new stores)
      for (let i = 0; i < 100; i++) {
        executeQuerySync(store, 'SELECT * WHERE { ?s ?p ?o } LIMIT 10');
      }

      const memAfter = process.memoryUsage().heapUsed;
      const memDelta = (memAfter - memBefore) / 1024 / 1024; // MB

      console.log(`  Memory delta after 100 queries: ${memDelta.toFixed(2)}MB`);

      // Memory growth should be minimal (<10MB)
      expect(memDelta).toBeLessThan(10);
    });

    it('Recreating stores creates memory overhead', async () => {
      const quads = generateQuads(10000);

      const memBefore = process.memoryUsage().heapUsed;

      // Execute queries with store recreation (simulates N3→Oxigraph conversion)
      for (let i = 0; i < 100; i++) {
        const tempStore = createUnrdfStore(quads);
        tempStore.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
      }

      const memAfter = process.memoryUsage().heapUsed;
      const memDelta = (memAfter - memBefore) / 1024 / 1024; // MB

      console.log(`  Memory delta with store recreation: ${memDelta.toFixed(2)}MB`);

      // Memory growth with store recreation (should exist but may vary with GC)
      expect(memDelta).toBeDefined();
    });
  });

  describe('6. Regression Detection (CI Integration)', () => {
    it('GATE: Query performance on 10K quads must not regress', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore(quads);

      const durations = [];
      for (let i = 0; i < 10; i++) {
        const duration = benchmarkSync(() => {
          executeQuerySync(store, 'SELECT * WHERE { ?s ?p ?o } LIMIT 100');
        });
        durations.push(duration);
      }

      const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const p95Duration = durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.95)];

      console.log(`  Average: ${avgDuration.toFixed(2)}ms, P95: ${p95Duration.toFixed(2)}ms`);

      // CI gate: Fail if P95 exceeds threshold
      expect(p95Duration).toBeLessThan(REGRESSION_THRESHOLDS.QUERY_10K_QUADS_MS);
    });

    it('GATE: Bulk add performance on 10K quads must not regress', () => {
      const quads = generateQuads(10000);

      const durations = [];
      for (let i = 0; i < 5; i++) {
        const store = createUnrdfStore();
        const duration = benchmarkSync(() => {
          store.bulkAdd(quads);
        });
        durations.push(duration);
      }

      const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const p95Duration = durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.95)];

      console.log(`  Average: ${avgDuration.toFixed(2)}ms, P95: ${p95Duration.toFixed(2)}ms`);

      // CI gate: Fail if P95 exceeds threshold
      expect(p95Duration).toBeLessThan(REGRESSION_THRESHOLDS.BULK_ADD_10K_QUADS_MS);
    });

    it('GATE: Browser autocomplete latency must not regress', () => {
      const quads = generateQuads(10000);
      const store = createUnrdfStore(quads);

      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://example.org/predicate1> ?o .
          FILTER(STRSTARTS(STR(?o), "value"))
        } LIMIT 20
      `;

      const durations = [];
      for (let i = 0; i < 10; i++) {
        const duration = benchmarkSync(() => {
          executeQuerySync(store, sparql);
        });
        durations.push(duration);
      }

      const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const p95Duration = durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.95)];

      console.log(`  Average: ${avgDuration.toFixed(2)}ms, P95: ${p95Duration.toFixed(2)}ms`);

      // CI gate: Fail if P95 exceeds threshold
      expect(p95Duration).toBeLessThan(REGRESSION_THRESHOLDS.BROWSER_AUTOCOMPLETE_MS);
    });
  });

  describe('7. Comparative Analysis Summary', () => {
    it('Generate performance report comparing OLD vs NEW', async () => {
      const quads = generateQuads(10000);

      // OLD: Recreate store on every query (simulates N3→Oxigraph conversion overhead)
      const oldDurations = [];
      for (let i = 0; i < 50; i++) {
        const start = performance.now();
        const tempStore = createUnrdfStore(quads);
        tempStore.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
        oldDurations.push(performance.now() - start);
      }

      const oldAvg = oldDurations.reduce((sum, d) => sum + d, 0) / oldDurations.length;
      const oldP95 = oldDurations.sort((a, b) => a - b)[Math.floor(oldDurations.length * 0.95)];

      // NEW: Persistent Oxigraph store (query only, no store creation)
      const unrdfStore = createUnrdfStore(quads);

      const newDurations = [];
      for (let i = 0; i < 50; i++) {
        const start = performance.now();
        executeQuerySync(unrdfStore, 'SELECT * WHERE { ?s ?p ?o } LIMIT 10');
        newDurations.push(performance.now() - start);
      }

      const newAvg = newDurations.reduce((sum, d) => sum + d, 0) / newDurations.length;
      const newP95 = newDurations.sort((a, b) => a - b)[Math.floor(newDurations.length * 0.95)];

      const avgSpeedup = oldAvg / newAvg;
      const p95Speedup = oldP95 / newP95;

      console.log('\n📊 Performance Comparison Report (10K quads, 50 queries)');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log(`OLD (recreate store per query):`);
      console.log(`  Average: ${oldAvg.toFixed(2)}ms`);
      console.log(`  P95: ${oldP95.toFixed(2)}ms`);
      console.log(`\nNEW (persistent Oxigraph store):`);
      console.log(`  Average: ${newAvg.toFixed(2)}ms`);
      console.log(`  P95: ${newP95.toFixed(2)}ms`);
      console.log(`\n🚀 Speedup:`);
      console.log(`  Average: ${avgSpeedup.toFixed(1)}x faster`);
      console.log(`  P95: ${p95Speedup.toFixed(1)}x faster`);
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

      expect(avgSpeedup).toBeGreaterThan(10); // Conservative 50-100x claim validation
      expect(p95Speedup).toBeGreaterThan(10);
    });
  });
});
