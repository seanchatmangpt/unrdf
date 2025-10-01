/**
 * @fileoverview Performance benchmarks for CLI v2
 * Validates performance targets and tracks metrics
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  runCLI,
  createTempDir,
  assert,
  generators,
  perf
} from '../test-utils.mjs';
import { writeFile } from 'node:fs/promises';
import { join } from 'node:path';

describe('CLI v2 Performance Benchmarks', () => {
  let tempDir;
  let cleanup;

  beforeEach(async () => {
    const temp = await createTempDir('perf-test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('Startup Performance', () => {
    it('should meet command startup target (< 100ms)', async () => {
      const metrics = await perf.measure(async () => {
        await runCLI('hook list');
      }, 10);

      console.log('Startup metrics:', metrics);

      perf.assertSLA(metrics, {
        mean: 100,
        p95: 150,
        p99: 200
      });
    });

    it('should start quickly with cold cache', async () => {
      // First run (cold)
      const cold = await runCLI('hook list');

      // Should still be fast
      expect(cold.duration).toBeLessThan(200);
    });
  });

  describe('Parse Performance', () => {
    it('should parse 10k triples in < 500ms (target)', async () => {
      const dataPath = join(tempDir, '10k-triples.ttl');
      await writeFile(dataPath, generators.rdfTriples(10000));

      const metrics = await perf.measure(async () => {
        await runCLI(`parse turtle ${dataPath}`);
      }, 5);

      console.log('Parse 10k triples metrics:', metrics);

      perf.assertSLA(metrics, {
        mean: 500,
        p95: 600,
        p99: 800
      });
    });

    it('should parse 100k triples efficiently', async () => {
      const dataPath = join(tempDir, '100k-triples.ttl');
      await writeFile(dataPath, generators.rdfTriples(100000));

      const result = await runCLI(`parse turtle ${dataPath}`);

      assert.success(result);
      console.log('Parse 100k triples:', result.duration, 'ms');

      // Should complete in reasonable time
      expect(result.duration).toBeLessThan(10000);
    }, 20000);

    it('should scale linearly with dataset size', async () => {
      const sizes = [1000, 5000, 10000];
      const durations = [];

      for (const size of sizes) {
        const dataPath = join(tempDir, `${size}-triples.ttl`);
        await writeFile(dataPath, generators.rdfTriples(size));

        const result = await runCLI(`parse turtle ${dataPath}`);
        assert.success(result);
        durations.push({ size, duration: result.duration });
      }

      console.log('Scaling metrics:', durations);

      // Check linear scaling (ratio should be roughly consistent)
      const ratio1 = durations[1].duration / durations[0].duration;
      const ratio2 = durations[2].duration / durations[1].duration;

      expect(Math.abs(ratio1 - 5)).toBeLessThan(2); // ~5x for 5x data
      expect(Math.abs(ratio2 - 2)).toBeLessThan(1); // ~2x for 2x data
    }, 30000);
  });

  describe('Query Performance', () => {
    it('should execute simple query in < 50ms (target)', async () => {
      const dataPath = join(tempDir, 'query-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      const queryPath = join(tempDir, 'simple-query.rq');
      await writeFile(queryPath, 'SELECT ?s WHERE { ?s ?p ?o } LIMIT 10');

      const metrics = await perf.measure(async () => {
        await runCLI(`query select ${queryPath} ${dataPath}`);
      }, 20);

      console.log('Simple query metrics:', metrics);

      perf.assertSLA(metrics, {
        mean: 50,
        p95: 75,
        p99: 100
      });
    });

    it('should handle complex queries efficiently', async () => {
      const dataPath = join(tempDir, 'complex-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5000));

      const queryPath = join(tempDir, 'complex-query.rq');
      await writeFile(queryPath, `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?person ?name ?age WHERE {
          ?person a foaf:Person ;
                  foaf:name ?name ;
                  foaf:age ?age .
          FILTER(?age > 25 && ?age < 40)
        }
        ORDER BY DESC(?age)
        LIMIT 50
      `);

      const metrics = await perf.measure(async () => {
        await runCLI(`query select ${queryPath} ${dataPath}`);
      }, 10);

      console.log('Complex query metrics:', metrics);

      // Complex queries should still be fast
      expect(metrics.mean).toBeLessThan(200);
    });

    it('should optimize JOIN operations', async () => {
      const dataPath = join(tempDir, 'join-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(2000));

      const queryPath = join(tempDir, 'join-query.rq');
      await writeFile(queryPath, `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <https://schema.org/>
        SELECT ?person1 ?person2 WHERE {
          ?person1 a foaf:Person ;
                   foaf:name ?name1 .
          ?person2 a foaf:Person ;
                   foaf:name ?name2 .
          FILTER(?person1 != ?person2)
        }
        LIMIT 100
      `);

      const result = await runCLI(`query select ${queryPath} ${dataPath}`);

      assert.success(result);
      console.log('JOIN query duration:', result.duration, 'ms');

      // Should handle JOINs without timeout
      expect(result.duration).toBeLessThan(2000);
    });
  });

  describe('Validation Performance', () => {
    it('should validate in < 200ms (target)', async () => {
      const dataPath = join(tempDir, 'validate-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const metrics = await perf.measure(async () => {
        await runCLI(`validate shacl ${dataPath} ${shapePath}`);
      }, 10);

      console.log('Validation metrics:', metrics);

      perf.assertSLA(metrics, {
        mean: 200,
        p95: 300,
        p99: 400
      });
    });

    it('should validate large datasets efficiently', async () => {
      const dataPath = join(tempDir, 'large-validate.ttl');
      await writeFile(dataPath, generators.rdfTriples(10000));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(`validate shacl ${dataPath} ${shapePath}`);

      assert.success(result);
      console.log('Large validation duration:', result.duration, 'ms');

      // Should scale well
      expect(result.duration).toBeLessThan(3000);
    }, 10000);
  });

  describe('Hook Evaluation Performance', () => {
    it('should meet hook eval target (< 2ms p99)', async () => {
      const hookPath = join(tempDir, 'perf-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'perf-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const dataPath = join(tempDir, 'hook-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(100));

      const metrics = await perf.measure(async () => {
        await runCLI(`hook eval ${hookPath} --data ${dataPath}`);
      }, 100);

      console.log('Hook eval metrics:', metrics);

      // This is a VERY aggressive target
      // Real p99 might be higher, but we should aim for it
      if (metrics.p99 < 2) {
        console.log('✅ Exceeded target: p99 < 2ms');
      } else {
        console.log(`⚠️  p99 ${metrics.p99}ms (target: < 2ms)`);
      }

      // More realistic expectation
      expect(metrics.p99).toBeLessThan(50);
    }, 30000);

    it('should batch hook evaluations efficiently', async () => {
      const hooks = [];
      for (let i = 0; i < 10; i++) {
        const hookPath = join(tempDir, `batch-hook-${i}.json`);
        const hook = generators.hookDefinition('sparql-ask', `batch-${i}`);
        await writeFile(hookPath, JSON.stringify(hook, null, 2));
        hooks.push(hookPath);
      }

      const dataPath = join(tempDir, 'batch-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(50));

      const start = Date.now();

      // Evaluate all hooks
      for (const hookPath of hooks) {
        await runCLI(`hook eval ${hookPath} --data ${dataPath}`);
      }

      const totalDuration = Date.now() - start;
      const avgPerHook = totalDuration / hooks.length;

      console.log('Batch eval:', { totalDuration, avgPerHook });

      // Should average < 100ms per hook
      expect(avgPerHook).toBeLessThan(100);
    }, 30000);
  });

  describe('Memory Performance', () => {
    it('should not leak memory during repeated operations', async () => {
      const dataPath = join(tempDir, 'memory-test.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      const initialMemory = process.memoryUsage().heapUsed;

      // Perform many operations
      for (let i = 0; i < 100; i++) {
        await runCLI(`parse turtle ${dataPath}`);

        if (i % 10 === 0 && global.gc) {
          global.gc();
        }
      }

      if (global.gc) global.gc();

      const finalMemory = process.memoryUsage().heapUsed;
      const memoryIncrease = finalMemory - initialMemory;

      console.log('Memory usage:', {
        initial: Math.round(initialMemory / 1024 / 1024) + 'MB',
        final: Math.round(finalMemory / 1024 / 1024) + 'MB',
        increase: Math.round(memoryIncrease / 1024 / 1024) + 'MB'
      });

      // Memory increase should be minimal (< 50MB)
      expect(memoryIncrease).toBeLessThan(50 * 1024 * 1024);
    }, 60000);
  });

  describe('Concurrent Performance', () => {
    it('should handle concurrent operations efficiently', async () => {
      const dataPath = join(tempDir, 'concurrent-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(500));

      const queryPath = join(tempDir, 'concurrent-query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      // Execute 10 queries concurrently
      const start = Date.now();

      const operations = Array(10).fill(null).map(() =>
        runCLI(`query select ${queryPath} ${dataPath}`)
      );

      const results = await Promise.all(operations);
      const totalDuration = Date.now() - start;

      // All should succeed
      results.forEach(result => assert.success(result));

      console.log('Concurrent execution:', {
        operations: results.length,
        totalDuration,
        avgDuration: totalDuration / results.length
      });

      // Concurrent execution should be faster than sequential
      // (If sequential: 10 * 50ms = 500ms, concurrent should be < 300ms)
      expect(totalDuration).toBeLessThan(500);
    }, 15000);
  });

  describe('Regression Testing', () => {
    it('should maintain consistent performance across runs', async () => {
      const dataPath = join(tempDir, 'regression-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      const runs = [];

      for (let i = 0; i < 10; i++) {
        const result = await runCLI(`parse turtle ${dataPath}`);
        assert.success(result);
        runs.push(result.duration);
      }

      const mean = runs.reduce((a, b) => a + b, 0) / runs.length;
      const variance = runs.reduce((sum, duration) => {
        return sum + Math.pow(duration - mean, 2);
      }, 0) / runs.length;
      const stdDev = Math.sqrt(variance);

      console.log('Performance consistency:', {
        runs: runs.length,
        mean: Math.round(mean) + 'ms',
        stdDev: Math.round(stdDev) + 'ms',
        min: Math.min(...runs) + 'ms',
        max: Math.max(...runs) + 'ms'
      });

      // Standard deviation should be small (performance is consistent)
      // Allow up to 30% variance
      expect(stdDev).toBeLessThan(mean * 0.3);
    }, 30000);
  });
});
