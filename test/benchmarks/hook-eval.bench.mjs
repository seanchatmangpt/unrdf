/**
 * @fileoverview Hook Evaluation Performance Benchmark
 *
 * @description
 * Measures hook evaluation latency to validate p99 < 2ms target.
 * This is the MOST FREQUENT operation - 80% of runtime.
 *
 * Performance Targets:
 * - ASK hooks: p99 < 2ms
 * - SELECT hooks: p99 < 5ms
 * - SHACL hooks: p99 < 10ms
 * - Throughput: > 10k exec/min sustained
 */

import { bench, describe } from 'vitest';
import { Store } from 'n3';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { useTurtle } from '../../src/composables/use-turtle.mjs';
import { performance } from 'node:perf_hooks';

/**
 * Create test store with sample data
 * @returns {Promise<Store>} Store with test data
 */
async function createTestStore() {
  const turtle = await useTurtle();
  const store = new Store();

  const testData = `
    @prefix ex: <http://example.org/> .
    @prefix schema: <http://schema.org/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    ex:person1 a schema:Person ;
      schema:name "Alice" ;
      schema:age "30"^^xsd:integer ;
      schema:email "alice@example.org" .

    ex:person2 a schema:Person ;
      schema:name "Bob" ;
      schema:age "25"^^xsd:integer ;
      schema:email "bob@example.org" .

    ex:person3 a schema:Person ;
      schema:name "Carol" ;
      schema:age "35"^^xsd:integer ;
      schema:email "carol@example.org" .

    ex:person4 a schema:Person ;
      schema:name "David" ;
      schema:age "28"^^xsd:integer ;
      schema:email "david@example.org" .

    ex:person5 a schema:Person ;
      schema:name "Eve" ;
      schema:age "32"^^xsd:integer ;
      schema:email "eve@example.org" .
  `;

  const quads = await turtle.parse(testData);
  store.addQuads(quads);

  return store;
}

describe('Hook Evaluation Performance', () => {
  // Benchmark simple ASK hook (most common, must be fastest)
  bench('ASK hook evaluation', async () => {
    const store = await createTestStore();
    const manager = new KnowledgeHookManager();

    const hook = {
      meta: { name: 'ask-hook', description: 'Simple ASK query' },
      when: {
        kind: 'sparql-ask',
        query: 'ASK { ?s a <http://schema.org/Person> }'
      },
      run: async () => ({ success: true })
    };

    manager.addKnowledgeHook(hook);

    // Execute hook (triggers evaluation)
    await manager.applyTransaction(store, { added: [], removed: [] });
  }, {
    iterations: 1000,
    time: 5000,
    warmupIterations: 100,
    warmupTime: 1000
  });

  // Benchmark SELECT hook
  bench('SELECT hook evaluation', async () => {
    const store = await createTestStore();
    const manager = new KnowledgeHookManager();

    const hook = {
      meta: { name: 'select-hook', description: 'Simple SELECT query' },
      when: {
        kind: 'sparql-select',
        query: 'SELECT ?s ?name WHERE { ?s <http://schema.org/name> ?name }'
      },
      run: async () => ({ success: true })
    };

    manager.addKnowledgeHook(hook);
    await manager.applyTransaction(store, { added: [], removed: [] });
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark COUNT/THRESHOLD hook
  bench('THRESHOLD hook evaluation', async () => {
    const store = await createTestStore();
    const manager = new KnowledgeHookManager();

    const hook = {
      meta: { name: 'threshold-hook', description: 'Count-based threshold' },
      when: {
        kind: 'threshold',
        query: 'SELECT (COUNT(?s) AS ?count) WHERE { ?s a <http://schema.org/Person> }',
        threshold: 3,
        operator: 'gt'
      },
      run: async () => ({ success: true })
    };

    manager.addKnowledgeHook(hook);
    await manager.applyTransaction(store, { added: [], removed: [] });
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark DELTA hook (change detection)
  bench('DELTA hook evaluation', async () => {
    const store = await createTestStore();
    const manager = new KnowledgeHookManager();

    const hook = {
      meta: { name: 'delta-hook', description: 'Change detection' },
      when: {
        kind: 'delta',
        pattern: { subject: '?s', predicate: 'http://schema.org/age', object: '?o' }
      },
      run: async () => ({ success: true })
    };

    manager.addKnowledgeHook(hook);

    // Add delta to trigger hook
    const delta = {
      added: [{
        subject: { value: 'http://example.org/person1' },
        predicate: { value: 'http://schema.org/age' },
        object: { value: '31' }
      }],
      removed: []
    };

    await manager.applyTransaction(store, delta);
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });
});

describe('Hook Performance Targets Validation', () => {
  bench('Validate p99 < 2ms target for ASK hooks', async () => {
    const store = await createTestStore();
    const manager = new KnowledgeHookManager();

    const hook = {
      meta: { name: 'perf-test-ask', description: 'Performance test' },
      when: {
        kind: 'sparql-ask',
        query: 'ASK { ?s a <http://schema.org/Person> }'
      },
      run: async () => ({ success: true })
    };

    manager.addKnowledgeHook(hook);

    // Measure 100 evaluations
    const durations = [];
    for (let i = 0; i < 100; i++) {
      const start = performance.now();
      await manager.applyTransaction(store, { added: [], removed: [] });
      durations.push(performance.now() - start);
    }

    // Calculate percentiles
    const sorted = durations.sort((a, b) => a - b);
    const p50 = sorted[Math.floor(sorted.length * 0.50)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    const p99 = sorted[Math.floor(sorted.length * 0.99)];
    const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;

    console.log('\nASK Hook Performance:');
    console.log(`  Mean: ${mean.toFixed(3)}ms`);
    console.log(`  P50:  ${p50.toFixed(3)}ms`);
    console.log(`  P95:  ${p95.toFixed(3)}ms`);
    console.log(`  P99:  ${p99.toFixed(3)}ms`);
    console.log(`  Target: < 2ms`);
    console.log(`  Status: ${p99 < 2 ? '✅ PASS' : '❌ FAIL'}`);

    return mean;
  }, {
    iterations: 5,
    time: 10000
  });

  bench('Validate throughput > 10k exec/min', async () => {
    const store = await createTestStore();
    const manager = new KnowledgeHookManager();

    const hook = {
      meta: { name: 'throughput-test', description: 'Throughput test' },
      when: {
        kind: 'sparql-ask',
        query: 'ASK { ?s ?p ?o }'
      },
      run: async () => ({ success: true })
    };

    manager.addKnowledgeHook(hook);

    // Measure executions per minute
    const startTime = performance.now();
    let executions = 0;

    // Run for 5 seconds and extrapolate to 1 minute
    const runDuration = 5000; // 5 seconds
    const endTime = startTime + runDuration;

    while (performance.now() < endTime) {
      await manager.applyTransaction(store, { added: [], removed: [] });
      executions++;
    }

    const actualDuration = performance.now() - startTime;
    const execPerMinute = Math.round((executions / actualDuration) * 60000);

    console.log('\nHook Throughput:');
    console.log(`  Executions: ${executions}`);
    console.log(`  Duration:   ${(actualDuration / 1000).toFixed(2)}s`);
    console.log(`  Rate:       ${execPerMinute.toLocaleString()} exec/min`);
    console.log(`  Target:     > 10,000 exec/min`);
    console.log(`  Status:     ${execPerMinute > 10000 ? '✅ PASS' : '❌ FAIL'}`);

    return execPerMinute;
  }, {
    iterations: 3,
    time: 20000
  });
});

describe('Hook Performance Optimization Analysis', () => {
  bench('Compare hook types performance', async () => {
    const store = await createTestStore();

    const hookTypes = [
      {
        name: 'ASK',
        hook: {
          meta: { name: 'ask-test', description: 'ASK test' },
          when: { kind: 'sparql-ask', query: 'ASK { ?s a <http://schema.org/Person> }' },
          run: async () => ({ success: true })
        }
      },
      {
        name: 'SELECT',
        hook: {
          meta: { name: 'select-test', description: 'SELECT test' },
          when: { kind: 'sparql-select', query: 'SELECT ?s WHERE { ?s a <http://schema.org/Person> }' },
          run: async () => ({ success: true })
        }
      },
      {
        name: 'THRESHOLD',
        hook: {
          meta: { name: 'threshold-test', description: 'THRESHOLD test' },
          when: { kind: 'threshold', query: 'SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }', threshold: 1, operator: 'gt' },
          run: async () => ({ success: true })
        }
      }
    ];

    console.log('\nHook Type Performance Comparison:');

    for (const { name, hook } of hookTypes) {
      const manager = new KnowledgeHookManager();
      manager.addKnowledgeHook(hook);

      // Measure 100 executions
      const durations = [];
      for (let i = 0; i < 100; i++) {
        const start = performance.now();
        await manager.applyTransaction(store, { added: [], removed: [] });
        durations.push(performance.now() - start);
      }

      const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const sorted = durations.sort((a, b) => a - b);
      const p99 = sorted[Math.floor(sorted.length * 0.99)];

      console.log(`  ${name.padEnd(10)} - Mean: ${mean.toFixed(3)}ms, P99: ${p99.toFixed(3)}ms`);
    }
  }, {
    iterations: 1,
    time: 30000
  });
});
