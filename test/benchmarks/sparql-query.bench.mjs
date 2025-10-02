/**
 * @fileoverview SPARQL Query Performance Benchmark
 *
 * @description
 * Measures SPARQL query execution to validate p99 < 50ms target.
 * This is the DEBUGGING BOTTLENECK - developers spend time here.
 *
 * Performance Targets:
 * - Simple ASK: p99 < 10ms
 * - Simple SELECT: p99 < 20ms
 * - Complex SELECT: p99 < 50ms
 * - Federated query: p99 < 200ms
 */

import { bench, describe } from 'vitest';
import { Store } from 'n3';
import { QueryEngine } from '@comunica/query-sparql';
import { useTurtle } from '../../src/composables/use-turtle.mjs';
import { performance } from 'node:perf_hooks';

/**
 * Create test store with larger dataset
 * @param {number} size - Number of person entities to create
 * @returns {Promise<Store>} Store with test data
 */
async function createLargeTestStore(size = 100) {
  const turtle = await useTurtle();
  const store = new Store();

  let testData = `
    @prefix ex: <http://example.org/> .
    @prefix schema: <http://schema.org/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
  `;

  // Generate N person entities
  for (let i = 1; i <= size; i++) {
    testData += `
    ex:person${i} a schema:Person ;
      schema:name "Person ${i}" ;
      schema:age "${20 + (i % 50)}"^^xsd:integer ;
      schema:email "person${i}@example.org" ;
      schema:jobTitle "Role ${i % 10}" ;
      schema:department "Dept ${i % 5}" .
    `;
  }

  const quads = await turtle.parse(testData);
  store.addQuads(quads);

  return store;
}

describe('SPARQL Query Performance', () => {
  // Benchmark simple ASK query
  bench('Simple ASK query', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const result = await engine.queryBoolean(
      'ASK { ?s a <http://schema.org/Person> }',
      { sources: [store] }
    );
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark simple SELECT query
  bench('Simple SELECT query', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const bindings = await engine.queryBindings(
      'SELECT ?s ?name WHERE { ?s <http://schema.org/name> ?name }',
      { sources: [store] }
    );

    // Consume stream
    const results = await bindings.toArray();
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });

  // Benchmark filtered SELECT query
  bench('Filtered SELECT query', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const bindings = await engine.queryBindings(
      `SELECT ?s ?name ?age WHERE {
        ?s <http://schema.org/name> ?name ;
           <http://schema.org/age> ?age .
        FILTER (?age > 30)
      }`,
      { sources: [store] }
    );

    const results = await bindings.toArray();
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });

  // Benchmark complex JOIN query
  bench('Complex SELECT with JOIN', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const bindings = await engine.queryBindings(
      `SELECT ?s ?name ?age ?email ?job WHERE {
        ?s <http://schema.org/name> ?name ;
           <http://schema.org/age> ?age ;
           <http://schema.org/email> ?email ;
           <http://schema.org/jobTitle> ?job .
        FILTER (?age > 25 && ?age < 40)
      }
      ORDER BY DESC(?age)
      LIMIT 10`,
      { sources: [store] }
    );

    const results = await bindings.toArray();
  }, {
    iterations: 100,
    time: 5000,
    warmupIterations: 10
  });

  // Benchmark aggregation query
  bench('Aggregation query (COUNT)', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const bindings = await engine.queryBindings(
      `SELECT (COUNT(?s) AS ?count) WHERE {
        ?s a <http://schema.org/Person>
      }`,
      { sources: [store] }
    );

    const results = await bindings.toArray();
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });

  // Benchmark GROUP BY query
  bench('GROUP BY query', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const bindings = await engine.queryBindings(
      `SELECT ?dept (COUNT(?s) AS ?count) WHERE {
        ?s <http://schema.org/department> ?dept
      }
      GROUP BY ?dept`,
      { sources: [store] }
    );

    const results = await bindings.toArray();
  }, {
    iterations: 100,
    time: 5000,
    warmupIterations: 10
  });
});

describe('SPARQL Performance Targets Validation', () => {
  bench('Validate p99 < 50ms for complex queries', async () => {
    const store = await createLargeTestStore(100);
    const engine = new QueryEngine();

    const query = `SELECT ?s ?name ?age ?email ?job WHERE {
      ?s <http://schema.org/name> ?name ;
         <http://schema.org/age> ?age ;
         <http://schema.org/email> ?email ;
         <http://schema.org/jobTitle> ?job .
      FILTER (?age > 25 && ?age < 40)
    }
    ORDER BY DESC(?age)
    LIMIT 20`;

    // Measure 100 query executions
    const durations = [];
    for (let i = 0; i < 100; i++) {
      const start = performance.now();
      const bindings = await engine.queryBindings(query, { sources: [store] });
      await bindings.toArray();
      durations.push(performance.now() - start);
    }

    // Calculate percentiles
    const sorted = durations.sort((a, b) => a - b);
    const p50 = sorted[Math.floor(sorted.length * 0.50)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    const p99 = sorted[Math.floor(sorted.length * 0.99)];
    const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;

    console.log('\nComplex SPARQL Query Performance:');
    console.log(`  Mean: ${mean.toFixed(2)}ms`);
    console.log(`  P50:  ${p50.toFixed(2)}ms`);
    console.log(`  P95:  ${p95.toFixed(2)}ms`);
    console.log(`  P99:  ${p99.toFixed(2)}ms`);
    console.log(`  Target: < 50ms`);
    console.log(`  Status: ${p99 < 50 ? '✅ PASS' : '❌ FAIL'}`);

    return mean;
  }, {
    iterations: 5,
    time: 20000
  });
});

describe('Query Engine Optimization Analysis', () => {
  bench('Compare query engine reuse vs recreation', async () => {
    const store = await createLargeTestStore(50);
    const query = 'SELECT ?s ?name WHERE { ?s <http://schema.org/name> ?name } LIMIT 10';

    // Test 1: Create new engine for each query
    const newEngineDurations = [];
    for (let i = 0; i < 50; i++) {
      const start = performance.now();
      const engine = new QueryEngine();
      const bindings = await engine.queryBindings(query, { sources: [store] });
      await bindings.toArray();
      newEngineDurations.push(performance.now() - start);
    }

    // Test 2: Reuse same engine
    const engine = new QueryEngine();
    const reusedEngineDurations = [];
    for (let i = 0; i < 50; i++) {
      const start = performance.now();
      const bindings = await engine.queryBindings(query, { sources: [store] });
      await bindings.toArray();
      reusedEngineDurations.push(performance.now() - start);
    }

    const newMean = newEngineDurations.reduce((sum, d) => sum + d, 0) / newEngineDurations.length;
    const reusedMean = reusedEngineDurations.reduce((sum, d) => sum + d, 0) / reusedEngineDurations.length;
    const speedup = newMean / reusedMean;

    console.log('\nQuery Engine Reuse Impact:');
    console.log(`  New engine each query: ${newMean.toFixed(2)}ms`);
    console.log(`  Reused engine:         ${reusedMean.toFixed(2)}ms`);
    console.log(`  Speedup:               ${speedup.toFixed(2)}x`);
    console.log(`  Recommendation:        ${speedup > 1.5 ? '✅ Reuse engines' : '⚠️ Minimal benefit'}`);
  }, {
    iterations: 3,
    time: 30000
  });

  bench('Dataset size impact on query performance', async () => {
    const sizes = [10, 50, 100, 500, 1000];
    const query = 'SELECT ?s ?name WHERE { ?s <http://schema.org/name> ?name }';

    console.log('\nDataset Size Impact:');
    console.log('  Size    |  Mean (ms)  |  P99 (ms)');
    console.log('  --------|-------------|----------');

    for (const size of sizes) {
      const store = await createLargeTestStore(size);
      const engine = new QueryEngine();

      const durations = [];
      for (let i = 0; i < 20; i++) {
        const start = performance.now();
        const bindings = await engine.queryBindings(query, { sources: [store] });
        await bindings.toArray();
        durations.push(performance.now() - start);
      }

      const sorted = durations.sort((a, b) => a - b);
      const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const p99 = sorted[Math.floor(sorted.length * 0.99)];

      console.log(`  ${String(size).padEnd(7)} | ${mean.toFixed(2).padEnd(11)} | ${p99.toFixed(2)}`);
    }
  }, {
    iterations: 1,
    time: 60000
  });
});
