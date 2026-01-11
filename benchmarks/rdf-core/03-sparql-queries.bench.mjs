/**
 * @file SPARQL Query Performance Benchmarks
 * @module benchmarks/rdf-core/03-sparql-queries
 * @description Benchmarks for SPARQL SELECT, ASK, and CONSTRUCT queries
 */

import { performance } from 'perf_hooks';
import { createStore, addQuad, namedNode, literal, executeSelect, executeAsk, executeConstruct } from '@unrdf/core';
import { RDF, FOAF } from '@unrdf/core';
import { analyzeVariance } from './suite.mjs';

/**
 * Generate test dataset
 * @param {number} entityCount - Number of entities to generate
 * @returns {Object} RDF store
 */
function generateTestDataset(entityCount) {
  const store = createStore();

  for (let i = 0; i < entityCount; i++) {
    const person = namedNode(`http://example.org/person${i}`);

    addQuad(store, {
      subject: person,
      predicate: RDF.type,
      object: FOAF.Person,
    });

    addQuad(store, {
      subject: person,
      predicate: FOAF.name,
      object: literal(`Person ${i}`),
    });

    addQuad(store, {
      subject: person,
      predicate: FOAF.age,
      object: literal(String(20 + (i % 50))),
    });

    if (i % 3 === 0) {
      addQuad(store, {
        subject: person,
        predicate: FOAF.knows,
        object: namedNode(`http://example.org/person${(i + 1) % entityCount}`),
      });
    }
  }

  return store;
}

/**
 * Benchmark SPARQL query
 * @param {Object} store - RDF store
 * @param {string} query - SPARQL query
 * @param {Function} executor - Query executor function
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
async function benchmarkQuery(store, query, executor, iterations = 100) {
  const latencies = [];
  let resultCount = 0;

  // Warmup
  for (let i = 0; i < 10; i++) {
    await executor(store, query);
  }

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    const result = await executor(store, query);
    const duration = performance.now() - start;
    latencies.push(duration);

    if (i === 0) {
      resultCount = Array.isArray(result) ? result.length : (result ? 1 : 0);
    }
  }

  const stats = analyzeVariance(latencies);
  const throughput = (iterations / (latencies.reduce((a, b) => a + b, 0) / 1000));

  return {
    latency: stats,
    throughput: { mean: throughput },
    resultCount,
    iterations,
  };
}

/**
 * Run all SPARQL benchmarks
 * @returns {Promise<Object>} Benchmark results
 */
export async function runSparqlBenchmarks() {
  console.log('\n▶ Running SPARQL Query Benchmarks...');

  const results = {};

  // Small dataset (100 entities = 400 triples)
  const storeSmall = generateTestDataset(100);

  // Medium dataset (1000 entities = 4000 triples)
  const storeMedium = generateTestDataset(1000);

  // SELECT - Simple (small dataset)
  const selectSimple = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE {
      ?person foaf:name ?name
    }
  `;
  const selectSimpleResult = await benchmarkQuery(storeSmall, selectSimple, executeSelect, 100);
  results['select-simple-small'] = {
    ...selectSimpleResult,
    passed: selectSimpleResult.latency.p95 < 10,
    target: 'P95 < 10ms',
    unit: 'queries/s',
  };

  // SELECT - Filter (small dataset)
  const selectFilter = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person foaf:name ?name .
      FILTER(REGEX(?name, "Person [0-9]$"))
    }
  `;
  const selectFilterResult = await benchmarkQuery(storeSmall, selectFilter, executeSelect, 100);
  results['select-filter-small'] = {
    ...selectFilterResult,
    passed: selectFilterResult.latency.p95 < 20,
    target: 'P95 < 20ms',
    unit: 'queries/s',
  };

  // SELECT - Join (small dataset)
  const selectJoin = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person1 ?person2 WHERE {
      ?person1 foaf:knows ?person2 .
      ?person1 foaf:name ?name1 .
      ?person2 foaf:name ?name2 .
    }
  `;
  const selectJoinResult = await benchmarkQuery(storeSmall, selectJoin, executeSelect, 50);
  results['select-join-small'] = {
    ...selectJoinResult,
    passed: selectJoinResult.latency.p95 < 30,
    target: 'P95 < 30ms',
    unit: 'queries/s',
  };

  // SELECT - Medium dataset
  const selectMediumResult = await benchmarkQuery(storeMedium, selectSimple, executeSelect, 50);
  results['select-simple-medium'] = {
    ...selectMediumResult,
    passed: selectMediumResult.latency.p95 < 50,
    target: 'P95 < 50ms',
    unit: 'queries/s',
  };

  // ASK - Simple
  const askSimple = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    ASK {
      ?person foaf:name "Person 0"
    }
  `;
  const askSimpleResult = await benchmarkQuery(storeSmall, askSimple, executeAsk, 100);
  results['ask-simple-small'] = {
    ...askSimpleResult,
    passed: askSimpleResult.latency.p95 < 5,
    target: 'P95 < 5ms',
    unit: 'queries/s',
  };

  // ASK - Complex
  const askComplex = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    ASK {
      ?person1 foaf:knows ?person2 .
      ?person2 foaf:name ?name .
      FILTER(REGEX(?name, "Person 1"))
    }
  `;
  const askComplexResult = await benchmarkQuery(storeSmall, askComplex, executeAsk, 100);
  results['ask-complex-small'] = {
    ...askComplexResult,
    passed: askComplexResult.latency.p95 < 15,
    target: 'P95 < 15ms',
    unit: 'queries/s',
  };

  // CONSTRUCT - Simple
  const constructSimple = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/vocab#>
    CONSTRUCT {
      ?person ex:hasName ?name
    } WHERE {
      ?person foaf:name ?name
    }
  `;
  const constructSimpleResult = await benchmarkQuery(storeSmall, constructSimple, executeConstruct, 50);
  results['construct-simple-small'] = {
    ...constructSimpleResult,
    passed: constructSimpleResult.latency.p95 < 20,
    target: 'P95 < 20ms',
    unit: 'queries/s',
  };

  const summary = {
    total: Object.keys(results).length,
    passed: Object.values(results).filter(r => r.passed).length,
    failed: Object.values(results).filter(r => !r.passed).length,
  };

  console.log(`✓ Completed: ${summary.passed}/${summary.total} passed`);

  return { results, summary };
}
