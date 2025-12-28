#!/usr/bin/env node
/**
 * V6 Query Performance Benchmark
 *
 * Measures: Query time with/without receipts
 * Target: <5% overhead
 * Benchmark: 100 SPARQL queries (simple, complex, aggregation)
 * Report: Query type breakdown
 *
 * @module benchmarks/v6/3-query-performance
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import { createReceipt } from '../../packages/v6-core/src/receipts/index.mjs';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Benchmark Configuration
// =============================================================================

const CONFIG = {
  dataSize: 1_000, // Number of entities
  queriesPerType: 100,
  warmupQueries: 10,
};

// =============================================================================
// Data Generation
// =============================================================================

/**
 * Generate test data
 * @param {Object} store - Oxigraph store
 * @param {number} count - Number of entities
 */
function populateStore(store, count) {
  for (let i = 0; i < count; i++) {
    const subject = namedNode(`http://example.org/entity${i}`);

    // Add properties
    store.add(quad(subject, namedNode('http://example.org/name'), literal(`Entity ${i}`)));
    store.add(quad(subject, namedNode('http://example.org/value'), literal(i.toString())));
    store.add(
      quad(subject, namedNode('http://example.org/type'), literal(i % 2 === 0 ? 'even' : 'odd'))
    );
    store.add(
      quad(
        subject,
        namedNode('http://example.org/category'),
        literal(`cat${i % 10}`)
      )
    );
  }
}

/**
 * Add receipt metadata to store
 * @param {Object} store - Oxigraph store
 * @param {Object} receipt - Receipt object
 */
function storeReceiptMetadata(store, receipt) {
  const receiptSubject = namedNode(`http://example.org/receipt/${receipt.id}`);

  store.add(
    quad(receiptSubject, namedNode('http://example.org/receiptHash'), literal(receipt.receiptHash))
  );
  store.add(
    quad(
      receiptSubject,
      namedNode('http://example.org/timestamp'),
      literal(receipt.timestamp_iso)
    )
  );
  store.add(
    quad(receiptSubject, namedNode('http://example.org/receiptType'), literal(receipt.receiptType))
  );
}

// =============================================================================
// Query Definitions
// =============================================================================

const QUERIES = {
  simple: `
    SELECT ?entity ?name
    WHERE {
      ?entity <http://example.org/name> ?name .
    }
    LIMIT 10
  `,

  filtered: `
    SELECT ?entity ?name ?type
    WHERE {
      ?entity <http://example.org/name> ?name .
      ?entity <http://example.org/type> ?type .
      FILTER(?type = "even")
    }
    LIMIT 10
  `,

  join: `
    SELECT ?entity ?name ?value ?category
    WHERE {
      ?entity <http://example.org/name> ?name .
      ?entity <http://example.org/value> ?value .
      ?entity <http://example.org/category> ?category .
    }
    LIMIT 10
  `,

  aggregation: `
    SELECT ?type (COUNT(?entity) as ?count)
    WHERE {
      ?entity <http://example.org/type> ?type .
    }
    GROUP BY ?type
  `,

  complex: `
    SELECT ?category (COUNT(?entity) as ?count) (AVG(xsd:integer(?value)) as ?avgValue)
    WHERE {
      ?entity <http://example.org/category> ?category .
      ?entity <http://example.org/value> ?value .
    }
    GROUP BY ?category
    ORDER BY DESC(?count)
    LIMIT 5
  `,
};

// =============================================================================
// Benchmark Runner
// =============================================================================

/**
 * Run query benchmark for a specific query type
 * @param {Object} store - Oxigraph store
 * @param {string} queryType - Type of query
 * @param {string} queryString - SPARQL query
 * @param {number} iterations - Number of iterations
 * @returns {Promise<Object>} Timing statistics
 */
async function benchmarkQueryType(store, queryType, queryString, iterations) {
  const timings = [];

  // Warmup
  for (let i = 0; i < CONFIG.warmupQueries; i++) {
    store.query(queryString);
  }

  // Actual benchmark
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    const results = store.query(queryString);
    // Consume results to ensure query executes fully
    Array.from(results);
    const elapsed = performance.now() - start;
    timings.push(elapsed);
  }

  const sorted = [...timings].sort((a, b) => a - b);
  const sum = timings.reduce((a, b) => a + b, 0);
  const mean = sum / timings.length;

  return {
    queryType,
    mean,
    median: sorted[Math.floor(sorted.length / 2)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
    min: sorted[0],
    max: sorted[sorted.length - 1],
  };
}

/**
 * Run full query performance benchmark
 * @returns {Promise<Object>} Benchmark results
 */
async function runQueryPerformanceBenchmark() {
  console.log('\n[Query Performance] Setting up stores...');

  // Create baseline store (no receipts)
  const baselineStore = createStore();
  populateStore(baselineStore, CONFIG.dataSize);

  // Create store with receipts
  const receiptStore = createStore();
  populateStore(receiptStore, CONFIG.dataSize);

  // Add receipt metadata
  console.log('[Query Performance] Generating receipt metadata...');
  const receipts = [];
  for (let i = 0; i < 100; i++) {
    const receipt = await createReceipt('execution', {
      eventType: 'TASK_COMPLETED',
      caseId: `case-${i}`,
      taskId: `task-${i}`,
      payload: { operation: `query-${i}` },
    });
    receipts.push(receipt);
    storeReceiptMetadata(receiptStore, receipt);
  }

  console.log(
    `[Query Performance] Baseline store: ${baselineStore.size} quads`
  );
  console.log(`[Query Performance] Receipt store: ${receiptStore.size} quads`);

  const results = {
    baseline: {},
    withReceipts: {},
  };

  // Benchmark each query type
  for (const [queryType, queryString] of Object.entries(QUERIES)) {
    console.log(`\n[Query Performance] Benchmarking ${queryType} queries...`);

    // Baseline
    const baselineStats = await benchmarkQueryType(
      baselineStore,
      queryType,
      queryString,
      CONFIG.queriesPerType
    );
    results.baseline[queryType] = baselineStats;

    // With receipts
    const receiptStats = await benchmarkQueryType(
      receiptStore,
      queryType,
      queryString,
      CONFIG.queriesPerType
    );
    results.withReceipts[queryType] = receiptStats;

    // Calculate overhead
    const overheadPercent =
      ((receiptStats.median - baselineStats.median) / baselineStats.median) * 100;

    console.log(`  Baseline median:  ${baselineStats.median.toFixed(4)} ms`);
    console.log(`  Receipt median:   ${receiptStats.median.toFixed(4)} ms`);
    console.log(`  Overhead:         ${overheadPercent.toFixed(2)}%`);
  }

  return results;
}

// =============================================================================
// Main Benchmark
// =============================================================================

async function main() {
  console.log('='.repeat(80));
  console.log('V6 Query Performance Benchmark');
  console.log('='.repeat(80));
  console.log(`Target: <5% overhead`);
  console.log(`Data size: ${CONFIG.dataSize.toLocaleString()} entities`);
  console.log(`Queries per type: ${CONFIG.queriesPerType}`);

  const results = await runQueryPerformanceBenchmark();

  // Print detailed results
  console.log('\n' + '='.repeat(80));
  console.log('QUERY PERFORMANCE RESULTS');
  console.log('='.repeat(80));

  const overheads = [];
  for (const queryType of Object.keys(QUERIES)) {
    const baseline = results.baseline[queryType];
    const withReceipts = results.withReceipts[queryType];
    const overhead = ((withReceipts.median - baseline.median) / baseline.median) * 100;
    overheads.push(overhead);

    console.log(`\n${queryType.toUpperCase()} QUERIES:`);
    console.log(`  Baseline median:    ${baseline.median.toFixed(4)} ms`);
    console.log(`  Receipt median:     ${withReceipts.median.toFixed(4)} ms`);
    console.log(`  Baseline P95:       ${baseline.p95.toFixed(4)} ms`);
    console.log(`  Receipt P95:        ${withReceipts.p95.toFixed(4)} ms`);
    console.log(`  Overhead:           ${overhead.toFixed(2)}%`);
    console.log(`  Status:             ${overhead < 5.0 ? '✅ PASS' : '❌ FAIL'}`);
  }

  // Overall statistics
  const avgOverhead = overheads.reduce((sum, o) => sum + o, 0) / overheads.length;
  const maxOverhead = Math.max(...overheads);
  const allPass = overheads.every((o) => o < 5.0);

  console.log('\n' + '='.repeat(80));
  console.log('OVERALL SUMMARY');
  console.log('='.repeat(80));
  console.log(`Average overhead:    ${avgOverhead.toFixed(2)}%`);
  console.log(`Max overhead:        ${maxOverhead.toFixed(2)}%`);
  console.log(`Target:              <5.00%`);
  console.log(`Status:              ${allPass ? '✅ ALL PASS' : '❌ SOME FAIL'}`);

  // JSON output for aggregation
  console.log('\n__JSON_RESULTS__');
  const jsonResults = {
    benchmark: 'query-performance',
    timestamp: new Date().toISOString(),
    config: CONFIG,
    results: {
      baseline: results.baseline,
      withReceipts: results.withReceipts,
      overheads: Object.keys(QUERIES).reduce((acc, queryType, idx) => {
        acc[queryType] = overheads[idx];
        return acc;
      }, {}),
      avgOverheadPercent: avgOverhead,
      maxOverheadPercent: maxOverhead,
    },
    target: {
      maxOverheadPercent: 5.0,
      pass: allPass,
    },
  };
  console.log(JSON.stringify(jsonResults, null, 2));

  // Exit with appropriate code
  process.exit(allPass ? 0 : 1);
}

main().catch((error) => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
