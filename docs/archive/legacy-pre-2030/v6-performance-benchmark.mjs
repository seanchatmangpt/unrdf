#!/usr/bin/env node
/**
 * @file V6 Performance Benchmarks
 * @module benchmarks/v6-performance-benchmark
 *
 * @description
 * Comprehensive benchmarks for V6 knowledge engine performance:
 * - Parsing (Turtle, N-Quads)
 * - SPARQL querying
 * - Query optimization and caching
 * - Memory allocation patterns
 * - Throughput and latency measurements
 */

import { performance } from 'node:perf_hooks';
import { suite, formatDetailedReport, formatMarkdownTable } from './framework.mjs';
import { parseTurtle, toTurtle } from '../src/knowledge-engine/parse.mjs';
import { query } from '../src/knowledge-engine/query.mjs';
import { QueryOptimizer } from '../src/knowledge-engine/query-optimizer.mjs';
import { Store } from 'n3';

// =============================================================================
// Test Data Generation
// =============================================================================

/**
 * Generate sample Turtle data
 * @param {number} tripleCount - Number of triples to generate
 * @returns {string} Turtle string
 */
function generateTurtleData(tripleCount = 100) {
  const lines = ['@prefix ex: <http://example.org/> .'];

  for (let i = 0; i < tripleCount; i++) {
    const subject = `ex:entity${i % 50}`;
    const predicate = `ex:property${i % 10}`;
    const object = i % 3 === 0 ? `ex:value${i}` : `"Value ${i}"`;
    lines.push(`${subject} ${predicate} ${object} .`);
  }

  return lines.join('\n');
}

/**
 * Create a populated store
 * @param {number} size - Number of triples
 * @returns {Promise<Store>} Populated store
 */
async function createTestStore(size = 100) {
  const ttl = generateTurtleData(size);
  return await parseTurtle(ttl);
}

/**
 * Generate SPARQL SELECT query
 * @param {number} complexity - Query complexity (0-2)
 * @returns {string} SPARQL query
 */
function generateSparqlQuery(complexity = 0) {
  const queries = [
    // Simple query
    `PREFIX ex: <http://example.org/>
     SELECT ?s ?o WHERE { ?s ex:property0 ?o }`,

    // Medium complexity
    `PREFIX ex: <http://example.org/>
     SELECT ?s ?p ?o WHERE {
       ?s ?p ?o .
       FILTER(?p = ex:property1 || ?p = ex:property2)
     }`,

    // High complexity
    `PREFIX ex: <http://example.org/>
     SELECT ?s1 ?s2 ?o WHERE {
       ?s1 ex:property0 ?o1 .
       ?s2 ex:property0 ?o2 .
       ?s1 ex:property1 ?o .
       FILTER(?o1 != ?o2)
     }`
  ];

  return queries[complexity % queries.length];
}

// =============================================================================
// Parsing Benchmarks
// =============================================================================

export const parsingBenchmarks = suite('V6 Parsing Performance', {
  'parse small Turtle (100 triples)': {
    setup: () => ({ ttl: generateTurtleData(100) }),
    fn: async function() {
      return await parseTurtle(this.ttl);
    },
    iterations: 1000,
    warmup: 100
  },

  'parse medium Turtle (1000 triples)': {
    setup: () => ({ ttl: generateTurtleData(1000) }),
    fn: async function() {
      return await parseTurtle(this.ttl);
    },
    iterations: 500,
    warmup: 50
  },

  'parse large Turtle (5000 triples)': {
    setup: () => ({ ttl: generateTurtleData(5000) }),
    fn: async function() {
      return await parseTurtle(this.ttl);
    },
    iterations: 100,
    warmup: 10
  },

  'serialize small store (100 triples)': {
    setup: async () => ({ store: await createTestStore(100) }),
    fn: async function() {
      return await toTurtle(this.store);
    },
    iterations: 1000,
    warmup: 100
  },

  'serialize large store (1000 triples)': {
    setup: async () => ({ store: await createTestStore(1000) }),
    fn: async function() {
      return await toTurtle(this.store);
    },
    iterations: 200,
    warmup: 20
  }
});

// =============================================================================
// Query Benchmarks
// =============================================================================

export const queryBenchmarks = suite('V6 Query Performance', {
  'SPARQL SELECT (simple, 100 triples)': {
    setup: async () => ({
      store: await createTestStore(100),
      sparql: generateSparqlQuery(0)
    }),
    fn: async function() {
      return await query(this.store, this.sparql);
    },
    iterations: 500,
    warmup: 50
  },

  'SPARQL SELECT (medium complexity, 1000 triples)': {
    setup: async () => ({
      store: await createTestStore(1000),
      sparql: generateSparqlQuery(1)
    }),
    fn: async function() {
      return await query(this.store, this.sparql);
    },
    iterations: 200,
    warmup: 20
  },

  'SPARQL SELECT (high complexity, 1000 triples)': {
    setup: async () => ({
      store: await createTestStore(1000),
      sparql: generateSparqlQuery(2)
    }),
    fn: async function() {
      return await query(this.store, this.sparql);
    },
    iterations: 100,
    warmup: 10
  },

  'SPARQL ASK (simple)': {
    setup: async () => ({
      store: await createTestStore(100),
      sparql: 'PREFIX ex: <http://example.org/> ASK { ex:entity0 ex:property0 ?o }'
    }),
    fn: async function() {
      return await query(this.store, this.sparql);
    },
    iterations: 1000,
    warmup: 100
  },

  'repeated queries (cache effectiveness)': {
    setup: async () => ({
      store: await createTestStore(500),
      queries: [
        generateSparqlQuery(0),
        generateSparqlQuery(1),
        generateSparqlQuery(0), // Repeat for cache hit
        generateSparqlQuery(1)  // Repeat for cache hit
      ]
    }),
    fn: async function() {
      const results = [];
      for (const sparql of this.queries) {
        results.push(await query(this.store, sparql));
      }
      return results;
    },
    iterations: 200,
    warmup: 20
  }
});

// =============================================================================
// Query Optimizer Benchmarks
// =============================================================================

export const optimizerBenchmarks = suite('V6 Query Optimizer Performance', {
  'optimizer initialization': {
    fn: () => {
      return new QueryOptimizer({
        enableCaching: true,
        enableIndexing: true,
        maxCacheSize: 1000
      });
    },
    iterations: 5000,
    warmup: 500
  },

  'query optimization (cold cache)': {
    setup: async () => ({
      optimizer: new QueryOptimizer(),
      store: await createTestStore(100),
      query: generateSparqlQuery(0)
    }),
    fn: async function() {
      // Create new optimizer each time to ensure cold cache
      const opt = new QueryOptimizer();
      return await opt.optimizeQuery(this.query, 'sparql-select', this.store);
    },
    iterations: 500,
    warmup: 50
  },

  'query optimization (warm cache)': {
    setup: async () => {
      const optimizer = new QueryOptimizer();
      const store = await createTestStore(100);
      const sparql = generateSparqlQuery(0);
      // Warm up the cache
      await optimizer.optimizeQuery(sparql, 'sparql-select', store);
      return { optimizer, store, query: sparql };
    },
    fn: async function() {
      return await this.optimizer.optimizeQuery(this.query, 'sparql-select', this.store);
    },
    iterations: 10000,
    warmup: 1000
  },

  'optimizer stats tracking': {
    setup: () => ({
      optimizer: new QueryOptimizer()
    }),
    fn: function() {
      this.optimizer.stats.totalQueries++;
      this.optimizer.stats.cacheHits++;
      return this.optimizer.getStats();
    },
    iterations: 100000,
    warmup: 10000
  }
});

// =============================================================================
// Memory Allocation Benchmarks
// =============================================================================

export const memoryBenchmarks = suite('V6 Memory Allocation Performance', {
  'store creation and population': {
    fn: async () => {
      const store = new Store();
      const ttl = generateTurtleData(100);
      return await parseTurtle(ttl);
    },
    iterations: 1000,
    warmup: 100
  },

  'query result object allocation': {
    setup: async () => ({
      store: await createTestStore(500),
      sparql: generateSparqlQuery(0)
    }),
    fn: async function() {
      const results = await query(this.store, this.sparql);
      // Force result materialization
      return JSON.parse(JSON.stringify(results));
    },
    iterations: 500,
    warmup: 50
  },

  'repeated store operations (GC pressure)': {
    fn: async () => {
      for (let i = 0; i < 10; i++) {
        const ttl = generateTurtleData(50);
        const store = await parseTurtle(ttl);
        const sparql = generateSparqlQuery(0);
        await query(store, sparql);
      }
    },
    iterations: 100,
    warmup: 10
  }
});

// =============================================================================
// Throughput Benchmarks
// =============================================================================

export const throughputBenchmarks = suite('V6 Throughput Performance', {
  'concurrent queries (5 parallel)': {
    setup: async () => ({
      store: await createTestStore(500),
      queries: Array(5).fill(0).map((_, i) => generateSparqlQuery(i % 3))
    }),
    fn: async function() {
      return await Promise.all(
        this.queries.map(sparql => query(this.store, sparql))
      );
    },
    iterations: 200,
    warmup: 20
  },

  'sequential batch processing (100 queries)': {
    setup: async () => ({
      store: await createTestStore(200),
      queries: Array(100).fill(0).map(() => generateSparqlQuery(0))
    }),
    fn: async function() {
      const results = [];
      for (const sparql of this.queries) {
        results.push(await query(this.store, sparql));
      }
      return results;
    },
    iterations: 10,
    warmup: 2
  }
});

// =============================================================================
// Runner
// =============================================================================

async function runAllBenchmarks() {
  console.log('╔═══════════════════════════════════════════════════════════════════════╗');
  console.log('║              V6 Performance Benchmark Suite                           ║');
  console.log('╚═══════════════════════════════════════════════════════════════════════╝\n');

  const results = [];

  // Run all benchmark suites
  const suites = [
    { name: 'Parsing', fn: parsingBenchmarks },
    { name: 'Query', fn: queryBenchmarks },
    { name: 'Optimizer', fn: optimizerBenchmarks },
    { name: 'Memory', fn: memoryBenchmarks },
    { name: 'Throughput', fn: throughputBenchmarks }
  ];

  for (const suite of suites) {
    try {
      console.log(`\nRunning ${suite.name} benchmarks...`);
      const result = await suite.fn();
      results.push(result);
    } catch (error) {
      console.error(`Failed to run ${suite.name} benchmarks:`, error.message);
      results.push({
        suite: suite.name,
        error: error.message,
        failed: true
      });
    }
  }

  // Generate summary report
  console.log('\n' + '═'.repeat(80));
  console.log('BENCHMARK SUMMARY');
  console.log('═'.repeat(80) + '\n');

  for (const result of results) {
    console.log(formatMarkdownTable(result));
  }

  return results;
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const results = await runAllBenchmarks();

  // Write results to file
  const { writeFile, mkdir } = await import('node:fs/promises');
  const { join } = await import('node:path');

  const reportsDir = join(process.cwd(), 'benchmarks', 'reports');
  await mkdir(reportsDir, { recursive: true });

  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const reportPath = join(reportsDir, `v6-baseline-${timestamp}.md`);

  const reportLines = [
    '# V6 Performance Baseline Report\n',
    `**Generated**: ${new Date().toISOString()}\n`,
    '## Summary\n'
  ];

  for (const result of results) {
    if (!result.failed) {
      reportLines.push(formatMarkdownTable(result));
    }
  }

  await writeFile(reportPath, reportLines.join('\n'), 'utf-8');
  console.log(`\n📊 Baseline report written to: ${reportPath}`);

  process.exit(0);
}

export { runAllBenchmarks };
