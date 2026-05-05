#!/usr/bin/env node
/**
 * @file V6 Optimized Performance Benchmarks
 * @module benchmarks/v6-optimized-benchmark
 *
 * @description
 * Benchmarks for optimized v6 implementation to compare against baseline
 */

import { performance } from 'node:perf_hooks';
import { suite, formatDetailedReport, formatMarkdownTable } from './framework.mjs';
import { parseTurtleOptimized, toTurtleOptimized } from '../src/knowledge-engine/parse-optimized.mjs';
import { queryOptimized, queryBatch, clearQueryCache } from '../src/knowledge-engine/query-optimized.mjs';
import { parseTurtle } from '../src/knowledge-engine/parse.mjs';
import { query } from '../src/knowledge-engine/query.mjs';
import { Store } from 'n3';

// =============================================================================
// Test Data Generation (same as baseline)
// =============================================================================

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

async function createTestStore(size = 100) {
  const ttl = generateTurtleData(size);
  return await parseTurtle(ttl);
}

function generateSparqlQuery(complexity = 0) {
  const queries = [
    `PREFIX ex: <http://example.org/>
     SELECT ?s ?o WHERE { ?s ex:property0 ?o }`,
    `PREFIX ex: <http://example.org/>
     SELECT ?s ?p ?o WHERE {
       ?s ?p ?o .
       FILTER(?p = ex:property1 || ?p = ex:property2)
     }`,
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
// Optimized Parsing Benchmarks
// =============================================================================

export const parsingOptimizedBenchmarks = suite('V6 Parsing Performance (Optimized)', {
  'parse small Turtle (100 triples) - OPTIMIZED': {
    setup: () => ({ ttl: generateTurtleData(100) }),
    fn: async function() {
      return await parseTurtleOptimized(this.ttl);
    },
    iterations: 1000,
    warmup: 100
  },

  'parse medium Turtle (1000 triples) - OPTIMIZED': {
    setup: () => ({ ttl: generateTurtleData(1000) }),
    fn: async function() {
      return await parseTurtleOptimized(this.ttl, 'http://example.org/', { batchSize: 500 });
    },
    iterations: 500,
    warmup: 50
  },

  'parse large Turtle (5000 triples) - OPTIMIZED': {
    setup: () => ({ ttl: generateTurtleData(5000) }),
    fn: async function() {
      return await parseTurtleOptimized(this.ttl, 'http://example.org/', { batchSize: 1000 });
    },
    iterations: 100,
    warmup: 10
  },

  'serialize small store (100 triples) - OPTIMIZED': {
    setup: async () => ({ store: await createTestStore(100) }),
    fn: async function() {
      return await toTurtleOptimized(this.store);
    },
    iterations: 1000,
    warmup: 100
  }
});

// =============================================================================
// Optimized Query Benchmarks
// =============================================================================

export const queryOptimizedBenchmarks = suite('V6 Query Performance (Optimized)', {
  'SPARQL SELECT (simple, 100 triples) - OPTIMIZED': {
    setup: async () => ({
      store: await createTestStore(100),
      sparql: generateSparqlQuery(0)
    }),
    fn: async function() {
      return await queryOptimized(this.store, this.sparql);
    },
    iterations: 500,
    warmup: 50
  },

  'SPARQL SELECT (with cache) - OPTIMIZED': {
    setup: async () => {
      const store = await createTestStore(100);
      const sparql = generateSparqlQuery(0);
      // Prime the cache
      await queryOptimized(store, sparql);
      return { store, sparql };
    },
    fn: async function() {
      return await queryOptimized(this.store, this.sparql);
    },
    iterations: 2000,
    warmup: 200
  },

  'batch query (5 parallel) - OPTIMIZED': {
    setup: async () => ({
      store: await createTestStore(500),
      queries: Array(5).fill(0).map((_, i) => generateSparqlQuery(i % 3))
    }),
    fn: async function() {
      return await queryBatch(this.store, this.queries);
    },
    iterations: 200,
    warmup: 20
  }
});

// =============================================================================
// Comparison Benchmarks (Direct)
// =============================================================================

export const comparisonBenchmarks = suite('V6 Baseline vs Optimized Comparison', {
  'parse 1000 triples - BASELINE': {
    setup: () => ({ ttl: generateTurtleData(1000) }),
    fn: async function() {
      return await parseTurtle(this.ttl);
    },
    iterations: 500,
    warmup: 50
  },

  'parse 1000 triples - OPTIMIZED': {
    setup: () => ({ ttl: generateTurtleData(1000) }),
    fn: async function() {
      return await parseTurtleOptimized(this.ttl, 'http://example.org/', { batchSize: 500 });
    },
    iterations: 500,
    warmup: 50
  },

  'query simple - BASELINE': {
    setup: async () => ({
      store: await createTestStore(500),
      sparql: generateSparqlQuery(0)
    }),
    fn: async function() {
      return await query(this.store, this.sparql);
    },
    iterations: 500,
    warmup: 50
  },

  'query simple - OPTIMIZED (cold cache)': {
    setup: async () => {
      clearQueryCache();
      return {
        store: await createTestStore(500),
        sparql: generateSparqlQuery(0)
      };
    },
    fn: async function() {
      clearQueryCache();
      return await queryOptimized(this.store, this.sparql);
    },
    iterations: 500,
    warmup: 50
  },

  'query simple - OPTIMIZED (warm cache)': {
    setup: async () => {
      const store = await createTestStore(500);
      const sparql = generateSparqlQuery(0);
      await queryOptimized(store, sparql);
      return { store, sparql };
    },
    fn: async function() {
      return await queryOptimized(this.store, this.sparql);
    },
    iterations: 2000,
    warmup: 200
  }
});

// =============================================================================
// Runner
// =============================================================================

async function runAllBenchmarks() {
  console.log('╔═══════════════════════════════════════════════════════════════════════╗');
  console.log('║          V6 Optimized Performance Benchmark Suite                     ║');
  console.log('╚═══════════════════════════════════════════════════════════════════════╝\n');

  const results = [];

  const suites = [
    { name: 'Parsing (Optimized)', fn: parsingOptimizedBenchmarks },
    { name: 'Query (Optimized)', fn: queryOptimizedBenchmarks },
    { name: 'Comparison', fn: comparisonBenchmarks }
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

  console.log('\n' + '═'.repeat(80));
  console.log('BENCHMARK SUMMARY');
  console.log('═'.repeat(80) + '\n');

  for (const result of results) {
    console.log(formatMarkdownTable(result));
  }

  return results;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const results = await runAllBenchmarks();

  const { writeFile, mkdir } = await import('node:fs/promises');
  const { join } = await import('node:path');

  const reportsDir = join(process.cwd(), 'benchmarks', 'reports');
  await mkdir(reportsDir, { recursive: true });

  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const reportPath = join(reportsDir, `v6-optimized-${timestamp}.md`);

  const reportLines = [
    '# V6 Optimized Performance Report\n',
    `**Generated**: ${new Date().toISOString()}\n`,
    '## Summary\n'
  ];

  for (const result of results) {
    if (!result.failed) {
      reportLines.push(formatMarkdownTable(result));
    }
  }

  await writeFile(reportPath, reportLines.join('\n'), 'utf-8');
  console.log(`\n📊 Optimized report written to: ${reportPath}`);

  process.exit(0);
}

export { runAllBenchmarks };
