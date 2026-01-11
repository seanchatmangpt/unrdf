/**
 * @file Quick RDF Core Benchmark Runner
 * @description Fast benchmark execution with reduced iterations for CI/CD
 */

import { performance } from 'perf_hooks';
import { createStore, addQuad, namedNode, literal, executeSelect, getQuads, countQuads, validateQuad } from '@unrdf/core';
import { Parser, Writer } from '@unrdf/core/rdf/n3-justified-only';
import { RDF, FOAF } from '@unrdf/core';
import { saveReport, generateReport } from './suite.mjs';

// Simple benchmark helper
function benchmark(name, fn, iterations = 10) {
  const times = [];
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    fn();
    times.push(performance.now() - start);
  }
  const sorted = times.sort((a, b) => a - b);
  return {
    mean: times.reduce((a, b) => a + b) / times.length,
    p50: sorted[Math.floor(sorted.length * 0.5)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

async function benchmarkAsync(name, fn, iterations = 10) {
  const times = [];
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    times.push(performance.now() - start);
  }
  const sorted = times.sort((a, b) => a - b);
  return {
    mean: times.reduce((a, b) => a + b) / times.length,
    p50: sorted[Math.floor(sorted.length * 0.5)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

console.log('═══════════════════════════════════════════════════════════════');
console.log('         RDF CORE QUICK BENCHMARK SUITE');
console.log('═══════════════════════════════════════════════════════════════\n');

const startTime = performance.now();
const results = {};

// 1. Parsing Benchmark
console.log('▶ Parsing Benchmarks...');
const turtleData = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
ex:person1 a foaf:Person ; foaf:name "Alice" .
ex:person2 a foaf:Person ; foaf:name "Bob" .
`;

const parseLatency = await benchmarkAsync('parse-turtle', async () => {
  return new Promise((resolve, reject) => {
    const parser = new Parser({ format: 'text/turtle' });
    const quads = [];
    parser.parse(turtleData, (error, quad) => {
      if (error) reject(error);
      else if (quad) quads.push(quad);
      else resolve(quads);
    });
  });
}, 20);

results.parsing = {
  results: {
    'turtle-small': {
      latency: parseLatency,
      passed: parseLatency.p95 < 50,
      target: 'P95 < 50ms',
      unit: 'parses/s',
    }
  },
  summary: { total: 1, passed: parseLatency.p95 < 50 ? 1 : 0, failed: 0 }
};
console.log(`✓ Parsing: P95=${parseLatency.p95.toFixed(2)}ms\n`);

// 2. Graph Operations Benchmark
console.log('▶ Graph Operations Benchmarks...');
const addLatency = benchmark('add-quads', () => {
  const store = createStore();
  for (let i = 0; i < 100; i++) {
    addQuad(store, {
      subject: namedNode(`http://example.org/s${i}`),
      predicate: namedNode('http://example.org/p'),
      object: literal(`Object ${i}`),
    });
  }
}, 20);

const store = createStore();
for (let i = 0; i < 100; i++) {
  addQuad(store, {
    subject: namedNode(`http://example.org/s${i}`),
    predicate: FOAF.name,
    object: literal(`Person ${i}`),
  });
}

const findLatency = benchmark('find-quads', () => {
  getQuads(store, null, FOAF.name);
}, 50);

const countLatency = benchmark('count-quads', () => {
  countQuads(store);
}, 100);

results['graph-operations'] = {
  results: {
    'add-100': {
      latency: addLatency,
      passed: addLatency.p95 < 10,
      target: 'P95 < 10ms',
      unit: 'ops/s',
    },
    'find-pattern': {
      latency: findLatency,
      passed: findLatency.p95 < 5,
      target: 'P95 < 5ms',
      unit: 'queries/s',
    },
    'count-100': {
      latency: countLatency,
      passed: countLatency.p95 < 1,
      target: 'P95 < 1ms',
      unit: 'counts/s',
    }
  },
  summary: {
    total: 3,
    passed: (addLatency.p95 < 10 ? 1 : 0) + (findLatency.p95 < 5 ? 1 : 0) + (countLatency.p95 < 1 ? 1 : 0),
    failed: 0
  }
};
console.log(`✓ Add: P95=${addLatency.p95.toFixed(2)}ms`);
console.log(`✓ Find: P95=${findLatency.p95.toFixed(2)}ms`);
console.log(`✓ Count: P95=${countLatency.p95.toFixed(2)}ms\n`);

// 3. SPARQL Benchmark
console.log('▶ SPARQL Benchmarks...');
const queryLatency = await benchmarkAsync('sparql-select', async () => {
  return await executeSelect(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE { ?s foaf:name ?name }
  `);
}, 20);

results.sparql = {
  results: {
    'select-simple': {
      latency: queryLatency,
      passed: queryLatency.p95 < 10,
      target: 'P95 < 10ms',
      unit: 'queries/s',
    }
  },
  summary: { total: 1, passed: queryLatency.p95 < 10 ? 1 : 0, failed: 0 }
};
console.log(`✓ SELECT: P95=${queryLatency.p95.toFixed(2)}ms\n`);

// 4. Validation Benchmark
console.log('▶ Validation Benchmarks...');
const validationLatency = benchmark('quad-validation', () => {
  validateQuad({
    subject: namedNode('http://example.org/s'),
    predicate: namedNode('http://example.org/p'),
    object: literal('value'),
  });
}, 500);

results.validation = {
  results: {
    'quad-validation': {
      latency: validationLatency,
      passed: validationLatency.p95 < 0.1,
      target: 'P95 < 0.1ms',
      unit: 'validations/s',
    }
  },
  summary: { total: 1, passed: validationLatency.p95 < 0.1 ? 1 : 0, failed: 0 }
};
console.log(`✓ Validation: P95=${validationLatency.p95.toFixed(3)}ms\n`);

// Generate report
const report = generateReport(results, {});
const totalTime = performance.now() - startTime;

console.log('═══════════════════════════════════════════════════════════════');
console.log('                        SUMMARY');
console.log('═══════════════════════════════════════════════════════════════\n');
console.log(`Total Benchmarks:  ${report.summary.totalBenchmarks}`);
console.log(`Passed:            ${report.summary.passed} ✓`);
console.log(`Failed:            ${report.summary.failed}`);
console.log(`Execution Time:    ${(totalTime / 1000).toFixed(2)}s\n`);

console.log('── Performance Results ──────────────────────────────────────');
for (const [name, result] of Object.entries(report.benchmarks)) {
  const status = result.passed ? '✓' : '✗';
  console.log(`${status} ${name}: P95=${result.latency.p95.toFixed(3)}ms (${result.target})`);
}
console.log('═══════════════════════════════════════════════════════════════\n');

// Save report
saveReport(report);

console.log(report.summary.failed === 0 ? '✅ All benchmarks passed!\n' : '❌ Some benchmarks failed!\n');
process.exit(report.summary.failed === 0 ? 0 : 1);
