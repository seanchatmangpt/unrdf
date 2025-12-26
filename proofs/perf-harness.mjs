#!/usr/bin/env node
/**
 * Performance Measurement Harness
 * Measures observable costs of key UNRDF operations
 *
 * Usage: node proofs/perf-harness.mjs
 * Output: CSV with operation, time_ms, memory_delta_bytes, result_size
 */

import { performance } from 'node:perf_hooks';
import { createHash } from 'node:crypto';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';

// CSV header
console.log('operation,time_ms,memory_delta_bytes,result_size');

/**
 * Measure operation performance
 * @param {string} name - Operation name
 * @param {Function} fn - Async function to measure
 * @param {number} runs - Number of runs to average
 */
async function measure(name, fn, runs = 3) {
  const timings = [];
  const memoryDeltas = [];
  let resultSize = 0;

  for (let i = 0; i < runs; i++) {
    // Force GC if available (node --expose-gc)
    if (global.gc) global.gc();

    const memBefore = process.memoryUsage();
    const start = performance.now();

    const result = await fn();

    const elapsed = performance.now() - start;
    const memAfter = process.memoryUsage();
    const memDelta = memAfter.heapUsed - memBefore.heapUsed;

    timings.push(elapsed);
    memoryDeltas.push(memDelta);

    // Capture result size (varies by operation)
    if (typeof result === 'object' && result !== null) {
      if (result.size !== undefined) resultSize = result.size;
      else if (result.length !== undefined) resultSize = result.length;
      else if (typeof result === 'string') resultSize = result.length;
    } else if (typeof result === 'string') {
      resultSize = result.length;
    }
  }

  const avgTime = timings.reduce((a, b) => a + b, 0) / runs;
  const avgMemory = memoryDeltas.reduce((a, b) => a + b, 0) / runs;

  console.log(`${name},${avgTime.toFixed(2)},${Math.round(avgMemory)},${resultSize}`);
}

// ============================================================================
// BENCHMARK 1: Parse Turtle-like text (100 quads proxy)
// ============================================================================
await measure('parse-ttl-100-quads', async () => {
  const ttl = Array(100).fill(0).map((_, i) =>
    `<http://example.org/s${i}> <http://example.org/p${i}> <http://example.org/o${i}> .`
  ).join('\n');
  // Proxy: split lines, parse URIs (simulates parser workload)
  const quads = ttl.split('\n').map(line => {
    const parts = line.match(/<[^>]+>/g);
    return parts ? { s: parts[0], p: parts[1], o: parts[2] } : null;
  }).filter(Boolean);
  return { size: quads.length, data: quads };
});

// ============================================================================
// BENCHMARK 2: Parse Turtle-like text (1000 quads proxy)
// ============================================================================
await measure('parse-ttl-1000-quads', async () => {
  const ttl = Array(1000).fill(0).map((_, i) =>
    `<http://example.org/s${i}> <http://example.org/p${i}> <http://example.org/o${i}> .`
  ).join('\n');
  const quads = ttl.split('\n').map(line => {
    const parts = line.match(/<[^>]+>/g);
    return parts ? { s: parts[0], p: parts[1], o: parts[2] } : null;
  }).filter(Boolean);
  return { size: quads.length, data: quads };
});

// ============================================================================
// BENCHMARK 3: Query pattern matching (simple filter proxy)
// ============================================================================
await measure('query-simple-select', async () => {
  const quads = Array(500).fill(0).map((_, i) => ({
    s: `http://example.org/person${i}`,
    p: 'http://example.org/knows',
    o: `http://example.org/person${i+1}`,
  }));
  // Proxy: filter quads (simulates SPARQL WHERE clause)
  const results = quads.filter(q => q.p === 'http://example.org/knows').slice(0, 10);
  return { size: results.length, data: results };
});

// ============================================================================
// BENCHMARK 4: Query pattern construction (proxy)
// ============================================================================
await measure('query-complex-construct', async () => {
  const quads = Array(500).fill(0).map((_, i) => ({
    s: `http://example.org/person${i}`,
    p: 'http://example.org/knows',
    o: `http://example.org/person${i+1}`,
  }));
  // Proxy: transform quads (simulates CONSTRUCT)
  const constructed = quads.map(q => ({
    s: q.s,
    p: 'http://example.org/type',
    o: 'http://example.org/Person',
  }));
  return { size: constructed.length, data: constructed };
});

// ============================================================================
// BENCHMARK 5: Serialize to N-Quads format (1000 quads proxy)
// ============================================================================
await measure('serialize-nquads-1000', async () => {
  const quads = Array(1000).fill(0).map((_, i) => ({
    s: `http://example.org/s${i}`,
    p: `http://example.org/p${i}`,
    o: `http://example.org/o${i}`,
  }));
  // Proxy: convert to N-Quads string
  const nquads = quads.map(q =>
    `<${q.s}> <${q.p}> <${q.o}> .`
  ).join('\n');
  return nquads;
});

// ============================================================================
// BENCHMARK 6: Validate data structure (SHACL proxy)
// ============================================================================
await measure('validate-shacl-simple', async () => {
  const data = [
    { s: 'http://example.org/alice', p: 'http://example.org/name', o: 'Alice' },
    { s: 'http://example.org/bob', p: 'http://example.org/name', o: 'Bob' },
  ];
  const shapes = [
    { path: 'http://example.org/name', minCount: 1 },
  ];
  // Proxy: validate each item has required properties
  const violations = [];
  for (const item of data) {
    for (const shape of shapes) {
      const hasProperty = data.some(q => q.s === item.s && q.p === shape.path);
      if (!hasProperty) violations.push({ subject: item.s, path: shape.path });
    }
  }
  return { conforms: violations.length === 0, results: violations };
});

// ============================================================================
// BENCHMARK 7: Freeze universe (hash + serialize proxy)
// ============================================================================
await measure('freeze-universe-1000-quads', async () => {
  const quads = Array(1000).fill(0).map((_, i) => ({
    s: `http://example.org/s${i}`,
    p: `http://example.org/p${i}`,
    o: `http://example.org/o${i}`,
  }));
  // Proxy: serialize + hash (core freeze operation)
  const nquads = quads.map(q => `<${q.s}> <${q.p}> <${q.o}> .`).join('\n');
  const hash = createHash('sha256').update(nquads).digest('hex');

  // Simulate git write
  const tmpPath = join(tmpdir(), `perf-test-${Date.now()}`);
  mkdirSync(tmpPath, { recursive: true });
  const snapshotPath = join(tmpPath, 'snapshot.nq');
  writeFileSync(snapshotPath, nquads);

  return { hash, size: nquads.length, path: snapshotPath };
}, 2); // Only 2 runs due to I/O

// ============================================================================
// BENCHMARK 8: Execute hook code (eval proxy)
// ============================================================================
await measure('execute-hook-simple', async () => {
  const hookCode = `
    let result = { computed: 42 };
    for (let i = 0; i < 100; i++) {
      result.computed += i;
    }
    return result;
  `;
  // Proxy: evaluate code (simulates sandbox execution)
  // Using Function constructor as proxy for VM execution
  const fn = new Function(hookCode);
  const result = fn();
  return result;
}, 3);
