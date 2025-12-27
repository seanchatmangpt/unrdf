#!/usr/bin/env node
/**
 * @file UNRDF v6 Performance Benchmark Suite (Lite Version)
 * @description Performance benchmarking for v6 features without heavy dependencies
 *
 * Features benchmarked:
 * - Delta capsule validation (Zod) - target <5ms
 * - Zod schema validation - target <2ms
 * - Store initialization - target <50ms
 * - Simple SPARQL query (10 triples) - target <10ms
 * - Memory profiling (heap usage, leak detection)
 * - Scalability analysis
 *
 * Usage:
 *   node benchmarks/v6-perf-lite.mjs                    # Run all benchmarks
 *   node benchmarks/v6-perf-lite.mjs --baseline         # Create new baseline
 *   node benchmarks/v6-perf-lite.mjs --regression       # Check for regression
 */

import { performance } from 'node:perf_hooks';
import { writeFileSync, readFileSync, existsSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';

// Import v6 modules (avoiding hash-wasm dependencies)
import { DeltaCapsule, DeltaCapsuleSchema } from '../src/admission/delta-capsule.mjs';
import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';

// Import benchmark framework
import {
  runBenchmark,
  suite,
  calculateStats,
  getMemoryUsage,
  calculateMemoryDelta,
  formatBytes,
  formatMarkdownTable
} from './framework.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Constants
const BASELINE_PATH = join(__dirname, 'v6-baseline.csv');
const REPORT_PATH = join(__dirname, 'v6-performance-report.md');
const ITERATIONS = 1000;
const WARMUP = 100;

// =============================================================================
// Mock Receipt Implementation (for benchmarking without hash-wasm)
// =============================================================================

class MockReceipt {
  constructor(data) {
    Object.assign(this, data);
    this.receiptHash = this._computeHash();
    Object.freeze(this);
  }

  _computeHash() {
    const content = JSON.stringify({
      inputHashes: this.inputHashes,
      decision: this.decision,
      outputHash: this.outputHash
    });
    return createHash('sha256').update(content).digest('hex');
  }

  static async create(options) {
    const epoch = `τ_${new Date().toISOString().replace(/[:-]/g, '_').slice(0, 23)}`;

    return new MockReceipt({
      inputHashes: options.inputHashes,
      decision: options.decision,
      epoch,
      outputHash: options.outputHash,
      toolchainVersion: options.toolchainVersion,
      generatedAtTime: options.timestamp?.toISOString() || new Date().toISOString(),
      beforeHash: options.beforeHash,
      merkleRoot: options.merkleRoot || null
    });
  }
}

class MockReceiptChain {
  constructor() {
    this.receipts = [];
  }

  async append(receipt) {
    this.receipts.push(receipt);
  }

  getLast() {
    return this.receipts[this.receipts.length - 1] || null;
  }

  async verify() {
    // Simple chain verification
    for (let i = 1; i < this.receipts.length; i++) {
      const current = this.receipts[i];
      const previous = this.receipts[i - 1];

      if (current.beforeHash !== previous.receiptHash) {
        return {
          valid: false,
          errors: [`Chain broken at index ${i}`]
        };
      }
    }

    return { valid: true, errors: [] };
  }
}

class MockReceiptGenerator {
  constructor() {
    this.chain = new MockReceiptChain();
  }

  async emitAdmissibilityReceipt(options) {
    const outputHash = createHash('sha256')
      .update(JSON.stringify(options.universeState))
      .digest('hex');

    const beforeHash = this.chain.getLast()?.receiptHash || null;

    const receipt = await MockReceipt.create({
      inputHashes: {
        ontologyReleases: options.ontologyReleases,
        deltaCapsule: options.deltaCapsule
      },
      decision: options.decision,
      outputHash,
      toolchainVersion: {
        node: process.version,
        packages: {}
      },
      beforeHash,
      timestamp: options.timestamp
    });

    await this.chain.append(receipt);
    return receipt;
  }

  getChain() {
    return this.chain;
  }

  async verifyChain() {
    return this.chain.verify();
  }
}

// =============================================================================
// Core Operation Benchmarks
// =============================================================================

/**
 * Benchmark: Mock Receipt Creation (SHA-256)
 * Target: <1ms
 */
const receiptCreationBenchmark = {
  fn: async function() {
    const receipt = await MockReceipt.create({
      inputHashes: {
        ontologyReleases: ['hash1', 'hash2'],
        deltaCapsule: 'hash3'
      },
      decision: 'allow',
      outputHash: 'hash4',
      toolchainVersion: {
        node: process.version,
        packages: { '@unrdf/core': '6.0.0' }
      },
      beforeHash: null
    });
    return receipt;
  },
  iterations: ITERATIONS,
  warmup: WARMUP
};

/**
 * Benchmark: Delta Capsule Validation
 * Target: <5ms
 */
const deltaValidationBenchmark = {
  fn: async function() {
    const delta = new DeltaCapsule({
      partition: {
        namespace: 'http://example.org/',
        name: 'overlay-1',
        protected: false
      },
      changes: [{
        operation: 'add',
        quads: [{
          subject: { termType: 'NamedNode', value: 'http://example.org/entity1' },
          predicate: { termType: 'NamedNode', value: 'http://example.org/prop' },
          object: { termType: 'Literal', value: 'value1' }
        }]
      }],
      invariants: [
        { name: 'Q_typing', enabled: true, strictness: 'error' }
      ],
      provenance: {
        agent: 'benchmark',
        timestamp: new Date().toISOString()
      }
    });
    return delta;
  },
  iterations: ITERATIONS,
  warmup: WARMUP
};

/**
 * Benchmark: Zod Schema Validation
 * Target: <2ms
 */
const zodValidationBenchmark = {
  fn: async function() {
    const result = DeltaCapsuleSchema.parse({
      id: crypto.randomUUID(),
      partition: {
        namespace: 'http://example.org/',
        name: 'test-partition',
        protected: false
      },
      changes: [{
        operation: 'add',
        quads: [{
          subject: { termType: 'NamedNode', value: 'http://example.org/s' },
          predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
          object: { termType: 'Literal', value: 'o' }
        }]
      }],
      invariants: [{ name: 'test', enabled: true, strictness: 'error' }],
      provenance: {
        agent: 'test',
        timestamp: new Date().toISOString()
      }
    });
    return result;
  },
  iterations: ITERATIONS,
  warmup: WARMUP
};

/**
 * Benchmark: Store Initialization
 * Target: <50ms
 */
const storeInitBenchmark = {
  fn: async function() {
    const store = createStore();
    return store;
  },
  iterations: 100,
  warmup: 10
};

/**
 * Benchmark: Simple SPARQL Query (10 triples)
 * Target: <10ms
 */
const sparqlQueryBenchmark = {
  setup: async function() {
    const store = createStore();
    const { quad, namedNode, literal } = dataFactory;

    for (let i = 0; i < 10; i++) {
      store.add(quad(
        namedNode(`http://example.org/entity${i}`),
        namedNode('http://example.org/prop'),
        literal(`value${i}`)
      ));
    }

    return { store };
  },
  fn: async function() {
    const results = this.store.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
    const bindings = Array.from(results);
    return bindings;
  },
  iterations: ITERATIONS,
  warmup: WARMUP
};

/**
 * Benchmark: Receipt Chain Generation (10 receipts)
 * Target: <50ms
 */
const receiptChainBenchmark = {
  fn: async function() {
    const generator = new MockReceiptGenerator();

    for (let i = 0; i < 10; i++) {
      await generator.emitAdmissibilityReceipt({
        ontologyReleases: [`hash_${i}`],
        deltaCapsule: `delta_${i}`,
        decision: 'allow',
        universeState: { step: i }
      });
    }

    return generator.getChain();
  },
  iterations: 100,
  warmup: 10
};

/**
 * Benchmark: Receipt Chain Verification
 * Target: <20ms for 10 receipts
 */
const receiptChainVerificationBenchmark = {
  setup: async function() {
    const generator = new MockReceiptGenerator();

    for (let i = 0; i < 10; i++) {
      await generator.emitAdmissibilityReceipt({
        ontologyReleases: [`hash_${i}`],
        deltaCapsule: `delta_${i}`,
        decision: 'allow',
        universeState: { step: i }
      });
    }

    return { generator };
  },
  fn: async function() {
    const result = await this.generator.verifyChain();
    return result;
  },
  iterations: 100,
  warmup: 10
};

// =============================================================================
// Memory Profiling
// =============================================================================

async function memoryProfileDeltas() {
  console.log('\n' + '='.repeat(80));
  console.log('Memory Profile: Delta Capsule Creation (1,000 deltas)');
  console.log('='.repeat(80));

  if (global.gc) global.gc();
  await new Promise(resolve => setTimeout(resolve, 100));

  const memBefore = getMemoryUsage();
  const deltas = [];

  for (let i = 0; i < 1000; i++) {
    const delta = new DeltaCapsule({
      partition: { namespace: 'http://example.org/', name: 'overlay', protected: false },
      changes: [{
        operation: 'add',
        quads: [{
          subject: { termType: 'NamedNode', value: `http://example.org/s${i}` },
          predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
          object: { termType: 'Literal', value: `value${i}` }
        }]
      }],
      invariants: [{ name: 'test', enabled: true, strictness: 'error' }],
      provenance: { agent: 'benchmark', timestamp: new Date().toISOString() }
    });
    deltas.push(delta);
  }

  const memAfter = getMemoryUsage();
  const delta = calculateMemoryDelta(memBefore, memAfter);

  console.log(`  Deltas created: 1,000`);
  console.log(`  Heap used: ${formatBytes(delta.heapUsed)}`);
  console.log(`  RSS: ${formatBytes(delta.rss)}`);
  console.log(`  Avg per delta: ${formatBytes(delta.heapUsed / 1000)}`);

  return {
    deltasCreated: 1000,
    memoryDelta: delta,
    avgPerDelta: delta.heapUsed / 1000
  };
}

async function memoryLeakDetection() {
  console.log('\n' + '='.repeat(80));
  console.log('Memory Leak Detection: Delta Capsules (10,000 iterations)');
  console.log('='.repeat(80));

  if (global.gc) global.gc();
  await new Promise(resolve => setTimeout(resolve, 100));

  const memSnapshots = [];
  const iterations = 10;
  const deltasPerIteration = 1000;

  for (let iter = 0; iter < iterations; iter++) {
    for (let i = 0; i < deltasPerIteration; i++) {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.org/s' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
            object: { termType: 'Literal', value: 'o' }
          }]
        }],
        invariants: [{ name: 'test', enabled: true, strictness: 'error' }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });
    }

    if (global.gc) global.gc();
    await new Promise(resolve => setTimeout(resolve, 50));

    memSnapshots.push(getMemoryUsage());
  }

  const firstHeap = memSnapshots[0].heapUsed;
  const lastHeap = memSnapshots[memSnapshots.length - 1].heapUsed;
  const growth = lastHeap - firstHeap;
  const growthPercent = (growth / firstHeap) * 100;

  console.log(`  Iterations: ${iterations}`);
  console.log(`  Deltas per iteration: ${deltasPerIteration}`);
  console.log(`  Initial heap: ${formatBytes(firstHeap)}`);
  console.log(`  Final heap: ${formatBytes(lastHeap)}`);
  console.log(`  Growth: ${formatBytes(growth)} (${growthPercent.toFixed(2)}%)`);
  console.log(`  Leak detected: ${growthPercent > 10 ? 'YES ⚠️' : 'NO ✓'}`);

  return {
    iterations,
    deltasPerIteration,
    initialHeap: firstHeap,
    finalHeap: lastHeap,
    growth,
    growthPercent,
    leakDetected: growthPercent > 10
  };
}

async function stressTestLargeOperation() {
  console.log('\n' + '='.repeat(80));
  console.log('Stress Test: Large Operation (10,000 deltas)');
  console.log('='.repeat(80));

  if (global.gc) global.gc();
  await new Promise(resolve => setTimeout(resolve, 100));

  const memBefore = getMemoryUsage();
  const startTime = performance.now();

  const deltas = [];
  for (let i = 0; i < 10000; i++) {
    const delta = new DeltaCapsule({
      partition: { namespace: 'http://example.org/', name: 'stress', protected: false },
      changes: [{
        operation: 'add',
        quads: [{
          subject: { termType: 'NamedNode', value: `http://example.org/s${i}` },
          predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
          object: { termType: 'Literal', value: `value${i}` }
        }]
      }],
      invariants: [{ name: 'test', enabled: true, strictness: 'error' }],
      provenance: { agent: 'stress', timestamp: new Date().toISOString() }
    });
    deltas.push(delta);
  }

  const elapsed = performance.now() - startTime;
  const memAfter = getMemoryUsage();
  const delta = calculateMemoryDelta(memBefore, memAfter);

  console.log(`  Deltas created: 10,000`);
  console.log(`  Time: ${elapsed.toFixed(2)}ms`);
  console.log(`  Throughput: ${(10000 / (elapsed / 1000)).toFixed(0)} deltas/sec`);
  console.log(`  Heap used: ${formatBytes(delta.heapUsed)}`);
  console.log(`  Avg per delta: ${formatBytes(delta.heapUsed / 10000)}`);

  return {
    deltasCreated: 10000,
    timeMs: elapsed,
    throughput: 10000 / (elapsed / 1000),
    memoryDelta: delta,
    avgPerDelta: delta.heapUsed / 10000
  };
}

// =============================================================================
// Scalability Analysis
// =============================================================================

async function scalabilityReceiptChain() {
  console.log('\n' + '='.repeat(80));
  console.log('Scalability Analysis: Receipt Verification vs Chain Length');
  console.log('='.repeat(80));

  const chainLengths = [1, 10, 50, 100, 500, 1000];
  const results = [];

  for (const length of chainLengths) {
    const generator = new MockReceiptGenerator();

    for (let i = 0; i < length; i++) {
      await generator.emitAdmissibilityReceipt({
        ontologyReleases: [`hash_${i}`],
        deltaCapsule: `delta_${i}`,
        decision: 'allow',
        universeState: { step: i }
      });
    }

    const times = [];
    for (let run = 0; run < 10; run++) {
      const start = performance.now();
      await generator.verifyChain();
      const elapsed = performance.now() - start;
      times.push(elapsed);
    }

    const stats = calculateStats(times);
    results.push({
      chainLength: length,
      medianMs: stats.median,
      p95Ms: stats.p95
    });

    console.log(`  Chain length ${length}: ${stats.median.toFixed(3)}ms (p95: ${stats.p95.toFixed(3)}ms)`);
  }

  const ratio1_10 = results[1].medianMs / results[0].medianMs;
  const ratio10_100 = results[3].medianMs / results[1].medianMs;

  let scaling = 'unknown';
  if (ratio1_10 < 2 && ratio10_100 < 2) {
    scaling = 'logarithmic';
  } else if (ratio1_10 < 15 && ratio10_100 < 15) {
    scaling = 'linear';
  } else {
    scaling = 'exponential';
  }

  console.log(`\n  Scaling behavior: ${scaling.toUpperCase()}`);
  console.log(`  1→10 ratio: ${ratio1_10.toFixed(2)}x`);
  console.log(`  10→100 ratio: ${ratio10_100.toFixed(2)}x`);

  return {
    results,
    scaling,
    ratios: { '1_to_10': ratio1_10, '10_to_100': ratio10_100 }
  };
}

async function scalabilityDeltaReconciliation() {
  console.log('\n' + '='.repeat(80));
  console.log('Scalability Analysis: Delta Creation vs Package Count');
  console.log('='.repeat(80));

  const packageCounts = [1, 5, 10, 20, 50];
  const results = [];

  for (const count of packageCounts) {
    const times = [];

    for (let run = 0; run < 10; run++) {
      const start = performance.now();

      const deltas = [];
      for (let i = 0; i < count; i++) {
        const delta = new DeltaCapsule({
          partition: { namespace: `http://pkg${i}.org/`, name: 'overlay', protected: false },
          changes: [{
            operation: 'add',
            quads: [{
              subject: { termType: 'NamedNode', value: `http://pkg${i}.org/s` },
              predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
              object: { termType: 'Literal', value: `value${i}` }
            }]
          }],
          invariants: [{ name: 'test', enabled: true, strictness: 'error' }],
          provenance: { agent: 'test', timestamp: new Date().toISOString() }
        });
        deltas.push(delta);
      }

      const elapsed = performance.now() - start;
      times.push(elapsed);
    }

    const stats = calculateStats(times);
    results.push({
      packageCount: count,
      medianMs: stats.median,
      p95Ms: stats.p95
    });

    console.log(`  Package count ${count}: ${stats.median.toFixed(3)}ms (p95: ${stats.p95.toFixed(3)}ms)`);
  }

  return { results };
}

// =============================================================================
// Baseline & Regression Detection
// =============================================================================

function saveBaseline(results) {
  const rows = [
    ['operation', 'median_ms', 'p95_ms', 'max_ms', 'throughput_ops_sec', 'target_ms', 'status']
  ];

  for (const [name, result] of Object.entries(results)) {
    if (result.failed || result.error) continue;

    const target = getTarget(name);
    const medianMs = result.latency ? result.latency.median : 0;
    const p95Ms = result.latency ? result.latency.p95 : 0;
    const maxMs = result.latency ? result.latency.max : 0;
    const throughput = result.throughput || 0;
    const status = medianMs <= target ? 'PASS' : 'FAIL';

    rows.push([
      name,
      medianMs.toFixed(3),
      p95Ms.toFixed(3),
      maxMs.toFixed(3),
      throughput.toFixed(2),
      target,
      status
    ]);
  }

  const csv = rows.map(row => row.join(',')).join('\n');
  writeFileSync(BASELINE_PATH, csv, 'utf-8');
  console.log(`\n✓ Baseline saved to ${BASELINE_PATH}`);
}

function checkRegression(currentResults) {
  if (!existsSync(BASELINE_PATH)) {
    console.log('\n⚠️  No baseline found. Run with --baseline first.');
    return false;
  }

  const baselineCSV = readFileSync(BASELINE_PATH, 'utf-8');
  const baselineRows = baselineCSV.trim().split('\n').slice(1);
  const baseline = {};

  for (const row of baselineRows) {
    const [operation, medianMs, p95Ms] = row.split(',');
    baseline[operation] = { medianMs: parseFloat(medianMs), p95Ms: parseFloat(p95Ms) };
  }

  console.log('\n' + '='.repeat(80));
  console.log('Regression Detection (10% threshold)');
  console.log('='.repeat(80));

  let regressions = 0;

  for (const [name, current] of Object.entries(currentResults)) {
    if (current.failed || current.error || !baseline[name]) continue;

    const baseMedian = baseline[name].medianMs;
    const currentMedian = current.latency.median;
    const delta = ((currentMedian - baseMedian) / baseMedian) * 100;

    const status = delta > 10 ? 'REGRESSION ✗' : delta < -10 ? 'IMPROVEMENT ✓' : 'STABLE';

    console.log(`  ${name}:`);
    console.log(`    Baseline: ${baseMedian.toFixed(3)}ms`);
    console.log(`    Current:  ${currentMedian.toFixed(3)}ms`);
    console.log(`    Delta:    ${delta > 0 ? '+' : ''}${delta.toFixed(2)}%`);
    console.log(`    Status:   ${status}`);

    if (delta > 10) regressions++;
  }

  console.log('\n' + '='.repeat(80));
  console.log(`Regressions found: ${regressions}`);
  console.log('='.repeat(80));

  return regressions === 0;
}

function getTarget(name) {
  const targets = {
    'Receipt Creation (Mock)': 1,
    'Delta Validation': 5,
    'Zod Validation': 2,
    'Store Initialization': 50,
    'SPARQL Query': 10,
    'Receipt Chain (10)': 50,
    'Chain Verification (10)': 20
  };
  return targets[name] || 100;
}

// =============================================================================
// Report Generation
// =============================================================================

function generateReport(results, memoryResults, scalabilityResults) {
  const lines = [];

  lines.push('# UNRDF v6 Performance Benchmark Report\n');
  lines.push(`**Generated**: ${new Date().toISOString()}\n`);
  lines.push(`**Node Version**: ${process.version}\n`);
  lines.push(`**Platform**: ${process.platform} ${process.arch}\n`);
  lines.push(`**Note**: Using mock receipts (SHA-256) instead of BLAKE3 for portability\n`);

  lines.push('\n## Core Operations\n');
  lines.push('| Operation | Median | P95 | P99 | Target | Status |');
  lines.push('|-----------|--------|-----|-----|--------|--------|');

  for (const [name, result] of Object.entries(results)) {
    if (result.failed || result.error) continue;

    const target = getTarget(name);
    const median = result.latency.median.toFixed(3);
    const p95 = result.latency.p95.toFixed(3);
    const p99 = result.latency.p99.toFixed(3);
    const status = result.latency.median <= target ? '✓ PASS' : '✗ FAIL';

    lines.push(`| ${name} | ${median}ms | ${p95}ms | ${p99}ms | <${target}ms | ${status} |`);
  }

  if (memoryResults) {
    lines.push('\n## Memory Profiling\n');
    lines.push('| Test | Result |');
    lines.push('|------|--------|');
    lines.push(`| 1,000 deltas heap | ${formatBytes(memoryResults.deltas.memoryDelta.heapUsed)} |`);
    lines.push(`| Avg per delta | ${formatBytes(memoryResults.deltas.avgPerDelta)} |`);
    lines.push(`| Memory leak detected | ${memoryResults.leak.leakDetected ? '⚠️ YES' : '✓ NO'} |`);
    lines.push(`| Memory growth | ${memoryResults.leak.growthPercent.toFixed(2)}% |`);
    lines.push(`| Stress test (10k deltas) | ${formatBytes(memoryResults.stress.memoryDelta.heapUsed)} |`);
    lines.push(`| Stress test throughput | ${memoryResults.stress.throughput.toFixed(0)} deltas/sec |`);
  }

  if (scalabilityResults) {
    lines.push('\n## Scalability Analysis\n');
    lines.push('\n### Receipt Chain Verification\n');
    lines.push('| Chain Length | Median | P95 |');
    lines.push('|--------------|--------|-----|');
    for (const result of scalabilityResults.receiptChain.results) {
      lines.push(`| ${result.chainLength} | ${result.medianMs.toFixed(3)}ms | ${result.p95Ms.toFixed(3)}ms |`);
    }
    lines.push(`\n**Scaling behavior**: ${scalabilityResults.receiptChain.scaling}\n`);
    lines.push(`- 1→10 ratio: ${scalabilityResults.receiptChain.ratios['1_to_10'].toFixed(2)}x`);
    lines.push(`- 10→100 ratio: ${scalabilityResults.receiptChain.ratios['10_to_100'].toFixed(2)}x\n`);

    lines.push('\n### Delta Creation Scalability\n');
    lines.push('| Package Count | Median | P95 |');
    lines.push('|---------------|--------|-----|');
    for (const result of scalabilityResults.deltaReconciliation.results) {
      lines.push(`| ${result.packageCount} | ${result.medianMs.toFixed(3)}ms | ${result.p95Ms.toFixed(3)}ms |`);
    }
  }

  lines.push('\n## Performance Claims Validation\n');
  lines.push('| Claim | Target | Actual | Status |');
  lines.push('|-------|--------|--------|--------|');

  for (const [name, result] of Object.entries(results)) {
    if (result.failed || result.error) continue;
    const target = getTarget(name);
    const actual = result.latency.median.toFixed(3);
    const status = result.latency.median <= target ? '✓ PASS' : '✗ FAIL';
    const penalty = target > 0 ? ((result.latency.median / target - 1) * 100).toFixed(1) : '0.0';
    lines.push(`| ${name} | <${target}ms | ${actual}ms (${penalty}% ${penalty[0] === '-' ? 'better' : 'worse'}) | ${status} |`);
  }

  lines.push('\n## Interpretation\n');
  lines.push('- **PASS**: Operation meets performance target (≤10% penalty acceptable)');
  lines.push('- **FAIL**: Operation exceeds target (requires optimization)');
  lines.push('- **Memory leak**: >10% heap growth after 10,000 iterations');
  lines.push('- **Scalability**: logarithmic < linear < exponential (prefer logarithmic)');

  const report = lines.join('\n');
  writeFileSync(REPORT_PATH, report, 'utf-8');
  console.log(`\n✓ Report saved to ${REPORT_PATH}`);

  return report;
}

// =============================================================================
// Main Runner
// =============================================================================

async function main() {
  const args = process.argv.slice(2);
  const mode = args[0] || 'all';

  console.log('='.repeat(80));
  console.log('UNRDF v6 Performance Benchmark Suite (Lite)');
  console.log('='.repeat(80));
  console.log(`Mode: ${mode}`);
  console.log(`Node: ${process.version}`);
  console.log(`Platform: ${process.platform} ${process.arch}`);
  console.log(`GC Available: ${global.gc ? 'YES' : 'NO (run with --expose-gc)'}`);
  console.log('='.repeat(80));

  let results = {};
  let memoryResults = null;
  let scalabilityResults = null;

  // Core benchmarks
  if (mode === 'all' || mode === '--baseline' || mode === '--regression') {
    console.log('\n--- Core Operations ---\n');

    const coreSuite = suite('Core v6 Operations', {
      'Receipt Creation (Mock)': receiptCreationBenchmark,
      'Delta Validation': deltaValidationBenchmark,
      'Zod Validation': zodValidationBenchmark,
      'Store Initialization': storeInitBenchmark,
      'SPARQL Query': sparqlQueryBenchmark,
      'Receipt Chain (10)': receiptChainBenchmark,
      'Chain Verification (10)': receiptChainVerificationBenchmark
    });

    const suiteResults = await coreSuite();

    for (const result of suiteResults.results) {
      results[result.name] = result;
    }

    console.log('\n' + formatMarkdownTable(suiteResults));
  }

  // Memory profiling
  if (mode === 'all' || mode === '--memory') {
    console.log('\n--- Memory Profiling ---\n');

    const deltas = await memoryProfileDeltas();
    const leak = await memoryLeakDetection();
    const stress = await stressTestLargeOperation();

    memoryResults = { deltas, leak, stress };
  }

  // Scalability analysis
  if (mode === 'all' || mode === '--scalability') {
    console.log('\n--- Scalability Analysis ---\n');

    const receiptChain = await scalabilityReceiptChain();
    const deltaReconciliation = await scalabilityDeltaReconciliation();

    scalabilityResults = { receiptChain, deltaReconciliation };
  }

  // Generate report
  if (mode === 'all' || mode === '--baseline') {
    generateReport(results, memoryResults, scalabilityResults);
    saveBaseline(results);
  }

  // Regression check
  if (mode === '--regression') {
    const passed = checkRegression(results);
    process.exit(passed ? 0 : 1);
  }

  console.log('\n' + '='.repeat(80));
  console.log('Benchmark Complete');
  console.log('='.repeat(80));
}

main().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
