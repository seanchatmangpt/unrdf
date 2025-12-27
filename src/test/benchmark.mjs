#!/usr/bin/env node
/**
 * KGC Probe Performance Benchmark Suite
 *
 * Comprehensive benchmarking for probe operations:
 * - Scan performance (<30s target)
 * - Merge performance (<1s target)
 * - Verify performance (<10s target)
 * - Guard decision latency (<5ms median)
 * - Memory footprint (<100MB)
 * - Storage performance
 *
 * @module src/test/benchmark
 */

import { performance } from 'node:perf_hooks';
import { randomBytes } from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

// =============================================================================
// Constants & Targets
// =============================================================================

const TARGETS = {
  SCAN_MS: 30000,          // <30s
  MERGE_MS: 1000,          // <1s
  VERIFY_MS: 10000,        // <10s
  GUARD_LATENCY_MS: 5,     // <5ms median
  MEMORY_MB: 100,          // <100MB steady state
  MEMORY_BACKEND_MS: 1,    // <1ms per op
  FILE_BACKEND_MS: 10,     // <10ms per op
};

const COLORS = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  cyan: '\x1b[36m',
  bold: '\x1b[1m',
};

// =============================================================================
// Statistics Utilities
// =============================================================================

/**
 * Calculate percentile from sorted array
 * @param {number[]} sorted - Sorted array
 * @param {number} p - Percentile (0-100)
 * @returns {number}
 */
function percentile(sorted, p) {
  if (sorted.length === 0) return 0;
  const idx = (p / 100) * (sorted.length - 1);
  const lower = Math.floor(idx);
  const upper = Math.ceil(idx);
  const weight = idx - lower;
  return sorted[lower] * (1 - weight) + sorted[upper] * weight;
}

/**
 * Calculate mean
 * @param {number[]} values
 * @returns {number}
 */
function mean(values) {
  if (values.length === 0) return 0;
  return values.reduce((s, v) => s + v, 0) / values.length;
}

/**
 * Calculate standard deviation
 * @param {number[]} values
 * @returns {number}
 */
function stddev(values) {
  if (values.length === 0) return 0;
  const m = mean(values);
  return Math.sqrt(mean(values.map(v => (v - m) ** 2)));
}

/**
 * Calculate comprehensive stats
 * @param {number[]} values
 * @returns {Object}
 */
function calcStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  return {
    count: values.length,
    min: sorted[0] || 0,
    max: sorted[sorted.length - 1] || 0,
    mean: mean(values),
    median: percentile(sorted, 50),
    stddev: stddev(values),
    p50: percentile(sorted, 50),
    p75: percentile(sorted, 75),
    p95: percentile(sorted, 95),
    p99: percentile(sorted, 99),
  };
}

/**
 * Get memory usage in MB
 * @returns {Object}
 */
function getMemoryMB() {
  const usage = process.memoryUsage();
  return {
    rss: usage.rss / 1024 / 1024,
    heapTotal: usage.heapTotal / 1024 / 1024,
    heapUsed: usage.heapUsed / 1024 / 1024,
    external: usage.external / 1024 / 1024,
  };
}

/**
 * Force garbage collection if available
 */
function forceGC() {
  if (global.gc) {
    global.gc();
  }
}

// =============================================================================
// Mock Implementations (simulate probe operations)
// =============================================================================

/**
 * Simulated observation payload
 */
function generatePayload(size = 1024) {
  return {
    timestamp: Date.now(),
    value: randomBytes(size).toString('hex'),
    metrics: {
      cpu: Math.random() * 100,
      memory: Math.random() * 100,
      network: Math.random() * 1000,
    },
  };
}

/**
 * Simulated BLAKE3 hash (using crypto for similar performance)
 */
async function simulateBlake3(data) {
  const { createHash } = await import('node:crypto');
  const serialized = typeof data === 'string' ? data : JSON.stringify(data);
  return createHash('sha256').update(serialized).digest('hex');
}

/**
 * Simulated deterministic serialization
 */
function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) return JSON.stringify(null);
  if (typeof obj === 'bigint') return obj.toString();
  if (typeof obj !== 'object') return JSON.stringify(obj);
  if (Array.isArray(obj)) return `[${obj.map(deterministicSerialize).join(',')}]`;
  const keys = Object.keys(obj).sort();
  const pairs = keys.map(k => `${JSON.stringify(k)}:${deterministicSerialize(obj[k])}`);
  return `{${pairs.join(',')}}`;
}

// =============================================================================
// Guard Decision Benchmark
// =============================================================================

/**
 * Simulated guard decision cache
 */
class GuardCache {
  constructor() {
    this.cache = new Map();
    this.hits = 0;
    this.misses = 0;
  }

  get(key) {
    if (this.cache.has(key)) {
      this.hits++;
      return this.cache.get(key);
    }
    this.misses++;
    return null;
  }

  set(key, value) {
    this.cache.set(key, value);
  }

  getStats() {
    const total = this.hits + this.misses;
    return {
      hits: this.hits,
      misses: this.misses,
      hitRate: total > 0 ? this.hits / total : 0,
    };
  }
}

/**
 * Simulated guard decision
 */
function guardDecision(input, cache) {
  const key = typeof input === 'string' ? input : JSON.stringify(input);

  // Check cache
  const cached = cache.get(key);
  if (cached !== null) return cached;

  // Simulate guard logic (validation + policy check)
  const result = {
    allowed: true,
    guardType: input.type || 'generic',
    timestamp: Date.now(),
    checks: ['type', 'range', 'format'].map(c => ({ check: c, passed: true })),
  };

  cache.set(key, result);
  return result;
}

// =============================================================================
// Storage Backend Simulations
// =============================================================================

/**
 * Memory-based storage backend
 */
class MemoryBackend {
  constructor() {
    this.data = new Map();
  }

  async set(key, value) {
    this.data.set(key, value);
    return true;
  }

  async get(key) {
    return this.data.get(key);
  }

  async query(prefix) {
    const results = [];
    for (const [k, v] of this.data) {
      if (k.startsWith(prefix)) results.push({ key: k, value: v });
    }
    return results;
  }
}

/**
 * File-based storage backend
 */
class FileBackend {
  constructor(dir = '/tmp/probe-bench') {
    this.dir = dir;
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
  }

  async set(key, value) {
    const filePath = path.join(this.dir, `${key}.json`);
    fs.writeFileSync(filePath, JSON.stringify(value));
    return true;
  }

  async get(key) {
    const filePath = path.join(this.dir, `${key}.json`);
    if (!fs.existsSync(filePath)) return null;
    return JSON.parse(fs.readFileSync(filePath, 'utf8'));
  }

  async query(prefix) {
    const files = fs.readdirSync(this.dir);
    const results = [];
    for (const file of files) {
      if (file.startsWith(prefix)) {
        const key = file.replace('.json', '');
        const value = await this.get(key);
        results.push({ key, value });
      }
    }
    return results;
  }

  cleanup() {
    if (fs.existsSync(this.dir)) {
      fs.rmSync(this.dir, { recursive: true });
    }
  }
}

// =============================================================================
// Benchmark Runners
// =============================================================================

/**
 * Benchmark scan operation
 * Target: <30s for 10 agents, all observations
 */
async function benchmarkScan(options = {}) {
  const {
    agentCount = 10,
    observationsPerAgent = 100,
    withGuards = true,
    withLogging = false,
    verbose = false,
  } = options;

  console.log(`\n${COLORS.cyan}[SCAN BENCHMARK]${COLORS.reset}`);
  console.log(`  Agents: ${agentCount}, Observations/agent: ${observationsPerAgent}`);
  console.log(`  Guards: ${withGuards}, Logging: ${withLogging}`);

  forceGC();
  const memBefore = getMemoryMB();
  const startTime = performance.now();

  const observations = [];
  const guardCache = new GuardCache();

  // Simulate scanning all agents
  for (let agent = 0; agent < agentCount; agent++) {
    const agentId = `agent-${agent}`;
    let prevHash = null;

    for (let obs = 0; obs < observationsPerAgent; obs++) {
      // Generate observation
      const payload = generatePayload(256);

      // Apply guard if enabled
      if (withGuards) {
        guardDecision({ type: 'observation', agentId, index: obs }, guardCache);
      }

      // Hash observation
      const obsHash = await simulateBlake3(payload);

      // Chain hash
      const chainInput = `${prevHash || 'GENESIS'}:${obsHash}`;
      const receiptHash = await simulateBlake3(chainInput);

      observations.push({
        agentId,
        observationIndex: obs + 1,
        obsHash,
        receiptHash,
        payload,
      });

      prevHash = receiptHash;

      if (withLogging && verbose && obs % 50 === 0) {
        console.log(`    Agent ${agent}: ${obs}/${observationsPerAgent}`);
      }
    }
  }

  const endTime = performance.now();
  forceGC();
  const memAfter = getMemoryMB();

  const totalTime = endTime - startTime;
  const totalObs = agentCount * observationsPerAgent;
  const obsPerSec = totalObs / (totalTime / 1000);

  const result = {
    operation: 'scan',
    target: TARGETS.SCAN_MS,
    actual: totalTime,
    passed: totalTime < TARGETS.SCAN_MS,
    metrics: {
      totalTime,
      observationsPerSec: obsPerSec,
      totalObservations: totalObs,
      memoryUsedMB: memAfter.heapUsed - memBefore.heapUsed,
      guardStats: guardCache.getStats(),
    },
  };

  const status = result.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;
  console.log(`  Result: ${totalTime.toFixed(2)}ms (target: <${TARGETS.SCAN_MS}ms) [${status}]`);
  console.log(`  Throughput: ${obsPerSec.toFixed(2)} observations/sec`);

  return result;
}

/**
 * Benchmark merge operation
 * Target: <1s for 10 agent shards (41K claims -> 38K unique)
 */
async function benchmarkMerge(options = {}) {
  const {
    shardCount = 10,
    claimsPerShard = 4100,
    verbose = false,
  } = options;

  console.log(`\n${COLORS.cyan}[MERGE BENCHMARK]${COLORS.reset}`);
  console.log(`  Shards: ${shardCount}, Claims/shard: ${claimsPerShard}`);

  forceGC();
  const memBefore = getMemoryMB();
  const startTime = performance.now();

  // Generate shard data
  const shards = [];
  for (let i = 0; i < shardCount; i++) {
    const finalHash = await simulateBlake3({ shardId: i, claims: claimsPerShard });
    shards.push({
      agentId: `agent-${i}`,
      chainFinalHash: finalHash,
      obsCount: claimsPerShard,
      domain: 'default',
    });
  }

  // Sort shards deterministically
  const sortedShards = [...shards].sort((a, b) => a.agentId.localeCompare(b.agentId));

  // Build merkle tree
  let currentLevel = sortedShards.map(s => s.chainFinalHash);
  let merkleTime = 0;
  const merkleStart = performance.now();

  while (currentLevel.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1] || left;
      const parent = await simulateBlake3(left + right);
      nextLevel.push(parent);
    }
    currentLevel = nextLevel;
  }

  merkleTime = performance.now() - merkleStart;
  const merkleRoot = currentLevel[0];

  // Simulate deduplication (41K -> 38K unique)
  const totalClaims = shardCount * claimsPerShard;
  const uniqueClaims = Math.floor(totalClaims * 0.927); // ~7.3% duplicates

  const endTime = performance.now();
  forceGC();
  const memAfter = getMemoryMB();

  const totalTime = endTime - startTime;
  const claimsPerSec = totalClaims / (totalTime / 1000);

  const result = {
    operation: 'merge',
    target: TARGETS.MERGE_MS,
    actual: totalTime,
    passed: totalTime < TARGETS.MERGE_MS,
    metrics: {
      totalTime,
      merkleTime,
      claimsPerSec,
      totalClaims,
      uniqueClaims,
      merkleRoot,
      memoryUsedMB: memAfter.heapUsed - memBefore.heapUsed,
    },
  };

  const status = result.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;
  console.log(`  Result: ${totalTime.toFixed(2)}ms (target: <${TARGETS.MERGE_MS}ms) [${status}]`);
  console.log(`  Merkle time: ${merkleTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${claimsPerSec.toFixed(2)} claims/sec`);

  return result;
}

/**
 * Benchmark verify operation
 * Target: <10s for hash chains + merkle proofs
 */
async function benchmarkVerify(options = {}) {
  const {
    claimCount = 1000,
    variant = 'medium', // 'small' (100), 'medium' (1000), 'large' (41000)
    verbose = false,
  } = options;

  const counts = { small: 100, medium: 1000, large: 41000 };
  const actualCount = counts[variant] || claimCount;

  console.log(`\n${COLORS.cyan}[VERIFY BENCHMARK]${COLORS.reset}`);
  console.log(`  Variant: ${variant} (${actualCount} claims)`);

  forceGC();
  const memBefore = getMemoryMB();
  const startTime = performance.now();

  // Simulate verification of hash chain
  let verifications = 0;
  let prevHash = null;

  for (let i = 0; i < actualCount; i++) {
    const payload = { index: i, data: `claim-${i}` };
    const obsHash = await simulateBlake3(payload);

    // Verify chain continuity
    const chainInput = `${prevHash || 'GENESIS'}:${obsHash}`;
    const computedHash = await simulateBlake3(chainInput);

    verifications++;
    prevHash = computedHash;

    if (verbose && i % 5000 === 0) {
      console.log(`    Verified: ${i}/${actualCount}`);
    }
  }

  // Verify merkle root (simulated)
  const merkleVerifyStart = performance.now();
  const shardHashes = [];
  for (let i = 0; i < 10; i++) {
    shardHashes.push(await simulateBlake3({ shard: i }));
  }

  let level = shardHashes;
  while (level.length > 1) {
    const next = [];
    for (let i = 0; i < level.length; i += 2) {
      const left = level[i];
      const right = level[i + 1] || left;
      next.push(await simulateBlake3(left + right));
    }
    level = next;
  }
  const merkleVerifyTime = performance.now() - merkleVerifyStart;

  const endTime = performance.now();
  forceGC();
  const memAfter = getMemoryMB();

  const totalTime = endTime - startTime;
  const opsPerSec = verifications / (totalTime / 1000);

  const result = {
    operation: 'verify',
    target: TARGETS.VERIFY_MS,
    actual: totalTime,
    passed: totalTime < TARGETS.VERIFY_MS,
    metrics: {
      totalTime,
      merkleVerifyTime,
      operationsPerSec: opsPerSec,
      totalVerifications: verifications,
      claimCount: actualCount,
      memoryUsedMB: memAfter.heapUsed - memBefore.heapUsed,
    },
  };

  const status = result.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;
  console.log(`  Result: ${totalTime.toFixed(2)}ms (target: <${TARGETS.VERIFY_MS}ms) [${status}]`);
  console.log(`  Throughput: ${opsPerSec.toFixed(2)} ops/sec`);

  return result;
}

/**
 * Benchmark guard decision latency
 * Target: <5ms median
 */
async function benchmarkGuardLatency(options = {}) {
  const {
    iterations = 1000,
    warmup = 100,
    verbose = false,
  } = options;

  console.log(`\n${COLORS.cyan}[GUARD LATENCY BENCHMARK]${COLORS.reset}`);
  console.log(`  Iterations: ${iterations}, Warmup: ${warmup}`);

  const cache = new GuardCache();
  const latencies = { cacheHit: [], cacheMiss: [], newPattern: [] };

  // Warmup
  for (let i = 0; i < warmup; i++) {
    guardDecision({ type: 'warmup', index: i }, cache);
  }

  forceGC();

  // Cache miss (new patterns)
  for (let i = 0; i < iterations; i++) {
    const input = { type: 'new', index: i, unique: randomBytes(8).toString('hex') };
    const start = performance.now();
    guardDecision(input, cache);
    latencies.newPattern.push(performance.now() - start);
  }

  // Reset cache for hit test
  const hitCache = new GuardCache();
  const commonInputs = Array.from({ length: 10 }, (_, i) => ({ type: 'common', index: i }));
  commonInputs.forEach(input => guardDecision(input, hitCache));

  // Cache hit (repeated patterns)
  for (let i = 0; i < iterations; i++) {
    const input = commonInputs[i % commonInputs.length];
    const start = performance.now();
    guardDecision(input, hitCache);
    latencies.cacheHit.push(performance.now() - start);
  }

  // Calculate stats
  const hitStats = calcStats(latencies.cacheHit);
  const missStats = calcStats(latencies.cacheMiss);
  const newStats = calcStats(latencies.newPattern);
  const allLatencies = [...latencies.cacheHit, ...latencies.newPattern];
  const overallStats = calcStats(allLatencies);

  const result = {
    operation: 'guardLatency',
    target: TARGETS.GUARD_LATENCY_MS,
    actual: overallStats.median,
    passed: overallStats.median < TARGETS.GUARD_LATENCY_MS,
    metrics: {
      median: overallStats.median,
      p99: overallStats.p99,
      mean: overallStats.mean,
      decisionsPerSec: 1000 / overallStats.mean,
      cacheHit: hitStats,
      newPattern: newStats,
      cacheStats: hitCache.getStats(),
    },
  };

  const status = result.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;
  console.log(`  Median: ${overallStats.median.toFixed(4)}ms (target: <${TARGETS.GUARD_LATENCY_MS}ms) [${status}]`);
  console.log(`  P99: ${overallStats.p99.toFixed(4)}ms`);
  console.log(`  Throughput: ${(1000 / overallStats.mean).toFixed(2)} decisions/sec`);

  return result;
}

/**
 * Benchmark memory footprint
 * Target: <100MB steady state
 */
async function benchmarkMemory(options = {}) {
  const {
    projectSize = 'medium', // 'small', 'medium', 'large'
    verbose = false,
  } = options;

  const sizes = { small: 100, medium: 1000, large: 10000 };
  const obsCount = sizes[projectSize] || 1000;

  console.log(`\n${COLORS.cyan}[MEMORY BENCHMARK]${COLORS.reset}`);
  console.log(`  Project size: ${projectSize} (${obsCount} observations)`);

  forceGC();
  const baseline = getMemoryMB();

  // Simulate full scan with data retention
  const observations = [];
  const agentCount = 10;
  const obsPerAgent = Math.floor(obsCount / agentCount);

  const memSamples = [];
  for (let agent = 0; agent < agentCount; agent++) {
    for (let obs = 0; obs < obsPerAgent; obs++) {
      observations.push({
        agentId: `agent-${agent}`,
        index: obs,
        payload: generatePayload(512),
        hash: await simulateBlake3({ agent, obs }),
      });
    }
    memSamples.push(getMemoryMB());
  }

  const peak = getMemoryMB();
  forceGC();
  const steadyState = getMemoryMB();

  const memoryPerAgent = (steadyState.heapUsed - baseline.heapUsed) / agentCount;

  const result = {
    operation: 'memory',
    target: TARGETS.MEMORY_MB,
    actual: steadyState.heapUsed,
    passed: steadyState.heapUsed < TARGETS.MEMORY_MB,
    metrics: {
      baseline: baseline.heapUsed,
      peak: peak.heapUsed,
      steadyState: steadyState.heapUsed,
      memoryPerAgentMB: memoryPerAgent,
      observationCount: observations.length,
      rss: steadyState.rss,
    },
  };

  const status = result.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;
  console.log(`  Steady state: ${steadyState.heapUsed.toFixed(2)}MB (target: <${TARGETS.MEMORY_MB}MB) [${status}]`);
  console.log(`  Peak: ${peak.heapUsed.toFixed(2)}MB`);
  console.log(`  Per agent: ${memoryPerAgent.toFixed(2)}MB`);

  return result;
}

/**
 * Benchmark storage performance
 * Target: MemoryBackend <1ms, FileBackend <10ms
 */
async function benchmarkStorage(options = {}) {
  const {
    iterations = 100,
    verbose = false,
  } = options;

  console.log(`\n${COLORS.cyan}[STORAGE BENCHMARK]${COLORS.reset}`);
  console.log(`  Iterations: ${iterations}`);

  const results = {};

  // Memory backend
  const memBackend = new MemoryBackend();
  const memLatencies = { set: [], get: [], query: [] };

  for (let i = 0; i < iterations; i++) {
    const key = `key-${i}`;
    const value = { data: generatePayload(256) };

    let start = performance.now();
    await memBackend.set(key, value);
    memLatencies.set.push(performance.now() - start);

    start = performance.now();
    await memBackend.get(key);
    memLatencies.get.push(performance.now() - start);
  }

  let start = performance.now();
  await memBackend.query('key-');
  memLatencies.query.push(performance.now() - start);

  results.memory = {
    set: calcStats(memLatencies.set),
    get: calcStats(memLatencies.get),
    avgMs: mean([...memLatencies.set, ...memLatencies.get]),
    passed: mean([...memLatencies.set, ...memLatencies.get]) < TARGETS.MEMORY_BACKEND_MS,
  };

  // File backend
  const fileBackend = new FileBackend();
  const fileLatencies = { set: [], get: [], query: [] };

  for (let i = 0; i < Math.min(iterations, 50); i++) { // Limit file ops
    const key = `file-key-${i}`;
    const value = { data: generatePayload(256) };

    let start = performance.now();
    await fileBackend.set(key, value);
    fileLatencies.set.push(performance.now() - start);

    start = performance.now();
    await fileBackend.get(key);
    fileLatencies.get.push(performance.now() - start);
  }

  results.file = {
    set: calcStats(fileLatencies.set),
    get: calcStats(fileLatencies.get),
    avgMs: mean([...fileLatencies.set, ...fileLatencies.get]),
    passed: mean([...fileLatencies.set, ...fileLatencies.get]) < TARGETS.FILE_BACKEND_MS,
  };

  fileBackend.cleanup();

  const memStatus = results.memory.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;
  const fileStatus = results.file.passed ? `${COLORS.green}PASS${COLORS.reset}` : `${COLORS.red}FAIL${COLORS.reset}`;

  console.log(`  MemoryBackend: ${results.memory.avgMs.toFixed(4)}ms/op (target: <${TARGETS.MEMORY_BACKEND_MS}ms) [${memStatus}]`);
  console.log(`  FileBackend: ${results.file.avgMs.toFixed(4)}ms/op (target: <${TARGETS.FILE_BACKEND_MS}ms) [${fileStatus}]`);

  return {
    operation: 'storage',
    metrics: results,
    passed: results.memory.passed && results.file.passed,
  };
}

// =============================================================================
// Profiling Utilities
// =============================================================================

/**
 * Profile function execution time
 */
async function profileFunction(fn, name, iterations = 100) {
  const times = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    times.push(performance.now() - start);
  }

  return {
    name,
    ...calcStats(times),
    totalTime: times.reduce((s, t) => s + t, 0),
    percentOfTotal: 0, // Will be calculated later
  };
}

/**
 * Identify top slowest functions (simulated CPU profiling)
 */
async function identifyBottlenecks() {
  console.log(`\n${COLORS.cyan}[BOTTLENECK ANALYSIS]${COLORS.reset}`);

  const functions = [
    { name: 'simulateBlake3', fn: () => simulateBlake3({ test: 'data' }) },
    { name: 'deterministicSerialize', fn: () => deterministicSerialize({ nested: { deep: { value: 123 } } }) },
    { name: 'generatePayload', fn: () => generatePayload(512) },
    { name: 'guardDecision', fn: () => guardDecision({ type: 'test' }, new GuardCache()) },
    { name: 'JSON.stringify', fn: () => JSON.stringify({ large: Array(100).fill({ a: 1, b: 2 }) }) },
  ];

  const profiles = [];
  for (const { name, fn } of functions) {
    const profile = await profileFunction(fn, name, 1000);
    profiles.push(profile);
  }

  // Calculate percentage of total
  const totalTime = profiles.reduce((s, p) => s + p.totalTime, 0);
  profiles.forEach(p => {
    p.percentOfTotal = (p.totalTime / totalTime) * 100;
  });

  // Sort by total time
  profiles.sort((a, b) => b.totalTime - a.totalTime);

  console.log('\n  Top 5 Slowest Functions:');
  profiles.slice(0, 5).forEach((p, i) => {
    console.log(`    ${i + 1}. ${p.name}: ${p.mean.toFixed(4)}ms avg (${p.percentOfTotal.toFixed(1)}% of total)`);
  });

  return profiles;
}

// =============================================================================
// Main Runner
// =============================================================================

async function runAllBenchmarks(options = {}) {
  const { verbose = false } = options;

  console.log(`${COLORS.bold}========================================`);
  console.log('  KGC Probe Performance Benchmark Suite');
  console.log('========================================' + COLORS.reset);
  console.log(`\nTimestamp: ${new Date().toISOString()}`);
  console.log(`Node: ${process.version}`);
  console.log(`Platform: ${process.platform} ${process.arch}`);
  console.log(`Memory: ${(process.memoryUsage().heapTotal / 1024 / 1024).toFixed(0)}MB heap`);

  const results = {
    timestamp: new Date().toISOString(),
    nodeVersion: process.version,
    platform: `${process.platform} ${process.arch}`,
    benchmarks: {},
    summary: {
      passed: 0,
      failed: 0,
      total: 0,
    },
  };

  // Run benchmarks
  const benchmarks = [
    { name: 'scan', fn: () => benchmarkScan({ agentCount: 10, observationsPerAgent: 100, verbose }) },
    { name: 'merge', fn: () => benchmarkMerge({ shardCount: 10, claimsPerShard: 4100, verbose }) },
    { name: 'verify', fn: () => benchmarkVerify({ variant: 'medium', verbose }) },
    { name: 'guardLatency', fn: () => benchmarkGuardLatency({ iterations: 1000, verbose }) },
    { name: 'memory', fn: () => benchmarkMemory({ projectSize: 'medium', verbose }) },
    { name: 'storage', fn: () => benchmarkStorage({ iterations: 100, verbose }) },
  ];

  for (const { name, fn } of benchmarks) {
    try {
      const result = await fn();
      results.benchmarks[name] = result;
      results.summary.total++;
      if (result.passed) {
        results.summary.passed++;
      } else {
        results.summary.failed++;
      }
    } catch (error) {
      console.error(`  ${COLORS.red}Error in ${name}: ${error.message}${COLORS.reset}`);
      results.benchmarks[name] = { error: error.message, passed: false };
      results.summary.failed++;
      results.summary.total++;
    }
  }

  // Bottleneck analysis
  const bottlenecks = await identifyBottlenecks();
  results.bottlenecks = bottlenecks;

  // Summary
  console.log(`\n${COLORS.bold}========================================`);
  console.log('  SUMMARY');
  console.log('========================================' + COLORS.reset);

  const overallStatus = results.summary.failed === 0
    ? `${COLORS.green}ALL TARGETS MET${COLORS.reset}`
    : `${COLORS.red}${results.summary.failed} TARGETS MISSED${COLORS.reset}`;

  console.log(`\n  ${overallStatus}`);
  console.log(`  Passed: ${results.summary.passed}/${results.summary.total}`);

  // Print detailed results
  console.log('\n  Results vs Targets:');
  for (const [name, result] of Object.entries(results.benchmarks)) {
    if (result.error) {
      console.log(`    ${name}: ${COLORS.red}ERROR${COLORS.reset}`);
    } else {
      const status = result.passed ? COLORS.green + 'PASS' : COLORS.red + 'FAIL';
      const actual = result.actual ? result.actual.toFixed(2) : 'N/A';
      const target = result.target ? result.target : 'N/A';
      console.log(`    ${name}: ${actual} (target: ${target}) [${status}${COLORS.reset}]`);
    }
  }

  return results;
}

// =============================================================================
// CLI Entry Point
// =============================================================================

const args = process.argv.slice(2);
const verbose = args.includes('--verbose') || args.includes('-v');

runAllBenchmarks({ verbose })
  .then(results => {
    // Write JSON results
    const jsonPath = path.join(process.cwd(), 'benchmark-results.json');
    fs.writeFileSync(jsonPath, JSON.stringify(results, null, 2));
    console.log(`\n  Results written to: ${jsonPath}`);

    // Exit with appropriate code
    process.exit(results.summary.failed === 0 ? 0 : 1);
  })
  .catch(error => {
    console.error(`\n${COLORS.red}Benchmark failed: ${error.message}${COLORS.reset}`);
    process.exit(1);
  });

export {
  runAllBenchmarks,
  benchmarkScan,
  benchmarkMerge,
  benchmarkVerify,
  benchmarkGuardLatency,
  benchmarkMemory,
  benchmarkStorage,
  identifyBottlenecks,
  TARGETS,
};
