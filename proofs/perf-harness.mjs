#!/usr/bin/env node
/**
 * UNRDF Performance Proxy Harness
 * 
 * Measures observable performance proxies using ONLY built-in Node.js APIs:
 * - process.hrtime.bigint() for high-precision timing
 * - process.memoryUsage() for memory deltas
 * - NO external benchmarking libraries
 * 
 * Outputs CSV for easy plotting and analysis.
 */

import { performance } from 'node:perf_hooks';
import { createHash } from 'node:crypto';

// Note: In a real environment, these would import from @unrdf packages
// For standalone execution, we'll simulate the operations

const measurements = [];

/**
 * Measure operation with timing + memory delta
 */
async function measure(name, fn) {
  // Force GC if available
  if (global.gc) {
    global.gc();
    await sleep(50);
  }

  const mem0 = process.memoryUsage();
  const t0 = performance.now();
  
  const result = await fn();
  
  const t1 = performance.now();
  const mem1 = process.memoryUsage();
  
  measurements.push({
    operation: name,
    time_ms: (t1 - t0).toFixed(3),
    memory_delta_bytes: (mem1.heapUsed - mem0.heapUsed),
    result_size: typeof result === 'string' ? result.length : 
                 Array.isArray(result) ? result.length :
                 JSON.stringify(result).length
  });
  
  return result;
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Generate N-Quads test data
 */
function generateNQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push(`<http://example.org/s${i}> <http://example.org/p> "value${i}" <http://example.org/g> .`);
  }
  return quads.join('\n');
}

/**
 * Simulate RDF store operations (proxy for real Oxigraph)
 */
class SimulatedStore {
  constructor() {
    this.quads = [];
  }
  
  async load(nquads) {
    const lines = nquads.split('\n').filter(l => l.trim());
    this.quads = lines.map(line => ({ raw: line }));
    return this.quads.length;
  }
  
  query(sparql) {
    // Simulate query execution
    if (sparql.includes('SELECT')) {
      return this.quads.slice(0, 10);
    } else if (sparql.includes('ASK')) {
      return this.quads.length > 0;
    }
    return [];
  }
  
  match() {
    return this.quads;
  }
  
  add(quad) {
    this.quads.push(quad);
  }
  
  dump() {
    return this.quads.map(q => q.raw || JSON.stringify(q)).join('\n');
  }
}

/**
 * Benchmark 1: RDF Parsing (N-Quads)
 */
async function benchParse() {
  const sizes = [100, 500, 1000];
  
  for (const size of sizes) {
    const nquads = generateNQuads(size);
    const store = new SimulatedStore();
    
    await measure(`parse-nquads-${size}`, async () => {
      await store.load(nquads);
      return store;
    });
  }
}

/**
 * Benchmark 2: SPARQL Query Latency
 */
async function benchQuery() {
  const store = new SimulatedStore();
  const nquads = generateNQuads(1000);
  await store.load(nquads);
  
  // SELECT query
  await measure('query-select-all', async () => {
    return store.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
  });
  
  // Pattern match
  await measure('query-pattern-match', async () => {
    return store.match();
  });
  
  // ASK query
  await measure('query-ask', async () => {
    return store.query('ASK { ?s ?p ?o }');
  });
}

/**
 * Benchmark 3: Quad Insertion Throughput
 */
async function benchInsert() {
  const counts = [100, 500, 1000];
  
  for (const count of counts) {
    const store = new SimulatedStore();
    
    await measure(`insert-quads-${count}`, async () => {
      for (let i = 0; i < count; i++) {
        store.add({
          subject: `http://example.org/s${i}`,
          predicate: 'http://example.org/p',
          object: `value${i}`,
          graph: 'http://example.org/g'
        });
      }
      return store;
    });
  }
}

/**
 * Benchmark 4: Serialization (N-Quads dump)
 */
async function benchSerialize() {
  const store = new SimulatedStore();
  const nquads = generateNQuads(1000);
  await store.load(nquads);
  
  await measure('serialize-nquads-1000', async () => {
    return store.dump();
  });
}

/**
 * Benchmark 5: Hash Computation (BLAKE3 simulation with crypto)
 */
async function benchHash() {
  const sizes = [1000, 5000, 10000];
  
  for (const size of sizes) {
    const data = generateNQuads(size);
    
    await measure(`hash-blake3-sim-${size}-quads`, async () => {
      // Simulate BLAKE3 with Node's built-in crypto (SHA256 for proxy)
      const hash = createHash('sha256');
      hash.update(data);
      return hash.digest('hex');
    });
  }
}

/**
 * Benchmark 6: Event Append Simulation
 */
async function benchEventAppend() {
  const counts = [10, 50, 100];
  
  for (const count of counts) {
    await measure(`event-append-${count}`, async () => {
      const events = [];
      for (let i = 0; i < count; i++) {
        const event = {
          id: `event-${i}`,
          timestamp: Date.now(),
          type: 'CREATE',
          payload: { index: i },
          deltas: [
            { type: 'add', subject: `s${i}`, predicate: 'p', object: `o${i}` }
          ]
        };
        events.push(event);
      }
      return events;
    });
  }
}

/**
 * Benchmark 7: Freeze Simulation (serialize + hash + commit)
 */
async function benchFreeze() {
  const sizes = [500, 1000];
  
  for (const size of sizes) {
    const nquads = generateNQuads(size);
    
    await measure(`freeze-universe-${size}`, async () => {
      // Simulate freeze: serialize + hash + git commit
      const serialized = nquads;
      const hash = createHash('sha256').update(serialized).digest('hex');
      const gitRef = `freeze-${hash.slice(0, 8)}`;
      
      return { hash, gitRef, quadCount: size };
    });
  }
}

/**
 * Main harness runner
 */
async function runHarness() {
  console.log('UNRDF Performance Proxy Harness');
  console.log('================================\n');
  console.log('Measuring observable costs with built-in APIs...\n');
  console.log('Note: Using simulated RDF store for standalone execution\n');
  
  await benchParse();
  await benchQuery();
  await benchInsert();
  await benchSerialize();
  await benchHash();
  await benchEventAppend();
  await benchFreeze();
  
  // Output CSV
  console.log('\n\nPerformance Measurements (CSV):');
  console.log('operation,time_ms,memory_delta_bytes,result_size');
  
  for (const m of measurements) {
    console.log(`${m.operation},${m.time_ms},${m.memory_delta_bytes},${m.result_size}`);
  }
  
  // Calculate statistics
  console.log('\n\nStatistical Summary:');
  console.log('===================\n');
  
  const groups = {};
  for (const m of measurements) {
    const category = m.operation.split('-')[0];
    if (!groups[category]) groups[category] = [];
    groups[category].push(parseFloat(m.time_ms));
  }
  
  for (const [category, times] of Object.entries(groups)) {
    const mean = times.reduce((a, b) => a + b, 0) / times.length;
    const min = Math.min(...times);
    const max = Math.max(...times);
    const sorted = [...times].sort((a, b) => a - b);
    const p50 = sorted[Math.floor(sorted.length * 0.5)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    
    console.log(`${category.toUpperCase()}:`);
    console.log(`  Mean: ${mean.toFixed(3)}ms`);
    console.log(`  Min:  ${min.toFixed(3)}ms`);
    console.log(`  Max:  ${max.toFixed(3)}ms`);
    console.log(`  p50:  ${p50.toFixed(3)}ms`);
    console.log(`  p95:  ${p95.toFixed(3)}ms`);
    console.log('');
  }
  
  // Performance budget validation
  console.log('Performance Budget Validation:');
  console.log('==============================\n');
  
  const budgets = {
    'parse-nquads-1000': 50,     // 50ms budget
    'query-select-all': 10,      // 10ms budget
    'insert-quads-1000': 30,     // 30ms budget
    'serialize-nquads-1000': 20, // 20ms budget
    'freeze-universe-1000': 100, // 100ms budget
  };
  
  let passed = 0;
  let failed = 0;
  
  for (const [op, budget] of Object.entries(budgets)) {
    const measurement = measurements.find(m => m.operation === op);
    if (measurement) {
      const actual = parseFloat(measurement.time_ms);
      const status = actual <= budget ? 'PASS' : 'FAIL';
      const symbol = actual <= budget ? '✓' : '✗';
      console.log(`${symbol} ${op}: ${actual.toFixed(3)}ms (budget: ${budget}ms) - ${status}`);
      
      if (actual <= budget) passed++;
      else failed++;
    }
  }
  
  console.log(`\nBudget Summary: ${passed} passed, ${failed} failed\n`);
  
  // Observable proxies identified
  console.log('Observable Performance Proxies Identified:');
  console.log('==========================================\n');
  
  const proxies = [
    { name: 'RDF Parsing', input: 'quad count', cost: 'time, memory' },
    { name: 'SPARQL Query', input: 'pattern complexity', cost: 'time' },
    { name: 'Quad Insertion', input: 'quad count', cost: 'time, memory' },
    { name: 'Serialization', input: 'quad count', cost: 'time, CPU' },
    { name: 'Hash (BLAKE3)', input: 'input size (bytes)', cost: 'time, CPU' },
    { name: 'Event Append', input: 'delta count', cost: 'time, memory' },
    { name: 'Freeze Universe', input: 'universe size', cost: 'time, memory, I/O' },
  ];
  
  for (const proxy of proxies) {
    console.log(`- ${proxy.name.padEnd(20)} | Input: ${proxy.input.padEnd(25)} | Cost: ${proxy.cost}`);
  }
  
  console.log('\n✓ Harness complete. All operations measured with built-in APIs only.\n');
  
  return measurements;
}

// Run if invoked directly
if (import.meta.url === `file://${process.argv[1]}`) {
  try {
    await runHarness();
    process.exit(0);
  } catch (error) {
    console.error('Harness failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

export { runHarness, measure };
