#!/usr/bin/env node
/**
 * V6 Core Performance Benchmark Suite
 *
 * SLA Targets:
 * - Cold start: <500ms
 * - Receipt generation: <10ms per operation
 * - Merkle proof: <50ms for 100 receipts
 * - Memory: <200MB base, <500MB peak
 */

import { createReceipt } from './packages/v6-core/src/receipts/index.mjs';
import {
  buildMerkleTree,
  getMerkleRoot,
  getProofPath,
  verifyInclusion,
} from './packages/v6-core/src/receipts/merkle/tree.mjs';
import { performance } from 'node:perf_hooks';

// =============================================================================
// Utilities
// =============================================================================

function formatMemory(bytes) {
  return `${(bytes / 1024 / 1024).toFixed(2)} MB`;
}

function getMemoryUsage() {
  const mem = process.memoryUsage();
  return {
    rss: mem.rss,
    heapTotal: mem.heapTotal,
    heapUsed: mem.heapUsed,
    external: mem.external,
  };
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// =============================================================================
// Benchmark 1: Cold Start
// =============================================================================

async function benchmarkColdStart() {
  console.log('\n=== BENCHMARK 1: Cold Start ===');

  const startTime = performance.now();

  // Simulate module load + first receipt creation
  const receipt = await createReceipt('execution', {
    eventType: 'TASK_COMPLETED',
    caseId: 'cold-start-test',
    taskId: 'init',
    payload: { decision: 'COMPLETE', context: { status: 'READY' } }
  });

  const duration = performance.now() - startTime;

  console.log(`Cold start time: ${duration.toFixed(2)}ms`);
  console.log(`SLA: <500ms`);
  console.log(`Status: ${duration < 500 ? '✅ PASS' : '❌ FAIL'}`);

  return {
    metric: 'cold_start',
    duration_ms: duration,
    sla_ms: 500,
    pass: duration < 500,
  };
}

// =============================================================================
// Benchmark 2: Receipt Generation Throughput
// =============================================================================

async function benchmarkReceiptThroughput(count = 1000) {
  console.log(`\n=== BENCHMARK 2: Receipt Generation Throughput (${count} receipts) ===`);

  const memBefore = getMemoryUsage();
  const startTime = performance.now();

  const receipts = [];
  let previousReceipt = null;

  for (let i = 0; i < count; i++) {
    const receipt = await createReceipt('execution', {
      eventType: 'TASK_COMPLETED',
      caseId: `case-${i}`,
      taskId: `task-${i}`,
      payload: { decision: 'COMPLETE', context: { index: i, data: 'benchmark' } }
    }, previousReceipt);

    receipts.push(receipt);
    previousReceipt = receipt;
  }

  const duration = performance.now() - startTime;
  const memAfter = getMemoryUsage();

  const avgLatency = duration / count;
  const throughput = (count / duration) * 1000; // per second

  console.log(`Total time: ${duration.toFixed(2)}ms`);
  console.log(`Average latency per receipt: ${avgLatency.toFixed(2)}ms`);
  console.log(`Throughput: ${throughput.toFixed(2)} receipts/sec`);
  console.log(`SLA: <10ms per operation`);
  console.log(`Status: ${avgLatency < 10 ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`Memory delta: ${formatMemory(memAfter.heapUsed - memBefore.heapUsed)}`);

  return {
    metric: 'receipt_generation',
    count: count,
    total_duration_ms: duration,
    avg_latency_ms: avgLatency,
    throughput_per_sec: throughput,
    sla_ms: 10,
    pass: avgLatency < 10,
    memory_delta_bytes: memAfter.heapUsed - memBefore.heapUsed,
    receipts: receipts, // For Merkle benchmark
  };
}

// =============================================================================
// Benchmark 3: Merkle Tree Build & Proof
// =============================================================================

async function benchmarkMerkleTree(receipts) {
  console.log(`\n=== BENCHMARK 3: Merkle Tree Build & Proof (${receipts.length} receipts) ===`);

  const memBefore = getMemoryUsage();

  // Adapt receipts to have 'hash' field (merkle tree expects 'hash', receipts have 'receiptHash')
  const adaptedReceipts = receipts.map(r => ({ ...r, hash: r.receiptHash }));

  // Build Merkle tree
  const buildStart = performance.now();
  const merkleTree = await buildMerkleTree(adaptedReceipts);
  const buildDuration = performance.now() - buildStart;

  console.log(`Merkle tree build time: ${buildDuration.toFixed(2)}ms`);

  // Generate proof for middle element
  const targetIndex = Math.floor(receipts.length / 2);
  const targetReceipt = adaptedReceipts[targetIndex];
  const proofStart = performance.now();
  const proof = await getProofPath(merkleTree, targetReceipt.id, adaptedReceipts);
  const proofDuration = performance.now() - proofStart;

  console.log(`Proof generation time: ${proofDuration.toFixed(2)}ms`);

  // Verify proof
  const verifyStart = performance.now();
  const root = getMerkleRoot(merkleTree);
  const isValid = await verifyInclusion(root, targetReceipt, proof);
  const verifyDuration = performance.now() - verifyStart;

  console.log(`Proof verification time: ${verifyDuration.toFixed(2)}ms`);
  console.log(`Proof valid: ${isValid}`);

  const totalDuration = buildDuration + proofDuration;

  const memAfter = getMemoryUsage();

  console.log(`Total time (build + proof): ${totalDuration.toFixed(2)}ms`);
  console.log(`SLA: <50ms`);
  console.log(`Status: ${totalDuration < 50 ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`Memory delta: ${formatMemory(memAfter.heapUsed - memBefore.heapUsed)}`);

  return {
    metric: 'merkle_tree',
    receipt_count: receipts.length,
    build_duration_ms: buildDuration,
    proof_generation_ms: proofDuration,
    proof_verification_ms: verifyDuration,
    total_duration_ms: totalDuration,
    sla_ms: 50,
    pass: totalDuration < 50,
    proof_valid: isValid,
    memory_delta_bytes: memAfter.heapUsed - memBefore.heapUsed,
  };
}

// =============================================================================
// Benchmark 4: Memory Usage Under Load
// =============================================================================

async function benchmarkMemoryUsage() {
  console.log('\n=== BENCHMARK 4: Memory Usage Under Load ===');

  const baseMemory = getMemoryUsage();
  console.log(`Baseline memory: ${formatMemory(baseMemory.heapUsed)}`);

  let peakMemory = baseMemory.heapUsed;
  const measurements = [];

  // Generate receipts in batches and monitor memory
  const batches = 10;
  const receiptsPerBatch = 100;

  for (let batch = 0; batch < batches; batch++) {
    const batchReceipts = [];

    for (let i = 0; i < receiptsPerBatch; i++) {
      const receipt = await createReceipt('execution', {
        eventType: 'TASK_COMPLETED',
        caseId: `batch-${batch}-case-${i}`,
        taskId: `task-${i}`,
        payload: { decision: 'COMPLETE', context: { batch, index: i, data: 'x'.repeat(100) } }
      });
      batchReceipts.push(receipt);
    }

    // Build Merkle tree for batch
    const adaptedReceipts = batchReceipts.map(r => ({ ...r, hash: r.receiptHash }));
    const tree = await buildMerkleTree(adaptedReceipts);

    const currentMemory = getMemoryUsage();
    peakMemory = Math.max(peakMemory, currentMemory.heapUsed);

    measurements.push({
      batch: batch + 1,
      heapUsed: currentMemory.heapUsed,
      receipts: (batch + 1) * receiptsPerBatch,
    });

    console.log(`Batch ${batch + 1}/${batches}: ${formatMemory(currentMemory.heapUsed)} (${(batch + 1) * receiptsPerBatch} receipts)`);
  }

  console.log(`\nPeak memory: ${formatMemory(peakMemory)}`);
  console.log(`SLA: Base <200MB, Peak <500MB`);
  console.log(`Base status: ${baseMemory.heapUsed < 200 * 1024 * 1024 ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`Peak status: ${peakMemory < 500 * 1024 * 1024 ? '✅ PASS' : '❌ FAIL'}`);

  return {
    metric: 'memory_usage',
    base_memory_bytes: baseMemory.heapUsed,
    peak_memory_bytes: peakMemory,
    base_sla_bytes: 200 * 1024 * 1024,
    peak_sla_bytes: 500 * 1024 * 1024,
    base_pass: baseMemory.heapUsed < 200 * 1024 * 1024,
    peak_pass: peakMemory < 500 * 1024 * 1024,
    measurements: measurements,
  };
}

// =============================================================================
// Main Benchmark Suite
// =============================================================================

async function runBenchmarkSuite() {
  console.log('╔════════════════════════════════════════════════════════════════╗');
  console.log('║  V6 CORE PERFORMANCE BENCHMARK SUITE                           ║');
  console.log('╚════════════════════════════════════════════════════════════════╝');

  const results = [];

  try {
    // Benchmark 1: Cold Start
    const coldStartResult = await benchmarkColdStart();
    results.push(coldStartResult);

    await sleep(100); // Brief pause

    // Benchmark 2: Receipt Generation
    const throughputResult = await benchmarkReceiptThroughput(1000);
    results.push(throughputResult);

    await sleep(100);

    // Benchmark 3: Merkle Tree (using receipts from throughput test)
    const merkleResult = await benchmarkMerkleTree(throughputResult.receipts.slice(0, 100));
    results.push(merkleResult);

    await sleep(100);

    // Benchmark 4: Memory Usage
    const memoryResult = await benchmarkMemoryUsage();
    results.push(memoryResult);

    // Summary
    console.log('\n╔════════════════════════════════════════════════════════════════╗');
    console.log('║  PERFORMANCE SUMMARY                                           ║');
    console.log('╚════════════════════════════════════════════════════════════════╝');

    const allPassed = results.every(r => r.pass || (r.base_pass && r.peak_pass));

    results.forEach((result, i) => {
      const status = result.pass || (result.base_pass && result.peak_pass) ? '✅' : '❌';
      console.log(`\n${i + 1}. ${result.metric.toUpperCase()}: ${status}`);

      if (result.metric === 'cold_start') {
        console.log(`   Duration: ${result.duration_ms.toFixed(2)}ms (SLA: <${result.sla_ms}ms)`);
      } else if (result.metric === 'receipt_generation') {
        console.log(`   Avg Latency: ${result.avg_latency_ms.toFixed(2)}ms (SLA: <${result.sla_ms}ms)`);
        console.log(`   Throughput: ${result.throughput_per_sec.toFixed(2)} receipts/sec`);
      } else if (result.metric === 'merkle_tree') {
        console.log(`   Total Time: ${result.total_duration_ms.toFixed(2)}ms (SLA: <${result.sla_ms}ms)`);
        console.log(`   Build: ${result.build_duration_ms.toFixed(2)}ms, Proof: ${result.proof_generation_ms.toFixed(2)}ms`);
      } else if (result.metric === 'memory_usage') {
        console.log(`   Base: ${formatMemory(result.base_memory_bytes)} (SLA: <${formatMemory(result.base_sla_bytes)})`);
        console.log(`   Peak: ${formatMemory(result.peak_memory_bytes)} (SLA: <${formatMemory(result.peak_sla_bytes)})`);
      }
    });

    console.log('\n╔════════════════════════════════════════════════════════════════╗');
    console.log(`║  OVERALL RESULT: ${allPassed ? '✅ ALL TESTS PASSED' : '❌ SOME TESTS FAILED'}                       ║`);
    console.log('╚════════════════════════════════════════════════════════════════╝\n');

    // Output JSON results (with BigInt serialization support)
    console.log('\n--- JSON RESULTS ---');
    console.log(JSON.stringify(results, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    , 2));

    process.exit(allPassed ? 0 : 1);

  } catch (error) {
    console.error('\n❌ BENCHMARK FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run benchmarks
runBenchmarkSuite();
