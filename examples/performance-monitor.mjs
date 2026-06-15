/**
 * @file Performance Monitor Example
 * @description Demonstrates PerformanceMonitor for streaming throughput, latency, and memory tracking
 *
 * API demonstrated: packages/streaming/src/performance-monitor.mjs
 * Docs: packages/streaming/README.md — "Performance Monitor" section
 *
 * PerformanceMonitor collects streaming metrics in a rolling time window,
 * emits threshold-violation events when performance degrades, and produces
 * statistical reports (mean, p50, p95, p99) over the monitored period.
 *
 * EXPECTED OUTPUT:
 *   [start] monitor is monitoring
 *   [recordQuad/Bytes/Chunk] getCurrentMetrics reflects recorded operations
 *   [recordError] errorsCount increments and error-recorded event fires
 *   [recordBackpressure] backpressureEvents increments and backpressure event fires
 *   [threshold-violation] fires when heap exceeds memoryMax: 1 byte
 *   [getReport] report contains throughput, latency, memory, backpressure, errors sections
 *   [reset] metrics clear after reset()
 */

import { EventEmitter } from 'events';
import { PerformanceMonitor, createPerformanceMonitor } from '../packages/streaming/src/performance-monitor.mjs';

// ─── 1. start() / stop() lifecycle ────────────────────────────────────────────

const monitor = createPerformanceMonitor({
  sampleInterval: 50, // fast for example; real usage: 1000ms
  windowSize: 10,
  enableMemoryTracking: true,
  enableLatencyTracking: true,
  enableThroughputTracking: true,
});

if (!(monitor instanceof PerformanceMonitor)) throw new Error('createPerformanceMonitor must return PerformanceMonitor');
if (!(monitor instanceof EventEmitter)) throw new Error('PerformanceMonitor must extend EventEmitter');

let startedFired = false;
monitor.on('started', () => { startedFired = true; });

monitor.start();
if (!monitor.isMonitoring) throw new Error('Expected isMonitoring = true after start()');
if (!startedFired) throw new Error('Expected "started" event to fire');

console.log('[start] ✅ monitor is monitoring');

// ─── 2. recordQuad, recordBytes, recordChunk ──────────────────────────────────

for (let i = 0; i < 100; i++) monitor.recordQuad(i % 10); // latencies 0–9ms
monitor.recordBytes(4096);
for (let i = 0; i < 5; i++) monitor.recordChunk();

const snap = monitor.getCurrentMetrics();
if (snap.quadsProcessed !== 100) throw new Error(`Expected 100 quads, got ${snap.quadsProcessed}`);
if (snap.bytesProcessed !== 4096) throw new Error(`Expected 4096 bytes, got ${snap.bytesProcessed}`);
if (snap.chunksProcessed !== 5) throw new Error(`Expected 5 chunks, got ${snap.chunksProcessed}`);

console.log(`[recordQuad/Bytes/Chunk] ✅ getCurrentMetrics: ${snap.quadsProcessed} quads, ${snap.bytesProcessed} bytes, ${snap.chunksProcessed} chunks`);

// ─── 3. recordError — increments count and emits event ───────────────────────

let errorEventFired = false;
monitor.on('error-recorded', () => { errorEventFired = true; });
monitor.recordError(new Error('test-stream-error'));

const snapAfterError = monitor.getCurrentMetrics();
if (snapAfterError.errorsCount !== 1) throw new Error(`Expected errorsCount 1, got ${snapAfterError.errorsCount}`);
if (!errorEventFired) throw new Error('Expected "error-recorded" event');

console.log(`[recordError] ✅ errorsCount=${snapAfterError.errorsCount}, error-recorded event fired`);

// ─── 4. recordBackpressure — increments count and emits event ─────────────────

let backpressureFired = false;
monitor.on('backpressure', () => { backpressureFired = true; });
monitor.recordBackpressure();

const snapAfterBP = monitor.getCurrentMetrics();
if (snapAfterBP.backpressureEvents !== 1) throw new Error(`Expected 1 backpressure event, got ${snapAfterBP.backpressureEvents}`);
if (!backpressureFired) throw new Error('Expected "backpressure" event');

console.log(`[recordBackpressure] ✅ backpressureEvents=${snapAfterBP.backpressureEvents}, event fired`);

// ─── 5. threshold-violation — fires when memory exceeds memoryMax ─────────────

const thresholdMonitor = createPerformanceMonitor({
  sampleInterval: 20,
  enableMemoryTracking: true,
  enableThroughputTracking: false,
  thresholds: { memoryMax: 1 }, // 1 byte — guaranteed to exceed
});

const violation = await new Promise((resolve) => {
  thresholdMonitor.on('threshold-violation', resolve);
  thresholdMonitor.start();
});

thresholdMonitor.stop();

if (violation.metric !== 'memory') throw new Error(`Expected memory violation, got ${violation.metric}`);
if (typeof violation.value !== 'number' || violation.value < 1) throw new Error(`Expected numeric heapUsed > 1, got ${violation.value}`);
if (violation.threshold !== 1) throw new Error(`Expected threshold=1, got ${violation.threshold}`);

console.log(`[threshold-violation] ✅ fires when heap (${(violation.value / 1024 / 1024).toFixed(1)}MB) exceeds memoryMax (${violation.threshold} byte)`);

// ─── 6. getReport() — statistical structure ───────────────────────────────────

const report = monitor.getReport();

if (typeof report.summary !== 'object') throw new Error('report.summary must be an object');
if (typeof report.throughput !== 'object') throw new Error('report.throughput must be an object');
if (typeof report.latency !== 'object') throw new Error('report.latency must be an object');
if (typeof report.memory !== 'object') throw new Error('report.memory must be an object');
if (typeof report.backpressure !== 'object') throw new Error('report.backpressure must be an object');
if (typeof report.errors !== 'object') throw new Error('report.errors must be an object');
if (report.errors.count !== 1) throw new Error(`Expected 1 error in report, got ${report.errors.count}`);
if (report.errors.recent[0].message !== 'test-stream-error') {
  throw new Error(`Expected error message in report, got: ${report.errors.recent[0].message}`);
}

console.log(`[getReport] ✅ report has throughput/latency/memory/backpressure/errors sections`);
console.log(`[getReport] ✅ errors.count=${report.errors.count}, message="${report.errors.recent[0].message}"`);

// ─── 7. stop() emits report, reset() clears metrics ──────────────────────────

let stopReport = null;
monitor.on('stopped', report => { stopReport = report; });
monitor.stop();

if (!stopReport) throw new Error('Expected "stopped" event with report');
if (!monitor.isMonitoring === false) throw new Error('Expected isMonitoring=false after stop()');

let resetFired = false;
monitor.on('reset', () => { resetFired = true; });
monitor.reset();
if (!resetFired) throw new Error('Expected "reset" event');

const afterReset = monitor.getCurrentMetrics();
if (afterReset.quadsProcessed !== 0) throw new Error(`Expected 0 quads after reset, got ${afterReset.quadsProcessed}`);

console.log(`[reset] ✅ metrics cleared: quadsProcessed=${afterReset.quadsProcessed}`);

console.log('\n✅ All performance monitor assertions passed.');
