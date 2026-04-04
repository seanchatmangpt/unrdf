# How To: Monitor Stream Performance

Use `PerformanceMonitor` to collect real-time throughput, latency, memory, and backpressure metrics during streaming operations.

---

## Start and Stop a Monitor

`PerformanceMonitor` extends Node.js `EventEmitter`. Create it, start it before your workload, and stop it after:

```javascript
import { createPerformanceMonitor } from '@unrdf/streaming';

const monitor = createPerformanceMonitor({
  sampleInterval: 1000, // collect a metric sample every 1 second
  windowSize: 60, // keep last 60 samples (1-minute window)
  enableThroughputTracking: true,
  enableLatencyTracking: true,
  enableMemoryTracking: true,
});

monitor.start();

// ... do streaming work ...

monitor.stop();

const report = monitor.getReport();
console.log(report);
```

---

## Record Quads and Bytes

Inside your streaming pipeline, call `recordQuad()` for each processed quad, and `recordBytes()` for each input chunk:

```javascript
import { createRDFStreamParser, createPerformanceMonitor } from '@unrdf/streaming';
import { createReadStream } from 'fs';

const monitor = createPerformanceMonitor({ sampleInterval: 500 });
const parser = createRDFStreamParser({ format: 'n-triples', chunkSize: 200 });

monitor.start();

parser.on('data', chunk => {
  if (chunk.type === 'quads') {
    chunk.data.forEach((_quad, _i, _arr) => {
      const latency = Math.random() * 2; // replace with real latency measurement
      monitor.recordQuad(latency);
    });
    monitor.recordChunk();
  }
});

await new Promise((resolve, reject) => {
  const stream = createReadStream('data.nt');
  stream.on('data', buf => monitor.recordBytes(buf.length));
  stream.pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});

monitor.stop();
```

---

## Read Current Metrics (Snapshot)

`getCurrentMetrics()` returns a point-in-time snapshot of totals. Call it at any point while `isMonitoring` is `true`:

```javascript
monitor.start();

// Periodically print progress
const ticker = setInterval(() => {
  const m = monitor.getCurrentMetrics();
  console.log(
    `Quads: ${m.quadsProcessed} | ` +
      `Bytes: ${(m.bytesProcessed / 1024).toFixed(1)} KB | ` +
      `Avg throughput: ${m.averageThroughput.toFixed(0)} quads/sec | ` +
      `Duration: ${m.duration}ms`
  );
}, 2000);

// ... do work ...

clearInterval(ticker);
monitor.stop();
```

`getCurrentMetrics()` fields:

| Field                | Type   | Description                  |
| -------------------- | ------ | ---------------------------- |
| `quadsProcessed`     | number | Total quads recorded         |
| `bytesProcessed`     | number | Total bytes recorded         |
| `chunksProcessed`    | number | Total chunks recorded        |
| `errorsCount`        | number | Total errors recorded        |
| `backpressureEvents` | number | Total backpressure events    |
| `duration`           | number | Elapsed ms since `start()`   |
| `averageThroughput`  | number | quads/sec over full duration |

---

## Get a Full Performance Report

`getReport()` includes statistical summaries (mean, min, max, p50, p95, p99) for throughput, latency, and memory:

```javascript
monitor.stop();
const report = monitor.getReport();

console.log('=== Throughput ===');
console.log('Mean:', report.throughput.mean.toFixed(0), report.throughput.unit);
console.log('P95: ', report.throughput.p95.toFixed(0), report.throughput.unit);

console.log('=== Latency ===');
console.log('Mean:', report.latency.mean.toFixed(2), report.latency.unit);
console.log('P99: ', report.latency.p99.toFixed(2), report.latency.unit);

console.log('=== Memory ===');
console.log('Heap mean:', (report.memory.heapUsed.mean / 1024 / 1024).toFixed(1), 'MB');
console.log('Heap max: ', (report.memory.heapUsed.max / 1024 / 1024).toFixed(1), 'MB');

console.log('=== Errors ===');
console.log('Count:', report.errors.count);
report.errors.recent.forEach(e => console.log('  -', e.message));

console.log('=== Backpressure ===');
console.log('Events:', report.backpressure.events);
console.log('Rate:  ', report.backpressure.rate.toFixed(3), 'events/chunk');
```

---

## Set Threshold Alerts

Configure thresholds to emit `'threshold-violation'` events in real time:

```javascript
const monitor = createPerformanceMonitor({
  sampleInterval: 500,
  thresholds: {
    throughputMin: 5000, // alert if throughput drops below 5000 quads/sec
    memoryMax: 512 * 1024 * 1024, // alert if heap exceeds 512 MB
    // latencyMax is not yet implemented in the threshold checker
  },
});

monitor.on('threshold-violation', violation => {
  console.warn(
    `ALERT: ${violation.metric} = ${violation.value.toFixed(1)} ` +
      `(threshold: ${violation.threshold})`
  );
});

monitor.start();
// ... streaming work ...
monitor.stop();
```

---

## Record Errors and Backpressure

```javascript
monitor.recordError(new Error('Failed to write quad batch'));
monitor.recordBackpressure(); // call when the consumer signals it cannot keep up
```

After `stop()`, `report.errors.recent` contains the last 10 error objects with `message` and `stack`.

---

## Reset and Reuse

```javascript
monitor.stop();
const firstReport = monitor.getReport();

// Start a fresh measurement
monitor.reset();
monitor.start();
// ... second streaming task ...
monitor.stop();
const secondReport = monitor.getReport();
```

`reset()` clears all metrics arrays and counters but leaves configuration intact.

## See Also

- [How-To: Handle Backpressure](./02-handle-backpressure.md)
- [Reference: Performance Monitor API](../reference/performance-monitor-api.md)
- [Explanation: Backpressure and Flow Control](../explanation/02-backpressure-and-flow-control.md)
